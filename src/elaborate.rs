use crate::term::*;

pub fn elab_mod(m: &[PreItem], bindings: &mut Bindings) -> Result<Vec<Item>, Error> {
    let mut cxt = Cxt::new(bindings);
    let mut v = Vec::new();

    // Declare
    for i in m {
        match cxt.declare_item(i) {
            Ok(()) => (),
            Err(e) => return Err(e.to_error(bindings)),
        }
    }

    // Define
    for i in m {
        match cxt.check_item(i) {
            Ok(x) => {
                v.push(x);
            }
            Err(e) => return Err(e.to_error(bindings)),
        }
    }

    v.append(&mut cxt.extra_items);

    Ok(v)
}

#[derive(Debug, Clone)]
struct FnType(Vec<Type>, Type);

/// Implements scoping with a Vec instead of a HashMap. We search from the back, so we can use it like a stack.
struct Env<S, T> {
    symbols: Vec<(RawSym, S, T)>,
    /// Stores the length of `symbols` where we left off in the enclosing scope
    /// That way, we don't have to do anything extra when we add to `symbols`, and we can just `resize()` it when we `pop()` the scope
    scopes: Vec<usize>,
}
impl<S: Copy, T> Env<S, T> {
    fn new() -> Self {
        Env {
            symbols: Vec::new(),
            scopes: Vec::new(),
        }
    }

    /// Start a new scope
    fn push(&mut self) {
        self.scopes.push(self.symbols.len())
    }
    /// End the most recent scope
    fn pop(&mut self) {
        let n = self.scopes.pop().expect("Cxt::pop() without a push()!");
        self.symbols.resize_with(n, || {
            panic!("Env::pop() got a scope longer than `symbols`!")
        });
    }

    fn get(&self, s: RawSym) -> Option<(S, &T)> {
        self.symbols
            .iter()
            .rfind(|(x, _, _)| *x == s)
            .map(|(_, x, t)| (*x, t))
    }

    fn add(&mut self, k: RawSym, s: S, t: T) {
        self.symbols.push((k, s, t));
    }
}

struct ClassInfo {
    methods: Vec<(RawSym, FnId, FnType)>,
    variants: Option<Vec<RawSym>>,
}
impl ClassInfo {
    fn new_class(methods: Vec<(RawSym, FnId, FnType)>) -> Self {
        ClassInfo {
            methods,
            variants: None,
        }
    }

    fn new_enum(variants: Vec<RawSym>) -> Self {
        ClassInfo {
            methods: Vec::new(),
            variants: Some(variants),
        }
    }
}

struct Cxt<'b> {
    vars: Env<Sym, Type>,
    fns: Env<FnId, FnType>,
    classes: Env<TypeId, ClassInfo>,
    ret_tys: Vec<Option<Type>>,
    bindings: &'b mut Bindings,
    extra_items: Vec<Item>,
}
impl<'b> Cxt<'b> {
    fn new(bindings: &'b mut Bindings) -> Self {
        Cxt {
            vars: Env::new(),
            fns: Env::new(),
            classes: Env::new(),
            ret_tys: Vec::new(),
            bindings,
            extra_items: Vec::new(),
        }
    }

    fn var(&self, s: RawSym) -> Option<(Sym, &Type)> {
        self.vars.get(s)
    }
    fn fun(&self, s: RawSym) -> Option<(FnId, &FnType)> {
        self.fns.get(s)
    }
    fn class(&self, s: RawSym) -> Option<TypeId> {
        self.classes.get(s).map(|(x, _)| x)
    }
    fn class_info(&self, class: TypeId) -> &ClassInfo {
        &self
            .classes
            .symbols
            .iter()
            .find(|(_, x, _)| *x == class)
            .unwrap()
            .2
    }

    /// Returns the return type of the innermost function
    fn ret_ty(&self) -> Type {
        self.ret_tys
            .iter()
            .rfind(|x| x.is_some())
            .unwrap()
            .clone()
            .unwrap()
    }

    /// Start a new scope
    fn push(&mut self, rty: Option<Type>) {
        self.vars.push();
        self.fns.push();
        self.ret_tys.push(rty);
    }
    /// End the most recent scope
    fn pop(&mut self) {
        self.vars.pop();
        self.fns.pop();
        self.ret_tys.pop();
    }

    /// Creates a new binding with a name
    fn create(&mut self, k: RawSym, ty: Type, public: bool) -> Sym {
        let s = self.bindings.create(k, public);
        self.vars.add(k, s, ty);
        s
    }

    fn create_fn(&mut self, k: Spanned<RawSym>, ty: FnType) -> Result<FnId, TypeError> {
        if self.fun(*k).is_some() {
            return Err(TypeError::Duplicate(k.span, *k));
        }
        let s = self.bindings.add_fn(*k);
        self.fns.add(*k, s, ty);
        Ok(s)
    }

    fn create_class(&mut self, k: RawSym, info: ClassInfo) -> TypeId {
        let s = self.bindings.add_type(k);
        self.classes.add(k, s, info);
        s
    }
}

enum TypeError {
    NotFound(Span, RawSym),
    /// Unify(span, found, expected)
    Unify(Span, Type, Type),
    WrongArity(Span, usize, usize),
    NoMethods(Span, Type),
    NoVariants(Span, Type),
    MissingPattern(Span, Vec<RawSym>),
    Duplicate(Span, RawSym),
}
impl TypeError {
    fn to_error(self, bindings: &Bindings) -> Error {
        match self {
            TypeError::NotFound(span, raw) => Spanned::new(
                Doc::start("Name not found: ").add(bindings.resolve_raw(raw)),
                span,
            ),
            TypeError::Unify(span, ity, ety) => Spanned::new(
                Doc::start("Type mismatch: expected ").chain(
                    ety.pretty(bindings)
                        .add(" but found ")
                        .chain(ity.pretty(bindings)),
                ),
                span,
            ),
            TypeError::WrongArity(span, ia, ea) => Spanned::new(
                Doc::start("Wrong number of arguments passed to function: expected ")
                    .add(ea)
                    .add(" but found ")
                    .add(ia)
                    .add(if ia == 1 { " argument" } else { " arguments" }),
                span,
            ),
            TypeError::NoMethods(span, ty) => Spanned::new(
                Doc::start("Value of type ")
                    .chain(ty.pretty(bindings))
                    .add(" doesn't have methods"),
                span,
            ),
            TypeError::NoVariants(span, ty) => Spanned::new(
                Doc::start("Value of type ")
                    .chain(ty.pretty(bindings))
                    .add(" doesn't have variants and can't be matched on"),
                span,
            ),
            TypeError::MissingPattern(span, missing) => Spanned::new(
                Doc::start("Inexhaustive match: missing patterns: ").chain(Doc::intersperse(
                    missing.iter().map(|x| Doc::start(bindings.resolve_raw(*x))),
                    Doc::start(','),
                )),
                span,
            ),
            TypeError::Duplicate(span, name) => Spanned::new(
                Doc::start("Duplicate definition of item named '")
                    .add(bindings.resolve_raw(name))
                    .add("'"),
                span,
            ),
        }
    }
}

impl<'b> Cxt<'b> {
    fn elab_type(&mut self, ty: &PreType) -> Result<Type, TypeError> {
        match ty {
            PreType::I32 => Ok(Type::I32),
            PreType::I64 => Ok(Type::I64),
            PreType::Bool => Ok(Type::Bool),
            PreType::Str => Ok(Type::Str),
            PreType::Tuple(v) if v.is_empty() => Ok(Type::Unit),
            PreType::Class(name) => self
                .class(**name)
                .map(Type::Class)
                .ok_or(TypeError::NotFound(name.span, **name)),
            PreType::Tuple(v) => v
                .iter()
                .map(|x| self.elab_type(x))
                .collect::<Result<Vec<_>, _>>()
                .map(Type::Tuple),
            PreType::Array(t) => Ok(Type::Array(Box::new(self.elab_type(t)?))),
        }
    }

    fn declare_item(&mut self, item: &PreItem) -> Result<(), TypeError> {
        match item {
            PreItem::InlineJava(_) => Ok(()),
            PreItem::Fn(f) => {
                let mut args = Vec::new();
                for (_s, t, _) in &f.args {
                    let t = self.elab_type(t)?;
                    args.push(t);
                }
                let rty = self.elab_type(&f.ret_ty)?;
                self.create_fn(f.name, FnType(args, rty))?;
                Ok(())
            }
            PreItem::ExternFn(f) => {
                let mut args = Vec::new();
                for (_s, t, _) in &f.args {
                    let t = self.elab_type(t)?;
                    args.push(t);
                }
                let rty = self.elab_type(&f.ret_ty)?;
                self.create_fn(f.name, FnType(args, rty))?;
                Ok(())
            }
            PreItem::Let(name, ty, x, public) => {
                if self.var(**name).is_some() {
                    return Err(TypeError::Duplicate(name.span, **name));
                }
                let ty = match ty {
                    Some(ty) => self.elab_type(ty)?,
                    None => self.infer(x)?.1,
                };
                self.create(**name, ty, *public);
                Ok(())
            }
            PreItem::ExternClass(name, methods) => {
                let methods = methods
                    .iter()
                    .map(|f| {
                        let mut args = Vec::new();
                        let mut args2 = Vec::new();
                        for (s, t, _) in &f.args {
                            let t = self.elab_type(t)?;
                            args.push(t.clone());
                            args2.push((self.bindings.create(*s, true), t));
                        }
                        let rty = self.elab_type(&f.ret_ty)?;
                        let ty = FnType(args, rty.clone());
                        let id = self.bindings.add_fn(*f.name);
                        // Make sure the mapping gets through to the backend.
                        // This technically has the wrong type since it doesn't include the object,
                        // but it doesn't matter because the function is only accessible as a method.
                        self.extra_items.push(Item::ExternFn(ExternFn {
                            id,
                            ret_ty: rty,
                            args: args2,
                            mapping: f.mapping,
                        }));

                        Ok((*f.name, id, ty))
                    })
                    .collect::<Result<_, _>>()?;
                self.create_class(*name, ClassInfo::new_class(methods));
                Ok(())
            }
            PreItem::Enum(name, variants, _ext) => {
                self.create_class(*name, ClassInfo::new_enum(variants.clone()));
                Ok(())
            }
        }
    }

    fn check_item(&mut self, item: &PreItem) -> Result<Item, TypeError> {
        match item {
            PreItem::InlineJava(s) => Ok(Item::InlineJava(*s)),
            PreItem::Fn(f) => {
                let PreFn {
                    name,
                    ret_ty: _,
                    args,
                    body,
                    public,
                } = f;
                let (fid, fty) = self.fun(**name).unwrap();
                let FnType(atys, rty) = fty.clone();

                self.push(Some(rty.clone()));
                let mut args2 = Vec::new();
                for ((a, _, public), t) in args.iter().zip(atys) {
                    let a = self.create(*a, t.clone(), *public);
                    args2.push((a, t));
                }
                let body = self.check(body, rty.clone())?;
                self.pop();

                Ok(Item::Fn(Fn {
                    id: fid,
                    ret_ty: rty,
                    args: args2,
                    public: *public,
                    body,
                }))
            }
            PreItem::ExternFn(f) => {
                let PreEFn {
                    name,
                    ret_ty: _,
                    args,
                    mapping,
                } = f;
                let (fid, fty) = self.fun(**name).unwrap();
                let FnType(atys, rty) = fty.clone();

                self.push(Some(rty.clone()));
                let mut args2 = Vec::new();
                for ((a, _, public), t) in args.iter().zip(atys) {
                    let a = self.create(*a, t.clone(), *public);
                    args2.push((a, t));
                }
                // let body = self.check(body, rty.clone())?;
                self.pop();

                Ok(Item::ExternFn(ExternFn {
                    id: fid,
                    ret_ty: rty,
                    args: args2,
                    mapping: *mapping,
                }))
            }
            PreItem::Let(name, _, x, _) => {
                let (s, t) = self.var(**name).unwrap();
                let t = t.clone();
                let x = self.check(x, t.clone())?;
                Ok(Item::Let(s, t, x))
            }
            PreItem::ExternClass(s, _) => Ok(Item::ExternClass(self.class(*s).unwrap())),
            PreItem::Enum(name, variants, ext) => Ok(Item::Enum(
                self.class(*name).unwrap(),
                variants.clone(),
                *ext,
            )),
        }
    }

    fn check_stmt(&mut self, stmt: &PreStatement) -> Result<Option<Statement>, TypeError> {
        match stmt {
            PreStatement::Item(
                item
                @
                (PreItem::Fn(_)
                | PreItem::ExternFn(_)
                | PreItem::ExternClass(_, _)
                | PreItem::Enum(_, _, _)),
            ) => {
                self.declare_item(item)?;
                let item = self.check_item(item)?;
                self.extra_items.push(item);
                Ok(None)
            }
            PreStatement::Item(PreItem::InlineJava(s)) => Ok(Some(Statement::InlineJava(*s))),
            PreStatement::Item(PreItem::Let(name, ty, value, public)) => {
                let (x, t) = match ty {
                    Some(t) => {
                        let t = self.elab_type(t)?;
                        let x = self.check(value, t.clone())?;
                        (x, t)
                    }
                    None => self.infer(value)?,
                };
                let n = self.create(**name, t.clone(), *public);
                Ok(Some(Statement::Let(n, t, x)))
            }
            PreStatement::Term(t) => self.infer(t).map(|(x, _)| Some(Statement::Term(x))),
            PreStatement::While(cond, block) => {
                let cond = self.check(cond, Type::Bool)?;
                let mut block2 = Vec::new();
                for i in block {
                    self.check_stmt(i)?.map(|x| block2.push(x));
                }

                Ok(Some(Statement::While(cond, block2)))
            }
            PreStatement::For(s, public, a, b, block) => {
                let (iter, t) = match b {
                    // Range
                    Some(b) => {
                        let a = self.check(a, Type::I32)?;
                        let b = self.check(b, Type::I32)?;
                        (ForIter::Range(Box::new(a), Box::new(b)), Type::I32)
                    }
                    // Array
                    None => {
                        let (a, t) = self.infer(a)?;
                        match t {
                            Type::Array(t) => (ForIter::Array(Box::new(a)), *t),
                            _ => todo!("error: not array"),
                        }
                    }
                };
                let n = self.create(*s, t, *public);
                let mut block2 = Vec::new();
                for i in block {
                    self.check_stmt(i)?.map(|x| block2.push(x));
                }
                Ok(Some(Statement::For(n, iter, block2)))
            }
        }
    }

    fn infer(&mut self, pre: &SPre) -> Result<(Term, Type), TypeError> {
        match &***pre {
            Pre::Var(raw) => self
                .var(*raw)
                .map(|(s, t)| (Term::Var(s), t.clone()))
                .ok_or(TypeError::NotFound(pre.span, *raw)),
            Pre::Lit(l, t) => match l {
                Literal::Int(_) => match t {
                    Some(t) => {
                        let t = self.elab_type(t)?;
                        Ok((Term::Lit(*l, t.clone()), t))
                    }
                    // Default to i32
                    None => Ok((Term::Lit(*l, Type::I32), Type::I32)),
                },
                Literal::Str(_) => Ok((Term::Lit(*l, Type::Str), Type::Str)),
                Literal::Bool(_) => Ok((Term::Lit(*l, Type::Bool), Type::Bool)),
            },
            // These default to (), but can be coerced to any type - see check()
            Pre::Break => Ok((Term::Break, Type::Unit)),
            Pre::Continue => Ok((Term::Continue, Type::Unit)),
            Pre::Return(x) => {
                let rty = self.ret_ty();
                if x.is_none() && rty != Type::Unit {
                    return Err(TypeError::Unify(pre.span, Type::Unit, rty));
                }
                let x = x
                    .as_ref()
                    .map(|x| self.check(x, rty))
                    .transpose()?
                    .map(Box::new);
                Ok((Term::Return(x), Type::Unit))
            }
            Pre::Variant(a, b) => {
                let class = self
                    .class(**a)
                    .ok_or(TypeError::NotFound(a.span, a.inner))?;
                let variants = self.class_info(class).variants.as_ref();
                if variants.map_or(true, |v| v.iter().all(|x| *x != **b)) {
                    return Err(TypeError::NotFound(b.span, b.inner));
                }

                Ok((Term::Variant(class, **b), Type::Class(class)))
            }
            Pre::Tuple(v) => {
                let mut terms = Vec::new();
                let mut tys = Vec::new();
                for i in v {
                    let (term, ty) = self.infer(i)?;
                    terms.push(term);
                    tys.push(ty);
                }
                Ok((Term::Tuple(terms), Type::Tuple(tys)))
            }
            Pre::TupleIdx(x, i) => {
                let (x, t) = self.infer(x)?;
                let t = match t {
                    Type::Tuple(mut v) => {
                        if *i < v.len() {
                            v.swap_remove(*i)
                        } else {
                            todo!("out of bounds error")
                        }
                    }
                    _ => todo!("not tuple error"),
                };
                Ok((Term::TupleIdx(Box::new(x), *i), t))
            }
            Pre::Array(v) => {
                let mut ty = None;
                let mut v2 = Vec::new();
                for i in v {
                    match ty.clone() {
                        None => {
                            let (i, t) = self.infer(i)?;
                            v2.push(i);
                            ty = Some(t);
                        }
                        Some(ty) => {
                            let i = self.check(i, ty)?;
                            v2.push(i);
                        }
                    }
                }
                // Make an empty array default to [()]
                let ty = ty.unwrap_or(Type::Unit);
                Ok((Term::Array(v2, ty.clone()), Type::Array(Box::new(ty))))
            }
            Pre::ArrayIdx(arr, idx) => {
                let (arr, aty) = self.infer(arr)?;
                let ty = match aty {
                    Type::Array(t) => *t,
                    _ => todo!("not an array error"),
                };
                let idx = self.check(idx, Type::I32)?;
                Ok((Term::ArrayIdx(Box::new(arr), Box::new(idx)), ty))
            }
            Pre::Set(l, op, x) => {
                let (l, t) = self.infer(l)?;
                let l = match l {
                    Term::Var(s) => LValue::Var(s),
                    Term::ArrayIdx(b, i) if matches!(&*b, Term::Var(_)) => match *b {
                        Term::Var(s) => LValue::Idx(s, i),
                        _ => unreachable!(),
                    },
                    _ => todo!("error: not an lvalue"),
                };
                let x = self.check(x, t)?;

                Ok((Term::Set(l, *op, Box::new(x)), Type::Unit))
            }
            Pre::Call(f, a) => {
                let (fid, FnType(atys, rty)) =
                    self.fun(**f).ok_or(TypeError::NotFound(f.span, **f))?;
                let rty = rty.clone();
                if a.len() != atys.len() {
                    return Err(TypeError::WrongArity(pre.span, a.len(), atys.len()));
                }
                let mut a2 = Vec::new();
                for (a, t) in a.iter().zip(atys.clone()) {
                    a2.push(self.check(a, t)?);
                }
                Ok((Term::Call(None, fid, a2), rty))
            }
            Pre::Method(o_, f, a) => {
                let (o, t) = self.infer(o_)?;
                match t {
                    Type::Class(c) => {
                        let methods = &self.class_info(c).methods;
                        let (_, fid, FnType(atys, rty)) = methods
                            .iter()
                            .find(|(s, _, _)| *s == **f)
                            .ok_or(TypeError::NotFound(f.span, f.inner))?;
                        let fid = *fid;

                        let rty = rty.clone();
                        if a.len() != atys.len() {
                            return Err(TypeError::WrongArity(pre.span, a.len(), atys.len()));
                        }
                        let mut a2 = Vec::new();
                        for (a, t) in a.iter().zip(atys.clone()) {
                            a2.push(self.check(a, t)?);
                        }
                        Ok((Term::Call(Some(Box::new(o)), fid, a2), rty))
                    }
                    Type::Array(t) => match self.bindings.resolve_raw(**f) {
                        "len" => {
                            if a.len() != 0 {
                                return Err(TypeError::WrongArity(pre.span, a.len(), 0));
                            }
                            Ok((Term::ArrayMethod(Box::new(o), ArrayMethod::Len), Type::I32))
                        }
                        "pop" => {
                            if a.len() != 0 {
                                return Err(TypeError::WrongArity(pre.span, a.len(), 0));
                            }
                            Ok((Term::ArrayMethod(Box::new(o), ArrayMethod::Pop), *t))
                        }
                        "push" => {
                            if a.len() != 1 {
                                return Err(TypeError::WrongArity(pre.span, a.len(), 1));
                            }
                            let x = self.check(&a[0], *t)?;
                            Ok((
                                Term::ArrayMethod(Box::new(o), ArrayMethod::Push(Box::new(x))),
                                Type::Unit,
                            ))
                        }
                        _ => return Err(TypeError::NotFound(f.span, f.inner)),
                    },
                    t => return Err(TypeError::NoMethods(o_.span, t)),
                }
            }
            Pre::BinOp(op, a, b) => {
                let (a, bt, rt) = match op.ty() {
                    BinOpType::Comp => {
                        let (a, t) = self.infer(a)?;
                        (a, t, Type::Bool)
                    }
                    BinOpType::Arith => {
                        let (a, t) = self.infer(a)?;
                        (a, t.clone(), t)
                    }
                    BinOpType::Logic => (self.check(a, Type::Bool)?, Type::Bool, Type::Bool),
                };
                let b = self.check(b, bt)?;
                Ok((Term::BinOp(*op, Box::new(a), Box::new(b)), rt))
            }
            Pre::Block(v, e) => {
                self.push(None);

                let mut v2 = Vec::new();
                for i in v {
                    if let Some(x) = self.check_stmt(i)? {
                        v2.push(x);
                    }
                }
                match e {
                    Some(e) => {
                        let (e, t) = self.infer(e)?;

                        self.pop();
                        Ok((Term::Block(v2, Some(Box::new(e))), t))
                    }
                    None => {
                        self.pop();
                        Ok((Term::Block(v2, None), Type::Unit))
                    }
                }
            }
            Pre::If(cond, yes, no) => {
                let cond = self.check(cond, Type::Bool)?;
                let (yes, ty) = self.infer(yes)?;
                let no = no
                    .as_ref()
                    .map(|no| self.check(no, ty.clone()))
                    .transpose()?
                    .map(Box::new);

                Ok((Term::If(Box::new(cond), Box::new(yes), no), ty))
            }
            Pre::Match(x, branches) => {
                let xspan = x.span;
                let (x, xty) = self.infer(x)?;

                let variants = match &xty {
                    Type::Class(tid) => self
                        .class_info(*tid)
                        .variants
                        .as_ref()
                        .ok_or(TypeError::NoVariants(xspan, xty))?,
                    _ => return Err(TypeError::NoVariants(xspan, xty)),
                };
                let mut covered: Vec<_> = variants.iter().map(|x| (*x, false)).collect();

                let mut v = Vec::new();
                let mut rty = None;
                let mut had_default = false;
                for (s, t) in branches {
                    let t = match &rty {
                        None => {
                            let (t, ty) = self.infer(t)?;
                            rty = Some(ty);
                            t
                        }
                        Some(rty) => self.check(t, rty.clone())?,
                    };
                    if let Some(s2) = **s {
                        let (_, b) = covered
                            .iter_mut()
                            .find(|(x, _)| *x == s2)
                            .ok_or(TypeError::NotFound(s.span, s2))?;
                        if *b {
                            Spanned::new(
                                Doc::start("Duplicate branch for pattern ")
                                    .add(self.bindings.resolve_raw(s2)),
                                s.span,
                            )
                            .emit(Severity::Warning);
                        } else {
                            *b = true;
                        }
                    } else {
                        if had_default {
                            Spanned::new(Doc::start("Duplicate default branch in pattern match, this one is unreachable"),
                                s.span,
                            ).emit(Severity::Warning);
                            continue;
                        } else {
                            had_default = true;
                        }
                    }
                    v.push((**s, t));
                }

                if !had_default {
                    let mut missing = Vec::new();
                    for (s, b) in covered {
                        if !b {
                            missing.push(s);
                        }
                    }
                    if !missing.is_empty() {
                        return Err(TypeError::MissingPattern(xspan, missing));
                    }
                }

                Ok((Term::Match(Box::new(x), v), rty.unwrap()))
            }
        }
    }

    fn check(&mut self, pre: &SPre, ty: Type) -> Result<Term, TypeError> {
        match (&***pre, &ty) {
            (Pre::BinOp(op, a, b), _) if op.ty() == BinOpType::Arith => {
                let a = self.check(a, ty.clone())?;
                let b = self.check(b, ty)?;
                Ok(Term::BinOp(*op, Box::new(a), Box::new(b)))
            }
            (Pre::Lit(l @ Literal::Int(_), None), Type::I32 | Type::I64) => Ok(Term::Lit(*l, ty)),

            (Pre::Tuple(a), Type::Tuple(b)) => a
                .iter()
                .zip(b)
                .map(|(x, ty)| self.check(x, ty.clone()))
                .collect::<Result<Vec<_>, _>>()
                .map(Term::Tuple),

            (Pre::Array(v), Type::Array(t)) => {
                let mut v2 = Vec::new();
                for i in v {
                    v2.push(self.check(i, (**t).clone())?);
                }
                Ok(Term::Array(v2, (**t).clone()))
            }

            // These technically return the never type `!`, but that's too complicated for bytec
            // Instead, they just coerce to anything they're checked against, but default to ()
            (Pre::Break, _) => Ok(Term::Break),
            (Pre::Continue, _) => Ok(Term::Continue),
            (Pre::Return(x), _) => {
                let rty = self.ret_ty();
                if x.is_none() && rty != Type::Unit {
                    return Err(TypeError::Unify(pre.span, Type::Unit, rty));
                }
                let x = x
                    .as_ref()
                    .map(|x| self.check(x, rty))
                    .transpose()?
                    .map(Box::new);
                Ok(Term::Return(x))
            }

            _ => {
                let (term, ity) = self.infer(pre)?;
                if ty == ity {
                    Ok(term)
                } else {
                    Err(TypeError::Unify(pre.span, ity, ty))
                }
            }
        }
    }
}
