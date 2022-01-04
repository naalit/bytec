use std::collections::HashMap;

use crate::term::*;

pub fn declare_mod_p1(
    m: &[PreItem],
    bindings: &mut Bindings,
    file_id: FileId,
) -> Result<(ModType, Vec<Item>), Error> {
    let mut cxt = Cxt::new(bindings, file_id);

    // Declare
    for i in m {
        match cxt.declare_item_p1(i) {
            Ok(()) => (),
            Err(e) => return Err(e.to_error(bindings)),
        }
    }

    let Cxt {
        vars,
        fns,
        classes,
        local_classes,
        extra_items,
        ..
    } = cxt;

    Ok((
        ModType {
            vars: vars.symbols,
            fns: fns.symbols,
            classes,
            local_classes,
        },
        extra_items,
    ))
}

pub fn declare_mod_p2(
    m: &[PreItem],
    t: ModType,
    mods: &[(RawSym, ModType)],
    extra_items: Vec<Item>,
    bindings: &mut Bindings,
    file_id: FileId,
) -> Result<(ModType, Vec<Item>), Error> {
    let mut cxt = Cxt::from_type(
        t,
        mods.into_iter().cloned().collect(),
        extra_items,
        bindings,
        file_id,
    );

    // Declare
    for i in m {
        match cxt.declare_item_p2(i) {
            Ok(()) => (),
            Err(e) => return Err(e.to_error(bindings)),
        }
    }

    let Cxt {
        vars,
        fns,
        classes,
        local_classes,
        extra_items,
        ..
    } = cxt;

    Ok((
        ModType {
            vars: vars.symbols,
            fns: fns.symbols,
            classes,
            local_classes,
        },
        extra_items,
    ))
}

pub fn declare_mod_p3(
    m: &[PreItem],
    t: ModType,
    mods: &[(RawSym, ModType)],
    extra_items: Vec<Item>,
    bindings: &mut Bindings,
    file_id: FileId,
) -> Result<(ModType, Vec<Item>), Error> {
    let mut cxt = Cxt::from_type(
        t,
        mods.into_iter().cloned().collect(),
        extra_items,
        bindings,
        file_id,
    );

    // Declare
    for i in m {
        match cxt.declare_item_p3(i) {
            Ok(()) => (),
            Err(e) => return Err(e.to_error(bindings)),
        }
    }

    let Cxt {
        vars,
        fns,
        classes,
        local_classes,
        extra_items,
        ..
    } = cxt;

    Ok((
        ModType {
            vars: vars.symbols,
            fns: fns.symbols,
            classes,
            local_classes,
        },
        extra_items,
    ))
}

pub fn declare_mod_p4(
    m: &[PreItem],
    t: ModType,
    mods: &[(RawSym, ModType)],
    extra_items: Vec<Item>,
    bindings: &mut Bindings,
    file_id: FileId,
) -> Result<(ModType, Vec<Item>), Error> {
    let mut cxt = Cxt::from_type(
        t,
        mods.into_iter().cloned().collect(),
        extra_items,
        bindings,
        file_id,
    );

    // Declare
    for i in m {
        match cxt.declare_item_p4(i) {
            Ok(()) => (),
            Err(e) => return Err(e.to_error(bindings)),
        }
    }

    let Cxt {
        vars,
        fns,
        classes,
        local_classes,
        extra_items,
        ..
    } = cxt;

    Ok((
        ModType {
            vars: vars.symbols,
            fns: fns.symbols,
            classes,
            local_classes,
        },
        extra_items,
    ))
}

pub fn elab_mod(
    m: &[PreItem],
    t: ModType,
    mods: &[(RawSym, ModType)],
    extra_items: Vec<Item>,
    bindings: &mut Bindings,
    file_id: FileId,
) -> Result<Vec<Item>, Error> {
    let mut cxt = Cxt::from_type(
        t,
        mods.into_iter().cloned().collect(),
        extra_items,
        bindings,
        file_id,
    );
    let mut v = Vec::new();

    // Define
    for i in m {
        match cxt.check_item(i) {
            Ok(x) => {
                for x in x {
                    v.push(x);
                }
            }
            Err(e) => return Err(e.to_error(bindings)),
        }
    }

    v.append(&mut cxt.extra_items);

    Ok(v)
}

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

    fn from_vec(symbols: Vec<(RawSym, S, T)>) -> Self {
        Env {
            symbols,
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

struct Cxt<'b> {
    vars: Env<Sym, Type>,
    fns: Env<FnId, FnType>,
    local_classes: HashMap<RawSym, TypeId>,
    classes: HashMap<RawPath, (TypeId, ClassInfo)>,
    ret_tys: Vec<Option<Type>>,
    bindings: &'b mut Bindings,
    mods: HashMap<RawSym, ModType>,
    extra_items: Vec<Item>,
    file_id: FileId,
}
impl<'b> Cxt<'b> {
    fn new(bindings: &'b mut Bindings, file_id: FileId) -> Self {
        Cxt {
            vars: Env::new(),
            fns: Env::new(),
            local_classes: HashMap::new(),
            classes: HashMap::new(),
            ret_tys: Vec::new(),
            bindings,
            mods: HashMap::new(),
            extra_items: Vec::new(),
            file_id,
        }
    }

    fn from_type(
        ModType {
            vars,
            fns,
            local_classes,
            classes,
        }: ModType,
        mods: HashMap<RawSym, ModType>,
        extra_items: Vec<Item>,
        bindings: &'b mut Bindings,
        file_id: FileId,
    ) -> Self {
        Cxt {
            vars: Env::from_vec(vars),
            fns: Env::from_vec(fns),
            local_classes,
            classes,
            ret_tys: Vec::new(),
            bindings,
            mods,
            extra_items,
            file_id,
        }
    }

    fn var(&self, s: &RawPath) -> Option<(Sym, &Type)> {
        if s.len() == 1 {
            self.vars.get(*s.stem())
        } else if s.len() == 2 {
            let module = self.module(**s.0.first().unwrap())?;
            module
                .vars
                .iter()
                .rfind(|(x, _, _)| *x == *s.stem())
                .map(|(_, s, t)| (*s, t))
        } else {
            None
        }
    }
    fn fun(&self, s: &RawPath) -> Option<(FnId, &FnType)> {
        if s.len() == 1 {
            self.fns.get(*s.stem())
        } else if s.len() == 2 {
            let module = self.module(**s.0.first().unwrap())?;
            module
                .fns
                .iter()
                .rfind(|(x, _, _)| *x == *s.stem())
                .map(|(_, s, t)| (*s, t))
        } else {
            None
        }
    }
    fn class(&self, s: &RawPath) -> Option<TypeId> {
        let mut s = s.clone();
        if s.len() == 1 {
            if let Some(t) = self.local_classes.get(&*s.stem()) {
                return Some(*t);
            }
            s = self.path(s.1);
        }
        if let Some(x) = self.classes.get(&s).map(|(x, _)| x) {
            Some(*x)
        } else if s.len() == 2 {
            let module = self.module(**s.0.first().unwrap())?;
            module.classes.get(&s).map(|(s, _)| *s)
        } else {
            None
        }
    }
    fn module(&self, s: RawSym) -> Option<&ModType> {
        self.mods.get(&s)
    }
    fn class_info(&self, class: TypeId) -> &ClassInfo {
        self.classes
            .values()
            .find(|(x, _)| *x == class)
            .map(|(_, i)| i)
            .or_else(|| {
                for (_, m) in &self.mods {
                    for (_, (tid, info)) in &m.classes {
                        if *tid == class {
                            return Some(info);
                        }
                    }
                }
                None
            })
            .unwrap()
    }
    fn class_info_mut(&mut self, class: TypeId) -> &mut ClassInfo {
        self.classes
            .values_mut()
            .find(|(x, _)| *x == class)
            .map(|(_, i)| i)
            .or_else(|| {
                for (_, m) in &mut self.mods {
                    for (_, (tid, info)) in &mut m.classes {
                        if *tid == class {
                            return Some(info);
                        }
                    }
                }
                None
            })
            .unwrap()
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

    fn path(&self, n: Spanned<RawSym>) -> RawPath {
        RawPath(vec![Spanned::new(self.file_id.1, Span(0, 0))], n)
    }

    /// Creates a new binding with a name
    fn create(&mut self, k: Spanned<RawSym>, ty: Type, public: bool) -> Sym {
        let s = self.bindings.create(self.path(k), public);
        self.vars.add(*k, s, ty);
        s
    }

    fn create_fn(&mut self, k: Spanned<RawSym>, ty: FnType) -> Result<FnId, TypeError> {
        if self.fun(&lpath(k)).is_some() {
            return Err(TypeError::Duplicate(k.span, *k));
        }
        let s = self.bindings.add_fn(self.path(k));
        self.fns.add(*k, s, ty);
        Ok(s)
    }

    fn create_class(&mut self, mut k: RawPath, info: ClassInfo) -> TypeId {
        if k.len() == 1 {
            k = self.path(k.1);
        }
        let s = self.bindings.add_type(k.clone());
        self.classes.insert(k, (s, info));
        s
    }
}

enum TypeError {
    NotFound(RawPath),
    /// Unify(span, found, expected)
    Unify(Span, Type, Type),
    WrongArity(Span, usize, usize),
    NoMethods(Span, Type),
    NoVariants(Span, Type),
    MissingPattern(Span, Vec<RawSym>),
    Duplicate(Span, RawSym),
    NotArray(Span, Type),
    NotTuple(Span, Type),
    TupleOutOfBounds(Span, Type, usize),
    NotLValue(Span, Term),
}
impl TypeError {
    fn to_error(self, bindings: &Bindings) -> Error {
        match self {
            TypeError::NotFound(path) => Spanned::new(
                Doc::start("Name not found: ").chain(path.pretty(bindings)),
                path.span(),
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
            TypeError::NotArray(span, t) => Spanned::new(
                Doc::start("Expected an array, got value of type ").chain(t.pretty(bindings)),
                span,
            ),
            TypeError::NotTuple(span, t) => Spanned::new(
                Doc::start("Expected a tuple, got value of type ").chain(t.pretty(bindings)),
                span,
            ),
            TypeError::TupleOutOfBounds(span, t, i) => Spanned::new(
                Doc::start("Tuple index ")
                    .add(i)
                    .add(" out of bounds for type ")
                    .chain(t.pretty(bindings)),
                span,
            ),
            TypeError::NotLValue(span, v) => Spanned::new(
                Doc::start("Can only assign to variables and array indices, not ")
                    .chain(v.pretty(bindings)),
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
                .class(name)
                .map(Type::Class)
                .ok_or(TypeError::NotFound(name.clone())),
            PreType::Tuple(v) => v
                .iter()
                .map(|x| self.elab_type(x))
                .collect::<Result<Vec<_>, _>>()
                .map(Type::Tuple),
            PreType::Array(t) => Ok(Type::Array(Box::new(self.elab_type(t)?))),
        }
    }

    fn declare_item_p1(&mut self, item: &PreItem) -> Result<(), TypeError> {
        match item {
            PreItem::InlineJava(_) => Ok(()),
            PreItem::Fn(_) => Ok(()),
            PreItem::ExternFn(_) => Ok(()),
            PreItem::Let(_, _, _, _) => Ok(()),
            PreItem::Class { path, variants, .. } => {
                self.create_class(
                    path.clone(),
                    ClassInfo {
                        variants: variants.clone(),
                        ..ClassInfo::default()
                    },
                );
                Ok(())
            }
            PreItem::Use(path, wildcard) => {
                // Only add types
                if !*wildcard {
                    if let Some(s) = self.class(path) {
                        self.local_classes.insert(*path.stem(), s);
                    }
                } else {
                    if path.len() == 1 {
                        if let Some(m) = self.module(*path.stem()).cloned() {
                            for (s, (t, _)) in m.classes {
                                self.local_classes.insert(*s.stem(), t);
                            }
                        }
                    }
                }

                Ok(())
            }
        }
    }

    fn declare_item_p2(&mut self, item: &PreItem) -> Result<(), TypeError> {
        match item {
            PreItem::Class {
                path,
                methods,
                members,
                constructor,
                variants,
                ext: _,
            } => {
                let methods = methods
                    .iter()
                    .map(|f| {
                        let mut args = Vec::new();
                        let mut args2 = Vec::new();
                        for (s, t, _) in &f.args {
                            let t = self.elab_type(t)?;
                            args.push(t.clone());
                            args2.push((self.bindings.create(lpath(*s), true), t));
                        }
                        let rty = self.elab_type(&f.ret_ty)?;
                        let ty = FnType(args, rty.clone());
                        let id = self.bindings.add_fn(lpath(f.name));
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
                let members = members
                    .iter()
                    .map(|(s, t)| {
                        let t = self.elab_type(t)?;
                        Ok((*s, t))
                    })
                    .collect::<Result<_, _>>()?;
                let constructor = constructor
                    .as_ref()
                    .map(|x| x.iter().map(|x| self.elab_type(x)).collect())
                    .transpose()?;
                let id = self.class(path).unwrap();
                let info = self.class_info_mut(id);
                *info = ClassInfo {
                    methods,
                    members,
                    constructor,
                    variants: variants.clone(),
                };
                Ok(())
            }
            PreItem::Use(path, wildcard) => {
                // Add remaining types
                if !*wildcard {
                    if let Some(t) = self.class(path) {
                        // probably done in p1, but might not due to declaration order
                        if !self.local_classes.contains_key(&*path.stem()) {
                            self.local_classes.insert(*path.stem(), t);
                        }
                    }
                } else {
                    if path.len() == 1 {
                        if let Some(m) = self.module(*path.stem()).cloned() {
                            for (s, (t, _)) in m.classes {
                                if !self.local_classes.contains_key(&*s.stem()) {
                                    self.local_classes.insert(*s.stem(), t);
                                }
                            }
                        }
                    }
                }

                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn declare_item_p3(&mut self, item: &PreItem) -> Result<(), TypeError> {
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
                if self.var(&lpath(*name)).is_some() {
                    return Err(TypeError::Duplicate(name.span, **name));
                }
                let ty = match ty {
                    Some(ty) => self.elab_type(ty)?,
                    None => {
                        self.infer(x.as_ref().expect("can't infer type of nothing"))?
                            .1
                    }
                };
                self.create(*name, ty, *public);
                Ok(())
            }
            PreItem::Class { .. } => Ok(()),
            PreItem::Use(path, wildcard) => {
                // Add remaining types
                if !*wildcard {
                    if let Some(t) = self.class(path) {
                        // probably done in p1, but might not due to declaration order
                        if !self.local_classes.contains_key(&*path.stem()) {
                            self.local_classes.insert(*path.stem(), t);
                        }
                    }
                } else {
                    if path.len() == 1 {
                        if let Some(m) = self.module(*path.stem()).cloned() {
                            for (s, (t, _)) in m.classes {
                                if !self.local_classes.contains_key(&*s.stem()) {
                                    self.local_classes.insert(*s.stem(), t);
                                }
                            }
                        }
                    }
                }

                Ok(())
            }
        }
    }

    fn declare_item_p4(&mut self, item: &PreItem) -> Result<(), TypeError> {
        match item {
            PreItem::Use(path, wildcard) => {
                // Add everything but types
                if !*wildcard {
                    if let Some((s, t)) = self.var(path) {
                        let t = t.clone();
                        self.vars.add(*path.stem(), s, t);
                    } else if let Some((s, t)) = self.fun(path) {
                        let t = t.clone();
                        self.fns.add(*path.stem(), s, t);
                    } else if let Some(_) = self.class(path) {
                        // done in p1 and p2
                    } else {
                        return Err(TypeError::NotFound(path.clone()));
                    }
                } else {
                    if path.len() == 1 {
                        if let Some(m) = self.module(*path.stem()).cloned() {
                            for (r, s, t) in m.vars {
                                self.vars.add(r, s, t);
                            }
                            for (r, s, t) in m.fns {
                                self.fns.add(r, s, t);
                            }
                            // classes done in p1 and p2
                            return Ok(());
                        }
                    }
                    if let Some(c) = self.class(path) {
                        let info = self.class_info(c);
                        if let Some(v) = info.variants.clone() {
                            let ty = Type::Class(c);
                            for i in v {
                                self.create(Spanned::new(i, path.span()), ty.clone(), false);
                            }
                        } else {
                            return Err(TypeError::NoVariants(path.span(), Type::Class(c)));
                        }
                    } else {
                        return Err(TypeError::NotFound(path.clone()));
                    }
                }

                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn check_item(&mut self, item: &PreItem) -> Result<Vec<Item>, TypeError> {
        match item {
            PreItem::InlineJava(s) => Ok(vec![Item::InlineJava(*s)]),
            PreItem::Fn(f) => {
                let PreFn {
                    name,
                    ret_ty: _,
                    args,
                    body,
                    public,
                    throws,
                } = f;
                let (fid, fty) = self.fun(&lpath(*name)).unwrap();
                let FnType(atys, rty) = fty.clone();

                self.push(Some(rty.clone()));
                let mut args2 = Vec::new();
                for ((a, _, public), t) in args.iter().zip(atys) {
                    let a = self.create(*a, t.clone(), *public);
                    args2.push((a, t));
                }
                let body = self.check(body, rty.clone())?;
                self.pop();

                Ok(vec![Item::Fn(Fn {
                    id: fid,
                    ret_ty: rty,
                    args: args2,
                    public: *public,
                    body,
                    throws: throws.clone(),
                })])
            }
            PreItem::ExternFn(f) => {
                let PreEFn {
                    name,
                    ret_ty: _,
                    args,
                    mapping,
                } = f;
                let (fid, fty) = self.fun(&lpath(*name)).unwrap();
                let FnType(atys, rty) = fty.clone();

                self.push(Some(rty.clone()));
                let mut args2 = Vec::new();
                for ((a, _, public), t) in args.iter().zip(atys) {
                    let a = self.create(*a, t.clone(), *public);
                    args2.push((a, t));
                }
                // let body = self.check(body, rty.clone())?;
                self.pop();

                Ok(vec![Item::ExternFn(ExternFn {
                    id: fid,
                    ret_ty: rty,
                    args: args2,
                    mapping: *mapping,
                })])
            }
            PreItem::Let(name, _, x, _) => {
                let (s, t) = self.var(&lpath(*name)).unwrap();
                let t = t.clone();
                let x = x.as_ref().map(|x| self.check(x, t.clone())).transpose()?;
                Ok(vec![Item::Let(s, t, x)])
            }
            PreItem::Class {
                path,
                variants: None,
                ..
            } => Ok(vec![Item::ExternClass(self.class(path).unwrap())]),
            PreItem::Class {
                path,
                variants: Some(variants),
                ext,
                ..
            } => Ok(vec![Item::Enum(
                self.class(path).unwrap(),
                variants.clone(),
                *ext,
            )]),
            PreItem::Use(path, wildcard) => {
                if *wildcard {
                    if path.len() == 1 {
                        if let Some(_) = self.module(*path.stem()).cloned() {
                            return Ok(Vec::new());
                        }
                    }
                    if let Some(c) = self.class(path) {
                        let info = self.class_info(c);
                        if let Some(v) = info.variants.clone() {
                            let ty = Type::Class(c);
                            let mut block = Vec::new();
                            for i in v {
                                let (v, _) = self.var(&lpath(Spanned::hack(i))).unwrap();
                                block.push(Item::Let(v, ty.clone(), Some(Term::Variant(c, i))));
                            }
                            return Ok(block);
                        }
                    }
                }

                Ok(Vec::new())
            }
        }
    }

    fn check_stmt(&mut self, stmt: &PreStatement) -> Result<Option<Statement>, TypeError> {
        match stmt {
            PreStatement::Item(
                item
                @
                (PreItem::Fn(_)
                | PreItem::ExternFn(_)
                | PreItem::Class { .. }
                | PreItem::Use(_, _)),
            ) => {
                self.declare_item_p1(item)?;
                self.declare_item_p2(item)?;
                self.declare_item_p3(item)?;
                self.declare_item_p4(item)?;
                for item in self.check_item(item)? {
                    self.extra_items.push(item);
                }
                Ok(None)
            }
            PreStatement::Item(PreItem::InlineJava(s)) => Ok(Some(Statement::InlineJava(*s))),
            PreStatement::Item(PreItem::Let(name, ty, value, public)) => {
                let value = value.as_ref().expect("statement let must have a value");
                let (x, t) = match ty {
                    Some(t) => {
                        let t = self.elab_type(t)?;
                        let x = self.check(value, t.clone())?;
                        (x, t)
                    }
                    None => self.infer(value)?,
                };
                let n = self.create(*name, t.clone(), *public);
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
            PreStatement::For(s, public, pa, b, block) => {
                let (iter, t) = match b {
                    // Range
                    Some(b) => {
                        let a = self.check(pa, Type::I32)?;
                        let b = self.check(b, Type::I32)?;
                        (ForIter::Range(Box::new(a), Box::new(b)), Type::I32)
                    }
                    // Array
                    None => {
                        let (a, t) = self.infer(pa)?;
                        match t {
                            Type::Array(t) => (ForIter::Array(Box::new(a)), *t),
                            t => return Err(TypeError::NotArray(pa.span, t)),
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
                .var(raw)
                .map(|(s, t)| (Term::Var(s), t.clone()))
                .ok_or(TypeError::NotFound(raw.clone()))
                .or_else(|e| {
                    if raw.len() > 1 {
                        let mut v = raw.0.clone();
                        let last = v.pop().unwrap();
                        let a = RawPath(v, last);
                        let b = raw.1;
                        if let Some(class) = self.class(&a) {
                            let variants = self.class_info(class).variants.as_ref();
                            if variants.map_or(true, |v| v.iter().all(|x| *x != *b)) {
                                return Err(TypeError::NotFound(lpath(b)));
                            }

                            return Ok((Term::Variant(class, *b), Type::Class(class)));
                        } else if let Some(ty) = self.module(*a.stem()) {
                            let (_, s, t) = ty
                                .vars
                                .iter()
                                .rfind(|(s, _, _)| *s == *b)
                                .ok_or(TypeError::NotFound(lpath(b)))?;
                            return Ok((Term::Var(*s), t.clone()));
                        }
                    }
                    Err(e)
                }),
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
            Pre::TupleIdx(px, i) => {
                let (x, t) = self.infer(px)?;
                let t = match t {
                    Type::Tuple(mut v) => {
                        if *i < v.len() {
                            v.swap_remove(*i)
                        } else {
                            return Err(TypeError::TupleOutOfBounds(pre.span, Type::Tuple(v), *i));
                        }
                    }
                    t => return Err(TypeError::NotTuple(px.span, t)),
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
            Pre::ArrayIdx(parr, idx) => {
                let (arr, aty) = self.infer(parr)?;
                let ty = match aty {
                    Type::Array(t) => *t,
                    t => return Err(TypeError::NotArray(parr.span, t)),
                };
                let idx = self.check(idx, Type::I32)?;
                Ok((Term::ArrayIdx(Box::new(arr), Box::new(idx)), ty))
            }
            Pre::Member(px, m) => {
                let (x, t) = self.infer(px)?;
                match t {
                    Type::Class(t) => {
                        let info = self.class_info(t);
                        if let Some((_, t)) = info.members.iter().find(|(s, _)| *s == **m) {
                            Ok((Term::Member(Box::new(x), **m), t.clone()))
                        } else {
                            Err(TypeError::NotFound(lpath(*m)))
                        }
                    }
                    t => Err(TypeError::NotTuple(px.span, t)),
                }
            }
            Pre::Set(pl, op, x) => {
                let (l, t) = self.infer(pl)?;
                let l = match l {
                    Term::Var(s) => LValue::Var(s),
                    Term::ArrayIdx(b, i) if matches!(&*b, Term::Var(_)) => match *b {
                        Term::Var(s) => LValue::Idx(s, i),
                        _ => unreachable!(),
                    },
                    l => return Err(TypeError::NotLValue(pl.span, l)),
                };
                let x = self.check(x, t)?;

                Ok((Term::Set(l, *op, Box::new(x)), Type::Unit))
            }
            Pre::Call(f, a) => {
                if let Some((fid, FnType(atys, rty))) = self.fun(f) {
                    let rty = rty.clone();
                    if a.len() != atys.len() {
                        return Err(TypeError::WrongArity(pre.span, a.len(), atys.len()));
                    }
                    let mut a2 = Vec::new();
                    for (a, t) in a.iter().zip(atys.clone()) {
                        a2.push(self.check(a, t)?);
                    }
                    Ok((Term::Call(None, fid, a2), rty))
                } else if let Some(t) = self.class(f) {
                    let info = self.class_info(t);
                    if let Some(atys) = &info.constructor {
                        if a.len() != atys.len() {
                            return Err(TypeError::WrongArity(pre.span, a.len(), atys.len()));
                        }
                        let mut a2 = Vec::new();
                        for (a, t) in a.iter().zip(atys.clone()) {
                            a2.push(self.check(a, t)?);
                        }
                        Ok((Term::Constructor(t, a2), Type::Class(t)))
                    } else {
                        Err(TypeError::NotFound(f.clone()))
                    }
                } else {
                    Err(TypeError::NotFound(f.clone()))
                }
            }
            Pre::Method(o_, f, a) => {
                let (o, t) = self.infer(o_)?;
                match t {
                    Type::Class(c) => {
                        let methods = &self.class_info(c).methods;
                        let (_, fid, FnType(atys, rty)) = methods
                            .iter()
                            .find(|(s, _, _)| *s == **f)
                            .ok_or(TypeError::NotFound(lpath(*f)))?;
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
                        _ => return Err(TypeError::NotFound(lpath(*f))),
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
                    for x in self.check_stmt(i)? {
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
                            .ok_or(TypeError::NotFound(lpath(Spanned::new(s2, s.span))))?;
                        if *b {
                            Spanned::new(
                                Doc::start("Duplicate branch for pattern ")
                                    .add(self.bindings.resolve_raw(s2)),
                                s.span,
                            )
                            .emit(Severity::Warning, self.file_id);
                        } else {
                            *b = true;
                        }
                    } else {
                        if had_default {
                            Spanned::new(Doc::start("Duplicate default branch in pattern match, this one is unreachable"),
                                s.span,
                            ).emit(Severity::Warning, self.file_id);
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
