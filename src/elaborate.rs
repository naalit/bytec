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

struct Cxt<'b> {
    vars: Env<Sym, Type>,
    fns: Env<FnId, FnType>,
    bindings: &'b mut Bindings,
}
impl<'b> Cxt<'b> {
    fn new(bindings: &'b mut Bindings) -> Self {
        Cxt {
            vars: Env::new(),
            fns: Env::new(),
            bindings,
        }
    }

    fn var(&self, s: RawSym) -> Option<(Sym, &Type)> {
        self.vars.get(s)
    }
    fn fun(&self, s: RawSym) -> Option<(FnId, &FnType)> {
        self.fns.get(s)
    }

    /// Start a new scope
    fn push(&mut self) {
        self.vars.push();
        self.fns.push();
    }
    /// End the most recent scope
    fn pop(&mut self) {
        self.vars.pop();
        self.fns.pop();
    }

    /// Creates a new binding with a name
    fn create(&mut self, k: RawSym, ty: Type) -> Sym {
        let s = self.bindings.create(k);
        self.vars.add(k, s, ty);
        s
    }

    fn create_fn(&mut self, k: RawSym, ty: FnType) -> FnId {
        let s = self.bindings.add_fn(k);
        self.fns.add(k, s, ty);
        s
    }
}

enum TypeError {
    NotFound(Span, RawSym),
    /// Unify(span, found, expected)
    Unify(Span, Type, Type),
    WrongArity(Span, usize, usize),
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
                    ity.pretty(bindings)
                        .add(" but found ")
                        .chain(ety.pretty(bindings)),
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
        }
    }
}

impl<'b> Cxt<'b> {
    fn elab_type(&mut self, ty: &PreType) -> Result<Type, TypeError> {
        match ty {
            PreType::I32 => Ok(Type::I32),
            PreType::I64 => Ok(Type::I64),
            PreType::Unit => Ok(Type::Unit),
        }
    }

    fn declare_item(&mut self, item: &PreItem) -> Result<(), TypeError> {
        match item {
            PreItem::Fn(f) => {
                let mut args = Vec::new();
                for (_s, t) in &f.args {
                    let t = self.elab_type(t)?;
                    args.push(t);
                }
                let rty = self.elab_type(&f.ret_ty)?;
                self.create_fn(*f.name, FnType(args, rty));
                Ok(())
            }
        }
    }

    fn check_item(&mut self, item: &PreItem) -> Result<Item, TypeError> {
        match item {
            PreItem::Fn(f) => {
                let PreFn {
                    name,
                    ret_ty: _,
                    args,
                    body,
                } = f;
                let (fid, fty) = self.fun(**name).unwrap();
                let FnType(atys, rty) = fty.clone();

                self.push();
                let mut args2 = Vec::new();
                for ((a, _), t) in args.iter().zip(atys) {
                    let a = self.create(*a, t.clone());
                    args2.push((a, t));
                }
                let body = self.check(body, rty.clone())?;
                self.pop();

                Ok(Item::Fn(Fn {
                    id: fid,
                    ret_ty: rty,
                    args: args2,
                    body,
                }))
            }
        }
    }

    fn check_stmt(&mut self, stmt: &PreStatement) -> Result<Option<Statement>, TypeError> {
        match stmt {
            PreStatement::Item(_) => {
                todo!("add to some top-level item list, elab it, and return None")
            }
            PreStatement::Term(t) => self.infer(t).map(|(x, _)| Some(Statement::Term(x))),
            PreStatement::Let(n, t, x) => {
                let (x, t) = match t {
                    Some(t) => {
                        let t = self.elab_type(t)?;
                        let x = self.check(x, t.clone())?;
                        (x, t)
                    }
                    None => self.infer(x)?,
                };
                let n = self.create(*n, t.clone());
                Ok(Some(Statement::Let(n, t, Box::new(x))))
            }
        }
    }

    fn infer(&mut self, pre: &SPre) -> Result<(Term, Type), TypeError> {
        match &***pre {
            Pre::Var(raw) => self
                .var(*raw)
                .map(|(s, t)| (Term::Var(s), t.clone()))
                .ok_or(TypeError::NotFound(pre.span, *raw)),
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
                Ok((Term::Call(fid, a2), rty))
            }
            Pre::BinOp(_, _, _) => todo!(),
            Pre::Block(v, e) => {
                self.push();

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
        }
    }

    fn check(&mut self, pre: &SPre, ty: Type) -> Result<Term, TypeError> {
        match (&***pre, &ty) {
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
