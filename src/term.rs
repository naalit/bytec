use std::collections::HashMap;

pub use crate::binding::*;
pub use crate::pretty::Doc;

// Common types

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Span(pub usize, pub usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}
impl<T> Spanned<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Spanned { inner, span }
    }
}
impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
pub type Error = Spanned<Doc>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Literal {
    /// Java doesn't have unsigned integers, which makes int literals convenient
    Int(i64),
}

// Syntax

pub enum Term {
    Var(Sym),
    Lit(Literal, Type),
    Call(FnId, Vec<Term>),
    BinOp(BinOp, Box<Term>, Box<Term>),
    Block(Vec<Statement>, Option<Box<Term>>),
}
pub enum Statement {
    Term(Term),
    Let(Sym, Type, Box<Term>),
}

pub enum Item {
    Fn(Fn),
}
pub struct Fn {
    pub id: FnId,
    pub ret_ty: Type,
    pub args: Vec<(Sym, Type)>,
    pub body: Term,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    I32,
    I64,
    Unit,
}

// Presyntax

pub type SPre = Box<Spanned<Pre>>;
#[derive(Clone, Debug, PartialEq)]
pub enum Pre {
    // a
    Var(RawSym),
    // 2
    Lit(Literal, Option<PreType>),
    // f(a, b, c)
    Call(Spanned<RawSym>, Vec<SPre>),
    // a + b
    BinOp(BinOp, SPre, SPre),
    // { a; b; c }
    Block(Vec<PreStatement>, Option<SPre>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PreFn {
    pub name: Spanned<RawSym>,
    pub ret_ty: PreType,
    pub args: Vec<(RawSym, PreType)>,
    pub body: SPre,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PreItem {
    Fn(PreFn),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PreStatement {
    Item(PreItem),
    Term(SPre),
    Let(RawSym, Option<PreType>, SPre),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PreType {
    I32,
    I64,
    Unit,
}

// Cloning logic

struct Cloner<'a> {
    bindings: &'a mut Bindings,
    map: HashMap<Sym, Sym>,
}
impl<'a> Cloner<'a> {
    fn new(bindings: &'a mut Bindings) -> Self {
        Cloner {
            bindings,
            map: HashMap::new(),
        }
    }

    fn set(&mut self, from: Sym, to: Sym) {
        self.map.insert(from, to);
    }

    /// Gets it from the rename map, or returns it as-is if there's no rename set for it.
    /// This takes care of recursive renaming (x -> y -> z)
    fn get(&self, s: Sym) -> Sym {
        if let Some(k) = self.map.get(&s).copied() {
            // Here's how this (`k == s`) happens:
            // 1. We come up with a Elab using, say, x3. That Elab gets stored in Salsa's database.
            // 2. We reset the Bindings, define x0, x1, and x2, and ask for the Elab again.
            // 3. Salsa gives us the Elab from the database, which references x3. We call cloned() on it.
            // 4. We see a bound variable, x3, and define a fresh variable to replace it with. The fresh variable is now also x3.
            // 5. Now we want to replace x3 with x3, so we better not call get() recursively or we'll get stuck in a loop.
            // Note, though, that this is expected behaviour and doesn't need fixing.
            if k == s {
                k
            } else {
                self.get(k)
            }
        } else {
            s
        }
    }

    /// Freshen `s` and add it to the mappings
    pub fn fresh(&mut self, s: Sym) -> Sym {
        let s2 = self.bindings.fresh(s);
        self.set(s, s2);
        s2
    }
}

impl Term {
    pub fn cloned(&self, bindings: &mut Bindings) -> Term {
        self.cloned_(&mut Cloner::new(bindings))
    }

    fn cloned_(&self, cln: &mut Cloner) -> Term {
        match self {
            Term::Var(s) => Term::Var(cln.get(*s)),
            Term::Lit(l, t) => Term::Lit(*l, t.clone()),
            Term::Call(f, a) => Term::Call(*f, a.iter().map(|x| x.cloned_(cln)).collect()),
            Term::BinOp(_, _, _) => todo!(),
            Term::Block(v, e) => {
                let v = v.iter().map(|x| x.cloned_(cln)).collect();
                Term::Block(v, e.as_ref().map(|x| Box::new(x.cloned_(cln))))
            }
        }
    }
}
impl Statement {
    fn cloned_(&self, cln: &mut Cloner) -> Statement {
        match self {
            Statement::Term(t) => Statement::Term(t.cloned_(cln)),
            Statement::Let(n, t, x) => Statement::Let(*n, t.clone(), Box::new(x.cloned_(cln))),
        }
    }
}

// Pretty-printing

impl BinOp {
    pub fn char(&self) -> char {
        match self {
            BinOp::Add => '+',
            BinOp::Sub => '-',
            BinOp::Mul => '*',
            BinOp::Div => '/',
        }
    }
}
impl Term {
    pub fn pretty(&self, cxt: &Bindings) -> Doc {
        match self {
            Term::Var(x) => Doc::start(cxt.resolve(*x)),
            Term::Lit(l, t) => match l {
                Literal::Int(i) => Doc::start(i).add(match t {
                    Type::I32 => "i32",
                    Type::I64 => "i64",
                    _ => unreachable!(),
                }),
            },
            Term::Call(f, a) => Doc::start(cxt.resolve_raw(cxt.fn_name(*f)))
                .add("(")
                .chain(Doc::intersperse(
                    a.iter().map(|x| x.pretty(cxt)),
                    Doc::start(",").space(),
                ))
                .add(")"),
            Term::BinOp(op, a, b) => a
                .pretty(cxt)
                .space()
                .add(op.char())
                .space()
                .chain(b.pretty(cxt)),
            Term::Block(v, x) => {
                let mut d = Doc::start("{").line().chain(Doc::intersperse(
                    v.iter().map(|x| x.pretty(cxt)),
                    Doc::none().line(),
                ));
                if let Some(x) = x {
                    d = d.line().chain(x.pretty(cxt));
                }
                d.indent().line().add("}")
            }
        }
    }
}
impl Item {
    pub fn pretty(&self, cxt: &Bindings) -> Doc {
        match self {
            Item::Fn(f) => Doc::keyword("fn")
                .space()
                .add(cxt.resolve_raw(cxt.fn_name(f.id)))
                .add("(")
                .chain(Doc::intersperse(
                    f.args.iter().map(|(name, ty)| {
                        Doc::start(cxt.resolve(*name))
                            .add(":")
                            .space()
                            .chain(ty.pretty(cxt))
                    }),
                    Doc::start(",").space(),
                ))
                .add(")")
                .add(":")
                .space()
                .chain(f.ret_ty.pretty(cxt))
                .space()
                .add("=")
                .space()
                .chain(f.body.pretty(cxt))
                .add(";"),
        }
    }
}
impl Statement {
    pub fn pretty(&self, cxt: &Bindings) -> Doc {
        match self {
            Statement::Term(x) => x.pretty(cxt).add(";"),
            Statement::Let(n, t, x) => Doc::keyword("let")
                .space()
                .add(cxt.resolve(*n))
                .add(":")
                .space()
                .chain(t.pretty(cxt))
                .space()
                .add("=")
                .space()
                .chain(x.pretty(cxt))
                .add(";"),
        }
    }
}
impl Type {
    pub fn pretty(&self, cxt: &Bindings) -> Doc {
        match self {
            Type::I32 => Doc::keyword("i32"),
            Type::I64 => Doc::keyword("i64"),
            Type::Unit => Doc::start("()"),
        }
    }
}
