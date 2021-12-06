use crate::pretty::Doc;

// Common types

// TODO interner
// #[derive(Clone, PartialEq, Debug)]
pub type Name = String;

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
pub type Error = Spanned<String>;

// AST

pub type STerm = Box<Spanned<Term>>;
#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    // a
    Var(Name),
    // f(a, b, c)
    Call(STerm, Vec<STerm>),
    // a + b
    BinOp(BinOp, STerm, STerm),
    // { a; b; c }
    Block(Vec<Statement>, Option<STerm>),
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Plus,
    Minus,
}
#[derive(Clone, Debug, PartialEq)]
pub struct Fn {
    pub name: Spanned<Name>,
    pub ret_type: Type,
    pub args: Vec<(Name, Type)>,
    pub body: Term,
}
#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Fn(Fn),
}
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Item(Item),
    Term(STerm),
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Type {
    I32,
    I64,
    Unit,
}

// PRETTY-PRINTING

impl Term {
    pub fn pretty(&self) -> Doc {
        match self {
            Term::Var(x) => Doc::start(x),
            Term::Call(f, a) => f
                .pretty()
                .add("(")
                .chain(Doc::intersperse(
                    a.iter().map(|x| x.pretty()),
                    Doc::start(",").space(),
                ))
                .add(")"),
            Term::BinOp(_, _, _) => todo!(),
            Term::Block(v, x) => {
                let mut d = Doc::start("{").line().chain(Doc::intersperse(
                    v.iter().map(|x| x.pretty()),
                    Doc::none().line(),
                ));
                if let Some(x) = x {
                    d = d.line().chain(x.pretty());
                }
                d.indent().line().add("}")
            }
        }
    }
}
impl Item {
    pub fn pretty(&self) -> Doc {
        match self {
            Item::Fn(f) => Doc::keyword("fn")
                .space()
                .add(&*f.name)
                .add("(")
                .chain(Doc::intersperse(
                    f.args
                        .iter()
                        .map(|(name, ty)| Doc::start(name).add(":").space().chain(ty.pretty())),
                    Doc::start(",").space(),
                ))
                .add(")")
                .add(":")
                .space()
                .chain(f.ret_type.pretty())
                .space()
                .add("=")
                .space()
                .chain(f.body.pretty())
                .add(";"),
        }
    }
}
impl Statement {
    pub fn pretty(&self) -> Doc {
        match self {
            Statement::Item(i) => i.pretty(),
            Statement::Term(x) => x.pretty().add(";"),
        }
    }
}
impl Type {
    pub fn pretty(&self) -> Doc {
        match self {
            Type::I32 => Doc::keyword("i32"),
            Type::I64 => Doc::keyword("i64"),
            Type::Unit => Doc::start("()"),
        }
    }
}
