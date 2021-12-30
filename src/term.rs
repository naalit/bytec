use std::{collections::HashMap, path::PathBuf, sync::RwLock};

pub use crate::binding::*;
pub use crate::pretty::Doc;
use crate::pretty::{Prec, Style};

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
    Gt,
    Lt,
    Eq,
    Neq,
    Geq,
    Leq,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOpType {
    Comp,
    Arith,
    Logic,
}
impl BinOp {
    pub fn ty(self) -> BinOpType {
        match self {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => BinOpType::Arith,
            BinOp::Gt | BinOp::Lt | BinOp::Eq | BinOp::Neq | BinOp::Geq | BinOp::Leq => {
                BinOpType::Comp
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Literal {
    /// Java doesn't have unsigned integers, which makes int literals convenient
    Int(i64),
    Str(RawSym),
    Bool(bool),
}

lazy_static::lazy_static! {
    pub static ref INPUT_PATH: RwLock<PathBuf> = RwLock::new(PathBuf::new());
    pub static ref INPUT_SOURCE: RwLock<String> = RwLock::new(String::new());
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}
impl Severity {
    pub fn start(self) -> Doc {
        match self {
            Severity::Error => Doc::start("error").style(Style::BoldRed),
            Severity::Warning => Doc::start("warning").style(Style::BoldYellow),
        }
    }
    pub fn style(self) -> Style {
        match self {
            Severity::Error => Style::BoldRed,
            Severity::Warning => Style::BoldYellow,
        }
    }
}

impl Error {
    pub fn emit(self, severity: Severity) {
        // An extremely simple copy of Rust's error message design
        // TODO show the whole span, show secondary message
        let source = INPUT_SOURCE.read().unwrap();
        let (mut line, mut col) = (1, self.span.0);
        let mut line_str = None;
        for l in source.lines() {
            if col <= l.len() {
                line_str = Some(l);
                break;
            } else {
                line += 1;
                col -= l.len() + 1;
            }
        }
        severity
            .start()
            .add(": ")
            .chain(self.inner)
            .style(Style::Bold)
            .hardline()
            .chain(
                Doc::start("    --> ")
                    .style(Style::Special)
                    .add(
                        INPUT_PATH
                            .read()
                            .unwrap()
                            .file_name()
                            .unwrap()
                            .to_str()
                            .unwrap(),
                    )
                    .chain(Doc::start(":"))
                    .add(line)
                    .chain(Doc::start(":"))
                    .add(col + 1),
            )
            .hardline()
            .chain(Doc::start("     |").style(Style::Special))
            .hardline()
            .chain(
                Doc::start(format!("{:4} |", line))
                    .style(Style::Special)
                    .space()
                    .add(line_str.unwrap_or("")),
            )
            .hardline()
            .chain(Doc::start("     |").style(Style::Special).space().chain(
                Doc::start(format!("{:>width$}", "^", width = col + 1)).style(severity.style()),
            ))
            .emit();
    }
}

// Syntax

pub enum Term {
    Var(Sym),
    Lit(Literal, Type),
    Call(Option<Box<Term>>, FnId, Vec<Term>),
    BinOp(BinOp, Box<Term>, Box<Term>),
    Block(Vec<Statement>, Option<Box<Term>>),
    If(Box<Term>, Box<Term>, Option<Box<Term>>),
    Break,
    Continue,
    Return(Option<Box<Term>>),
    Variant(TypeId, RawSym),
    Match(Box<Term>, Vec<(Option<RawSym>, Term)>),
}
pub enum Statement {
    Term(Term),
    Let(Sym, Type, Term),
    While(Term, Vec<Statement>),
    InlineJava(RawSym),
}

pub enum Item {
    Fn(Fn),
    ExternFn(ExternFn),
    ExternClass(TypeId),
    InlineJava(RawSym),
    /// If the bool is true, it's extern and shouldn't be generated
    Enum(TypeId, Vec<RawSym>, bool),
}
pub struct Fn {
    pub id: FnId,
    pub public: bool,
    pub ret_ty: Type,
    pub args: Vec<(Sym, Type)>,
    pub body: Term,
}
pub struct ExternFn {
    pub id: FnId,
    pub ret_ty: Type,
    pub args: Vec<(Sym, Type)>,
    pub mapping: RawSym,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    I32,
    I64,
    Bool,
    Str,
    Unit,
    Class(TypeId),
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
    // o.f(a, b, c)
    Method(SPre, Spanned<RawSym>, Vec<SPre>),
    // a + b
    BinOp(BinOp, SPre, SPre),
    // { a; b; c }
    Block(Vec<PreStatement>, Option<SPre>),
    // if a { b } else { c }
    If(SPre, SPre, Option<SPre>),
    // break
    Break,
    // continue
    Continue,
    // return x
    Return(Option<SPre>),
    // Direction::North
    Variant(Spanned<RawSym>, Spanned<RawSym>),
    // match x { s => t, else => u }
    Match(SPre, Vec<(Spanned<Option<RawSym>>, SPre)>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PreFn {
    pub name: Spanned<RawSym>,
    pub public: bool,
    pub ret_ty: PreType,
    pub args: Vec<(RawSym, PreType, bool)>,
    pub body: SPre,
}
#[derive(Clone, Debug, PartialEq)]
pub struct PreEFn {
    pub name: Spanned<RawSym>,
    pub ret_ty: PreType,
    pub args: Vec<(RawSym, PreType, bool)>,
    pub mapping: RawSym,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PreItem {
    Fn(PreFn),
    ExternFn(PreEFn),
    InlineJava(RawSym),
    ExternClass(RawSym, Vec<PreEFn>),
    Enum(RawSym, Vec<RawSym>, bool),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PreStatement {
    Item(PreItem),
    Term(SPre),
    While(SPre, Vec<PreStatement>),
    Let {
        name: RawSym,
        ty: Option<PreType>,
        value: SPre,
        public: bool,
    },
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PreType {
    I32,
    I64,
    Bool,
    Str,
    Unit,
    Class(Spanned<RawSym>),
}

impl Pre {
    pub fn needs_semicolon(&self) -> bool {
        match self {
            Pre::Var(_)
            | Pre::Lit(_, _)
            | Pre::Call(_, _)
            | Pre::Method(_, _, _)
            | Pre::Variant(_, _)
            | Pre::Break
            | Pre::Continue
            | Pre::Return(_)
            | Pre::BinOp(_, _, _) => true,
            // These don't need semicolons since they end in a closing brace
            Pre::Block(_, _) | Pre::If(_, _, _) | Pre::Match(_, _) => false,
        }
    }
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
            Term::Variant(tid, s) => Term::Variant(*tid, *s),
            Term::Call(o, f, a) => Term::Call(
                o.as_ref().map(|o| Box::new(o.cloned_(cln))),
                *f,
                a.iter().map(|x| x.cloned_(cln)).collect(),
            ),
            Term::BinOp(_, _, _) => todo!(),
            Term::Block(v, e) => {
                let v = v.iter().map(|x| x.cloned_(cln)).collect();
                Term::Block(v, e.as_ref().map(|x| Box::new(x.cloned_(cln))))
            }
            Term::If(a, b, c) => Term::If(
                Box::new(a.cloned_(cln)),
                Box::new(b.cloned_(cln)),
                c.as_ref().map(|x| Box::new(x.cloned_(cln))),
            ),
            Term::Match(x, branches) => {
                let branches = branches.iter().map(|(s, t)| (*s, t.cloned_(cln))).collect();
                Term::Match(Box::new(x.cloned_(cln)), branches)
            }
            Term::Break => Term::Break,
            Term::Continue => Term::Continue,
            Term::Return(x) => Term::Return(x.as_ref().map(|x| Box::new(x.cloned_(cln)))),
        }
    }
}
impl Statement {
    fn cloned_(&self, cln: &mut Cloner) -> Statement {
        match self {
            Statement::Term(t) => Statement::Term(t.cloned_(cln)),
            Statement::Let(n, t, x) => Statement::Let(*n, t.clone(), x.cloned_(cln)),
            Statement::While(a, b) => {
                Statement::While(a.cloned_(cln), b.iter().map(|x| x.cloned_(cln)).collect())
            }
            Statement::InlineJava(s) => Self::InlineJava(*s),
        }
    }
}

// Pretty-printing

impl BinOp {
    pub fn repr(&self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",

            BinOp::Gt => ">",
            BinOp::Lt => "<",
            BinOp::Eq => "==",
            BinOp::Neq => "!=",
            BinOp::Geq => ">=",
            BinOp::Leq => "<=",
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
                Literal::Str(s) => Doc::start('"').add(cxt.resolve_raw(*s)).add('"'),
                Literal::Bool(t) => Doc::start(t),
            }
            .style(Style::Literal),
            Term::Variant(tid, s) => Doc::start(cxt.resolve_raw(cxt.type_name(*tid)))
                .add("::")
                .add(cxt.resolve_raw(*s)),
            Term::Call(None, f, a) => Doc::start(cxt.resolve_raw(cxt.fn_name(*f)))
                .add("(")
                .chain(Doc::intersperse(
                    a.iter().map(|x| x.pretty(cxt)),
                    Doc::start(",").space(),
                ))
                .add(")"),
            Term::Call(Some(o), f, a) => o
                .pretty(cxt)
                .add('.')
                .add(cxt.resolve_raw(cxt.fn_name(*f)))
                .add("(")
                .chain(Doc::intersperse(
                    a.iter().map(|x| x.pretty(cxt)),
                    Doc::start(",").space(),
                ))
                .add(")"),
            Term::BinOp(op, a, b) => a
                .pretty(cxt)
                .nest(Prec::Atom)
                .space()
                .add(op.repr())
                .space()
                .chain(b.pretty(cxt).nest(Prec::Atom))
                .prec(Prec::Term),
            Term::Block(v, x) => {
                let mut d = Doc::start("{").chain(Doc::intersperse(
                    v.iter().map(|x| Doc::none().line().chain(x.pretty(cxt))),
                    Doc::none(),
                ));
                if let Some(x) = x {
                    d = d.line().chain(x.pretty(cxt));
                }
                d.indent().line().add("}")
            }
            Term::If(cond, yes, no) => Doc::keyword("if")
                .space()
                .chain(cond.pretty(cxt))
                .space()
                // a and b should be blocks already
                .chain(yes.pretty(cxt))
                .chain(
                    no.as_ref()
                        .map(|no| Doc::keyword(" else").space().chain(no.pretty(cxt)))
                        .unwrap_or(Doc::none()),
                ),
            Term::Match(x, branches) => Doc::keyword("match")
                .space()
                .chain(x.pretty(cxt))
                .space()
                .add('{')
                .line()
                .chain(Doc::intersperse(
                    branches.iter().map(|(s, t)| {
                        match s {
                            Some(s) => Doc::start(cxt.resolve_raw(*s)),
                            None => Doc::keyword("else"),
                        }
                        .space()
                        .add("=>")
                        .space()
                        .chain(t.pretty(cxt))
                    }),
                    Doc::start(',').line(),
                ))
                .indent()
                .line()
                .add('}'),
            Term::Break => Doc::keyword("break"),
            Term::Continue => Doc::keyword("continue"),
            Term::Return(None) => Doc::keyword("return"),
            Term::Return(Some(x)) => Doc::keyword("return").space().chain(x.pretty(cxt)),
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
            Item::ExternFn(f) => Doc::keyword("extern")
                .space()
                .chain(Doc::keyword("fn"))
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
                .chain(
                    Doc::start('"')
                        .add(cxt.resolve_raw(f.mapping))
                        .add('"')
                        .style(Style::Literal),
                )
                .add(";"),
            Item::Enum(id, variants, ext) => {
                let mut doc = Doc::none();
                if *ext {
                    doc = Doc::keyword("extern").space();
                }
                doc.chain(Doc::keyword("enum"))
                    .space()
                    .add(cxt.resolve_raw(cxt.type_name(*id)))
                    .space()
                    .add('{')
                    .line()
                    .chain(Doc::intersperse(
                        variants
                            .iter()
                            .map(|x| Doc::start(cxt.resolve_raw(*x)).add(',')),
                        Doc::none().line(),
                    ))
                    .indent()
                    .line()
                    .add('}')
            }
            Item::ExternClass(c) => Doc::start(cxt.resolve_raw(cxt.type_name(*c))),
            Item::InlineJava(s) => Doc::keyword("extern")
                .space()
                .chain(
                    Doc::start('"')
                        .add(cxt.resolve_raw(*s))
                        .add('"')
                        .style(Style::Literal),
                )
                .add(';'),
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
            Statement::While(cond, block) => Doc::keyword("while")
                .space()
                .chain(cond.pretty(cxt))
                .space()
                .add("{")
                .line()
                .chain(Doc::intersperse(
                    block.iter().map(|x| x.pretty(cxt)),
                    Doc::none().line(),
                ))
                .indent()
                .line()
                .add("}"),
            Statement::InlineJava(s) => Doc::keyword("extern")
                .space()
                .chain(
                    Doc::start('"')
                        .add(cxt.resolve_raw(*s))
                        .add('"')
                        .style(Style::Literal),
                )
                .add(';'),
        }
    }
}
impl Type {
    pub fn pretty(&self, cxt: &Bindings) -> Doc {
        match self {
            Type::I32 => Doc::keyword("i32"),
            Type::I64 => Doc::keyword("i64"),
            Type::Bool => Doc::keyword("bool"),
            Type::Str => Doc::keyword("str"),
            Type::Unit => Doc::start("()"),
            Type::Class(c) => Doc::start(cxt.resolve_raw(cxt.type_name(*c))),
        }
    }
}
