use std::{collections::HashMap, path::PathBuf, sync::RwLock};

pub use crate::binding::*;
pub use crate::pretty::Doc;
use crate::pretty::{Prec, Style};

// Common types

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct FileId(pub usize, pub RawSym);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Span(pub usize, pub usize);

#[derive(Debug, Clone, Copy)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}
impl<T> Spanned<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Spanned { inner, span }
    }
    pub fn hack(inner: T) -> Self {
        Spanned {
            inner,
            span: Span(0, 0),
        }
    }
}
impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}
impl<T: Eq> Eq for Spanned<T> {}
impl<T: std::hash::Hash> std::hash::Hash for Spanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}
pub type Error = Spanned<Doc>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Gt,
    Lt,
    Eq,
    Neq,
    Geq,
    Leq,
    BitAnd,
    BitOr,
    BitXor,
    BitShr,
    BitShl,
    And,
    Or,
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
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Mod
            | BinOp::BitAnd
            | BinOp::BitOr
            | BinOp::BitXor
            | BinOp::BitShr
            | BinOp::BitShl => BinOpType::Arith,
            BinOp::Gt | BinOp::Lt | BinOp::Eq | BinOp::Neq | BinOp::Geq | BinOp::Leq => {
                BinOpType::Comp
            }
            BinOp::And | BinOp::Or => BinOpType::Logic,
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
    pub static ref INPUT_PATH: RwLock<HashMap<FileId, PathBuf>> = RwLock::new(Default::default());
    pub static ref INPUT_SOURCE: RwLock<HashMap<FileId, String>> = RwLock::new(Default::default());
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
    pub fn emit(self, severity: Severity, file: FileId) {
        // An extremely simple copy of Rust's error message design
        // TODO show multiline spans, show secondary message
        let source = INPUT_SOURCE.read().unwrap();
        let source = source.get(&file).unwrap();
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
        let line_str = line_str.unwrap_or("");
        let mut end = col + self.span.1 - self.span.0;
        if end > line_str.len() {
            end = line_str.len();
        }
        let caret_str: String = std::iter::repeat('^').take(end - col).collect();
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
                            .get(&file)
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
                    .add(line_str),
            )
            .hardline()
            .chain(
                Doc::start("     |").style(Style::Special).space().chain(
                    Doc::start(format!("{:>width$}{}", "", caret_str, width = col))
                        .style(severity.style()),
                ),
            )
            .emit();
    }
}

// Syntax

#[derive(Clone)]
pub struct ModType {
    pub vars: Vec<(RawSym, Sym, Type)>,
    pub fns: Vec<(RawSym, FnId, FnType)>,
    pub classes: HashMap<RawPath, (TypeId, ClassInfo)>,
    pub local_classes: HashMap<RawSym, TypeId>,
}

#[derive(Debug, Clone)]
pub struct FnType(pub Vec<Type>, pub Type);

#[derive(Clone, Default)]
pub struct ClassInfo {
    pub methods: Vec<(RawSym, FnId, FnType)>,
    pub variants: Option<Vec<(RawSym, Vec<Type>)>>,
    pub members: Vec<(RawSym, Type)>,
    pub constructor: Option<Vec<Type>>,
}

pub enum ArrayMethod {
    Len,
    Pop,
    Clear,
    Push(Box<Term>),
}

pub enum LValue {
    // v = x
    Var(Sym),
    // arr[i] = x
    Idx(Sym, Box<Term>),
    // a.b = x
    Member(Box<Term>, RawSym),
}

pub enum ForIter {
    // for i in 0..10 (note: only i32)
    Range(Box<Term>, Box<Term>),
    // for i: t in arr
    Array(Box<Term>),
}

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
    Variant(TypeId, RawSym, Vec<Term>),
    Tuple(Vec<Term>),
    TupleIdx(Box<Term>, usize),
    // (array, inner type (needed for empty arrays in backend))
    Array(Vec<Term>, Type),
    ArrayIdx(Box<Term>, Box<Term>),
    ArrayMethod(Box<Term>, ArrayMethod),
    Member(Box<Term>, RawSym),
    Constructor(TypeId, Vec<Term>),
    Set(LValue, Option<BinOp>, Box<Term>),
    Match(Box<Term>, Vec<(Option<RawSym>, Vec<(Sym, Type)>, Term)>),
    Not(Box<Term>),
    Null(Type),
}
pub enum Statement {
    Term(Term),
    Let(Sym, Type, Term),
    While(Term, Vec<Statement>),
    For(Sym, ForIter, Vec<Statement>),
    InlineJava(RawSym),
}

pub enum Item {
    Fn(Fn),
    ExternFn(ExternFn),
    ExternClass(TypeId),
    InlineJava(RawSym),
    /// If the bool is true, it's extern and shouldn't be generated
    Enum(TypeId, Vec<(RawSym, Vec<Type>)>, bool),
    Let(Sym, Type, Option<Term>),
}
pub struct Fn {
    pub id: FnId,
    pub public: bool,
    pub ret_ty: Type,
    pub args: Vec<(Sym, Type)>,
    pub body: Term,
    pub throws: Vec<RawSym>,
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
    Tuple(Vec<Type>),
    Array(Box<Type>),
}

// Presyntax

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RawPath(pub Vec<Spanned<RawSym>>, pub Spanned<RawSym>);
impl RawPath {
    pub fn len(&self) -> usize {
        self.0.len() + 1
    }
    pub fn stem(&self) -> Spanned<RawSym> {
        self.1
    }
    pub fn span(&self) -> Span {
        Span(
            self.0.first().map_or(self.1.span, |x| x.span).0,
            self.1.span.1,
        )
    }
    pub fn pretty(&self, cxt: &Bindings) -> Doc {
        let mut doc = Doc::start(cxt.resolve_raw(*self.1));
        for i in self.0.iter().rev() {
            doc = Doc::start(cxt.resolve_raw(**i)).add("::").chain(doc);
        }
        doc
    }
}
pub fn lpath(x: Spanned<RawSym>) -> RawPath {
    RawPath(Vec::new(), x)
}

pub type SPre = Box<Spanned<Pre>>;
#[derive(Clone, Debug, PartialEq)]
pub enum Pre {
    // a
    Var(RawPath),
    // 2
    Lit(Literal, Option<PreType>),
    // f(a, b, c)
    Call(RawPath, Vec<SPre>),
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
    // (a, b)
    Tuple(Vec<SPre>),
    // x.0
    TupleIdx(SPre, usize),
    // [a, b, c]
    Array(Vec<SPre>),
    // x[i]
    ArrayIdx(SPre, SPre),
    // x.m
    Member(SPre, Spanned<RawSym>),
    // v op= x
    Set(SPre, Option<BinOp>, SPre),
    // match x { s => t, else => u }
    Match(
        SPre,
        Vec<(Spanned<Option<RawSym>>, Vec<(Spanned<RawSym>, bool)>, SPre)>,
    ),
    // !x
    Not(SPre),
    // null
    Null,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PreFn {
    pub name: Spanned<RawSym>,
    pub public: bool,
    pub ret_ty: PreType,
    pub args: Vec<(Spanned<RawSym>, PreType, bool)>,
    pub body: SPre,
    pub throws: Vec<RawSym>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct PreEFn {
    pub name: Spanned<RawSym>,
    pub ret_ty: PreType,
    pub args: Vec<(Spanned<RawSym>, PreType, bool)>,
    pub mapping: RawSym,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PreItem {
    Fn(PreFn),
    ExternFn(PreEFn),
    InlineJava(RawSym),
    Class {
        ext: bool,
        path: RawPath,
        variants: Option<Vec<(RawSym, Vec<PreType>)>>,
        methods: Vec<PreEFn>,
        members: Vec<(RawSym, PreType)>,
        constructor: Option<Vec<PreType>>,
    },
    Let(Spanned<RawSym>, Option<PreType>, Option<SPre>, bool),
    // use a::b; the bool is true if it's a wildcard a::b::*
    Use(RawPath, bool),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PreStatement {
    Item(PreItem),
    Term(SPre),
    While(SPre, Vec<PreStatement>),
    // for pub a in b..c
    For(Spanned<RawSym>, bool, SPre, Option<SPre>, Vec<PreStatement>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PreType {
    I32,
    I64,
    Bool,
    Str,
    Class(RawPath),
    Tuple(Vec<PreType>),
    Array(Box<PreType>),
}

impl Pre {
    pub fn needs_semicolon(&self) -> bool {
        match self {
            // These don't need semicolons since they end in a closing brace
            Pre::Block(_, _) | Pre::If(_, _, _) | Pre::Match(_, _) => false,
            _ => true,
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
            Term::Variant(tid, s, xs) => {
                Term::Variant(*tid, *s, xs.iter().map(|x| x.cloned_(cln)).collect())
            }
            Term::Tuple(v) => Term::Tuple(v.iter().map(|x| x.cloned_(cln)).collect()),
            Term::TupleIdx(x, i) => Term::TupleIdx(Box::new(x.cloned_(cln)), *i),
            Term::Array(v, t) => Term::Array(v.iter().map(|x| x.cloned_(cln)).collect(), t.clone()),
            Term::ArrayIdx(arr, i) => {
                Term::ArrayIdx(Box::new(arr.cloned_(cln)), Box::new(i.cloned_(cln)))
            }
            Term::ArrayMethod(arr, m) => {
                Term::ArrayMethod(Box::new(arr.cloned_(cln)), m.cloned_(cln))
            }
            Term::Call(o, f, a) => Term::Call(
                o.as_ref().map(|o| Box::new(o.cloned_(cln))),
                *f,
                a.iter().map(|x| x.cloned_(cln)).collect(),
            ),
            Term::BinOp(op, a, b) => {
                Term::BinOp(*op, Box::new(a.cloned_(cln)), Box::new(b.cloned_(cln)))
            }
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
                let branches = branches
                    .iter()
                    .map(|(s, v, t)| (*s, v.clone(), t.cloned_(cln)))
                    .collect();
                Term::Match(Box::new(x.cloned_(cln)), branches)
            }
            Term::Set(l, op, x) => Term::Set(l.cloned_(cln), *op, Box::new(x.cloned_(cln))),
            Term::Break => Term::Break,
            Term::Continue => Term::Continue,
            Term::Return(x) => Term::Return(x.as_ref().map(|x| Box::new(x.cloned_(cln)))),
            Term::Member(a, b) => Term::Member(Box::new(a.cloned_(cln)), *b),
            Term::Constructor(f, a) => {
                Term::Constructor(*f, a.iter().map(|x| x.cloned_(cln)).collect())
            }
            Term::Not(x) => Term::Not(Box::new(x.cloned_(cln))),
            Term::Null(t) => Term::Null(t.clone()),
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
            Statement::For(s, i, b) => Statement::For(
                *s,
                i.cloned_(cln),
                b.iter().map(|x| x.cloned_(cln)).collect(),
            ),
            Statement::InlineJava(s) => Self::InlineJava(*s),
        }
    }
}
impl ArrayMethod {
    fn cloned_(&self, cln: &mut Cloner) -> ArrayMethod {
        match self {
            ArrayMethod::Len => ArrayMethod::Len,
            ArrayMethod::Pop => ArrayMethod::Pop,
            ArrayMethod::Clear => ArrayMethod::Clear,
            ArrayMethod::Push(x) => ArrayMethod::Push(Box::new(x.cloned_(cln))),
        }
    }
}
impl LValue {
    fn cloned_(&self, cln: &mut Cloner) -> LValue {
        match self {
            LValue::Var(x) => LValue::Var(*x),
            LValue::Idx(a, b) => LValue::Idx(*a, Box::new(b.cloned_(cln))),
            LValue::Member(a, b) => LValue::Member(Box::new(a.cloned_(cln)), *b),
        }
    }
}
impl ForIter {
    fn cloned_(&self, cln: &mut Cloner) -> ForIter {
        match self {
            ForIter::Range(a, b) => {
                ForIter::Range(Box::new(a.cloned_(cln)), Box::new(b.cloned_(cln)))
            }
            ForIter::Array(a) => ForIter::Array(Box::new(a.cloned_(cln))),
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
            BinOp::Mod => "%",

            BinOp::Gt => ">",
            BinOp::Lt => "<",
            BinOp::Eq => "==",
            BinOp::Neq => "!=",
            BinOp::Geq => ">=",
            BinOp::Leq => "<=",

            BinOp::BitAnd => "&",
            BinOp::BitOr => "|",
            BinOp::BitXor => "^",
            BinOp::BitShr => ">>",
            BinOp::BitShl => "<<",

            BinOp::And => "&&",
            BinOp::Or => "||",
        }
    }
}
impl Term {
    pub fn pretty(&self, cxt: &Bindings) -> Doc {
        match self {
            Term::Var(x) => Doc::start(cxt.resolve_local(*x)),
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
            Term::Variant(tid, s, todo) => cxt
                .type_name(*tid)
                .pretty(cxt)
                .add("::")
                .add(cxt.resolve_raw(*s)),
            Term::Tuple(v) => Doc::start('(')
                .chain(Doc::intersperse(
                    v.iter().map(|x| x.pretty(cxt)),
                    Doc::start(",").space(),
                ))
                .add(')'),
            Term::TupleIdx(x, i) => x.pretty(cxt).add('.').add(i),
            Term::Array(v, _) => Doc::start('[')
                .chain(Doc::intersperse(
                    v.iter().map(|x| x.pretty(cxt)),
                    Doc::start(',').space(),
                ))
                .add(']'),
            Term::ArrayIdx(arr, i) => arr.pretty(cxt).add('[').chain(i.pretty(cxt)).add(']'),
            Term::ArrayMethod(arr, m) => arr.pretty(cxt).add('.').chain(match m {
                ArrayMethod::Len => Doc::start("len()"),
                ArrayMethod::Pop => Doc::start("pop()"),
                ArrayMethod::Clear => Doc::start("clear()"),
                ArrayMethod::Push(x) => Doc::start("push(").chain(x.pretty(cxt)).add(')'),
            }),
            Term::Call(None, f, a) => cxt
                .fn_name(*f)
                .pretty(cxt)
                .add("(")
                .chain(Doc::intersperse(
                    a.iter().map(|x| x.pretty(cxt)),
                    Doc::start(",").space(),
                ))
                .add(")"),
            Term::Call(Some(o), f, a) => o
                .pretty(cxt)
                .add('.')
                .chain(cxt.fn_name(*f).pretty(cxt))
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
                    branches.iter().map(|(s, todo, t)| {
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
            Term::Set(v, op, x) => v
                .pretty(cxt)
                .space()
                .add(op.map_or("", |x| x.repr()))
                .add("=")
                .space()
                .chain(x.pretty(cxt)),
            Term::Break => Doc::keyword("break"),
            Term::Continue => Doc::keyword("continue"),
            Term::Return(None) => Doc::keyword("return"),
            Term::Return(Some(x)) => Doc::keyword("return").space().chain(x.pretty(cxt)),
            Term::Member(x, m) => x.pretty(cxt).add('.').add(cxt.resolve_raw(*m)),
            Term::Constructor(f, a) => cxt
                .type_name(*f)
                .pretty(cxt)
                .add("(")
                .chain(Doc::intersperse(
                    a.iter().map(|x| x.pretty(cxt)),
                    Doc::start(",").space(),
                ))
                .add(")"),
            Term::Not(x) => Doc::start("!").chain(x.pretty(cxt).nest(Prec::Atom)),
            Term::Null(_) => Doc::keyword("null"),
        }
    }
}
impl Item {
    pub fn pretty(&self, cxt: &Bindings) -> Doc {
        match self {
            Item::Fn(f) => Doc::keyword("fn")
                .space()
                .chain(cxt.fn_name(f.id).pretty(cxt))
                .add("(")
                .chain(Doc::intersperse(
                    f.args.iter().map(|(name, ty)| {
                        Doc::start(cxt.resolve_local(*name))
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
                .chain(cxt.fn_name(f.id).pretty(cxt))
                .add("(")
                .chain(Doc::intersperse(
                    f.args.iter().map(|(name, ty)| {
                        Doc::start(cxt.resolve_local(*name))
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
                    .chain(cxt.type_name(*id).pretty(cxt))
                    .space()
                    .add('{')
                    .line()
                    .chain(Doc::intersperse(
                        variants.iter().map(|(x, v)| {
                            Doc::start(cxt.resolve_raw(*x))
                                .chain(if v.is_empty() {
                                    Doc::none()
                                } else {
                                    Doc::start("(")
                                        .chain(Doc::intersperse(
                                            v.iter().map(|x| x.pretty(cxt)),
                                            Doc::start(", "),
                                        ))
                                        .add(")")
                                })
                                .add(',')
                        }),
                        Doc::none().line(),
                    ))
                    .indent()
                    .line()
                    .add('}')
            }
            Item::ExternClass(c) => cxt.type_name(*c).pretty(cxt),
            Item::InlineJava(s) => Doc::keyword("extern")
                .space()
                .chain(
                    Doc::start('"')
                        .add(cxt.resolve_raw(*s))
                        .add('"')
                        .style(Style::Literal),
                )
                .add(';'),
            Item::Let(n, t, x) => Doc::keyword("let")
                .space()
                .add(cxt.resolve_local(*n))
                .add(":")
                .space()
                .chain(t.pretty(cxt))
                .chain(if let Some(x) = x {
                    Doc::none().space().add("=").space().chain(x.pretty(cxt))
                } else {
                    Doc::none()
                })
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
                .add(cxt.resolve_local(*n))
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
            Statement::For(s, iter, block) => Doc::keyword("for")
                .space()
                .add(cxt.resolve_local(*s))
                .space()
                .chain(Doc::keyword("in"))
                .space()
                .chain(iter.pretty(cxt))
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
            Type::Class(c) => cxt.type_name(*c).pretty(cxt),
            Type::Tuple(v) => Doc::start('(')
                .chain(Doc::intersperse(
                    v.iter().map(|x| x.pretty(cxt)),
                    Doc::start(",").space(),
                ))
                .add(')'),
            Type::Array(t) => Doc::start('[').chain(t.pretty(cxt)).add(']'),
        }
    }
}
impl LValue {
    pub fn pretty(&self, cxt: &Bindings) -> Doc {
        match self {
            LValue::Var(x) => Doc::start(cxt.resolve_local(*x)),
            LValue::Idx(arr, i) => Doc::start(cxt.resolve_local(*arr))
                .add('[')
                .chain(i.pretty(cxt))
                .add(']'),
            LValue::Member(x, m) => x.pretty(cxt).add('.').add(cxt.resolve_raw(*m)),
        }
    }
}
impl ForIter {
    pub fn pretty(&self, cxt: &Bindings) -> Doc {
        match self {
            ForIter::Range(a, b) => a
                .pretty(cxt)
                .nest(Prec::Atom)
                .add("..")
                .chain(b.pretty(cxt).nest(Prec::Atom)),
            ForIter::Array(a) => a.pretty(cxt),
        }
    }
}
