use std::{collections::HashMap, path::PathBuf, rc::Rc, sync::RwLock};

use ropey::Rope;

pub use crate::binding::*;
pub use crate::pretty::Doc;
use crate::pretty::{Prec, Style};

// Common types

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct FileId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Span(pub usize, pub usize);
impl Span {
    pub fn lsp_range(&self, text: &ropey::Rope) -> lsp_types::Range {
        let start = text.byte_to_char(self.0) as u32;
        let end = text.byte_to_char(self.1) as u32;
        let start_line = text.char_to_line(start as usize);
        let start_line_start = text.line_to_char(start_line);
        let end_line = text.char_to_line(end as usize);
        let end_line_start = text.line_to_char(end_line);
        lsp_types::Range {
            start: lsp_types::Position {
                line: start_line as u32,
                character: start - start_line_start as u32,
            },
            end: lsp_types::Position {
                line: end_line as u32,
                character: end - end_line_start as u32,
            },
        }
    }
}

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
impl<T> std::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
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

    pub fn is_bit_op(self) -> bool {
        match self {
            BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor | BinOp::BitShr | BinOp::BitShl => true,
            _ => false,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Literal {
    /// Java doesn't have unsigned integers, which makes int literals convenient
    Int(i64),
    Float(f64),
    Str(RawSym),
    Bool(bool),
}

lazy_static::lazy_static! {
    pub static ref INPUT_PATH: RwLock<HashMap<FileId, PathBuf>> = RwLock::new(Default::default());
    pub static ref INPUT_SOURCE: RwLock<HashMap<FileId, Rope>> = RwLock::new(Default::default());
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

    pub fn lsp(self) -> lsp_types::DiagnosticSeverity {
        match self {
            Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
            Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
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
            if col <= l.len_bytes() {
                line_str = Some(l);
                break;
            } else {
                line += 1;
                col -= l.len_bytes();
            }
        }
        let line_str = line_str.unwrap_or("".into());
        let mut end = col + self.span.1 - self.span.0;
        if end > line_str.len_bytes() {
            end = line_str.len_bytes();
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
                    .add(line_str.to_string().trim_end()),
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

#[derive(Clone, Default)]
pub struct ModType {
    pub vars: Vec<(RawSym, Sym, MType)>,
    pub fns: Vec<(RawSym, FnId, FnType)>,
    pub classes: HashMap<RawPath, (TypeId, ClassInfo)>,
    pub local_classes: HashMap<RawSym, TypeId>,
    pub local_mods: HashMap<RawSym, RawPath>,
}

#[derive(Clone)]
pub struct FnType(pub Vec<(RawSym, Type)>, pub Type);

#[derive(Clone, Default)]
pub struct ClassInfo {
    pub methods: Vec<(RawSym, FnId, FnType)>,
    pub variants: Option<Vec<(RawSym, Vec<Type>)>>,
    pub members: Vec<(RawSym, Sym, Type)>,
    pub constructor: Option<Vec<Type>>,
}

#[derive(PartialEq)]
pub enum ArrayMethod {
    Len,
    Pop,
    Clear,
    Push(Box<Term>),
    Unknown(Span, bool),
}

#[derive(PartialEq)]
pub enum LValue {
    // v = x
    Var(Sym),
    // arr[i] = x (is_static)
    Idx(Box<LValue>, Box<Term>, bool),
    // a.b = x
    Member(Box<Term>, Sym),
}
impl LValue {
    pub fn check_mutability(&self, cxt: &mut crate::elaborate::Cxt) -> Result<(), Sym> {
        match self {
            LValue::Var(s) => {
                if !cxt.is_mutable(*s) {
                    return Err(*s);
                }
            }
            LValue::Idx(l, _, _) => l.check_mutability(cxt)?,
            // This is Java, objects are always mutable
            // (and that's not something we can change without a more powerful type system)
            LValue::Member(_, _) => (),
        }
        Ok(())
    }
}

#[derive(PartialEq)]
pub enum ForIter {
    // for i in [unroll] 0..10 (note: only i32)
    Range(Box<Term>, Box<Term>, bool),
    // for i: t in arr
    Array(Box<Term>),
    SArray(Box<Term>, Type),
}

#[derive(PartialEq)]
pub enum Term {
    Var(Sym),
    Lit(Literal, Type),
    Call(Option<Box<Term>>, Result<FnId, Spanned<TypeId>>, Vec<Term>),
    BinOp(BinOp, Box<Term>, Box<Term>),
    Block(Vec<Statement>, Option<Box<Term>>),
    If(Box<Term>, Box<Term>, Option<Box<Term>>),
    Break,
    Continue,
    Return(Option<Box<Term>>),
    Variant(TypeId, RawSym, Vec<Term>),
    Tuple(Vec<Term>),
    TupleIdx(Box<Term>, usize),
    // (array, inner type (needed for empty arrays in backend), is dynamic)
    Array(Vec<Term>, Type, bool),
    // (arr, idx, static, inner_ty, inline)
    ArrayIdx(Box<Term>, Box<Term>, bool, Type, bool),
    // (len, ty)
    ArrayNew(Box<Term>, Type),
    ArrayMethod(Box<Term>, ArrayMethod),
    Member(Box<Term>, TypeId, Spanned<Sym>),
    Constructor(TypeId, Vec<Term>),
    Set(LValue, Option<BinOp>, Box<Term>),
    Match(
        TypeId,
        Box<Term>,
        Vec<(Option<RawSym>, Vec<(Sym, Type)>, Term)>,
    ),
    As(Box<Term>, Type, Type),
    Not(Box<Term>),
    Null(Type),
    Selph(TypeId),
    Error,
}
#[derive(PartialEq)]
pub enum Statement {
    Term(Term),
    Let(Sym, bool, Type, Term),
    While(Term, Vec<Statement>),
    For(Sym, ForIter, Vec<Statement>),
    InlineJava(RawSym),
}

pub enum Item {
    Fn(Fn),
    ExternFn(ExternFn),
    ExternClass(TypeId, RawPath, Vec<(Sym, Type)>, Span),
    InlineJava(RawSym, Span),
    /// If the option is some, it's extern and shouldn't be generated
    Enum(
        TypeId,
        Vec<(RawSym, Vec<Type>)>,
        Option<RawPath>,
        Vec<(Sym, Type)>,
        Vec<Fn>,
        Span,
    ),
    Class(TypeId, Vec<(Sym, Type, Option<Term>)>, Vec<Fn>, Span),
    Let(Sym, bool, Type, Option<Term>, Span),
}
impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Fn(f) => f.span,
            Item::ExternFn(f) => f.span,
            Item::ExternClass(_, _, _, s) => *s,
            Item::InlineJava(_, s) => *s,
            Item::Enum(_, _, _, _, _, s) => *s,
            Item::Class(_, _, _, s) => *s,
            Item::Let(_, _, _, _, s) => *s,
        }
    }
}
pub struct Fn {
    pub id: FnId,
    pub public: bool,
    pub ret_ty: Type,
    pub args: Vec<(Sym, Type)>,
    pub body: Term,
    pub throws: Vec<RawSym>,
    pub inline: bool,
    pub span: Span,
}
pub struct ExternFn {
    pub id: FnId,
    pub ret_ty: Type,
    pub args: Vec<(Sym, Type)>,
    pub mapping: RawSym,
    pub span: Span,
}

#[derive(Clone, PartialEq)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    Bool,
    Str,
    Unit,
    Class(TypeId),
    Tuple(Vec<Type>),
    Array(Box<Type>),
    SArray(Box<Type>, Rc<Term>),
    Error,
}
impl Type {
    pub fn has_null(&self) -> bool {
        match self {
            Type::Class(_) | Type::Str => true,
            _ => false,
        }
    }
}

/// A type with mutability information
pub type MType = (Type, bool);

impl Term {
    pub fn to_lval(self) -> Option<LValue> {
        match self {
            Term::Var(s) => Some(LValue::Var(s)),
            Term::ArrayIdx(b, i, sta, _, _) => {
                let b = b.to_lval()?;
                Some(LValue::Idx(Box::new(b), i, sta))
            }
            Term::Member(a, _, b) => Some(LValue::Member(a, *b)),
            _ => None,
        }
    }
}

// Presyntax

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RawPath(pub Vec<Spanned<RawSym>>, pub Spanned<RawSym>);
impl RawPath {
    pub fn len(&self) -> usize {
        self.0.len() + 1
    }
    pub fn last(&self) -> Spanned<RawSym> {
        self.1
    }
    pub fn stem(&self) -> RawPath {
        RawPath(self.0[..self.0.len() - 1].to_vec(), *self.0.last().unwrap())
    }
    pub fn add(&self, s: Spanned<RawSym>) -> RawPath {
        RawPath(
            self.0
                .iter()
                .copied()
                .chain(std::iter::once(self.1))
                .collect(),
            s,
        )
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
    // x[(inline) i]
    ArrayIdx(SPre, SPre, bool),
    // [; 84]
    ArrayNew(SPre),
    // x.m
    Member(SPre, Spanned<RawSym>),
    // v op= x
    Set(SPre, Option<BinOp>, SPre),
    // match x { s => t, else => u }
    Match(
        SPre,
        // (name, pub, mut)
        Vec<(
            Spanned<Option<RawSym>>,
            Vec<(Spanned<RawSym>, bool, bool)>,
            SPre,
        )>,
    ),
    // x as t
    As(SPre, PreType),
    // !x
    Not(SPre),
    // null
    Null,
    // self
    Selph,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PreFn {
    pub name: Spanned<RawSym>,
    pub public: bool,
    pub ret_ty: PreType,
    // (name, type, pub, mut)
    pub args: Vec<(Spanned<RawSym>, PreType, bool, bool)>,
    pub body: SPre,
    pub throws: Vec<RawSym>,
    pub inline: bool,
}
#[derive(Clone, Debug, PartialEq)]
pub struct PreEFn {
    pub name: Spanned<RawSym>,
    pub ret_ty: PreType,
    // (name, type, pub, mut)
    pub args: Vec<(Spanned<RawSym>, PreType, bool, bool)>,
    pub mapping: RawSym,
}
#[derive(Clone, Debug, PartialEq)]
pub enum PreFnEither {
    Extern(PreEFn),
    Local(PreFn),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PreItem {
    Fn(PreFn),
    ExternFn(PreEFn),
    InlineJava(RawSym, Span),
    Class {
        ext: bool,
        path: RawPath,
        variants: Option<Vec<(RawSym, Vec<PreType>)>>,
        methods: Vec<PreFnEither>,
        members: Vec<(Spanned<RawSym>, bool, PreType, Option<SPre>)>,
        constructor: Option<Vec<PreType>>,
    },
    // (name, const, type, value, pub, mut)
    Let(
        Spanned<RawSym>,
        bool,
        Option<PreType>,
        Option<SPre>,
        bool,
        bool,
    ),
    // use a::b; the bool is true if it's a wildcard a::b::*
    Use(RawPath, bool),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PreStatement {
    Item(PreItem),
    Term(SPre),
    While(SPre, Vec<PreStatement>),
    // for pub a in [unroll] b..c
    For(
        Spanned<RawSym>,
        bool, // pub
        bool, // mut
        bool, // unroll
        SPre,
        Option<SPre>,
        Vec<PreStatement>,
    ),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PreType {
    I32,
    I64,
    F32,
    F64,
    Bool,
    Str,
    Class(RawPath),
    Tuple(Vec<PreType>),
    Array(Box<PreType>),
    SArray(Box<PreType>, SPre),
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

// Visitors

impl Item {
    pub fn visit(&self, f: &mut impl FnMut(&Term)) {
        match self {
            Item::Fn(x) => x.body.visit(f),
            Item::ExternFn(_) | Item::ExternClass(_, _, _, _) | Item::InlineJava(_, _) => (),
            Item::Enum(_, _, _, _, v, _) => v.iter().for_each(|x| x.body.visit(f)),
            Item::Class(_, a, b, _) => {
                a.iter()
                    .filter_map(|(_, _, x)| x.as_ref())
                    .for_each(|x| x.visit(f));
                b.iter().for_each(|x| x.body.visit(f));
            }
            Item::Let(_, _, _, x, _) => x.iter().for_each(|x| x.visit(f)),
        }
    }
}

impl Term {
    fn visit(&self, f: &mut impl FnMut(&Term)) {
        f(self);
        match self {
            Term::Call(a, _, c) => {
                a.iter().for_each(|x| x.visit(f));
                c.iter().for_each(|x| x.visit(f));
            }
            Term::BinOp(_, a, b) => {
                a.visit(f);
                b.visit(f);
            }
            Term::Block(a, b) => {
                a.iter().for_each(|x| x.visit(f));
                b.iter().for_each(|x| x.visit(f));
            }
            Term::If(a, b, c) => {
                a.visit(f);
                b.visit(f);
                c.iter().for_each(|x| x.visit(f));
            }
            Term::Return(a) => a.iter().for_each(|x| x.visit(f)),
            Term::Variant(_, _, a) => a.iter().for_each(|x| x.visit(f)),
            Term::Tuple(a) => a.iter().for_each(|x| x.visit(f)),
            Term::TupleIdx(x, _) => x.visit(f),
            Term::Array(a, _, _) => a.iter().for_each(|x| x.visit(f)),
            Term::ArrayIdx(a, b, _, _, _) => {
                a.visit(f);
                b.visit(f);
            }
            Term::ArrayNew(x, _) => x.visit(f),
            Term::ArrayMethod(x, _) => x.visit(f),
            Term::Member(x, _, _) => x.visit(f),
            Term::Constructor(_, a) => a.iter().for_each(|x| x.visit(f)),
            Term::Set(a, _, b) => {
                a.visit(f);
                b.visit(f);
            }
            Term::Match(_, x, v) => {
                x.visit(f);
                v.iter().for_each(|(_, _, x)| x.visit(f));
            }
            Term::As(x, _, _) => x.visit(f),
            Term::Not(x) => x.visit(f),
            _ => (),
        }
    }
}

impl LValue {
    fn visit(&self, f: &mut impl FnMut(&Term)) {
        match self {
            LValue::Var(_) => (),
            LValue::Idx(l, x, _) => {
                l.visit(f);
                x.visit(f);
            }
            LValue::Member(x, _) => x.visit(f),
        }
    }
}

impl Statement {
    fn visit(&self, f: &mut impl FnMut(&Term)) {
        match self {
            Statement::Term(x) => x.visit(f),
            Statement::Let(_, _, _, x) => x.visit(f),
            Statement::While(x, v) => {
                x.visit(f);
                v.iter().for_each(|x| x.visit(f));
            }
            Statement::For(_, i, v) => {
                match i {
                    ForIter::Range(a, b, _) => {
                        a.visit(f);
                        b.visit(f);
                    }
                    ForIter::Array(x) => x.visit(f),
                    ForIter::SArray(x, _) => x.visit(f),
                }
                v.iter().for_each(|x| x.visit(f));
            }
            Statement::InlineJava(_) => (),
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
            Term::Error => Term::Error,
            Term::Var(s) => Term::Var(cln.get(*s)),
            Term::Lit(l, t) => Term::Lit(*l, t.clone()),
            Term::Variant(tid, s, xs) => {
                Term::Variant(*tid, *s, xs.iter().map(|x| x.cloned_(cln)).collect())
            }
            Term::Tuple(v) => Term::Tuple(v.iter().map(|x| x.cloned_(cln)).collect()),
            Term::TupleIdx(x, i) => Term::TupleIdx(Box::new(x.cloned_(cln)), *i),
            Term::Array(v, t, d) => {
                Term::Array(v.iter().map(|x| x.cloned_(cln)).collect(), t.clone(), *d)
            }
            Term::ArrayIdx(arr, i, sta, t, inl) => Term::ArrayIdx(
                Box::new(arr.cloned_(cln)),
                Box::new(i.cloned_(cln)),
                *sta,
                t.clone(),
                *inl,
            ),
            Term::ArrayNew(x, t) => Term::ArrayNew(Box::new(x.cloned_(cln)), t.clone()),
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
            Term::Match(tid, x, branches) => {
                let branches = branches
                    .iter()
                    .map(|(s, v, t)| (*s, v.clone(), t.cloned_(cln)))
                    .collect();
                Term::Match(*tid, Box::new(x.cloned_(cln)), branches)
            }
            Term::Set(l, op, x) => Term::Set(l.cloned_(cln), *op, Box::new(x.cloned_(cln))),
            Term::Break => Term::Break,
            Term::Continue => Term::Continue,
            Term::Return(x) => Term::Return(x.as_ref().map(|x| Box::new(x.cloned_(cln)))),
            Term::Member(a, t, b) => Term::Member(Box::new(a.cloned_(cln)), *t, *b),
            Term::Constructor(f, a) => {
                Term::Constructor(*f, a.iter().map(|x| x.cloned_(cln)).collect())
            }
            Term::As(x, from, to) => Term::As(Box::new(x.cloned_(cln)), from.clone(), to.clone()),
            Term::Not(x) => Term::Not(Box::new(x.cloned_(cln))),
            Term::Null(t) => Term::Null(t.clone()),
            Term::Selph(t) => Term::Selph(*t),
        }
    }
}
impl Statement {
    fn cloned_(&self, cln: &mut Cloner) -> Statement {
        match self {
            Statement::Term(t) => Statement::Term(t.cloned_(cln)),
            Statement::Let(n, c, t, x) => Statement::Let(*n, *c, t.clone(), x.cloned_(cln)),
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
            ArrayMethod::Unknown(x, y) => ArrayMethod::Unknown(*x, *y),
        }
    }
}
impl LValue {
    fn cloned_(&self, cln: &mut Cloner) -> LValue {
        match self {
            LValue::Var(x) => LValue::Var(*x),
            LValue::Idx(a, b, s) => {
                LValue::Idx(Box::new(a.cloned_(cln)), Box::new(b.cloned_(cln)), *s)
            }
            LValue::Member(a, b) => LValue::Member(Box::new(a.cloned_(cln)), *b),
        }
    }
}
impl ForIter {
    fn cloned_(&self, cln: &mut Cloner) -> ForIter {
        match self {
            ForIter::Range(a, b, u) => {
                ForIter::Range(Box::new(a.cloned_(cln)), Box::new(b.cloned_(cln)), *u)
            }
            ForIter::Array(a) => ForIter::Array(Box::new(a.cloned_(cln))),
            ForIter::SArray(a, t) => ForIter::SArray(Box::new(a.cloned_(cln)), t.clone()),
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
            Term::Error => Doc::start("<error>"),
            Term::Var(x) => Doc::start(cxt.resolve_local(*x)),
            Term::Lit(l, t) => match l {
                Literal::Int(i) => Doc::start(i).add(match t {
                    Type::I32 => "i32",
                    Type::I64 => "i64",
                    _ => unreachable!(),
                }),
                Literal::Float(i) => Doc::start(i).add(match t {
                    Type::F32 => "f32",
                    Type::F64 => "f64",
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
            Term::Array(v, _, b) => Doc::start('[')
                .chain(Doc::intersperse(
                    v.iter().map(|x| x.pretty(cxt)),
                    Doc::start(',').space(),
                ))
                .add(']')
                .add(if *b { "&" } else { "" }),
            Term::ArrayIdx(arr, i, _, _, _) => {
                arr.pretty(cxt).add('[').chain(i.pretty(cxt)).add(']')
            }
            Term::ArrayNew(x, _) => Doc::start("[; ").chain(x.pretty(cxt)).add("]"),
            Term::ArrayMethod(arr, m) => arr.pretty(cxt).add('.').chain(match m {
                ArrayMethod::Len => Doc::start("len()"),
                ArrayMethod::Pop => Doc::start("pop()"),
                ArrayMethod::Clear => Doc::start("clear()"),
                ArrayMethod::Push(x) => Doc::start("push(").chain(x.pretty(cxt)).add(')'),
                ArrayMethod::Unknown(_, _) => Doc::start("<unknown method>"),
            }),
            Term::Call(None, Ok(f), a) => cxt
                .fn_name(*f)
                .pretty(cxt)
                .add("(")
                .chain(Doc::intersperse(
                    a.iter().map(|x| x.pretty(cxt)),
                    Doc::start(",").space(),
                ))
                .add(")"),
            Term::Call(Some(o), Ok(f), a) => o
                .pretty(cxt)
                .add('.')
                .chain(cxt.fn_name(*f).pretty(cxt))
                .add("(")
                .chain(Doc::intersperse(
                    a.iter().map(|x| x.pretty(cxt)),
                    Doc::start(",").space(),
                ))
                .add(")"),
            Term::Call(_, Err(_), _) => Doc::keyword("<unknown method>"),
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
            Term::Match(_, x, branches) => Doc::keyword("match")
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
            Term::As(x, _from, to) => x
                .pretty(cxt)
                .nest(Prec::App)
                .space()
                .chain(Doc::keyword("as"))
                .space()
                .chain(to.pretty(cxt)),
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
            Term::Member(x, _, m) => x.pretty(cxt).add('.').chain(cxt.sym_path(**m).pretty(cxt)),
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
            Term::Selph(_) => Doc::keyword("self"),
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
            Item::Enum(id, variants, ext, members, methods, span) => {
                let mut doc = Doc::none();
                if ext.is_some() {
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
            Item::Class(tid, _, _, _) => {
                Doc::keyword("class").chain(cxt.type_name(*tid).pretty(cxt))
            }
            Item::ExternClass(c, _, _, _) => cxt.type_name(*c).pretty(cxt),
            Item::InlineJava(s, _) => Doc::keyword("extern")
                .space()
                .chain(
                    Doc::start('"')
                        .add(cxt.resolve_raw(*s))
                        .add('"')
                        .style(Style::Literal),
                )
                .add(';'),
            Item::Let(n, c, t, x, _) => Doc::keyword(if *c { "const" } else { "let" })
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
            Statement::Let(n, c, t, x) => Doc::keyword(if *c { "const" } else { "let" })
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
            Type::F32 => Doc::keyword("f32"),
            Type::F64 => Doc::keyword("f64"),
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
            Type::SArray(t, u) => Doc::start('[')
                .chain(t.pretty(cxt))
                .add("; ")
                .chain(u.pretty(cxt))
                .add(']'),
            Type::Error => Doc::keyword("<error>"),
        }
    }
}
impl FnType {
    pub fn pretty(&self, cxt: &Bindings) -> Doc {
        Doc::start("fn(")
            .chain(Doc::intersperse(
                self.0.iter().map(|(n, t)| {
                    Doc::start(cxt.resolve_raw(*n))
                        .add(": ")
                        .chain(t.pretty(cxt))
                }),
                Doc::start(", "),
            ))
            .add("): ")
            .chain(self.1.pretty(cxt))
    }
}
impl LValue {
    pub fn pretty(&self, cxt: &Bindings) -> Doc {
        match self {
            LValue::Var(x) => Doc::start(cxt.resolve_local(*x)),
            LValue::Idx(arr, i, _) => arr.pretty(cxt).add('[').chain(i.pretty(cxt)).add(']'),
            LValue::Member(x, m) => x.pretty(cxt).add('.').chain(cxt.sym_path(*m).pretty(cxt)),
        }
    }
}
impl ForIter {
    pub fn pretty(&self, cxt: &Bindings) -> Doc {
        match self {
            ForIter::Range(a, b, u) => a
                .pretty(cxt)
                .nest(Prec::Atom)
                .add("..")
                .chain(b.pretty(cxt).nest(Prec::Atom)),
            ForIter::Array(a) | ForIter::SArray(a, _) => a.pretty(cxt),
        }
    }
}
