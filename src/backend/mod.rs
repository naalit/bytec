use std::collections::HashMap;
use std::fmt::Write;

use crate::term::*;

mod codegen;
mod lower;
mod optimize;

pub use lower::{declare_p1, declare_p2, Cxt};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Predef {
    /// System.arraycopy
    ArrayCopy,
}

pub struct IRMod {
    name: RawSym,
    code: Vec<Item>,
    mappings: Vec<(u64, RawPath, bool)>,
    java: Vec<RawSym>,
    out_class: String,
}

// Java AST

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// bool: whether it's public, so mangling should be skipped
struct JVar(u64, bool);
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct JFnId(u64);
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct JClass(u64);
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct JBlock(u64);

#[derive(Copy, Clone, Debug, PartialEq)]
enum JLit {
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    Str(RawSym),
    Bool(bool),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum Prop {
    Var(JVar),
    Raw(RawSym),
}

#[derive(Clone, Debug, PartialEq)]
enum JTerm {
    Var(JVar, JTy),
    Lit(JLit),
    Call(Option<Box<JTerm>>, JFnId, Vec<JTerm>, JTy),
    Prop(Box<JTerm>, Prop, JTy),
    BinOp(BinOp, Box<JTerm>, Box<JTerm>),
    Variant(JClass, RawSym),
    Array(Vec<JTerm>, JTy),
    ArrayNew(Box<JTerm>, JTy),
    ClassNew(JClass, Vec<JTerm>),
    Index(Box<JTerm>, Box<JTerm>, JTy),
    SIndex(Vec<JTerm>, Box<JTerm>),
    Not(Box<JTerm>),
    Null(JTy),
    This(JClass),
    InlineJava(RawSym, JTy),
    Cast(Box<JTerm>, JTy),
}
impl JTerm {
    fn to_lval(self) -> Option<JLVal> {
        match self {
            JTerm::Var(v, _) => Some(JLVal::Var(v)),
            JTerm::Index(a, b, _) => Some(JLVal::Idx(Box::new(a.to_lval()?), *b)),
            JTerm::Prop(a, b, _) => Some(JLVal::Prop(*a, b)),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum JLVal {
    Var(JVar),
    Idx(Box<JLVal>, JTerm),
    SIdx(Vec<JLVal>, JTerm),
    Prop(JTerm, Prop),
}

#[derive(Clone, Debug, PartialEq)]
enum JStmt {
    Let(RawSym, JTy, JVar, Option<JTerm>),
    Set(JLVal, Option<BinOp>, JTerm),
    Term(JTerm),
    If(JTerm, Vec<JStmt>, Vec<JStmt>),
    Switch(JBlock, JTerm, Vec<(RawSym, Vec<JStmt>)>, Vec<JStmt>),
    While(JBlock, JTerm, Vec<JStmt>),
    RangeFor(JBlock, RawSym, JVar, JTerm, JTerm, Vec<JStmt>, bool),
    Continue(JBlock),
    Break(JBlock),
    Ret(JFnId, Vec<JTerm>),
    MultiCall(
        Option<Box<JTerm>>,
        JFnId,
        Vec<JTerm>,
        Vec<(RawSym, JVar, JTy)>,
    ),
    InlineJava(RawSym),
    Multi(Vec<JStmt>),
}

#[derive(Clone, Debug, PartialEq)]
enum JTy {
    I32,
    I64,
    F32,
    F64,
    Bool,
    String,
    Class(JClass),
    Array(Box<JTy>),
}
impl JTy {
    fn primitive(&self) -> bool {
        match self {
            JTy::I32 => true,
            JTy::I64 => true,
            JTy::F32 => true,
            JTy::F64 => true,
            JTy::Bool => true,
            JTy::String => false,
            JTy::Class(_) => false,
            JTy::Array(_) => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct JFn {
    name: RawSym,
    fn_id: JFnId,
    ret_tys: Vec<JTy>,
    args: Vec<(RawSym, JVar, JTy)>,
    body: Vec<JStmt>,
    public: bool,
    throws: Vec<RawSym>,
}

/// This only includes the items that actually need to appear in the Java code
/// i.e. not extern things
#[derive(Clone, Debug, PartialEq)]
enum JItem {
    Fn(JFn),
    Enum(JClass, Vec<(RawSym, Vec<JTy>)>, Option<JClass>, Vec<JFn>),
    Class(
        JClass,
        Vec<(Vec<(JVar, JTy, Option<JTerm>)>, Vec<JStmt>)>,
        Vec<JFn>,
    ),
    // Unlike in statement position, a let may end up running statements that are in its value term
    // So it needs a block, which is realized as a `static { ... }` in Java
    Let(Vec<(JVar, JTy, Option<JTerm>)>, Vec<JStmt>),
}

#[derive(Clone, Debug, PartialEq)]
enum MaybeList<T> {
    One(T),
    Tuple(Vec<T>),
}
impl<T> MaybeList<T> {
    fn one(self) -> T {
        match self {
            MaybeList::One(t) => t,
            MaybeList::Tuple(mut v) => {
                if v.len() == 1 {
                    v.pop().unwrap()
                } else {
                    panic!("backend: one object required, but got {}", v.len())
                }
            }
        }
    }

    fn to_vec(self) -> Vec<T> {
        match self {
            MaybeList::One(t) => vec![t],
            MaybeList::Tuple(v) => v,
        }
    }

    fn len(&self) -> usize {
        match self {
            MaybeList::One(_) => 1,
            MaybeList::Tuple(v) => v.len(),
        }
    }

    fn map<U>(self, mut f: impl FnMut(T) -> U) -> MaybeList<U> {
        match self {
            MaybeList::One(x) => MaybeList::One(f(x)),
            MaybeList::Tuple(v) => MaybeList::Tuple(v.into_iter().map(f).collect()),
        }
    }

    fn empty() -> Self {
        Self::Tuple(Vec::new())
    }
}
impl<T> From<MaybeList<T>> for Vec<T> {
    fn from(j: MaybeList<T>) -> Vec<T> {
        j.to_vec()
    }
}
impl<T> IntoIterator for MaybeList<T> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.to_vec().into_iter()
    }
}
impl<T> FromIterator<T> for MaybeList<T> {
    fn from_iter<U: IntoIterator<Item = T>>(iter: U) -> Self {
        let mut v: Vec<T> = iter.into_iter().collect();
        if v.len() == 1 {
            MaybeList::One(v.pop().unwrap())
        } else {
            MaybeList::Tuple(v)
        }
    }
}

type JTerms = MaybeList<JTerm>;
type JTys = MaybeList<JTy>;
type JVars = MaybeList<JVar>;

impl JTerms {
    fn ty(&self) -> JTys {
        match self {
            MaybeList::One(t) => MaybeList::One(t.ty()),
            MaybeList::Tuple(v) => MaybeList::Tuple(v.iter().map(|x| x.ty()).collect()),
        }
    }
}
