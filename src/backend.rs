use std::collections::HashMap;
use std::fmt::Write;

use crate::term::*;

// Entry point

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Predef {
    /// java.util.Arrays.copyOf
    ArrayCopy,
}

pub struct IRMod {
    name: RawSym,
    code: Vec<Item>,
    mappings: Vec<(u64, RawPath, bool)>,
    java: Vec<RawSym>,
    out_class: String,
}

pub fn declare_p1(code: &[Item], cxt: &mut Cxt) {
    for i in code {
        match i {
            Item::ExternClass(c) => {
                let class = cxt.fresh_class();
                cxt.types.push((*c, class));

                continue;
            }
            Item::Enum(c, _, _) => {
                let class = cxt.fresh_class();
                cxt.types.push((*c, class));

                continue;
            }
            _ => (),
        }
    }
}

pub fn declare_p2(code: Vec<Item>, cxt: &mut Cxt, out_class: &str) -> IRMod {
    // Declare items
    let mut mappings = Vec::new();
    let mut java = Vec::new();

    let predefined = vec![(
        Predef::ArrayCopy,
        "java.util.Arrays.copyOf",
        JTy::Array(Box::new(JTy::I32)),
    )];
    for (d, s, t) in predefined {
        let fn_id = cxt.fresh_fn();
        cxt.fn_ret_tys.insert(fn_id, JTys::One(t));
        let raw = cxt.bindings.raw(s);
        mappings.push((fn_id.0, lpath(Spanned::new(raw, Span(0, 0))), false));
        cxt.predefs.push((d, fn_id));
    }

    for i in &code {
        let (name, ret, m, public) = match i {
            Item::Fn(f) => (f.id, &f.ret_ty, cxt.bindings.fn_name(f.id), f.public),
            Item::ExternFn(f) => (f.id, &f.ret_ty, lpath(Spanned::hack(f.mapping)), true),
            Item::ExternClass(c) => {
                let class = cxt.class(*c).unwrap();
                mappings.push((class.0, lpath(cxt.bindings.type_name(*c).stem()), false));

                continue;
            }
            Item::Enum(c, _, _) => {
                let class = cxt.class(*c).unwrap();
                mappings.push((class.0, cxt.bindings.type_name(*c), false));

                continue;
            }
            Item::InlineJava(s) => {
                java.push(*s);
                continue;
            }
            Item::Let(s, t, _) => {
                let t = t.lower(&cxt);
                let mut vars = Vec::new();
                for t in t {
                    let var = cxt.fresh_var(cxt.bindings.public(*s));
                    cxt.tys.insert(var, t);
                    mappings.push((var.0, cxt.bindings.sym_path(*s), !var.1));
                    vars.push(var);
                }
                cxt.vars.push((*s, JVars::Tuple(vars)));
                continue;
            }
        };
        let item = cxt.fresh_fn();
        cxt.fn_ids.push((name, item));

        let ret = ret.lower(&cxt);
        cxt.fn_ret_tys.insert(item, ret);
        mappings.push((item.0, m, !public));
    }

    IRMod {
        name: cxt.bindings.raw(out_class),
        code,
        mappings,
        java,
        out_class: out_class.to_string(),
    }
}

impl IRMod {
    pub fn codegen<T>(&self, cxt: &mut Cxt, mods: &[(IRMod, T)]) -> String {
        for i in &self.code {
            i.lower(cxt);
        }

        let mut names = HashMap::new();
        // Declare items
        for (m, _) in mods {
            for (i, m, b) in &m.mappings {
                let mut m = m.clone();
                if m.0.first().copied().as_deref() == Some(&self.name) {
                    m.0.remove(0);
                }
                names.insert(*i, (m, *b));
            }
        }
        let mut gen = Gen::new(cxt.bindings);
        gen.names = names;
        // Generate items
        let mut s = String::new();
        // Add module-level inline Java at the top
        for &i in &self.java {
            s.push_str(cxt.bindings.resolve_raw(i));
            s.push('\n');
        }
        write!(s, "\npublic class {} {{\n\n", self.out_class).unwrap();
        for i in cxt.items.drain(..) {
            s.push_str(&i.gen(&mut gen));
        }
        s.push_str("\n}");

        s
    }
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
    Str(RawSym),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq)]
enum JTerm {
    Var(JVar, JTy),
    Lit(JLit),
    Call(Option<Box<JTerm>>, JFnId, Vec<JTerm>, JTy),
    Prop(Box<JTerm>, RawSym, JTy),
    BinOp(BinOp, Box<JTerm>, Box<JTerm>),
    Variant(JClass, RawSym),
    Array(Vec<JTerm>, JTy),
    Index(Box<JTerm>, Box<JTerm>, JTy),
    InlineJava(RawSym, JTy),
}

#[derive(Clone, Debug, PartialEq)]
enum JStmt {
    Let(RawSym, JTy, JVar, Option<JTerm>),
    Set(JVar, Option<BinOp>, JTerm),
    IdxSet(JVar, JTerm, Option<BinOp>, JTerm),
    Term(JTerm),
    If(JTerm, Vec<JStmt>, Vec<JStmt>),
    Switch(JBlock, JTerm, Vec<(RawSym, Vec<JStmt>)>, Vec<JStmt>),
    While(JBlock, JTerm, Vec<JStmt>),
    RangeFor(JBlock, RawSym, JVar, JTerm, JTerm, Vec<JStmt>),
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
}

#[derive(Clone, Debug, PartialEq)]
enum JTy {
    I32,
    I64,
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
}

/// This only includes the items that actually need to appear in the Java code
/// i.e. not extern things
#[derive(Clone, Debug, PartialEq)]
enum JItem {
    Fn(JFn),
    Enum(JClass, Vec<RawSym>),
    Let(JVar, JTy, JTerm),
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

    fn is_none(&self) -> bool {
        matches!(self, MaybeList::Tuple(v) if v.is_empty())
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

// CODEGEN

#[derive(Clone, Debug)]
struct Gen<'a> {
    bindings: &'a Bindings,
    /// The bool is whether to mangle names for deduplication
    names: HashMap<u64, (RawPath, bool)>,
    indent: usize,
}
impl<'a> Gen<'a> {
    fn new(bindings: &'a Bindings) -> Self {
        Gen {
            bindings,
            names: HashMap::new(),
            indent: 0,
        }
    }

    fn push(&mut self) {
        self.indent += 1;
    }
    fn pop(&mut self) {
        self.indent -= 1;
    }
    fn indent(&self) -> &'static str {
        // More than five indentation levels will make it harder to read rather than easier
        let s = "\t\t\t\t\t";
        &s[0..self.indent.min(s.len())]
    }

    fn name_str(&self, v: JVar) -> String {
        let (i, b) = &self.names[&v.0];
        let s = self.bindings.resolve_path(i);
        if *b {
            format!("{}${}", s, v.0)
        } else {
            s.to_string()
        }
    }
    fn fn_str(&self, v: JFnId) -> String {
        let (i, b) = &self.names[&v.0];
        let s = self.bindings.resolve_path(i);
        if *b {
            format!("{}${}", s, v.0)
        } else {
            s.to_string()
        }
    }
    fn class_str(&self, v: JClass) -> String {
        let (i, b) = &self.names[&v.0];
        let s = self.bindings.resolve_path(i);
        if *b {
            format!("{}${}", s, v.0)
        } else {
            s.to_string()
        }
    }
}

impl JTerm {
    fn gen(&self, cxt: &Gen) -> String {
        match self {
            JTerm::Var(v, _) => cxt.name_str(*v),
            JTerm::Lit(l) => match l {
                JLit::Int(i) => i.to_string(),
                JLit::Long(i) => format!("{}L", i),
                JLit::Str(s) => format!("\"{}\"", cxt.bindings.resolve_raw(*s)),
                JLit::Bool(b) => b.to_string(),
            },
            JTerm::Call(None, f, a, _) => {
                let mut buf = String::new();
                buf.push_str(&cxt.fn_str(*f));
                buf.push('(');

                let mut first = true;
                for i in a {
                    if !first {
                        buf.push_str(", ");
                    }
                    first = false;

                    buf.push_str(&i.gen(cxt));
                }
                buf.push(')');

                buf
            }
            JTerm::Call(Some(obj), f, a, _) => {
                let mut buf = format!("({}).", obj.gen(cxt));
                buf.push_str(&cxt.fn_str(*f));
                buf.push('(');

                let mut first = true;
                for i in a {
                    if !first {
                        buf.push_str(", ");
                    }
                    first = false;

                    buf.push_str(&i.gen(cxt));
                }
                buf.push(')');

                buf
            }
            JTerm::Prop(obj, prop, _) => {
                format!("{}.{}", obj.gen(cxt), cxt.bindings.resolve_raw(*prop))
            }
            JTerm::BinOp(op @ (BinOp::Eq | BinOp::Neq), a, b) if !a.ty().primitive() => {
                let mut buf = String::new();
                if *op == BinOp::Neq {
                    buf.push('!');
                }
                write!(buf, "({}).equals({})", a.gen(cxt), b.gen(cxt)).unwrap();
                buf
            }
            JTerm::BinOp(op, a, b) => {
                let mut buf = String::new();
                write!(buf, "({}) ", a.gen(cxt)).unwrap();
                buf.push_str(op.repr());
                write!(buf, " ({})", b.gen(cxt)).unwrap();
                buf
            }
            JTerm::Variant(class, variant) => {
                format!(
                    "{}.{}",
                    cxt.bindings.resolve_path(&cxt.names[&class.0].0),
                    cxt.bindings.resolve_raw(*variant)
                )
            }
            // We expand capacity by doubling, so don't allow creating an array with 0 capacity
            // Instead, an empty array starts with 8 capacity
            JTerm::Array(v, t) if v.is_empty() => {
                let t = match t {
                    JTy::Array(t) => &**t,
                    _ => unreachable!(),
                };
                format!("new {}[8]", t.gen(cxt))
            }
            JTerm::Array(v, t) => {
                let mut buf = format!("new {}{{ ", t.gen(cxt));
                for i in v {
                    buf.push_str(&i.gen(cxt));
                    buf.push_str(", ");
                }
                buf.push('}');
                buf
            }
            JTerm::Index(arr, i, _) => {
                format!("{}[{}]", arr.gen(cxt), i.gen(cxt))
            }
            JTerm::InlineJava(raw, _) => cxt.bindings.resolve_raw(*raw).to_string(),
        }
    }
}
impl JStmt {
    fn gen(&self, cxt: &mut Gen) -> String {
        match self {
            JStmt::Let(n, t, v, None) => {
                cxt.names.insert(v.0, (lpath(Spanned::hack(*n)), !v.1));
                format!("{} {} = {};", t.gen(cxt), cxt.name_str(*v), t.null())
            }
            JStmt::Let(n, t, v, Some(x)) => {
                cxt.names.insert(v.0, (lpath(Spanned::hack(*n)), !v.1));
                format!("{} {} = {};", t.gen(cxt), cxt.name_str(*v), x.gen(cxt))
            }
            JStmt::Set(v, op, x) => {
                format!(
                    "{} {}= {};",
                    cxt.name_str(*v),
                    op.map_or("", |op| op.repr()),
                    x.gen(cxt)
                )
            }
            JStmt::IdxSet(v, idx, op, x) => {
                format!(
                    "{}[{}] {}= {};",
                    cxt.name_str(*v),
                    idx.gen(cxt),
                    op.map_or("", |op| op.repr()),
                    x.gen(cxt)
                )
            }
            JStmt::Term(x) => {
                let mut s = x.gen(cxt);
                s.push(';');
                s
            }
            JStmt::While(k, cond, block) => {
                let mut s = format!("b${}: while ({}) {{", k.0, cond.gen(cxt));
                cxt.push();
                for i in block {
                    s.push('\n');
                    s.push_str(cxt.indent());
                    s.push_str(&i.gen(cxt));
                }
                cxt.pop();

                s.push('\n');
                s.push_str(cxt.indent());
                s.push('}');

                s
            }
            JStmt::RangeFor(k, n, var, a, b, block) => {
                cxt.names.insert(var.0, (lpath(Spanned::hack(*n)), !var.1));
                let i = cxt.name_str(*var);
                let mut s = format!(
                    "b${}: for (int {} = {}, $end = {}; {} < $end; {}++) {{",
                    k.0,
                    i,
                    a.gen(cxt),
                    b.gen(cxt),
                    i,
                    i
                );

                cxt.push();
                for i in block {
                    s.push('\n');
                    s.push_str(cxt.indent());
                    s.push_str(&i.gen(cxt));
                }
                cxt.pop();

                s.push('\n');
                s.push_str(cxt.indent());
                s.push('}');

                s
            }
            JStmt::Continue(k) => format!("continue b${};", k.0),
            JStmt::Break(k) => format!("break b${};", k.0),
            JStmt::Ret(_, v) if v.is_empty() => "return;".into(),
            JStmt::Ret(_, v) if v.len() == 1 => format!("return {};", v[0].gen(cxt)),
            JStmt::Ret(f, v) => {
                let mut s = String::new();

                for (i, t) in v.iter().enumerate() {
                    write!(s, "{}$_ret{}$S = {};", cxt.fn_str(*f), i, t.gen(cxt)).unwrap();
                    s.push('\n');
                    s.push_str(cxt.indent());
                }

                s.push_str("return;");
                s
            }
            JStmt::If(cond, a, b) => {
                let mut s = format!("if ({}) {{", cond.gen(cxt));
                cxt.push();
                for i in a {
                    s.push('\n');
                    s.push_str(cxt.indent());
                    s.push_str(&i.gen(cxt));
                }
                cxt.pop();

                s.push('\n');
                s.push_str(cxt.indent());
                s.push('}');

                if !b.is_empty() {
                    cxt.push();

                    s.push_str(" else {");
                    for i in b {
                        s.push('\n');
                        s.push_str(cxt.indent());
                        s.push_str(&i.gen(cxt));
                    }
                    cxt.pop();

                    s.push('\n');
                    s.push_str(cxt.indent());
                    s.push('}');
                }

                s
            }
            JStmt::Switch(k, x, branches, default) => {
                let mut s = format!("b${}: switch ({}) {{", k.0, x.gen(cxt));
                for (sym, block) in branches {
                    // case Variant:
                    s.push('\n');
                    s.push_str(cxt.indent());
                    s.push_str("case ");
                    s.push_str(cxt.bindings.resolve_raw(*sym));
                    s.push(':');

                    cxt.push();
                    for i in block {
                        s.push('\n');
                        s.push_str(cxt.indent());
                        s.push_str(&i.gen(cxt));
                    }
                    s.push('\n');
                    s.push_str(cxt.indent());
                    write!(s, "break b${};", k.0).unwrap();
                    cxt.pop();
                }

                s.push('\n');
                s.push_str(cxt.indent());
                s.push_str("default:");
                cxt.push();
                for i in default {
                    s.push('\n');
                    s.push_str(cxt.indent());
                    s.push_str(&i.gen(cxt));
                }
                cxt.pop();

                s.push('\n');
                s.push_str(cxt.indent());
                s.push('}');

                s
            }
            JStmt::MultiCall(o, f, args, rets) => {
                let mut buf = o
                    .as_ref()
                    .map(|x| {
                        let mut s = x.gen(cxt);
                        s.push('.');
                        s
                    })
                    .unwrap_or_default();
                buf.push_str(&cxt.fn_str(*f));
                buf.push('(');

                let mut first = true;
                for i in args {
                    if !first {
                        buf.push_str(", ");
                    }
                    first = false;

                    buf.push_str(&i.gen(cxt));
                }
                buf.push_str(");");

                for (i, (raw, v, t)) in rets.iter().enumerate() {
                    cxt.names.insert(v.0, (lpath(Spanned::hack(*raw)), !v.1));
                    write!(
                        buf,
                        "\n{}{} {} = {}$_ret{}$S;",
                        cxt.indent(),
                        t.gen(cxt),
                        cxt.name_str(*v),
                        cxt.fn_str(*f),
                        i
                    )
                    .unwrap();
                }

                buf
            }
            JStmt::InlineJava(s) => cxt.bindings.resolve_raw(*s).to_string(),
        }
    }
}
impl JTy {
    fn gen(&self, cxt: &Gen) -> String {
        match self {
            JTy::I32 => "int".into(),
            JTy::I64 => "long".into(),
            JTy::Bool => "boolean".into(),
            JTy::String => "String".into(),
            // Classes are all external, so are never mangled
            JTy::Class(c) => cxt.bindings.resolve_path(&cxt.names[&c.0].0).into(),
            JTy::Array(t) => {
                let mut s = t.gen(cxt);
                s.push_str("[]");
                s
            }
        }
    }
    fn null(&self) -> &'static str {
        match self {
            JTy::I32 => "0",
            JTy::I64 => "0L",
            JTy::Bool => "false",
            JTy::String => "null",
            JTy::Class(_) => "null",
            JTy::Array(_) => "null",
        }
    }
}
impl JFn {
    fn gen(&self, cxt: &mut Gen) -> String {
        let mut buf = String::new();

        if self.ret_tys.len() != 1 {
            // Generate static variables to return tuples into
            // This uses a little bit less bytecode than using e.g. custom classes
            for (i, ty) in self.ret_tys.iter().enumerate() {
                write!(
                    buf,
                    "public static {} {}$_ret{}$S;\n{}",
                    ty.gen(cxt),
                    cxt.fn_str(self.fn_id),
                    i,
                    cxt.indent(),
                )
                .unwrap();
            }
        }

        write!(
            buf,
            "public static {} {}(",
            if self.ret_tys.len() == 1 {
                self.ret_tys[0].gen(cxt)
            } else {
                "void".into()
            },
            cxt.fn_str(self.fn_id)
        )
        .unwrap();
        let names = cxt.names.clone();

        let mut first = true;
        for (n, v, t) in &self.args {
            if !first {
                buf.push_str(", ");
            }
            first = false;
            cxt.names.insert(v.0, (lpath(Spanned::hack(*n)), !v.1));
            write!(buf, "{} {}", t.gen(cxt), cxt.name_str(*v),).unwrap();
        }
        buf.push_str(") {");

        cxt.push();

        for i in &self.body {
            buf.push('\n');
            buf.push_str(cxt.indent());
            buf.push_str(&i.gen(cxt));
        }

        cxt.names = names;
        cxt.pop();

        buf.push('\n');
        buf.push_str(cxt.indent());
        buf.push_str("}\n");
        buf.push_str(cxt.indent());
        buf
    }
}
impl JItem {
    fn gen(&self, cxt: &mut Gen) -> String {
        match self {
            JItem::Fn(f) => f.gen(cxt),
            JItem::Enum(tid, variants) => {
                let mut buf = String::new();
                write!(buf, "public static enum {} {{", cxt.class_str(*tid),).unwrap();
                cxt.push();

                for i in variants {
                    buf.push('\n');
                    buf.push_str(cxt.indent());
                    buf.push_str(cxt.bindings.resolve_raw(*i));
                    buf.push(',');
                }

                cxt.pop();
                buf.push('\n');
                buf.push_str(cxt.indent());
                buf.push_str("}\n");
                buf.push_str(cxt.indent());
                buf
            }
            JItem::Let(var, ty, x) => {
                format!(
                    "public static {} {} = {};\n{}",
                    ty.gen(cxt),
                    cxt.name_str(*var),
                    x.gen(cxt),
                    cxt.indent()
                )
            }
        }
    }
}

// LOWERING

#[derive(Debug)]
pub struct Cxt<'a> {
    bindings: &'a mut Bindings,
    scopes: Vec<(usize, usize, usize)>,
    vars: Vec<(Sym, JVars)>,
    tys: HashMap<JVar, JTy>,
    fn_ids: Vec<(FnId, JFnId)>,
    fn_ret_tys: HashMap<JFnId, JTys>,
    types: Vec<(TypeId, JClass)>,
    block: Vec<JStmt>,
    blocks: Vec<(Option<JBlock>, usize)>,
    current_fn: JFnId,
    items: Vec<JItem>,
    predefs: Vec<(Predef, JFnId)>,
    next: u64,
}
impl<'a> Cxt<'a> {
    pub fn new(bindings: &'a mut Bindings) -> Self {
        Cxt {
            bindings,
            scopes: Vec::new(),
            vars: Vec::new(),
            tys: HashMap::new(),
            fn_ids: Vec::new(),
            fn_ret_tys: HashMap::new(),
            types: Vec::new(),
            block: Vec::new(),
            blocks: Vec::new(),
            current_fn: JFnId(0),
            items: Vec::new(),
            predefs: Vec::new(),
            next: 0,
        }
    }

    fn var(&self, s: Sym) -> Option<JVars> {
        self.vars
            .iter()
            .rfind(|(k, _v)| *k == s)
            .map(|(_k, v)| v.clone())
    }
    fn fun(&self, s: FnId) -> Option<JFnId> {
        self.fn_ids
            .iter()
            .rfind(|(k, _v)| *k == s)
            .map(|(_k, v)| *v)
    }
    fn class(&self, s: TypeId) -> Option<JClass> {
        self.types.iter().rfind(|(k, _v)| *k == s).map(|(_k, v)| *v)
    }
    fn predef(&self, p: Predef) -> JFnId {
        self.predefs.iter().find(|(x, _)| *x == p).unwrap().1
    }

    fn block_label(&self) -> Option<JBlock> {
        self.blocks.iter().rev().find_map(|(x, _)| x.clone())
    }
    fn push_loop(&mut self, k: JBlock) {
        self.push();
        self.blocks.push((Some(k), self.block.len()));
    }
    /// Implies push()
    fn push_block(&mut self) {
        self.push();
        self.blocks.push((None, self.block.len()));
    }
    fn pop_block(&mut self) -> Vec<JStmt> {
        self.pop();
        self.block.split_off(self.blocks.pop().unwrap().1)
    }

    fn push(&mut self) {
        self.scopes
            .push((self.vars.len(), self.fn_ids.len(), self.types.len()));
    }
    fn pop(&mut self) {
        let (v, i, t) = self.scopes.pop().unwrap();
        self.vars.truncate(v);
        self.fn_ids.truncate(i);
        self.types.truncate(t);
    }

    fn fresh_var(&mut self, public: bool) -> JVar {
        self.next += 1;
        JVar(self.next, public)
    }
    fn fresh_fn(&mut self) -> JFnId {
        self.next += 1;
        JFnId(self.next)
    }
    fn fresh_class(&mut self) -> JClass {
        self.next += 1;
        JClass(self.next)
    }
    fn fresh_block(&mut self) -> JBlock {
        self.next += 1;
        JBlock(self.next)
    }
}

impl JTerm {
    /// Whether this term is simple enough to be 1 bytecode instruction.
    /// Simple instructions can be duplicated freely.
    fn simple(&self) -> bool {
        match self {
            JTerm::Var(_, _) | JTerm::Variant(_, _) | JTerm::Lit(_) => true,
            JTerm::Call(_, _, _, _)
            | JTerm::BinOp(_, _, _)
            | JTerm::Index(_, _, _)
            | JTerm::Prop(_, _, _)
            | JTerm::InlineJava(_, _)
            | JTerm::Array(_, _) => false,
        }
    }

    fn ty(&self) -> JTy {
        match self {
            JTerm::Var(_, t) => t.clone(),
            JTerm::Lit(l) => match l {
                JLit::Int(_) => JTy::I32,
                JLit::Long(_) => JTy::I64,
                JLit::Str(_) => JTy::String,
                JLit::Bool(_) => JTy::Bool,
            },
            JTerm::Call(_, _, _, t) => t.clone(),
            JTerm::Prop(_, _, t) => t.clone(),
            JTerm::InlineJava(_, t) => t.clone(),
            JTerm::Array(_, t) => t.clone(),
            JTerm::Index(_, _, t) => t.clone(),
            JTerm::BinOp(op, a, _) => match op.ty() {
                BinOpType::Comp => JTy::Bool,
                BinOpType::Arith => a.ty(),
                BinOpType::Logic => JTy::Bool,
            },
            JTerm::Variant(class, _) => JTy::Class(*class),
        }
    }
}

impl Term {
    fn lower(&self, cxt: &mut Cxt) -> JTerms {
        JTerms::One(match self {
            Term::Var(s) => {
                let var = cxt.var(*s).unwrap();
                return var.map(|var| JTerm::Var(var, cxt.tys.get(&var).unwrap().clone()));
            }
            Term::Lit(l, t) => match l {
                Literal::Int(i) => match t {
                    Type::I32 => JTerm::Lit(JLit::Int(*i as i32)),
                    Type::I64 => JTerm::Lit(JLit::Long(*i)),
                    _ => unreachable!(),
                },
                Literal::Str(s) => JTerm::Lit(JLit::Str(*s)),
                Literal::Bool(b) => JTerm::Lit(JLit::Bool(*b)),
            },
            Term::Break => {
                cxt.block.push(JStmt::Break(
                    cxt.block_label().expect("'break' outside of loop"),
                ));
                return JTerms::empty();
            }
            Term::Continue => {
                cxt.block.push(JStmt::Continue(
                    cxt.block_label().expect("'continue' outside of loop"),
                ));
                return JTerms::empty();
            }
            Term::Return(x) => {
                let x = x.as_ref().map(|x| x.lower(cxt));
                cxt.block.push(JStmt::Ret(
                    cxt.current_fn,
                    x.into_iter().flatten().collect(),
                ));
                return JTerms::empty();
            }
            Term::Variant(tid, s) => JTerm::Variant(cxt.class(*tid).unwrap(), *s),
            Term::Tuple(v) => return JTerms::Tuple(v.iter().flat_map(|x| x.lower(cxt)).collect()),
            Term::TupleIdx(x, i) => {
                let x = x.lower(cxt);
                x.to_vec().swap_remove(*i)
            }
            Term::Set(l, op, x) => match l {
                LValue::Var(v) => {
                    let v = cxt.var(*v).unwrap();
                    let x = x.lower(cxt);
                    for (v, x) in v.into_iter().zip(x) {
                        cxt.block.push(JStmt::Set(v, *op, x));
                    }
                    return JTerms::empty();
                }
                LValue::Idx(v, idx) => {
                    let v = cxt.var(*v).unwrap();
                    let mut idx = idx.lower(cxt).one();
                    if !idx.simple() {
                        // Don't recompute idx every time, store it in a local
                        let raw = cxt.bindings.raw("$_idx");
                        let var = cxt.fresh_var(false);
                        cxt.block.push(JStmt::Let(raw, JTy::I32, var, Some(idx)));
                        idx = JTerm::Var(var, JTy::I32);
                    }
                    let x = x.lower(cxt);
                    for (v, x) in v.into_iter().zip(x) {
                        cxt.block.push(JStmt::IdxSet(v, idx.clone(), *op, x));
                    }
                    return JTerms::empty();
                }
            },
            Term::Array(v, t) if v.is_empty() => {
                let t = t.lower(cxt);
                return JTerms::Tuple(
                    t.into_iter()
                        .map(|ty| JTerm::Array(Vec::new(), JTy::Array(Box::new(ty))))
                        .chain(std::iter::once(JTerm::Lit(JLit::Int(0))))
                        .collect(),
                );
            }
            Term::Array(v, _) => {
                let mut v2 = Vec::new();
                let mut len = 0;
                for i in v {
                    len += 1;
                    let i = i.lower(cxt);

                    // Split into tuple/struct members to put in arrays
                    if v2.is_empty() {
                        v2 = i.into_iter().map(|x| (x.ty(), vec![x])).collect();
                    } else {
                        assert_eq!(i.len(), v2.len());
                        for ((_, arr), elem) in v2.iter_mut().zip(i) {
                            arr.push(elem);
                        }
                    }
                }
                return JTerms::Tuple(
                    v2.into_iter()
                        .map(|(ty, arr)| JTerm::Array(arr, JTy::Array(Box::new(ty))))
                        .chain(std::iter::once(JTerm::Lit(JLit::Int(len))))
                        .collect(),
                );
            }
            Term::ArrayIdx(arr, idx) => {
                let arrs = arr.lower(cxt);
                let mut idx = idx.lower(cxt).one();
                // The last element in the list is the length
                let narrs = arrs.len() - 1;
                if narrs > 1 && !idx.simple() {
                    // Don't recompute idx every time, store it in a local
                    let raw = cxt.bindings.raw("$_idx");
                    let var = cxt.fresh_var(false);
                    cxt.block.push(JStmt::Let(raw, JTy::I32, var, Some(idx)));
                    idx = JTerm::Var(var, JTy::I32);
                }
                // TODO optional bounds checking
                return JTerms::Tuple(
                    arrs.into_iter()
                        .take(narrs)
                        .map(|arr| {
                            let ty = match arr.ty() {
                                JTy::Array(t) => *t,
                                _ => unreachable!(),
                            };
                            JTerm::Index(Box::new(arr), Box::new(idx.clone()), ty)
                        })
                        .collect(),
                );
            }
            Term::ArrayMethod(arr, m) => {
                let arrs = arr.lower(cxt);
                let len = arrs.clone().to_vec().pop().unwrap();
                match m {
                    ArrayMethod::Len => len,
                    ArrayMethod::Pop => {
                        match len {
                            JTerm::Var(v, _) => {
                                // `a -= 1` is as fast as `a--`, but `a = a - 1` is slower
                                cxt.block.push(JStmt::Set(
                                    v,
                                    Some(BinOp::Sub),
                                    JTerm::Lit(JLit::Int(1)),
                                ));
                                // return a[len]
                                let n = arrs.len() - 1;
                                return JTerms::Tuple(
                                    arrs.into_iter()
                                        .take(n)
                                        .map(|x| {
                                            let ty = match x.ty() {
                                                JTy::Array(t) => *t,
                                                _ => unreachable!(),
                                            };
                                            JTerm::Index(Box::new(x), Box::new(len.clone()), ty)
                                        })
                                        .collect(),
                                );
                            }
                            _ => unreachable!("pop() requires an rvalue"),
                        }
                    }
                    ArrayMethod::Push(x) => match &len {
                        JTerm::Var(v, _) => {
                            cxt.block.push(JStmt::Set(
                                *v,
                                Some(BinOp::Add),
                                JTerm::Lit(JLit::Int(1)),
                            ));
                            let x = x.lower(cxt);
                            assert_eq!(x.len(), arrs.len() - 1);
                            // Check if the array needs expanding
                            if arrs.len() != 1 {
                                let cap = JTerm::Prop(
                                    Box::new(arrs.clone().to_vec().swap_remove(0)),
                                    cxt.bindings.raw("length"),
                                    JTy::I32,
                                );
                                let too_small =
                                    JTerm::BinOp(BinOp::Gt, Box::new(len.clone()), Box::new(cap));
                                let mut block = Vec::new();
                                let mut block2 = Vec::new();
                                for (arr, x) in arrs.clone().into_iter().zip(x) {
                                    match &arr {
                                        JTerm::Var(var, _) => {
                                            // arr = Arrays.copyOf(arr, arr.length * 2);
                                            let copy_fn = cxt.predef(Predef::ArrayCopy);
                                            let cap = JTerm::Prop(
                                                Box::new(arr.clone()),
                                                cxt.bindings.raw("length"),
                                                JTy::I32,
                                            );
                                            let new_cap = JTerm::BinOp(
                                                BinOp::Mul,
                                                Box::new(cap),
                                                Box::new(JTerm::Lit(JLit::Int(2))),
                                            );
                                            let new = JTerm::Call(
                                                None,
                                                copy_fn,
                                                vec![arr.clone(), new_cap],
                                                arr.ty(),
                                            );
                                            block.push(JStmt::Set(*var, None, new));

                                            // arr[len-1] = x;
                                            let idx = JTerm::BinOp(
                                                BinOp::Sub,
                                                Box::new(len.clone()),
                                                Box::new(JTerm::Lit(JLit::Int(1))),
                                            );
                                            block2.push(JStmt::IdxSet(*var, idx, None, x));
                                        }
                                        _ => unreachable!(),
                                    }
                                }
                                // Expand, then set
                                cxt.block.push(JStmt::If(too_small, block, Vec::new()));
                                cxt.block.append(&mut block2);
                            }
                            return JTerms::empty();
                        }
                        _ => unreachable!("push() requires an rvalue"),
                    },
                }
            }
            Term::Call(o, f, a) => {
                let fn_id = cxt.fun(*f).unwrap();
                let o = o.as_ref().map(|x| Box::new(x.lower(cxt).one()));
                let args = a.iter().flat_map(|x| x.lower(cxt)).collect();
                let rtys = cxt.fn_ret_tys.get(&fn_id).unwrap().clone();
                match rtys {
                    MaybeList::One(rty) => JTerm::Call(o, fn_id, args, rty),
                    MaybeList::Tuple(v) => {
                        // MultiCall time
                        let mut vars = Vec::new();
                        let mut terms = Vec::new();
                        for (i, ty) in v.into_iter().enumerate() {
                            let var = cxt.fresh_var(false);
                            cxt.tys.insert(var, ty.clone());
                            let raw = cxt.bindings.raw(format!(
                                "{}$_call_ret{}",
                                cxt.bindings.resolve_raw(*cxt.bindings.fn_name(*f).stem()),
                                i
                            ));
                            terms.push(JTerm::Var(var, ty.clone()));
                            vars.push((raw, var, ty));
                        }
                        cxt.block.push(JStmt::MultiCall(o, fn_id, args, vars));

                        return JTerms::Tuple(terms);
                    }
                }
            }
            Term::BinOp(op, a, b) => JTerm::BinOp(
                *op,
                Box::new(a.lower(cxt).one()),
                Box::new(b.lower(cxt).one()),
            ),
            Term::Block(v, e) => {
                cxt.push();
                for i in v {
                    i.lower(cxt);
                }
                let r = e
                    .as_ref()
                    .map(|x| x.lower(cxt))
                    .unwrap_or(JTerms::Tuple(Vec::new()));
                cxt.pop();
                return r;
            }
            Term::If(cond, a, b) => {
                let cond = cond.lower(cxt).one();

                cxt.push_block();
                let a = a.lower(cxt);
                let ty = a.ty();

                let vars: Vec<_> = ty
                    .clone()
                    .into_iter()
                    .enumerate()
                    .map(|(i, t)| {
                        (
                            cxt.fresh_var(false),
                            cxt.bindings.raw(format!("_then${}", i)),
                            t,
                        )
                    })
                    .collect();
                for ((var, _, ty), a) in vars.iter().zip(a) {
                    cxt.tys.insert(*var, ty.clone());
                    cxt.block.push(JStmt::Set(*var, None, a));
                }
                let a = cxt.pop_block();

                let b = if let Some(b) = b {
                    cxt.push_block();
                    let b = b.lower(cxt);
                    for ((var, _, _), b) in vars.iter().zip(b) {
                        cxt.block.push(JStmt::Set(*var, None, b));
                    }
                    cxt.pop_block()
                } else {
                    Vec::new()
                };

                let mut ret = Vec::new();
                for (var, raw, ty) in vars {
                    cxt.block.push(JStmt::Let(raw, ty.clone(), var, None));
                    ret.push(JTerm::Var(var, ty));
                }
                cxt.block.push(JStmt::If(cond, a, b));

                return JTerms::Tuple(ret);
            }
            Term::Match(x, branches) => {
                let x = x.lower(cxt).one();

                let mut v = Vec::new();
                let mut default = None;
                let mut vars: Option<Vec<_>> = None;
                for (s, t) in branches {
                    cxt.push_block();
                    let t = t.lower(cxt);
                    if vars.is_none() {
                        let ty = t.ty();
                        vars = Some(
                            ty.clone()
                                .into_iter()
                                .enumerate()
                                .map(|(i, t)| {
                                    (
                                        cxt.fresh_var(false),
                                        cxt.bindings.raw(format!("_then${}", i)),
                                        t,
                                    )
                                })
                                .collect(),
                        );
                        for (var, _, ty) in vars.as_ref().unwrap() {
                            cxt.tys.insert(*var, ty.clone());
                        }
                    }
                    for ((var, _, _), t) in vars.as_ref().unwrap().iter().zip(t) {
                        cxt.block.push(JStmt::Set(*var, None, t));
                    }
                    let block = cxt.pop_block();

                    match s {
                        Some(s) => v.push((*s, block)),
                        None => {
                            if default.is_none() {
                                default = Some(block);
                            } else {
                                unreachable!()
                            }
                        }
                    }
                }

                let mut ret = Vec::new();
                for (var, raw, ty) in vars.unwrap() {
                    cxt.block.push(JStmt::Let(raw, ty.clone(), var, None));
                    ret.push(JTerm::Var(var, ty));
                }
                let k = cxt.fresh_block();
                cxt.block
                    .push(JStmt::Switch(k, x, v, default.unwrap_or_default()));

                return JTerms::Tuple(ret);
            }
        })
    }
}
impl Statement {
    fn lower(&self, cxt: &mut Cxt) {
        match self {
            Statement::Term(x) => {
                let terms = x.lower(cxt);
                for i in terms {
                    cxt.block.push(JStmt::Term(i));
                }
            }
            Statement::Let(n, t, x) => {
                let x = x.lower(cxt);
                let t = t.lower(cxt);
                let mut vars = Vec::new();

                for (x, t) in x.into_iter().zip(t) {
                    let var = cxt.fresh_var(cxt.bindings.public(*n));
                    cxt.tys.insert(var, t.clone());
                    cxt.block.push(JStmt::Let(
                        *cxt.bindings.sym_path(*n).stem(),
                        t,
                        var,
                        Some(x),
                    ));
                    vars.push(var);
                }

                cxt.vars.push((*n, JVars::Tuple(vars)));
            }
            Statement::While(cond, block) => {
                let cond = cond.lower(cxt).one();

                let k = cxt.fresh_block();
                cxt.push_loop(k);
                for i in block {
                    i.lower(cxt);
                }
                let block = cxt.pop_block();

                cxt.block.push(JStmt::While(k, cond, block));
            }
            Statement::For(s, iter, block) => {
                match iter {
                    ForIter::Range(a, b) => {
                        let a = a.lower(cxt).one();
                        let b = b.lower(cxt).one();

                        let v = cxt.fresh_var(cxt.bindings.public(*s));
                        cxt.tys.insert(v, JTy::I32);

                        let k = cxt.fresh_block();
                        cxt.push_loop(k);
                        for i in block {
                            i.lower(cxt);
                        }
                        let block = cxt.pop_block();

                        cxt.block.push(JStmt::RangeFor(
                            k,
                            *cxt.bindings.sym_path(*s).stem(),
                            v,
                            a,
                            b,
                            block,
                        ));
                    }
                    ForIter::Array(arr) => {
                        let arr = arr.lower(cxt);
                        let t = arr.ty();

                        let start = JTerm::Lit(JLit::Int(0));
                        let len = arr.clone().to_vec().pop().unwrap();

                        let ix_var = cxt.fresh_var(false);
                        cxt.tys.insert(ix_var, JTy::I32);

                        let k = cxt.fresh_block();
                        cxt.push_loop(k);
                        // let s = arr[i];
                        let mut vars = Vec::new();
                        for (x, t) in arr.clone().into_iter().zip(t) {
                            let t = match t {
                                JTy::Array(t) => *t,
                                // skip the array length
                                JTy::I32 => break,
                                _ => unreachable!(),
                            };
                            let x = JTerm::Index(
                                Box::new(x),
                                Box::new(JTerm::Var(ix_var, JTy::I32)),
                                t.clone(),
                            );
                            let var = cxt.fresh_var(cxt.bindings.public(*s));
                            cxt.tys.insert(var, t.clone());
                            cxt.block.push(JStmt::Let(
                                *cxt.bindings.sym_path(*s).stem(),
                                t,
                                var,
                                Some(x),
                            ));
                            vars.push(var);
                        }
                        cxt.vars.push((*s, JVars::Tuple(vars)));
                        for i in block {
                            i.lower(cxt);
                        }
                        let block = cxt.pop_block();

                        cxt.block.push(JStmt::RangeFor(
                            k,
                            *cxt.bindings.sym_path(*s).stem(),
                            ix_var,
                            start,
                            len,
                            block,
                        ));
                    }
                }
            }
            Statement::InlineJava(s) => {
                cxt.block.push(JStmt::InlineJava(*s));
            }
        }
    }
}
impl Item {
    fn lower(&self, cxt: &mut Cxt) {
        match self {
            // Module-level inline java is handled by codegen()
            Item::InlineJava(_) => (),
            Item::Fn(f) => {
                let mut block = Vec::new();
                let fn_id = cxt.fun(f.id).unwrap();
                std::mem::swap(&mut block, &mut cxt.block);

                cxt.push();
                cxt.current_fn = fn_id;
                let mut args = Vec::new();
                for (name, ty) in &f.args {
                    let mut vars = Vec::new();
                    for ty in ty.lower(cxt) {
                        let var = cxt.fresh_var(cxt.bindings.public(*name));
                        args.push((*cxt.bindings.sym_path(*name).stem(), var, ty.clone()));
                        cxt.tys.insert(var, ty);
                        vars.push(var);
                    }
                    cxt.vars.push((name.clone(), JVars::Tuple(vars)));
                }
                let ret = f.body.lower(cxt);
                match (ret, &f.ret_ty) {
                    // Java doesn't like using 'return' with void functions
                    (ret, Type::Unit) => {
                        for i in ret {
                            cxt.block.push(JStmt::Term(i))
                        }
                    }
                    (ret, _) => cxt.block.push(JStmt::Ret(fn_id, ret.into())),
                }
                cxt.pop();

                std::mem::swap(&mut block, &mut cxt.block);
                let ret_ty = f.ret_ty.lower(cxt);
                cxt.items.push(JItem::Fn(JFn {
                    name: *cxt.bindings.fn_name(f.id).stem(),
                    fn_id,
                    ret_tys: ret_ty.into(),
                    args,
                    body: block,
                    public: f.public,
                }));
            }
            Item::Enum(tid, variants, ext) => {
                if !ext {
                    let class = cxt.class(*tid).unwrap();

                    cxt.items.push(JItem::Enum(class, variants.clone()));
                }
            }
            Item::ExternFn(_) => (),
            Item::ExternClass(_) => (),
            Item::Let(name, ty, x) => {
                let var = cxt.var(*name).unwrap();
                let ty = ty.lower(cxt);
                let x = x.lower(cxt);
                assert_eq!(var.len(), ty.len());
                assert_eq!(ty.len(), x.len());
                for ((var, ty), x) in var.into_iter().zip(ty).zip(x) {
                    cxt.items.push(JItem::Let(var, ty, x));
                }
            }
        }
    }
}
impl Type {
    fn lower(&self, cxt: &Cxt) -> JTys {
        JTys::One(match self {
            Type::I32 => JTy::I32,
            Type::I64 => JTy::I64,
            Type::Bool => JTy::Bool,
            Type::Str => JTy::String,
            Type::Unit => return JTys::empty(),
            Type::Class(c) => JTy::Class(cxt.class(*c).unwrap()),
            Type::Tuple(v) => return JTys::Tuple(v.iter().flat_map(|x| x.lower(cxt)).collect()),
            // Automatic struct-of-arrays
            // This actually has basically no effect on bytecode count - in testing, it only made a one instruction difference
            // It also includes the length at the end so that the array is dynamic
            Type::Array(t) => {
                return JTys::Tuple(
                    t.lower(cxt)
                        .into_iter()
                        .map(Box::new)
                        .map(JTy::Array)
                        .chain(std::iter::once(JTy::I32))
                        .collect(),
                )
            }
        })
    }
}
