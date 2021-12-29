use std::collections::HashMap;
use std::fmt::Write;

use crate::term::*;

// Entry point

pub fn codegen(code: &[Item], bindings: &mut Bindings, out_class: &str) -> String {
    let mut cxt = Cxt::new(bindings);
    // Declare items
    let mut mappings = Vec::new();
    let mut java = Vec::new();
    let mut enums = Vec::new();
    for i in code {
        let (name, ret, m, public) = match i {
            Item::Fn(f) => (f.id, &f.ret_ty, cxt.bindings.fn_name(f.id), f.public),
            Item::ExternFn(f) => (f.id, &f.ret_ty, f.mapping, true),
            Item::ExternClass(c) => {
                let class = cxt.fresh_class();
                cxt.types.push((*c, class));
                mappings.push((class.0, cxt.bindings.type_name(*c), false));

                continue;
            }
            Item::Enum(c, variants, ext) => {
                let class = cxt.fresh_class();
                cxt.types.push((*c, class));
                mappings.push((class.0, cxt.bindings.type_name(*c), false));

                if !ext {
                    enums.push((class, variants));
                }

                continue;
            }
            Item::InlineJava(s) => {
                java.push(*s);
                continue;
            }
        };
        let item = cxt.fresh_fn();
        cxt.fn_ids.push((name, item));

        let ret = ret.lower(&cxt);
        cxt.fn_ret_tys.insert(item, ret);
        mappings.push((item.0, m, !public));
    }
    for i in code {
        i.lower(&mut cxt);
    }

    let fns = cxt.items;
    let mut gen = Gen::new(bindings);
    // Declare items
    for (i, m, b) in mappings {
        gen.names.insert(i, (m, b));
    }
    // Generate items
    let mut s = String::new();
    // Add module-level inline Java at the top
    for i in java {
        s.push_str(bindings.resolve_raw(i));
        s.push('\n');
    }
    write!(s, "\npublic class {} {{\n\n", out_class).unwrap();
    for f in fns {
        s.push_str(&f.gen(&mut gen));
    }
    s.push_str("\n}");

    s
}

// Java AST

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// bool: whether it's public, so mangling should be skipped
struct JVar(u64, bool);
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct JFnId(u64);
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct JClass(u64);

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
    Call(JFnId, Vec<JTerm>, JTy),
    Method(Box<JTerm>, JFnId, Vec<JTerm>, JTy),
    BinOp(BinOp, Box<JTerm>, Box<JTerm>),
    Variant(JClass, RawSym),
    None,
}

#[derive(Clone, Debug, PartialEq)]
enum JStmt {
    Let(RawSym, JTy, JVar, Option<JTerm>),
    Set(JVar, JTerm),
    Term(JTerm),
    If(JTerm, Vec<JStmt>, Vec<JStmt>),
    Switch(JTerm, Vec<(RawSym, Vec<JStmt>)>, Vec<JStmt>),
    Ret(JTerm),
    InlineJava(RawSym),
}

#[derive(Clone, Debug, PartialEq)]
enum JTy {
    I32,
    I64,
    Bool,
    String,
    Void,
    Class(JClass),
}
impl JTy {
    fn primitive(&self) -> bool {
        match self {
            JTy::I32 => true,
            JTy::I64 => true,
            JTy::Bool => true,
            JTy::Void => true,
            JTy::String => false,
            JTy::Class(_) => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct JFn {
    name: RawSym,
    item: JFnId,
    ret_ty: JTy,
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
}

// CODEGEN

#[derive(Clone, Debug)]
struct Gen<'a> {
    bindings: &'a Bindings,
    /// The bool is whether to mangle names for deduplication
    names: HashMap<u64, (RawSym, bool)>,
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
        let (i, b) = self.names[&v.0];
        let s = self.bindings.resolve_raw(i);
        if b {
            format!("{}${}", s, v.0)
        } else {
            s.to_string()
        }
    }
    fn fn_str(&self, v: JFnId) -> String {
        let (i, b) = self.names[&v.0];
        let s = self.bindings.resolve_raw(i);
        if b {
            format!("{}${}", s, v.0)
        } else {
            s.to_string()
        }
    }
    fn class_str(&self, v: JClass) -> String {
        let (i, b) = self.names[&v.0];
        let s = self.bindings.resolve_raw(i);
        if b {
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
            JTerm::Call(f, a, _) => {
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
            JTerm::Method(obj, f, a, _) => {
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
                    cxt.bindings.resolve_raw(cxt.names[&class.0].0),
                    cxt.bindings.resolve_raw(*variant)
                )
            }
            JTerm::None => "<None>".into(),
        }
    }
}
impl JStmt {
    fn gen(&self, cxt: &mut Gen) -> String {
        match self {
            JStmt::Let(n, t, v, None) => {
                cxt.names.insert(v.0, (*n, !v.1));
                format!("{} {} = {};", t.gen(cxt), cxt.name_str(*v), t.null())
            }
            JStmt::Let(n, t, v, Some(x)) => {
                cxt.names.insert(v.0, (*n, !v.1));
                format!("{} {} = {};", t.gen(cxt), cxt.name_str(*v), x.gen(cxt))
            }
            JStmt::Set(v, x) => {
                format!("{} = {};", cxt.name_str(*v), x.gen(cxt))
            }
            JStmt::Term(JTerm::None) => "".to_string(),
            JStmt::Term(x) => {
                let mut s = x.gen(cxt);
                s.push(';');
                s
            }
            JStmt::Ret(x) => format!("return {};", x.gen(cxt)),
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
            JStmt::Switch(x, branches, default) => {
                let mut s = format!("switch ({}) {{", x.gen(cxt));
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
                    s.push_str("break;");
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
            JTy::Void => "void".into(),
            // Classes are all external, so are never mangled
            JTy::Class(c) => cxt.bindings.resolve_raw(cxt.names[&c.0].0).into(),
        }
    }
    fn null(&self) -> &'static str {
        match self {
            JTy::I32 => "0",
            JTy::I64 => "0L",
            JTy::Bool => "false",
            JTy::String => "null",
            JTy::Void => unreachable!(),
            JTy::Class(_) => "null",
        }
    }
}
impl JFn {
    fn gen(&self, cxt: &mut Gen) -> String {
        let mut buf = String::new();
        write!(
            buf,
            "public static {} {}(",
            self.ret_ty.gen(cxt),
            cxt.fn_str(self.item)
        )
        .unwrap();
        let names = cxt.names.clone();

        let mut first = true;
        for (n, v, t) in &self.args {
            if !first {
                buf.push_str(", ");
            }
            first = false;
            cxt.names.insert(v.0, (*n, !v.1));
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
        }
    }
}

// LOWERING

#[derive(Debug)]
struct Cxt<'a> {
    bindings: &'a mut Bindings,
    scopes: Vec<(usize, usize, usize)>,
    vars: Vec<(Sym, JVar)>,
    tys: HashMap<JVar, JTy>,
    fn_ids: Vec<(FnId, JFnId)>,
    fn_ret_tys: HashMap<JFnId, JTy>,
    types: Vec<(TypeId, JClass)>,
    block: Vec<JStmt>,
    blocks: Vec<usize>,
    items: Vec<JItem>,
    next: u64,
}
impl<'a> Cxt<'a> {
    fn new(bindings: &'a mut Bindings) -> Self {
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
            items: Vec::new(),
            next: 0,
        }
    }

    fn var(&self, s: Sym) -> Option<JVar> {
        self.vars.iter().rfind(|(k, _v)| *k == s).map(|(_k, v)| *v)
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

    /// Implies push()
    fn push_block(&mut self) {
        self.push();
        self.blocks.push(self.block.len());
    }
    fn pop_block(&mut self) -> Vec<JStmt> {
        self.pop();
        self.block.split_off(self.blocks.pop().unwrap())
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
}

impl JTerm {
    fn ty(&self) -> JTy {
        match self {
            JTerm::Var(_, t) => t.clone(),
            JTerm::Lit(l) => match l {
                JLit::Int(_) => JTy::I32,
                JLit::Long(_) => JTy::I64,
                JLit::Str(_) => JTy::String,
                JLit::Bool(_) => JTy::Bool,
            },
            JTerm::Call(_, _, t) => t.clone(),
            JTerm::Method(_, _, _, t) => t.clone(),
            JTerm::BinOp(op, a, _) => match op.ty() {
                BinOpType::Comp => JTy::Bool,
                BinOpType::Arith => a.ty(),
                BinOpType::Logic => JTy::Bool,
            },
            JTerm::Variant(class, _) => JTy::Class(*class),
            JTerm::None => JTy::Void,
        }
    }
}

impl Term {
    fn lower(&self, cxt: &mut Cxt) -> JTerm {
        match self {
            Term::Var(s) => {
                let var = cxt.var(*s).unwrap();
                JTerm::Var(var, cxt.tys.get(&var).unwrap().clone())
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
            Term::Variant(tid, s) => JTerm::Variant(cxt.class(*tid).unwrap(), *s),
            Term::Call(f, a) => {
                let item = cxt.fun(*f).unwrap();
                JTerm::Call(
                    item,
                    a.iter().map(|x| x.lower(cxt)).collect(),
                    cxt.fn_ret_tys.get(&item).unwrap().clone(),
                )
            }
            Term::Method(o, f, a) => {
                let item = cxt.fun(*f).unwrap();
                JTerm::Method(
                    Box::new(o.lower(cxt)),
                    item,
                    a.iter().map(|x| x.lower(cxt)).collect(),
                    cxt.fn_ret_tys.get(&item).unwrap().clone(),
                )
            }
            Term::BinOp(op, a, b) => {
                JTerm::BinOp(*op, Box::new(a.lower(cxt)), Box::new(b.lower(cxt)))
            }
            Term::Block(v, e) => {
                cxt.push();
                for i in v {
                    i.lower(cxt);
                }
                let r = e.as_ref().map(|x| x.lower(cxt)).unwrap_or(JTerm::None);
                cxt.pop();
                r
            }
            Term::If(cond, a, b) => {
                let cond = cond.lower(cxt);

                cxt.push_block();
                let a = a.lower(cxt);
                let ty = a.ty();
                // It's important that we don't try to make a void variable, Java doesn't like that
                let do_var = ty != JTy::Void;

                let var = cxt.fresh_var(false);
                let raw = cxt.bindings.raw("_then");
                if do_var {
                    cxt.tys.insert(var, ty.clone());
                    cxt.block.push(JStmt::Set(var, a));
                }
                let a = cxt.pop_block();

                let b = if let Some(b) = b {
                    cxt.push_block();
                    let b = b.lower(cxt);
                    if do_var {
                        cxt.block.push(JStmt::Set(var, b));
                    }
                    cxt.pop_block()
                } else {
                    Vec::new()
                };

                if do_var {
                    cxt.block.push(JStmt::Let(raw, ty.clone(), var, None));
                }
                cxt.block.push(JStmt::If(cond, a, b));

                if do_var {
                    JTerm::Var(var, ty)
                } else {
                    JTerm::None
                }
            }
            Term::Match(x, branches) => {
                let x = x.lower(cxt);

                let var = cxt.fresh_var(false);
                let raw = cxt.bindings.raw("_then");

                let mut v = Vec::new();
                let mut default = None;
                let mut rty = None;
                let mut do_var = false;
                for (s, t) in branches {
                    cxt.push_block();
                    let t = t.lower(cxt);
                    let ty = t.ty();
                    if rty.is_none() {
                        do_var = ty != JTy::Void;
                        if do_var {
                            cxt.tys.insert(var, ty.clone());
                        }
                        rty = Some(ty);
                    }
                    if do_var {
                        cxt.block.push(JStmt::Set(var, t));
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

                if do_var {
                    cxt.block
                        .push(JStmt::Let(raw, rty.as_ref().cloned().unwrap(), var, None));
                }
                cxt.block
                    .push(JStmt::Switch(x, v, default.unwrap_or_default()));

                if do_var {
                    JTerm::Var(var, rty.unwrap())
                } else {
                    JTerm::None
                }
            }
        }
    }
}
impl Statement {
    fn lower(&self, cxt: &mut Cxt) {
        match self {
            Statement::Term(x) => {
                let term = x.lower(cxt);
                cxt.block.push(JStmt::Term(term));
            }
            Statement::Let(n, t, x) => {
                let var = cxt.fresh_var(cxt.bindings.public(*n));
                cxt.vars.push((*n, var));
                let t = t.lower(cxt);
                let x = x.lower(cxt);
                cxt.tys.insert(var, t.clone());
                cxt.block.push(JStmt::Let(n.raw(), t, var, Some(x)));
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
                std::mem::swap(&mut block, &mut cxt.block);

                cxt.push();
                let mut args = Vec::new();
                for (name, ty) in &f.args {
                    let var = cxt.fresh_var(cxt.bindings.public(*name));
                    let ty = ty.lower(cxt);
                    args.push((name.raw(), var, ty.clone()));
                    cxt.tys.insert(var, ty);
                    cxt.vars.push((name.clone(), var));
                }
                let ret = f.body.lower(cxt);
                match (ret, &f.ret_ty) {
                    (JTerm::None, _) => (),
                    // Java doesn't like using 'return' with void functions
                    (ret, Type::Unit) => cxt.block.push(JStmt::Term(ret)),
                    (ret, _) => cxt.block.push(JStmt::Ret(ret)),
                }
                cxt.pop();

                std::mem::swap(&mut block, &mut cxt.block);
                let ret_ty = f.ret_ty.lower(cxt);
                let item = cxt.fun(f.id).unwrap();
                cxt.items.push(JItem::Fn(JFn {
                    name: cxt.bindings.fn_name(f.id),
                    item,
                    ret_ty,
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
        }
    }
}
impl Type {
    fn lower(&self, cxt: &Cxt) -> JTy {
        match self {
            Type::I32 => JTy::I32,
            Type::I64 => JTy::I64,
            Type::Bool => JTy::Bool,
            Type::Str => JTy::String,
            Type::Unit => JTy::Void,
            Type::Class(c) => JTy::Class(cxt.class(*c).unwrap()),
        }
    }
}
