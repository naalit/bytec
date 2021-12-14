use std::collections::HashMap;
use std::fmt::Write;

use crate::term::*;

// Entry point

pub fn codegen(code: &[Item], bindings: &Bindings, out_class: &str) -> String {
    let mut cxt = Cxt::new(bindings);
    // Declare items
    for i in code {
        let name = match i {
            Item::Fn(f) => f.id,
        };
        let item = cxt.fresh_item();
        cxt.items.push((name, item));
    }
    for i in code {
        i.lower(&mut cxt);
    }

    let mut gen = Gen::new(bindings);
    // Declare functions
    for f in &cxt.fns {
        gen.names.insert(f.item.0, f.name.clone());
    }
    // Generate functions
    let mut s = format!("public class {} {{\n\n", out_class);
    for f in cxt.fns {
        s.push_str(&f.gen(&mut gen));
    }
    s.push_str("\n}");

    s
}

// Java AST

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct JVar(u64);
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct JItem(u64);

#[derive(Clone, Debug, PartialEq)]
enum JTerm {
    Var(JVar),
    Call(JItem, Vec<JTerm>),
    BinOp(BinOp, Box<JTerm>, Box<JTerm>),
    None,
}

#[derive(Clone, Debug, PartialEq)]
enum JStmt {
    Let(RawSym, JTy, JVar, JTerm),
    Term(JTerm),
    Ret(JTerm),
}

#[derive(Clone, Debug, PartialEq)]
enum JTy {
    I32,
    I64,
    Void,
}

#[derive(Clone, Debug, PartialEq)]
struct JFn {
    name: RawSym,
    item: JItem,
    ret_ty: JTy,
    args: Vec<(RawSym, JVar, JTy)>,
    body: Vec<JStmt>,
}

// CODEGEN

#[derive(Clone, Debug)]
struct Gen<'a> {
    bindings: &'a Bindings,
    names: HashMap<u64, RawSym>,
}
impl<'a> Gen<'a> {
    fn new(bindings: &'a Bindings) -> Self {
        Gen {
            bindings,
            names: HashMap::new(),
        }
    }

    fn name_str(&self, v: JVar) -> &str {
        self.bindings.resolve_raw(self.names[&v.0])
    }
    fn item_str(&self, v: JItem) -> &str {
        self.bindings.resolve_raw(self.names[&v.0])
    }
}

impl JTerm {
    fn gen(&self, cxt: &Gen) -> String {
        match self {
            JTerm::Var(v) => {
                let mut s = cxt.name_str(*v).to_string();
                write!(s, "${}", v.0).unwrap();
                s
            }
            JTerm::Call(f, a) => {
                let mut buf = String::new();
                buf.push_str(cxt.item_str(*f));
                write!(buf, "${}", f.0).unwrap();
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
            JTerm::BinOp(op, a, b) => {
                let mut buf = String::new();
                write!(buf, "({}) ", a.gen(cxt)).unwrap();
                match op {
                    BinOp::Plus => buf.push('+'),
                    BinOp::Minus => buf.push('-'),
                }
                write!(buf, " ({})", b.gen(cxt)).unwrap();
                buf
            }
            JTerm::None => "<None>".into(),
        }
    }
}
impl JStmt {
    fn gen(&self, cxt: &mut Gen) -> String {
        match self {
            JStmt::Let(n, t, v, x) => {
                cxt.names.insert(v.0, *n);
                format!(
                    "{} {}${} = {};",
                    t.gen(cxt),
                    cxt.bindings.resolve_raw(*n),
                    v.0,
                    x.gen(cxt)
                )
            }
            JStmt::Term(x) => {
                let mut s = x.gen(cxt);
                s.push(';');
                s
            }
            JStmt::Ret(x) => format!("return {};", x.gen(cxt)),
        }
    }
}
impl JTy {
    fn gen(&self, _cxt: &Gen) -> String {
        match self {
            JTy::I32 => "int".into(),
            JTy::I64 => "long".into(),
            JTy::Void => "void".into(),
        }
    }
}
impl JFn {
    fn gen(&self, cxt: &mut Gen) -> String {
        let mut buf = String::new();
        write!(
            buf,
            "public static {} {}${}(",
            self.ret_ty.gen(cxt),
            cxt.bindings.resolve_raw(self.name),
            self.item.0
        )
        .unwrap();

        let names = cxt.names.clone();

        let mut first = true;
        for (n, v, t) in &self.args {
            if !first {
                buf.push_str(", ");
            }
            first = false;
            write!(
                buf,
                "{} {}${}",
                t.gen(cxt),
                cxt.bindings.resolve_raw(*n),
                v.0
            )
            .unwrap();
            cxt.names.insert(v.0, *n);
        }
        buf.push_str(") {");

        for i in &self.body {
            buf.push_str("\n\t");
            buf.push_str(&i.gen(cxt));
        }

        cxt.names = names;

        buf.push_str("\n}\n");
        buf
    }
}

// LOWERING

#[derive(Clone, Debug)]
struct Cxt<'a> {
    bindings: &'a Bindings,
    scopes: Vec<(usize, usize)>,
    vars: Vec<(Sym, JVar)>,
    items: Vec<(FnId, JItem)>,
    block: Vec<JStmt>,
    fns: Vec<JFn>,
    next: u64,
}
impl<'a> Cxt<'a> {
    fn new(bindings: &'a Bindings) -> Self {
        Cxt {
            bindings,
            scopes: Vec::new(),
            vars: Vec::new(),
            items: Vec::new(),
            block: Vec::new(),
            fns: Vec::new(),
            next: 0,
        }
    }

    fn var(&self, s: Sym) -> Option<JVar> {
        self.vars.iter().rfind(|(k, _v)| *k == s).map(|(_k, v)| *v)
    }
    fn item(&self, s: FnId) -> Option<JItem> {
        self.items.iter().rfind(|(k, _v)| *k == s).map(|(_k, v)| *v)
    }

    fn push(&mut self) {
        self.scopes.push((self.vars.len(), self.items.len()));
    }
    fn pop(&mut self) {
        let (v, i) = self.scopes.pop().unwrap();
        self.vars.truncate(v);
        self.items.truncate(i);
    }

    fn fresh_var(&mut self) -> JVar {
        self.next += 1;
        JVar(self.next)
    }
    fn fresh_item(&mut self) -> JItem {
        self.next += 1;
        JItem(self.next)
    }
}

impl Term {
    fn lower(&self, cxt: &mut Cxt) -> JTerm {
        match self {
            Term::Var(s) => JTerm::Var(cxt.var(*s).unwrap()),
            Term::Call(f, a) => JTerm::Call(
                cxt.item(*f).unwrap(),
                a.iter().map(|x| x.lower(cxt)).collect(),
            ),
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
                let var = cxt.fresh_var();
                cxt.vars.push((*n, var));
                let t = t.lower(cxt);
                let x = x.lower(cxt);
                cxt.block.push(JStmt::Let(n.raw(), t, var, x));
            }
        }
    }
}
impl Item {
    fn lower(&self, cxt: &mut Cxt) {
        match self {
            Item::Fn(f) => {
                let mut block = Vec::new();
                std::mem::swap(&mut block, &mut cxt.block);

                cxt.push();
                let mut args = Vec::new();
                for (name, ty) in &f.args {
                    let var = cxt.fresh_var();
                    args.push((name.raw(), var, ty.lower(cxt)));
                    cxt.vars.push((name.clone(), var));
                }
                let ret = f.body.lower(cxt);
                if !matches!(ret, JTerm::None) {
                    cxt.block.push(JStmt::Ret(ret));
                }
                cxt.pop();

                std::mem::swap(&mut block, &mut cxt.block);
                let ret_ty = f.ret_ty.lower(cxt);
                let item = cxt.item(f.id).unwrap();
                cxt.fns.push(JFn {
                    name: cxt.bindings.fn_name(f.id),
                    item,
                    ret_ty,
                    args,
                    body: block,
                });
            }
        }
    }
}
impl Type {
    fn lower(&self, _cxt: &Cxt) -> JTy {
        match self {
            Type::I32 => JTy::I32,
            Type::I64 => JTy::I64,
            Type::Unit => JTy::Void,
        }
    }
}
