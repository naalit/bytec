use std::collections::HashMap;
use std::fmt::Write;

use crate::term::*;

// Entry point

pub fn codegen(code: &[Item], out_class: &str) -> String {
    let mut cxt = Cxt::default();
    // Declare items
    for i in code {
        let name = match i {
            Item::Fn(f) => f.name.inner.clone(),
        };
        let item = cxt.fresh_item();
        cxt.items.push((name, item));
    }
    for i in code {
        i.lower(&mut cxt);
    }

    let mut gen = Gen::default();
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
    Let(Name, JTy, JVar, JTerm),
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
    name: Name,
    item: JItem,
    ret_ty: JTy,
    args: Vec<(Name, JVar, JTy)>,
    body: Vec<JStmt>,
}

// CODEGEN

#[derive(Clone, Debug, Default)]
struct Gen {
    names: HashMap<u64, String>,
}

impl JTerm {
    fn gen(&self, cxt: &Gen) -> String {
        match self {
            JTerm::Var(v) => {
                let mut s = cxt.names.get(&v.0).unwrap().clone();
                write!(s, "${}", v.0).unwrap();
                s
            }
            JTerm::Call(f, a) => {
                let mut buf = String::new();
                buf.push_str(cxt.names.get(&f.0).unwrap());
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
                cxt.names.insert(v.0, n.clone());
                format!("{} {}${} = {};", t.gen(cxt), n, v.0, x.gen(cxt))
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
            self.name,
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
            write!(buf, "{} {}${}", t.gen(cxt), n, v.0).unwrap();
            cxt.names.insert(v.0, n.clone());
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

#[derive(Clone, Debug, Default)]
struct Cxt {
    scopes: Vec<(usize, usize)>,
    vars: Vec<(String, JVar)>,
    items: Vec<(String, JItem)>,
    block: Vec<JStmt>,
    fns: Vec<JFn>,
    next: u64,
}
impl Cxt {
    fn var(&self, s: &str) -> Option<JVar> {
        self.vars.iter().rfind(|(k, _v)| k == s).map(|(_k, v)| *v)
    }
    fn item(&self, s: &str) -> Option<JItem> {
        self.items.iter().rfind(|(k, _v)| k == s).map(|(_k, v)| *v)
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
    fn lower_item(&self, cxt: &Cxt) -> Option<JItem> {
        match self {
            Term::Var(s) => cxt.item(s),
            _ => None,
        }
    }

    fn lower(&self, cxt: &mut Cxt) -> JTerm {
        match self {
            Term::Var(s) => JTerm::Var(
                cxt.var(s)
                    .unwrap_or_else(|| panic!("Variable not found: '{}'", s)),
            ),
            Term::Call(f, a) => {
                let item = f
                    .lower_item(cxt)
                    .unwrap_or_else(|| panic!("Not a function: '{}'", f.pretty().ansi_string()));
                let args = a.iter().map(|x| x.lower(cxt)).collect();
                JTerm::Call(item, args)
            }
            Term::BinOp(op, a, b) => {
                JTerm::BinOp(*op, Box::new(a.lower(cxt)), Box::new(b.lower(cxt)))
            }
            Term::Block(v, x) => {
                cxt.push();
                for i in v {
                    i.lower(cxt);
                }
                let r = x.as_ref().map(|x| x.lower(cxt)).unwrap_or(JTerm::None);
                cxt.pop();
                r
            }
        }
    }
}
impl Statement {
    fn lower(&self, cxt: &mut Cxt) {
        match self {
            Statement::Item(i) => i.lower(cxt),
            Statement::Term(x) => {
                let term = x.lower(cxt);
                cxt.block.push(JStmt::Term(term));
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
                    args.push((name.clone(), var, ty.lower(cxt)));
                    cxt.vars.push((name.clone(), var));
                }
                let ret = f.body.lower(cxt);
                if !matches!(ret, JTerm::None) {
                    cxt.block.push(JStmt::Ret(ret));
                }
                cxt.pop();

                std::mem::swap(&mut block, &mut cxt.block);
                let ret_ty = f.ret_type.lower(cxt);
                let item = cxt.item(&f.name).unwrap();
                cxt.fns.push(JFn {
                    name: f.name.inner.clone(),
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
