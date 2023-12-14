use super::*;

use std::collections::HashMap;

pub fn declare_p1(code: &[Item], cxt: &mut Cxt) {
    for i in code {
        match i {
            Item::ExternClass(c, _, _) => {
                let class = cxt.fresh_class();
                cxt.types.push((*c, class));

                continue;
            }
            Item::Class(c, _, _, _) => {
                let class = cxt.fresh_class();
                cxt.types.push((*c, class));

                continue;
            }
            Item::Enum(c, v, _, _, _, _) => {
                let class = cxt.fresh_class();
                cxt.types.push((*c, class));
                if v.iter().any(|(_, v)| !v.is_empty()) {
                    let wrapper = cxt.fresh_class();
                    cxt.enum_wrappers.insert(class, wrapper);
                }

                continue;
            }
            _ => (),
        }
    }
}

pub fn declare_p2(code: Vec<Item>, cxt: &mut Cxt, out_class: &str) -> Result<IRMod, Error> {
    // Declare items
    let mut mappings = Vec::new();
    let mut java = Vec::new();

    let predefined = vec![(Predef::ArrayCopy, "System.arraycopy", JTys::empty())];
    for (d, s, t) in predefined {
        let fn_id = cxt.fresh_fn();
        cxt.fn_ret_tys.insert(fn_id, t);
        let raw = cxt.bindings.raw(s);
        mappings.push((fn_id.0, lpath(Spanned::new(raw, Span(0, 0))), false));
        cxt.predefs.push((d, fn_id));
    }

    for i in &code {
        let (name, ret, m, public, ext, inline, span) = match i {
            Item::Fn(f) => (
                f.id,
                &f.ret_ty,
                cxt.bindings.fn_name(f.id),
                f.public,
                false,
                if f.inline {
                    Some((f.args.clone(), f.body.cloned(cxt.bindings)))
                } else {
                    None
                },
                f.span,
            ),
            Item::ExternFn(f) => (
                f.id,
                &f.ret_ty,
                lpath(Spanned::hack(f.mapping)),
                true,
                true,
                None,
                f.span,
            ),
            Item::ExternClass(c, members, _span) => {
                let class = cxt.class(*c).unwrap();
                mappings.push((class.0, lpath(cxt.bindings.type_name(*c).stem()), false));
                for (s, t) in members {
                    let t = t.lower(cxt);
                    let mut vars = Vec::new();
                    for t in t {
                        let var = cxt.fresh_var(cxt.bindings.public(*s));
                        cxt.tys.insert(var, t);
                        mappings.push((var.0, cxt.bindings.sym_path(*s), !var.1));
                        vars.push(var);
                    }
                    cxt.vars.push((*s, JVars::Tuple(vars)));
                }

                continue;
            }
            Item::Class(c, members, methods, _span) => {
                let class = cxt.class(*c).unwrap();
                mappings.push((class.0, cxt.bindings.type_name(*c), true));
                for f in methods {
                    let item = cxt.fresh_fn();
                    cxt.fn_ids.push((f.id, item));

                    let ret = f.ret_ty.lower(cxt);
                    cxt.fn_ret_tys.insert(item, ret);
                    mappings.push((item.0, cxt.bindings.fn_name(f.id), !f.public));

                    if f.inline {
                        cxt.inline_fns
                            .insert(item, (f.args.clone(), f.body.cloned(cxt.bindings)));
                    }
                }
                for (s, t, _) in members {
                    let t = t.lower(cxt);
                    let mut vars = Vec::new();
                    for t in t {
                        let var = cxt.fresh_var(cxt.bindings.public(*s));
                        cxt.tys.insert(var, t);
                        mappings.push((var.0, cxt.bindings.sym_path(*s), !var.1));
                        vars.push(var);
                    }
                    cxt.vars.push((*s, JVars::Tuple(vars)));
                }

                continue;
            }
            Item::Enum(c, _, ext, members, methods, _span) => {
                let class = cxt.class(*c).unwrap();
                if *ext {
                    mappings.push((class.0, lpath(cxt.bindings.type_name(*c).stem()), false));
                    for (s, t) in members {
                        let t = t.lower(cxt);
                        let mut vars = Vec::new();
                        for t in t {
                            let var = cxt.fresh_var(cxt.bindings.public(*s));
                            cxt.tys.insert(var, t);
                            mappings.push((var.0, cxt.bindings.sym_path(*s), !var.1));
                            vars.push(var);
                        }
                        cxt.vars.push((*s, JVars::Tuple(vars)));
                    }
                } else {
                    mappings.push((class.0, cxt.bindings.type_name(*c), true));
                    if let Some(&wrapper) = cxt.enum_wrappers.get(&class) {
                        mappings.push((wrapper.0, cxt.bindings.type_name(*c), true));
                    }
                    for f in methods {
                        let item = cxt.fresh_fn();
                        cxt.fn_ids.push((f.id, item));

                        let ret = f.ret_ty.lower(cxt);
                        cxt.fn_ret_tys.insert(item, ret);
                        mappings.push((item.0, cxt.bindings.fn_name(f.id), !f.public));

                        if f.inline {
                            cxt.inline_fns
                                .insert(item, (f.args.clone(), f.body.cloned(cxt.bindings)));
                        }
                    }
                }

                continue;
            }
            Item::InlineJava(s, _) => {
                java.push(*s);
                continue;
            }
            Item::Let(s, false, t, _, _) => {
                let t = t.lower(cxt);
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
            Item::Let(s, true, _, body, _) => {
                cxt.inline_vars
                    .insert(*s, body.as_ref().unwrap().cloned(cxt.bindings));
                continue;
            }
        };
        let item = cxt.fresh_fn();
        cxt.fn_ids.push((name, item));
        if let Some(i) = inline {
            cxt.inline_fns.insert(item, i);
        }

        let mut ret = ret.lower(cxt);
        // Try to convert certain types - for example, convert Java arrays to Bytec dynamic arrays
        if ext {
            if ret.len() > 1 {
                let mut fixed = false;
                if ret.len() == 2 {
                    let mut v = ret.to_vec();
                    if let JTy::Array(_) = &v[0] {
                        ret = JTys::One(v.swap_remove(0));
                        fixed = true;
                    } else {
                        ret = JTys::Tuple(v);
                    }
                }
                if !fixed {
                    return Err(Spanned::new(
                        Doc::start("Extern function cannot return a tuple or static array"),
                        span,
                    ));
                }
            }
        }
        cxt.fn_ret_tys.insert(item, ret);
        mappings.push((item.0, m, !public));
    }

    Ok(IRMod {
        name: cxt.bindings.raw(out_class),
        code,
        mappings,
        java,
        out_class: out_class.to_string(),
    })
}

pub struct Cxt<'a> {
    pub(super) bindings: &'a mut Bindings,
    scopes: Vec<(usize, usize, usize)>,
    vars: Vec<(Sym, JVars)>,
    tys: HashMap<JVar, JTy>,
    fn_ids: Vec<(FnId, JFnId)>,
    pub(super) fn_ret_tys: HashMap<JFnId, JTys>,
    inline_fns: HashMap<JFnId, (Vec<(Sym, Type)>, Term)>,
    inline_vars: HashMap<Sym, Term>,
    types: Vec<(TypeId, JClass)>,
    block: Vec<JStmt>,
    blocks: Vec<(Option<JBlock>, usize)>,
    current_fn: JFnId,
    pub(super) items: Vec<Spanned<JItem>>,
    predefs: Vec<(Predef, JFnId)>,
    enum_wrappers: HashMap<JClass, JClass>,
    pub(super) next: u64,
    pub(super) package: String,
}

impl<'a> Cxt<'a> {
    pub fn new(bindings: &'a mut Bindings, package: impl Into<String>) -> Self {
        Cxt {
            bindings,
            scopes: Vec::new(),
            vars: Vec::new(),
            tys: HashMap::new(),
            fn_ids: Vec::new(),
            fn_ret_tys: HashMap::new(),
            inline_fns: HashMap::new(),
            inline_vars: HashMap::new(),
            types: Vec::new(),
            block: Vec::new(),
            blocks: Vec::new(),
            current_fn: JFnId(0),
            items: Vec::new(),
            predefs: Vec::new(),
            enum_wrappers: HashMap::new(),
            next: 0,
            package: package.into(),
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

    pub(super) fn fresh_var(&mut self, public: bool) -> JVar {
        self.next += 1;
        JVar(self.next, public)
    }
    pub(super) fn fresh_fn(&mut self) -> JFnId {
        self.next += 1;
        JFnId(self.next)
    }
    pub(super) fn fresh_class(&mut self) -> JClass {
        self.next += 1;
        JClass(self.next)
    }
    pub(super) fn fresh_block(&mut self) -> JBlock {
        self.next += 1;
        JBlock(self.next)
    }
}

impl JTerm {
    /// Whether this term is simple enough to be 1 bytecode instruction.
    /// Simple instructions can be duplicated freely.
    fn simple(&self) -> bool {
        match self {
            JTerm::Var(_, _)
            | JTerm::Variant(_, _)
            | JTerm::Lit(_)
            | JTerm::Null(_)
            | JTerm::SIndex(_, _)
            | JTerm::Cast(_, _)
            | JTerm::This(_) => true,
            JTerm::Call(_, _, _, _)
            | JTerm::BinOp(_, _, _)
            | JTerm::Index(_, _, _)
            | JTerm::Prop(_, _, _)
            | JTerm::InlineJava(_, _)
            | JTerm::ArrayNew(_, _)
            | JTerm::ClassNew(_, _)
            | JTerm::Not(_)
            | JTerm::Array(_, _) => false,
        }
    }

    pub(super) fn ty(&self) -> JTy {
        match self {
            JTerm::Var(_, t) => t.clone(),
            JTerm::Null(t) => t.clone(),
            JTerm::This(s) => JTy::Class(*s),
            JTerm::Lit(l) => match l {
                JLit::Int(_) => JTy::I32,
                JLit::Long(_) => JTy::I64,
                JLit::Float(_) => JTy::F32,
                JLit::Double(_) => JTy::F64,
                JLit::Str(_) => JTy::String,
                JLit::Bool(_) => JTy::Bool,
            },
            JTerm::Cast(_, t) => t.clone(),
            JTerm::SIndex(v, _) => v[0].ty(),
            JTerm::Not(_) => JTy::Bool,
            JTerm::Call(_, _, _, t) => t.clone(),
            JTerm::Prop(_, _, t) => t.clone(),
            JTerm::InlineJava(_, t) => t.clone(),
            JTerm::Array(_, t) => t.clone(),
            JTerm::ArrayNew(_, t) => t.clone(),
            JTerm::ClassNew(c, _) => JTy::Class(*c),
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

impl LValue {
    fn lower(&self, cxt: &mut Cxt, nvals: usize) -> MaybeList<JLVal> {
        match self {
            LValue::Var(v) => {
                let v = cxt.var(*v).unwrap();
                v.map(JLVal::Var)
            }
            LValue::Idx(v, idx, false) => {
                let v = v.lower(cxt, nvals);
                let mut idx = idx.lower(cxt).one();
                if !idx.simple() {
                    // Don't recompute idx every time, store it in a local
                    let raw = cxt.bindings.raw("$_idx");
                    let var = cxt.fresh_var(false);
                    cxt.block.push(JStmt::Let(raw, JTy::I32, var, Some(idx)));
                    idx = JTerm::Var(var, JTy::I32);
                }
                v.map(|x| JLVal::Idx(Box::new(x), idx.clone()))
            }
            LValue::Idx(v, idx, true) => {
                let v = v.lower(cxt, nvals);
                let mut idx = idx.lower(cxt).one();
                if !idx.simple() {
                    // Don't recompute idx every time, store it in a local
                    let raw = cxt.bindings.raw("$_idx");
                    let var = cxt.fresh_var(false);
                    cxt.block.push(JStmt::Let(raw, JTy::I32, var, Some(idx)));
                    idx = JTerm::Var(var, JTy::I32);
                }
                let mut vs = vec![Vec::new(); nvals];
                for is in v.to_vec().chunks(nvals) {
                    for (i, x) in is.iter().enumerate() {
                        vs[i].push(x.clone());
                    }
                }
                let mut r = Vec::new();
                for v in vs {
                    r.push(JLVal::SIdx(v, idx.clone()));
                }
                MaybeList::Tuple(r)
            }
            LValue::Member(v, m) => {
                let mut x = v.lower(cxt).one();
                if !x.simple() {
                    let raw = cxt.bindings.raw("$_class");
                    let var = cxt.fresh_var(false);
                    let ty = x.ty();
                    cxt.block.push(JStmt::Let(raw, ty.clone(), var, Some(x)));
                    x = JTerm::Var(var, ty.clone());
                }

                let prop = cxt.var(*m).unwrap();
                prop.map(|m| JLVal::Prop(x.clone(), Prop::Var(m)))
            }
        }
    }
}

impl Term {
    fn lower(&self, cxt: &mut Cxt) -> JTerms {
        JTerms::One(match self {
            Term::Error => panic!("type error reached backend!"),
            Term::Var(s) => {
                if let Some(t) = cxt.inline_vars.get(s) {
                    return t.cloned(cxt.bindings).lower(cxt);
                } else {
                    let var = cxt.var(*s).unwrap();
                    return var.map(|var| JTerm::Var(var, cxt.tys.get(&var).unwrap().clone()));
                }
            }
            Term::Null(t) => JTerm::Null(t.lower(cxt).one()),
            Term::Selph(t) => {
                let class = cxt.class(*t).unwrap();
                if let Some(wrapper) = cxt.enum_wrappers.get(&class) {
                    JTerm::This(*wrapper)
                } else {
                    JTerm::This(class)
                }
            }
            Term::Not(x) => JTerm::Not(Box::new(x.lower(cxt).one())),
            Term::Lit(l, t) => match l {
                Literal::Int(i) => match t {
                    Type::I32 => JTerm::Lit(JLit::Int(*i as i32)),
                    Type::I64 => JTerm::Lit(JLit::Long(*i)),
                    _ => unreachable!(),
                },
                Literal::Float(i) => match t {
                    Type::F32 => JTerm::Lit(JLit::Float(*i as f32)),
                    Type::F64 => JTerm::Lit(JLit::Double(*i)),
                    _ => unreachable!(),
                },
                Literal::Str(s) => JTerm::Lit(JLit::Str(*s)),
                Literal::Bool(b) => JTerm::Lit(JLit::Bool(*b)),
            },
            Term::As(x, _from, to) => {
                return x
                    .lower(cxt)
                    .into_iter()
                    .zip(to.lower(cxt).into_iter())
                    .map(|(x, ty)| JTerm::Cast(Box::new(x), ty))
                    .collect()
            }
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
            Term::Variant(tid, s, v) => {
                let class = cxt.class(*tid).unwrap();
                let variant = JTerm::Variant(class, *s);
                if let Some(wrapper) = cxt.enum_wrappers.get(&class) {
                    let term = JTerm::ClassNew(*wrapper, Vec::new());
                    let ty = JTy::Class(*wrapper);
                    let var = cxt.fresh_var(false);
                    let raw = cxt.bindings.raw("$_variant");
                    cxt.tys.insert(var, ty.clone());
                    cxt.block.push(JStmt::Let(raw, ty.clone(), var, Some(term)));
                    let term = JTerm::Var(var, ty);
                    cxt.block.push(JStmt::Set(
                        JLVal::Prop(term.clone(), Prop::Raw(cxt.bindings.raw("$type"))),
                        None,
                        variant,
                    ));
                    let v: Vec<_> = v.iter().flat_map(|x| x.lower(cxt)).collect();
                    for (n, val) in v.into_iter().enumerate() {
                        let prop = format!("_enum${}${}", cxt.bindings.resolve_raw(*s), n);
                        let prop = cxt.bindings.raw(prop);
                        cxt.block.push(JStmt::Set(
                            JLVal::Prop(term.clone(), Prop::Raw(prop)),
                            None,
                            val,
                        ));
                    }
                    term
                } else {
                    assert_eq!(v.len(), 0);
                    variant
                }
            }
            Term::Tuple(v) => return JTerms::Tuple(v.iter().flat_map(|x| x.lower(cxt)).collect()),
            Term::TupleIdx(x, i) => {
                let x = x.lower(cxt);
                x.to_vec().swap_remove(*i)
            }
            Term::Member(x, _, m) => {
                let mut x = x.lower(cxt).one();
                // TODO get actual type somehow
                let m = cxt.var(**m).unwrap();
                if m.len() > 1 {
                    if !x.simple() {
                        let raw = cxt.bindings.raw("$_class");
                        let var = cxt.fresh_var(false);
                        let ty = x.ty();
                        cxt.block.push(JStmt::Let(raw, ty.clone(), var, Some(x)));
                        x = JTerm::Var(var, ty.clone());
                    }
                    return m.map(|v| {
                        JTerm::Prop(
                            Box::new(x.clone()),
                            Prop::Var(v),
                            cxt.tys.get(&v).unwrap().clone(),
                        )
                    });
                } else {
                    let m = m.one();
                    JTerm::Prop(Box::new(x), Prop::Var(m), cxt.tys.get(&m).unwrap().clone())
                }
            }
            Term::Constructor(t, args) => {
                let t = cxt.class(*t).unwrap();
                let mut a = Vec::new();
                for i in args {
                    a.extend(i.lower(cxt));
                }
                JTerm::ClassNew(t, a)
            }
            Term::Set(l, op, x) => {
                let x = x.lower(cxt);
                let v = l.lower(cxt, x.len());
                for (v, x) in v.into_iter().zip(x) {
                    cxt.block.push(JStmt::Set(v, *op, x));
                }
                return JTerms::empty();
            }
            Term::Array(v, t, true) if v.is_empty() => {
                let t = t.lower(cxt);
                return JTerms::Tuple(
                    t.into_iter()
                        .map(|ty| JTerm::Array(Vec::new(), JTy::Array(Box::new(ty))))
                        .chain(std::iter::once(JTerm::Lit(JLit::Int(0))))
                        .collect(),
                );
            }
            Term::Array(v, _, true) => {
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
            Term::Array(v, _t, false) => {
                return JTerms::Tuple(v.iter().flat_map(|x| x.lower(cxt)).collect())
            }
            Term::ArrayNew(len, t) => {
                let mut len = len.lower(cxt).one();
                if !len.simple() {
                    // Don't recompute len every time, store it in a local
                    let raw = cxt.bindings.raw("$_len");
                    let var = cxt.fresh_var(false);
                    cxt.block.push(JStmt::Let(raw, JTy::I32, var, Some(len)));
                    len = JTerm::Var(var, JTy::I32);
                }
                return JTerms::Tuple(
                    t.lower(cxt)
                        .into_iter()
                        .map(|ty| JTerm::ArrayNew(Box::new(len.clone()), JTy::Array(Box::new(ty))))
                        .chain(std::iter::once(len.clone()))
                        .collect(),
                );
            }
            Term::ArrayIdx(arr, idx, false, _, _) => {
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
            Term::ArrayIdx(arr, idx, true, ty, inline) => {
                let arrs = arr.lower(cxt);
                let idx = idx.lower(cxt).one();
                let ty = ty.lower(cxt);
                if *inline {
                    let mut vars = Vec::new();
                    for i in ty.clone() {
                        let var = cxt.fresh_var(false);
                        cxt.tys.insert(var, i.clone());
                        cxt.block
                            .push(JStmt::Let(cxt.bindings.raw("$_inline_sidx"), i, var, None));
                        vars.push(var);
                    }

                    let mut cases = Vec::new();
                    for (i, x) in arrs.to_vec().chunks(ty.len()).enumerate() {
                        let mut block = Vec::new();
                        for (v, x) in vars.iter().zip(x) {
                            block.push(JStmt::Set(JLVal::Var(*v), None, x.clone()));
                        }
                        cases.push((cxt.bindings.raw(i.to_string()), block));
                    }
                    let k = cxt.fresh_block();
                    cxt.block.push(JStmt::Switch(k, idx, cases, Vec::new()));

                    return JTerms::Tuple(
                        vars.into_iter()
                            .zip(ty)
                            .map(|(v, t)| JTerm::Var(v, t))
                            .collect(),
                    );
                } else {
                    let mut vs = vec![Vec::new(); ty.len()];
                    for is in arrs.to_vec().chunks(ty.len()) {
                        for (i, x) in is.iter().enumerate() {
                            vs[i].push(x.clone());
                        }
                    }
                    return JTerms::Tuple(
                        vs.into_iter()
                            .map(|arr| JTerm::SIndex(arr, Box::new(idx.clone())))
                            .collect(),
                    );
                }
            }
            Term::ArrayMethod(arr, m) => {
                let arrs = arr.lower(cxt);
                let len = arrs.clone().to_vec().pop().unwrap();
                match m {
                    ArrayMethod::Len => len,
                    ArrayMethod::Clear => {
                        let slen = len.to_lval().expect("clear() requires an lvalue");
                        // Just set len to 0
                        cxt.block
                            .push(JStmt::Set(slen, None, JTerm::Lit(JLit::Int(0))));
                        return JTerms::empty();
                    }
                    ArrayMethod::Pop => {
                        let slen = len.clone().to_lval().expect("pop() requires an lvalue");
                        // `a -= 1` is as fast as `a--`, but `a = a - 1` is slower
                        cxt.block.push(JStmt::Set(
                            slen,
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
                    ArrayMethod::Push(x) => {
                        let slen = len.clone().to_lval().expect("push() requires an lvalue");
                        cxt.block.push(JStmt::Set(
                            slen,
                            Some(BinOp::Add),
                            JTerm::Lit(JLit::Int(1)),
                        ));
                        let x = x.lower(cxt);
                        assert_eq!(x.len(), arrs.len() - 1);
                        // Check if the array needs expanding
                        if arrs.len() != 1 {
                            let cap = JTerm::Prop(
                                Box::new(arrs.clone().to_vec().swap_remove(0)),
                                Prop::Raw(cxt.bindings.raw("length")),
                                JTy::I32,
                            );
                            let too_small =
                                JTerm::BinOp(BinOp::Gt, Box::new(len.clone()), Box::new(cap));
                            let mut block = Vec::new();
                            let mut block2 = Vec::new();
                            for (arr, x) in arrs.clone().into_iter().zip(x) {
                                let sarr =
                                    arr.clone().to_lval().expect("push() requires an lvalue");
                                // let old = arr;
                                // arr = new T[old.length * 2];
                                // System.arraycopy(old, 0, arr, 0, old.length);

                                let old = cxt.fresh_var(false);
                                let raw = cxt.bindings.raw("$_old_array");
                                // cxt.vars.push((cxt.bindings.create(lpath(Spanned::hack(raw)), false), JVars::One(old)));
                                block.push(JStmt::Let(raw, arr.ty(), old, Some(arr.clone())));

                                let cap = JTerm::Prop(
                                    Box::new(JTerm::Var(old, arr.ty())),
                                    Prop::Raw(cxt.bindings.raw("length")),
                                    JTy::I32,
                                );
                                let new_cap = JTerm::BinOp(
                                    BinOp::Mul,
                                    Box::new(cap.clone()),
                                    Box::new(JTerm::Lit(JLit::Int(2))),
                                );
                                block.push(JStmt::Set(
                                    sarr.clone(),
                                    None,
                                    JTerm::ArrayNew(Box::new(new_cap), arr.ty()),
                                ));

                                let copy_fn = cxt.predef(Predef::ArrayCopy);
                                let call = JStmt::MultiCall(
                                    None,
                                    copy_fn,
                                    vec![
                                        JTerm::Var(old, arr.ty()),
                                        JTerm::Lit(JLit::Int(0)),
                                        arr.clone(),
                                        JTerm::Lit(JLit::Int(0)),
                                        cap,
                                    ],
                                    Vec::new(),
                                );
                                block.push(call);

                                // arr[len-1] = x;
                                let idx = JTerm::BinOp(
                                    BinOp::Sub,
                                    Box::new(len.clone()),
                                    Box::new(JTerm::Lit(JLit::Int(1))),
                                );
                                block2.push(JStmt::Set(JLVal::Idx(Box::new(sarr), idx), None, x));
                            }
                            // Expand, then set
                            cxt.block.push(JStmt::If(too_small, block, Vec::new()));
                            cxt.block.append(&mut block2);
                        }
                        return JTerms::empty();
                    }
                }
            }
            Term::Call(o, f, a) => {
                let fn_id = cxt.fun(f.unwrap()).unwrap();
                let o = o.as_ref().map(|x| Box::new(x.lower(cxt).one()));
                let args = a.iter().flat_map(|x| x.lower(cxt)).collect();
                let rtys = cxt.fn_ret_tys.get(&fn_id).unwrap().clone();
                if let Some((atys, body)) = cxt.inline_fns.get(&fn_id) {
                    let body = body.cloned(cxt.bindings);
                    let mut syms = HashMap::new();
                    for ((s, t), x) in atys
                        .clone()
                        .iter()
                        .flat_map(|(s, t)| t.lower(cxt).into_iter().map(|t| (*s, t)))
                        .collect::<Vec<_>>()
                        .into_iter()
                        .zip(args)
                    {
                        let public = cxt.bindings.public(s);
                        let v = cxt.fresh_var(public);
                        syms.entry(s).or_insert_with(Vec::new).push(v);
                        cxt.tys.insert(v, t.clone());
                        cxt.block
                            .push(JStmt::Let(*cxt.bindings.sym_path(s).stem(), t, v, Some(x)));
                    }
                    for (s, v) in syms {
                        cxt.vars.push((s, JVars::Tuple(v)));
                    }

                    return body.lower(cxt);
                }
                match rtys {
                    MaybeList::One(t @ JTy::Array(_)) => {
                        let arr = cxt.fresh_var(false);
                        let raw = cxt.bindings.raw("$_java_array");
                        cxt.block.push(JStmt::Let(
                            raw,
                            t.clone(),
                            arr,
                            Some(JTerm::Call(o, fn_id, args, t.clone())),
                        ));

                        let len = JTerm::Prop(
                            Box::new(JTerm::Var(arr, t.clone())),
                            Prop::Raw(cxt.bindings.raw("length")),
                            JTy::I32,
                        );

                        return JTerms::Tuple(vec![JTerm::Var(arr, t), len]);
                    }
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
                                cxt.bindings
                                    .resolve_raw(*cxt.bindings.fn_name(f.unwrap()).stem()),
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
                    cxt.block.push(JStmt::Set(JLVal::Var(*var), None, a));
                }
                let a = cxt.pop_block();

                let b = if let Some(b) = b {
                    cxt.push_block();
                    let b = b.lower(cxt);
                    for ((var, _, _), b) in vars.iter().zip(b) {
                        cxt.block.push(JStmt::Set(JLVal::Var(*var), None, b));
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
            Term::Match(tid, x, branches) => {
                let mut x = x.lower(cxt).one();
                let scrut = if let Some(_wrapper) = cxt.enum_wrappers.get(&cxt.class(*tid).unwrap())
                {
                    if !x.simple() {
                        // Don't recompute x every time, store it in a local
                        let raw = cxt.bindings.raw("$_scrutinee");
                        let var = cxt.fresh_var(false);
                        let ty = x.ty();
                        cxt.block.push(JStmt::Let(raw, ty.clone(), var, Some(x)));
                        x = JTerm::Var(var, ty.clone());
                    }
                    JTerm::Prop(
                        Box::new(x.clone()),
                        Prop::Raw(cxt.bindings.raw("$type")),
                        JTy::Class(cxt.class(*tid).unwrap()),
                    )
                } else {
                    // will only be used once, as the scrutinee
                    x.clone()
                };

                let mut v = Vec::new();
                let mut default = None;
                let mut vars: Option<Vec<_>> = None;
                for (variant, captures, body) in branches {
                    cxt.push_block();

                    if let Some(variant) = variant {
                        let mut n = 0;
                        for (s, t) in captures {
                            let mut vars = Vec::new();

                            let t = t.lower(cxt);
                            for t in t {
                                let prop =
                                    format!("_enum${}${}", cxt.bindings.resolve_raw(*variant), n);
                                let prop = cxt.bindings.raw(prop);
                                let x =
                                    JTerm::Prop(Box::new(x.clone()), Prop::Raw(prop), t.clone());
                                n += 1;

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
                        }
                    }

                    let body = body.lower(cxt);
                    if vars.is_none() {
                        let ty = body.ty();
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
                    for ((var, _, _), t) in vars.as_ref().unwrap().iter().zip(body) {
                        cxt.block.push(JStmt::Set(JLVal::Var(*var), None, t));
                    }
                    let block = cxt.pop_block();

                    match variant {
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
                    .push(JStmt::Switch(k, scrut, v, default.unwrap_or_default()));

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
            Statement::Let(n, false, t, x) => {
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
            Statement::Let(n, true, _t, x) => {
                cxt.inline_vars.insert(*n, x.cloned(cxt.bindings));
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
                    ForIter::Range(a, b, unroll) => {
                        let a = a.lower(cxt).one();
                        let b = b.lower(cxt).one();

                        let v = cxt.fresh_var(cxt.bindings.public(*s));
                        cxt.tys.insert(v, JTy::I32);
                        cxt.vars.push((*s, JVars::One(v)));

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
                            *unroll,
                        ));
                    }
                    ForIter::SArray(arr, t) => {
                        let arr = arr.lower(cxt);
                        let t = t.lower(cxt);

                        let mut vars = Vec::new();
                        for t in t {
                            let var = cxt.fresh_var(cxt.bindings.public(*s));
                            cxt.tys.insert(var, t.clone());
                            cxt.block.push(JStmt::Let(
                                *cxt.bindings.sym_path(*s).stem(),
                                t,
                                var,
                                None,
                            ));
                            vars.push(var);
                        }
                        cxt.vars.push((*s, JVars::Tuple(vars.clone())));

                        // Generate an unrolled loop in the current block
                        let k = cxt.fresh_block();
                        cxt.push_loop(k);
                        let mut vals = arr.to_vec();
                        while !vals.is_empty() {
                            for &v in &vars {
                                // TODO is this too slow?
                                let next = vals.remove(0);
                                cxt.block.push(JStmt::Set(JLVal::Var(v), None, next));
                            }

                            for i in block {
                                i.lower(cxt);
                            }
                        }
                        let mut block = cxt.pop_block();
                        cxt.block.append(&mut block);
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
                            false,
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

impl Fn {
    fn lower(&self, cxt: &mut Cxt) -> JFn {
        if self.inline {
            panic!("Don't lower inline functions");
        }
        let mut block = Vec::new();
        let fn_id = cxt.fun(self.id).unwrap();
        std::mem::swap(&mut block, &mut cxt.block);

        cxt.push();
        cxt.current_fn = fn_id;
        let mut args = Vec::new();
        for (name, ty) in &self.args {
            let mut vars = Vec::new();
            for ty in ty.lower(cxt) {
                let var = cxt.fresh_var(cxt.bindings.public(*name));
                args.push((*cxt.bindings.sym_path(*name).stem(), var, ty.clone()));
                cxt.tys.insert(var, ty);
                vars.push(var);
            }
            cxt.vars.push((name.clone(), JVars::Tuple(vars)));
        }
        let ret = self.body.lower(cxt);
        match (ret, &self.ret_ty) {
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
        let ret_ty = self.ret_ty.lower(cxt);
        JFn {
            name: *cxt.bindings.fn_name(self.id).stem(),
            fn_id,
            ret_tys: ret_ty.into(),
            args,
            body: block,
            public: self.public,
            throws: self.throws.clone(),
        }
    }
}

impl Item {
    pub(super) fn lower(&self, cxt: &mut Cxt) {
        match self {
            // Module-level inline java is handled by codegen()
            Item::InlineJava(_, _) => (),
            Item::Fn(f) => {
                if !f.inline {
                    let span = f.span;
                    let f = f.lower(cxt);
                    cxt.items.push(Spanned::new(JItem::Fn(f), span));
                }
            }
            Item::Enum(tid, variants, ext, _members, methods, span) => {
                if !ext {
                    let class = cxt.class(*tid).unwrap();
                    let variants = variants
                        .iter()
                        .map(|(s, t)| (*s, t.iter().flat_map(|x| x.lower(cxt)).collect()))
                        .collect();

                    let methods = methods.iter().map(|x| x.lower(cxt)).collect();

                    cxt.items.push(Spanned::new(
                        JItem::Enum(
                            class,
                            variants,
                            cxt.enum_wrappers.get(&class).copied(),
                            methods,
                        ),
                        *span,
                    ));
                }
            }
            Item::Class(tid, members, methods, span) => {
                let class = cxt.class(*tid).unwrap();
                let members = members
                    .iter()
                    .map(|(r, t, x)| {
                        cxt.push_block();
                        let t = t.lower(cxt);
                        let x = x.as_ref().map(|x| x.lower(cxt));
                        let block = cxt.pop_block();
                        // TODO multivalue members
                        let r = cxt.var(*r).unwrap();
                        if let Some(x) = x {
                            (
                                x.into_iter()
                                    .zip(t)
                                    .zip(r)
                                    .map(|((x, t), r)| (r, t, Some(x)))
                                    .collect::<Vec<_>>(),
                                block,
                            )
                        } else {
                            (
                                t.into_iter()
                                    .zip(r)
                                    .map(|(t, r)| (r, t, None))
                                    .collect::<Vec<_>>(),
                                block,
                            )
                        }
                    })
                    .collect();
                let methods = methods
                    .iter()
                    .filter(|x| !x.inline)
                    .map(|x| x.lower(cxt))
                    .collect();

                cxt.items
                    .push(Spanned::new(JItem::Class(class, members, methods), *span));
            }
            Item::ExternFn(_) => (),
            Item::ExternClass(_, _, _) => (),
            Item::Let(name, _, ty, None, span) => {
                let var = cxt.var(*name).unwrap();
                let ty = ty.lower(cxt);
                assert_eq!(var.len(), ty.len());
                cxt.items.push(Spanned::new(
                    JItem::Let(
                        var.into_iter().zip(ty).map(|(v, t)| (v, t, None)).collect(),
                        Vec::new(),
                    ),
                    *span,
                ));
            }
            Item::Let(name, c, ty, Some(x), span) => {
                if *c {
                    return;
                }
                cxt.push_block();
                let var = cxt.var(*name).unwrap();
                let ty = ty.lower(cxt);
                let x = x.lower(cxt);
                assert_eq!(var.len(), ty.len());
                assert_eq!(ty.len(), x.len());
                let block = cxt.pop_block();
                cxt.items.push(Spanned::new(
                    JItem::Let(
                        var.into_iter()
                            .zip(ty)
                            .zip(x)
                            .map(|((v, t), x)| (v, t, Some(x)))
                            .collect(),
                        block,
                    ),
                    *span,
                ));
            }
        }
    }
}

impl Type {
    pub(super) fn lower(&self, cxt: &mut Cxt) -> JTys {
        JTys::One(match self {
            Type::I32 => JTy::I32,
            Type::I64 => JTy::I64,
            Type::F32 => JTy::F32,
            Type::F64 => JTy::F64,
            Type::Bool => JTy::Bool,
            Type::Str => JTy::String,
            Type::Unit => return JTys::empty(),
            Type::Class(c) => {
                let class = cxt.class(*c).unwrap();
                if let Some(wrapper) = cxt.enum_wrappers.get(&class) {
                    JTy::Class(*wrapper)
                } else {
                    JTy::Class(class)
                }
            }
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
            Type::SArray(t, i) => {
                if let Some(optimize::CVal::Int(i)) = i
                    .lower(cxt)
                    .one()
                    .prop(&mut optimize::Env::new(cxt.bindings, 0))
                {
                    return JTys::Tuple(
                        std::iter::repeat(t.lower(cxt))
                            .take(i as _)
                            .flatten()
                            .collect(),
                    );
                } else {
                    // Propagating errors out of this function would require so much refactoring that it's not worth it in this case
                    // We have the expression, so the span of the item it's in isn't as important
                    Doc::start("error")
                        .style(crate::pretty::Style::BoldRed)
                        .add(": Array length does not resolve to statically known integer literal: '")
                        .chain(i.pretty(cxt.bindings))
                        .add("'")
                        .style(crate::pretty::Style::Bold)
                        .emit();
                    std::process::exit(1)
                }
            }
            Type::Error => panic!("reached backend with type errors"),
        })
    }
}
