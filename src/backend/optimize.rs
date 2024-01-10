use super::*;
use std::collections::HashSet;

// OPTIMIZATION
trait Visitor {
    fn visit_term(&mut self, _: &mut JTerm) {}
    fn visit_lval(&mut self, _: &mut JLVal) {}
    fn visit_stmt(&mut self, _: &mut JStmt) {}
}

impl<V: Visitor> Visitor for &mut V {
    fn visit_term(&mut self, t: &mut JTerm) {
        V::visit_term(self, t);
    }

    fn visit_lval(&mut self, t: &mut JLVal) {
        V::visit_lval(self, t);
    }

    fn visit_stmt(&mut self, t: &mut JStmt) {
        V::visit_stmt(self, t);
    }
}

struct VTerm<F: FnMut(&mut JTerm)>(F);

impl<F: FnMut(&mut JTerm)> Visitor for VTerm<F> {
    fn visit_term(&mut self, t: &mut JTerm) {
        self.0(t);
    }
}

struct VStmt<F: FnMut(&mut JStmt)>(F);

impl<F: FnMut(&mut JStmt)> Visitor for VStmt<F> {
    fn visit_stmt(&mut self, t: &mut JStmt) {
        self.0(t);
    }
}

struct VLVal<F: FnMut(&mut JLVal)>(F);

impl<F: FnMut(&mut JLVal)> Visitor for VLVal<F> {
    fn visit_lval(&mut self, t: &mut JLVal) {
        self.0(t);
    }
}

impl JTerm {
    /// A post-order traversal - f will be applied to this node's children first, then this node
    fn map(&mut self, f: &mut impl Visitor) {
        match self {
            JTerm::Var(_, _) => (),
            JTerm::Lit(_) => (),
            JTerm::Cast(x, _) => {
                x.map(f);
            }
            JTerm::Call(o, _, a, _) => {
                if let Some(o) = o {
                    o.map(f);
                }
                // o.as_mut().map(|x| x.map(&mut f));
                a.iter_mut().for_each(|x| x.map(f));
            }
            JTerm::Prop(x, _, _) => x.map(f),
            JTerm::BinOp(_, a, b) => {
                a.map(f);
                b.map(f);
            }
            JTerm::Variant(_, _) => (),
            JTerm::Array(v, _) => {
                v.iter_mut().for_each(|x| x.map(f));
            }
            JTerm::ArrayNew(v, _) => v.map(f),
            JTerm::ClassNew(_, v) => {
                v.iter_mut().for_each(|x| x.map(f));
            }
            JTerm::Index(x, y, _) => {
                x.map(f);
                y.map(f);
            }
            JTerm::SIndex(v, y) => {
                v.iter_mut().for_each(|x| x.map(f));
                y.map(f);
            }
            JTerm::Not(x) => x.map(f),
            JTerm::Null(_) => (),
            JTerm::This(_) => (),
            JTerm::InlineJava(_, _) => (),
        }
        f.visit_term(self);
    }
}

impl JLVal {
    fn map(&mut self, f: &mut impl Visitor) {
        match self {
            JLVal::Var(_) => (),
            JLVal::Idx(x, y) => {
                x.map(f);
                y.map(f);
            }
            JLVal::SIdx(v, y) => {
                v.iter_mut().for_each(|x| x.map(f));
                y.map(f);
            }
            JLVal::Prop(x, _) => x.map(f),
        }
        f.visit_lval(self);
    }
}

impl JStmt {
    fn map(&mut self, f: &mut impl Visitor) {
        match self {
            JStmt::Multi(b) => b.iter_mut().for_each(|x| x.map(f)),
            JStmt::Let(_, _, _, x) => {
                x.as_mut().map(|x| x.map(f));
            }
            JStmt::Set(l, _, x) => {
                l.map(f);
                x.map(f);
            }
            JStmt::Term(x) => x.map(f),
            JStmt::If(x, a, b) => {
                x.map(f);
                a.iter_mut().for_each(|x| x.map(f));
                b.iter_mut().for_each(|x| x.map(f));
            }
            JStmt::Switch(_, x, a, b) => {
                x.map(f);
                a.iter_mut()
                    .flat_map(|(_, b)| b.iter_mut())
                    .for_each(|x| x.map(f));
                b.iter_mut().for_each(|x| x.map(f));
            }
            JStmt::While(_, x, a) => {
                x.map(f);
                a.iter_mut().for_each(|x| x.map(f));
            }
            JStmt::RangeFor(_, _, _, x, y, b, _) => {
                x.map(f);
                y.map(f);
                b.iter_mut().for_each(|x| x.map(f));
            }
            JStmt::Continue(_) => (),
            JStmt::Break(_) => (),
            JStmt::Ret(_, v) => {
                v.iter_mut().for_each(|x| x.map(f));
            }
            JStmt::MultiCall(x, _, v, _) => {
                x.as_mut().map(|x| x.map(f));
                v.iter_mut().for_each(|x| x.map(f));
            }
            JStmt::InlineJava(_) => (),
        }
        f.visit_stmt(self);
    }
}

#[derive(Default)]
struct UseCounter {
    /// Doesn't count definition
    count: HashMap<JVar, usize>,
    mutated: HashSet<JVar>,
    defined: HashSet<JVar>,
}

impl Visitor for UseCounter {
    fn visit_term(&mut self, t: &mut JTerm) {
        match t {
            JTerm::Var(v, _) => *self.count.entry(*v).or_default() += 1,
            _ => (),
        }
    }

    fn visit_lval(&mut self, t: &mut JLVal) {
        match t {
            JLVal::Var(v) => {
                // *self.count.entry(*v).or_default() += 1;
                self.mutated.insert(*v);
            }
            _ => (),
        }
    }

    fn visit_stmt(&mut self, t: &mut JStmt) {
        match t {
            JStmt::Let(_, _, v, _) => {
                self.defined.insert(*v);
            }
            JStmt::RangeFor(_, _, v, _, _, _, _) => {
                self.defined.insert(*v);
            }
            JStmt::MultiCall(_, _, _, rs) => {
                for (_, v, _) in rs {
                    self.defined.insert(*v);
                }
            }
            _ => (),
        }
    }
}

struct SideEffects(bool);

impl Visitor for SideEffects {
    fn visit_term(&mut self, t: &mut JTerm) {
        self.0 |= match t {
            JTerm::Call(_, _, _, _) => true,
            JTerm::ClassNew(_, _) => true,
            JTerm::InlineJava(_, _) => true,

            JTerm::Var(_, _)
            | JTerm::Cast(_, _)
            | JTerm::Lit(_)
            | JTerm::Prop(_, _, _)
            | JTerm::BinOp(_, _, _)
            | JTerm::Variant(_, _)
            | JTerm::Array(_, _)
            | JTerm::ArrayNew(_, _)
            | JTerm::Index(_, _, _)
            | JTerm::SIndex(_, _)
            | JTerm::Not(_)
            | JTerm::Null(_)
            | JTerm::This(_) => false,
        };
    }

    fn visit_lval(&mut self, _: &mut JLVal) {
        // Lvalues don't directly have side effects
    }

    fn visit_stmt(&mut self, s: &mut JStmt) {
        self.0 |= match s {
            JStmt::Set(_, _, _) => true,
            JStmt::Continue(_) => true,
            JStmt::Break(_) => true,
            JStmt::Ret(_, _) => true,
            JStmt::MultiCall(_, _, _, _) => true,
            JStmt::InlineJava(_) => true,

            JStmt::Let(_, _, _, _)
            | JStmt::Term(_)
            | JStmt::If(_, _, _)
            | JStmt::Switch(_, _, _, _)
            | JStmt::While(_, _, _)
            | JStmt::RangeFor(_, _, _, _, _, _, _)
            | JStmt::Multi(_) => false,
        };
    }
}

impl JFn {
    fn map(&mut self, f: &mut impl Visitor) {
        self.body.iter_mut().for_each(|x| x.map(f));
    }
}

impl JItem {
    fn blocks(&mut self) -> Vec<&mut Vec<JStmt>> {
        match self {
            JItem::Fn(x) => vec![&mut x.body],
            JItem::Enum(_, _, _, methods) => methods.iter_mut().map(|x| &mut x.body).collect(),
            JItem::Class(_, members, methods) => {
                let mut r = Vec::new();
                for (_, b) in members {
                    r.push(b);
                }
                for i in methods {
                    r.push(&mut i.body);
                }
                r
            }
            JItem::Let(_, b) => vec![b],
        }
    }

    fn map(&mut self, f: &mut impl Visitor) {
        match self {
            JItem::Fn(x) => x.map(f),
            JItem::Enum(_, _, _, methods) => methods.iter_mut().for_each(|x| x.map(f)),
            JItem::Class(_, members, methods) => {
                for (v, b) in members {
                    for (_, _, x) in v {
                        if let Some(x) = x {
                            x.map(f);
                        }
                    }
                    b.iter_mut().for_each(|x| x.map(f));
                }
                methods.iter_mut().for_each(|x| x.map(f));
            }
            JItem::Let(v, b) => {
                for (_, _, x) in v {
                    if let Some(x) = x {
                        x.map(f);
                    }
                }
                b.iter_mut().for_each(|x| x.map(f));
            }
        }
    }
}

impl<'a> lower::Cxt<'a> {
    fn replace(&mut self, v: JVar, x: &JTerm) {
        for i in &mut self.items {
            i.map(&mut VTerm(|t| match t {
                JTerm::Var(y, _) if *y == v => {
                    *t = x.clone();
                }
                _ => (),
            }));
        }
    }

    pub(super) fn opt(&mut self) -> Result<(), Error> {
        // Constant propagation
        for item in &mut self.items {
            let span = item.span;
            for block in item.blocks() {
                let mut env = Env::new(self.bindings, self.next);
                for s in block {
                    s.prop(&mut env)
                        .map_err(|e| Spanned::new(Doc::start("In this item: ").chain(e), span))?;
                }
                self.next = env.next;
            }
        }

        // Remove unused variables
        let mut counter = UseCounter::default();
        for i in &mut self.items {
            i.map(&mut counter);
        }
        for i in &mut self.items {
            i.map(&mut VStmt(|t| match t {
                JStmt::Let(_, _, v, Some(x)) => {
                    if !v.1 && counter.count.get(v).map_or(true, |x| *x == 0) {
                        let mut effects = SideEffects(false);
                        x.map(&mut effects);
                        if !effects.0 {
                            *t = JStmt::Multi(Vec::new());
                        }
                    }
                }
                JStmt::Let(_, _, v, None) => {
                    if !v.1 && counter.count.get(v).map_or(true, |x| *x == 0) {
                        *t = JStmt::Multi(Vec::new());
                    }
                }
                JStmt::Set(l, _, x) => {
                    if let Some(v) = l.root_var() {
                        if !v.1
                            && counter.count.get(&v).map_or(true, |x| *x == 0)
                            && counter.defined.contains(&v)
                        {
                            let mut effects = SideEffects(false);
                            x.map(&mut effects);
                            if !effects.0 {
                                *t = JStmt::Multi(Vec::new());
                            }
                        }
                    }
                }
                _ => (),
            }));
        }
        Ok(())
    }
}

impl JLVal {
    fn root_var(&self) -> Option<JVar> {
        match self {
            JLVal::Var(v) => Some(*v),
            JLVal::Idx(l, _) => l.root_var(),
            JLVal::SIdx(l, i) => match i {
                JTerm::Lit(JLit::Int(i)) => l.get(*i as usize)?.root_var(),
                _ => None,
            },
            JLVal::Prop(JTerm::Var(v, _), _) => Some(*v),
            JLVal::Prop(_, _) => None,
        }
    }
}

struct Replacer(HashMap<JVar, JVar>);

impl Visitor for Replacer {
    fn visit_term(&mut self, t: &mut JTerm) {
        match t {
            JTerm::Var(v, _) => {
                if let Some(v2) = self.0.get(v) {
                    *v = *v2;
                }
            }
            _ => (),
        }
    }

    fn visit_lval(&mut self, l: &mut JLVal) {
        match l {
            JLVal::Var(v) => {
                if let Some(v2) = self.0.get(v) {
                    *v = *v2;
                }
            }
            _ => (),
        }
    }

    fn visit_stmt(&mut self, t: &mut JStmt) {
        match t {
            JStmt::Let(_, _, v, _) => {
                if let Some(v2) = self.0.get(v) {
                    *v = *v2;
                }
            }
            JStmt::RangeFor(_, _, v, _, _, _, _) => {
                if let Some(v2) = self.0.get(v) {
                    *v = *v2;
                }
            }
            JStmt::MultiCall(_, _, _, rs) => {
                for (_, v, _) in rs {
                    if let Some(v2) = self.0.get(v) {
                        *v = *v2;
                    }
                }
            }
            _ => (),
        }
    }
}

impl JTerm {
    fn ops(&self) -> usize {
        match self {
            JTerm::Var(_, _) => 1,
            JTerm::Lit(_) => 1,
            JTerm::Cast(a, _) => a.ops() + 1,
            JTerm::Call(_, _, _, _) => 100,
            JTerm::Prop(a, _, _) => a.ops() + 1,
            JTerm::BinOp(_, a, b) => a.ops() + b.ops() + 1,
            JTerm::Variant(_, _) => 1,
            JTerm::Array(_, _) => 100,
            JTerm::ArrayNew(_, _) => 100,
            JTerm::ClassNew(_, _) => 100,
            JTerm::Index(a, i, _) => a.ops() + i.ops() + 1,
            JTerm::SIndex(_, _) => 1,
            JTerm::Not(a) => a.ops() + 1,
            JTerm::Null(_) => 1,
            JTerm::This(_) => 1,
            JTerm::InlineJava(_, _) => 100,
        }
    }

    fn start_valid(&self, env: &mut Env) -> bool {
        match self {
            JTerm::Var(v, _) => {
                env.not_modified.insert(*v);
                true
            }
            JTerm::BinOp(_, a, b) => a.start_valid(env) && b.start_valid(env),
            JTerm::Lit(_) => true,
            JTerm::Prop(a, _, _) => a.start_valid(env),
            JTerm::Variant(_, _) => true,
            JTerm::Array(v, _) => v.iter().all(|x| x.start_valid(env)),
            JTerm::ArrayNew(l, _) => l.start_valid(env),
            JTerm::Index(a, i, _) => a.start_valid(env) && i.start_valid(env),
            JTerm::SIndex(v, i) => v.iter().all(|x| x.start_valid(env)) && i.start_valid(env),
            JTerm::Not(a) => a.start_valid(env),
            JTerm::Null(_) => true,
            _ => false,
        }
    }

    fn is_valid(&self, env: &Env) -> bool {
        match self {
            JTerm::Var(v, _) => env.not_modified.contains(v),
            JTerm::BinOp(_, a, b) => a.is_valid(env) && b.is_valid(env),
            JTerm::Lit(_) => true,
            JTerm::Prop(a, _, _) => a.is_valid(env),
            JTerm::Variant(_, _) => true,
            JTerm::Array(v, _) => v.iter().all(|x| x.is_valid(env)),
            JTerm::ArrayNew(l, _) => l.is_valid(env),
            JTerm::Index(a, i, _) => a.is_valid(env) && i.is_valid(env),
            JTerm::SIndex(v, i) => v.iter().all(|x| x.is_valid(env)) && i.is_valid(env),
            JTerm::Not(a) => a.is_valid(env),
            JTerm::Null(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum CVal {
    Null(JTy),
    Int(i32),
    Long(i64),
    Float(f32),
    Double(f64),
    Bool(bool),
    String(RawSym),
    Array {
        idxs: HashMap<usize, CVal>,
        end: Vec<CVal>,
        len: Option<usize>,
    },
    Class(HashMap<Prop, CVal>),
    Variant(JClass, RawSym),
    Term(JTerm),
}

impl CVal {
    fn to_term(&self, env: &Env) -> Option<JTerm> {
        match self {
            CVal::Null(ty) => Some(JTerm::Null(ty.clone())),
            CVal::Int(b) => Some(JTerm::Lit(JLit::Int(*b))),
            CVal::Long(b) => Some(JTerm::Lit(JLit::Long(*b))),
            CVal::Float(b) => Some(JTerm::Lit(JLit::Float(*b))),
            CVal::Double(b) => Some(JTerm::Lit(JLit::Double(*b))),
            CVal::Bool(b) => Some(JTerm::Lit(JLit::Bool(*b))),
            CVal::Variant(class, r) => Some(JTerm::Variant(*class, *r)),
            CVal::String(r) => Some(JTerm::Lit(JLit::Str(*r))),
            CVal::Array { idxs, end: _, len } => {
                let mut v = Vec::new();
                let mut ty = None;
                for i in 0..len.clone()? {
                    let t = idxs.get(&i)?.to_term(env)?;
                    ty = Some(t.ty());
                    v.push(t);
                }
                Some(JTerm::Array(v, ty?))
            }
            CVal::Class(_) => None,
            CVal::Term(t) => {
                if t.is_valid(env) {
                    Some(t.clone())
                } else {
                    None
                }
            }
        }
    }

    fn to_term_partial(&self) -> Option<JTerm> {
        match self {
            CVal::Null(ty) => Some(JTerm::Null(ty.clone())),
            CVal::Int(b) => Some(JTerm::Lit(JLit::Int(*b))),
            CVal::Long(b) => Some(JTerm::Lit(JLit::Long(*b))),
            CVal::Float(b) => Some(JTerm::Lit(JLit::Float(*b))),
            CVal::Double(b) => Some(JTerm::Lit(JLit::Double(*b))),
            CVal::Bool(b) => Some(JTerm::Lit(JLit::Bool(*b))),
            CVal::Variant(class, r) => Some(JTerm::Variant(*class, *r)),
            CVal::String(r) => Some(JTerm::Lit(JLit::Str(*r))),
            CVal::Array { idxs, end: _, len } => {
                let mut v = Vec::new();
                let mut ty = None;
                for i in 0..len.clone()? {
                    let t = idxs.get(&i)?.to_term_partial()?;
                    ty = Some(t.ty());
                    v.push(t);
                }
                Some(JTerm::Array(v, ty?))
            }
            CVal::Class(_) => None,
            CVal::Term(t) => Some(t.clone()),
        }
    }

    fn union(self, other: CVal) -> Option<CVal> {
        match (self, other) {
            (a, b) if a == b => Some(a),
            (
                CVal::Array {
                    mut idxs,
                    mut end,
                    mut len,
                },
                CVal::Array {
                    idxs: i2,
                    end: e2,
                    len: l2,
                },
            ) => {
                if len != l2 {
                    len = None;
                }
                let keys: Vec<_> = idxs.keys().cloned().collect();
                for k in keys {
                    let a = idxs[&k].clone();
                    let b = i2.get(&k).cloned();
                    if let Some(v) = b.and_then(|b| a.union(b)) {
                        idxs.insert(k, v);
                    } else {
                        idxs.remove(&k);
                    }
                }
                // TODO unify end and e2
                end.clear();
                Some(CVal::Array { idxs, end, len })
            }
            (CVal::Class(mut m1), CVal::Class(m2)) => {
                let keys: Vec<_> = m1.keys().cloned().collect();
                for k in keys {
                    let a = m1[&k].clone();
                    let b = m2.get(&k).cloned();
                    if let Some(v) = b.and_then(|b| a.union(b)) {
                        m1.insert(k, v);
                    } else {
                        m1.remove(&k);
                    }
                }
                Some(CVal::Class(m1))
            }
            (_, _) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct Env<'a> {
    locals: HashSet<JVar>,
    env: HashMap<JVar, CVal>,
    not_modified: HashSet<JVar>,
    bindings: &'a Bindings,
    next: u64,
}

impl<'a> Env<'a> {
    pub(super) fn new(bindings: &'a Bindings, next: u64) -> Self {
        Env {
            locals: HashSet::new(),
            env: HashMap::new(),
            not_modified: HashSet::new(),
            bindings,
            next,
        }
    }

    fn union(&mut self, other: &Env) {
        self.next = other.next;
        let keys: Vec<_> = self.env.keys().cloned().collect();
        for k in keys {
            let a = self.env[&k].clone();
            let b = other.env.get(&k).cloned();
            if let Some(v) = b.and_then(|b| a.union(b)) {
                self.env.insert(k, v);
            } else {
                self.env.remove(&k);
            }
        }
    }

    fn var(&self, v: JVar) -> Option<CVal> {
        self.env.get(&v).cloned()
    }

    /// This needs to be called whenever we clobber anything
    /// because other things could store a reference to an object in scope and clobber its members
    fn clobber_members(&mut self) {
        for (k, v) in &mut self.env {
            if let CVal::Class(m) = v {
                m.clear();
                self.not_modified.remove(k);
            }
        }
        for k in self.not_modified.iter().cloned().collect::<Vec<_>>() {
            if !self.env.contains_key(&k) {
                self.not_modified.remove(&k);
            }
        }
    }

    fn clobber_specific_members(&mut self, prop: &Prop, val: Option<CVal>) {
        for (k, v) in &mut self.env {
            if let CVal::Class(m) = v {
                if m.remove(prop) == val && val.is_some() {
                    m.insert(*prop, val.clone().unwrap());
                }
                self.not_modified.remove(k);
            }
        }
        for k in self.not_modified.iter().cloned().collect::<Vec<_>>() {
            if !self.env.contains_key(&k) {
                self.not_modified.remove(&k);
            }
        }
    }

    fn clobber_public(&mut self) {
        self.clobber_members();
        for k in self.env.keys().cloned().collect::<Vec<_>>() {
            if k.1 {
                self.env.remove(&k);
            }
        }
        for k in self.not_modified.iter().cloned().collect::<Vec<_>>() {
            if k.1 {
                self.not_modified.remove(&k);
            }
        }
    }

    fn clobber_globals(&mut self) {
        self.clobber_members();
        for k in self.env.keys().cloned().collect::<Vec<_>>() {
            if !self.locals.contains(&k) {
                self.env.remove(&k);
            }
        }
        for k in self.not_modified.iter().cloned().collect::<Vec<_>>() {
            if !self.locals.contains(&k) {
                self.not_modified.remove(&k);
            }
        }
    }

    fn fresh_var(&mut self, public: bool) -> JVar {
        self.next += 1;
        JVar(self.next, public)
    }
}

impl JLVal {
    fn get(&mut self, env: &mut Env) -> Option<CVal> {
        match self {
            JLVal::Var(v) => env.env.get(v).cloned(),
            JLVal::Idx(arr, idx) => {
                let idx = idx.prop(env)?;
                let arr = arr.get(env)?;
                match arr {
                    CVal::Array { idxs, .. } => match idx {
                        CVal::Int(i) => idxs.get(&(i as usize)).cloned(),
                        _ => None,
                    },
                    _ => None,
                }
            }
            JLVal::SIdx(arr, idx) => match idx.prop(env)? {
                CVal::Int(i) => arr[i as usize].get(env),
                _ => None,
            },
            JLVal::Prop(x, p) => match x.clone().to_lval()?.get(env) {
                Some(CVal::Class(m)) => m.get(p).cloned(),
                // We can't set array length
                _ => None,
            },
        }
    }

    // Should really be `&'a mut self` but that only works if references can outlive their referents
    fn get_mut<'a>(&mut self, env: &'a mut Env) -> Option<&'a mut CVal> {
        match self {
            JLVal::Var(v) => env.env.get_mut(v),
            JLVal::Idx(arr, idx) => {
                let idx = idx.prop(env)?;
                let arr = arr.get_mut(env)?;
                match arr {
                    CVal::Array { idxs, .. } => match idx {
                        CVal::Int(i) => idxs.get_mut(&(i as usize)),
                        _ => None,
                    },
                    _ => None,
                }
            }
            JLVal::SIdx(arr, idx) => match idx.prop(env)? {
                CVal::Int(i) => arr[i as usize].get_mut(env),
                _ => None,
            },
            JLVal::Prop(x, p) => match x.clone().to_lval()?.get_mut(env) {
                Some(CVal::Class(m)) => m.get_mut(p),
                // We can't set array length
                _ => None,
            },
        }
    }

    fn set(&mut self, env: &mut Env, val: Option<CVal>) -> Result<(), Doc> {
        match self {
            JLVal::Var(v) => {
                env.not_modified.remove(v);
                env.env.remove(v);
                let mut to_remove = Vec::new();
                for (k, v) in &env.env {
                    if let CVal::Term(v) = v {
                        if !v.is_valid(env) {
                            to_remove.push(*k);
                        }
                    }
                }
                for k in to_remove {
                    env.env.remove(&k);
                }
                if let Some(val) = val {
                    env.env.insert(*v, val);
                }
            }
            JLVal::SIdx(arr, idx) => match idx.prop(env) {
                Some(CVal::Int(i)) => {
                    let len = arr.len();
                    arr.get_mut(i as usize)
                        .ok_or(
                            Doc::start("Index ")
                                .add(i)
                                .add(" out of bounds to static array with size ")
                                .add(len),
                        )?
                        .set(env, val)?
                }
                _ => {
                    for i in arr {
                        i.set(env, None)?;
                    }
                }
            },
            JLVal::Prop(x, p) => {
                env.clobber_specific_members(p, None);
                match x.clone().to_lval().and_then(|mut x| x.get_mut(env)) {
                    Some(CVal::Class(m)) => {
                        if let Some(val) = val {
                            m.insert(*p, val);
                        }
                    }
                    _ => (),
                }
            }
            JLVal::Idx(l, i) => {
                if let Some(val) = val {
                    if let Some(CVal::Int(i)) = i.prop(env) {
                        if let Some(CVal::Array { mut idxs, end, len }) = l.get(env) {
                            idxs.insert(i as usize, val);
                            l.set(env, Some(CVal::Array { idxs, end, len }))?;
                            return Ok(());
                        }
                    }
                }

                l.set(env, None)?;
            }
        }
        Ok(())
    }
}

impl JStmt {
    fn prop(&mut self, env: &mut Env) -> Result<(), Doc> {
        match self {
            JStmt::Let(_, _, v, x) => {
                let x = x.as_mut().map(|x| x.prop(env)).flatten();
                env.locals.insert(*v);
                if let Some(x) = x {
                    env.env.insert(*v, x);
                }
            }
            JStmt::Set(l, op, x2) => {
                let x = x2.prop(env);
                if let Some(x) = x {
                    match op {
                        Some(op) => {
                            if let Some(y) = l.get(env) {
                                if let Some(x) = op.prop(x, y.clone()) {
                                    l.set(env, Some(x))?;
                                } else {
                                    l.set(env, None)?;
                                }
                            } else {
                                l.set(env, None)?;
                            }
                        }
                        None => l.set(env, Some(x))?,
                    }
                } else {
                    l.set(env, None)?;
                }
            }
            JStmt::Term(x) => {
                x.prop(env);
            }
            JStmt::If(cond, a, b) => {
                let cond = cond.prop(env);
                if let Some(CVal::Bool(cond)) = cond {
                    let tmp = if cond { a } else { b };
                    let mut block = Vec::new();
                    std::mem::swap(tmp, &mut block);
                    for b in &mut block {
                        b.prop(env)?;
                    }
                    *self = JStmt::Multi(block);
                } else {
                    let mut env2 = env.clone();
                    for b in a {
                        b.prop(&mut env2)?;
                    }
                    env.union(&env2);
                    let mut env3 = env.clone();
                    for b in b {
                        b.prop(&mut env3)?;
                    }
                    env.union(&env3);
                }
            }
            JStmt::Switch(_, x, cases, other) => {
                let x = x.prop(env);
                if let Some(CVal::Variant(_, r)) = x {
                    let tmp = cases
                        .iter_mut()
                        .find(|(c, _)| *c == r)
                        .map(|(_, b)| b)
                        .unwrap_or(other);
                    let mut block = Vec::new();
                    std::mem::swap(tmp, &mut block);
                    for b in &mut block {
                        b.prop(env)?;
                    }
                    *self = JStmt::Multi(block);
                } else {
                    for (_, c) in cases {
                        let mut env2 = env.clone();
                        for i in c {
                            i.prop(&mut env2)?;
                        }
                        env.union(&env2);
                    }
                    let mut env2 = env.clone();
                    for i in other {
                        i.prop(&mut env2)?;
                    }
                    env.union(&env2);
                }
            }
            JStmt::While(_, cond, block) => {
                let mut counter = UseCounter::default();
                for s in block.iter_mut() {
                    s.map(&mut counter);
                }
                for i in counter.mutated {
                    env.env.remove(&i);
                    env.not_modified.remove(&i);
                }
                cond.prop(env);
                for i in block {
                    i.prop(env)?;
                }
            }
            JStmt::RangeFor(_, raw, v, a, b, block, unroll) => {
                let a = a.prop(env);
                let b = b.prop(env);
                env.locals.insert(*v);
                if *unroll {
                    match (a, b) {
                        (Some(CVal::Int(a)), Some(CVal::Int(b))) => {
                            let mut counter = UseCounter::default();
                            for s in block.iter_mut() {
                                s.map(&mut counter);
                            }

                            let mut stmts = Vec::new();
                            stmts.push(JStmt::Let(*raw, JTy::I32, *v, None));
                            for i in a..b {
                                stmts.push(JStmt::Set(
                                    JLVal::Var(*v),
                                    None,
                                    JTerm::Lit(JLit::Int(i)),
                                ));
                                let mut replacer = Replacer(
                                    counter
                                        .defined
                                        .iter()
                                        .map(|v| (*v, env.fresh_var(false)))
                                        .collect(),
                                );
                                for s in block.iter() {
                                    let mut s = s.clone();
                                    s.map(&mut replacer);
                                    stmts.push(s);
                                }
                            }
                            *self = JStmt::Multi(stmts);
                            return self.prop(env);
                        }
                        _ => (),
                    }
                }
                env.locals.insert(*v);
                let mut counter = UseCounter::default();
                for s in block.iter_mut() {
                    s.map(&mut counter);
                }
                for i in counter.mutated {
                    env.env.remove(&i);
                    env.not_modified.remove(&i);
                }
                for i in block {
                    i.prop(env)?;
                }
            }
            JStmt::Continue(_) => (),
            JStmt::Break(_) => (),
            JStmt::Ret(_, v) => {
                for x in v {
                    x.prop(env);
                }
            }
            JStmt::MultiCall(o, _, args, rets) => {
                o.as_mut().map(|x| x.prop(env));
                args.iter_mut().for_each(|x| {
                    x.prop(env);
                });
                for (_, v, _) in rets {
                    env.locals.insert(*v);
                }
                env.clobber_globals();
            }
            JStmt::InlineJava(_) => env.clobber_public(),
            JStmt::Multi(v) => {
                for i in v {
                    i.prop(env)?;
                }
            }
        }
        Ok(())
    }
}

impl BinOp {
    fn prop(self, a: CVal, b: CVal) -> Option<CVal> {
        use CVal::*;
        match (a, b) {
            (Int(a), Int(b)) => Some(match self {
                BinOp::Add => Int(a + b),
                BinOp::Sub => Int(a - b),
                BinOp::Mul => Int(a * b),
                BinOp::Div => Int(a / b),
                BinOp::Mod => Int(a % b),
                BinOp::Gt => Bool(a > b),
                BinOp::Lt => Bool(a < b),
                BinOp::Eq => Bool(a == b),
                BinOp::Neq => Bool(a != b),
                BinOp::Geq => Bool(a >= b),
                BinOp::Leq => Bool(a <= b),
                BinOp::BitAnd => Int(a & b),
                BinOp::BitOr => Int(a | b),
                BinOp::BitXor => Int(a ^ b),
                BinOp::BitShr => Int(a >> b),
                BinOp::BitShl => Int(a << b),
                BinOp::And | BinOp::Or => unreachable!(),
            }),
            (Long(a), Long(b)) => Some(match self {
                BinOp::Add => Long(a + b),
                BinOp::Sub => Long(a - b),
                BinOp::Mul => Long(a * b),
                BinOp::Div => Long(a / b),
                BinOp::Mod => Long(a % b),
                BinOp::Gt => Bool(a > b),
                BinOp::Lt => Bool(a < b),
                BinOp::Eq => Bool(a == b),
                BinOp::Neq => Bool(a != b),
                BinOp::Geq => Bool(a >= b),
                BinOp::Leq => Bool(a <= b),
                BinOp::BitAnd => Long(a & b),
                BinOp::BitOr => Long(a | b),
                BinOp::BitXor => Long(a ^ b),
                BinOp::BitShr => Long(a >> b),
                BinOp::BitShl => Long(a << b),
                BinOp::And | BinOp::Or => unreachable!(),
            }),
            (Float(a), Float(b)) => Some(match self {
                BinOp::Add => Float(a + b),
                BinOp::Sub => Float(a - b),
                BinOp::Mul => Float(a * b),
                BinOp::Div => Float(a / b),
                BinOp::Mod => Float(a % b),
                BinOp::Gt => Bool(a > b),
                BinOp::Lt => Bool(a < b),
                BinOp::Eq => Bool(a == b),
                BinOp::Neq => Bool(a != b),
                BinOp::Geq => Bool(a >= b),
                BinOp::Leq => Bool(a <= b),
                BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor | BinOp::BitShr | BinOp::BitShl => {
                    unreachable!()
                }
                BinOp::And | BinOp::Or => unreachable!(),
            }),
            (Double(a), Double(b)) => Some(match self {
                BinOp::Add => Double(a + b),
                BinOp::Sub => Double(a - b),
                BinOp::Mul => Double(a * b),
                BinOp::Div => Double(a / b),
                BinOp::Mod => Double(a % b),
                BinOp::Gt => Bool(a > b),
                BinOp::Lt => Bool(a < b),
                BinOp::Eq => Bool(a == b),
                BinOp::Neq => Bool(a != b),
                BinOp::Geq => Bool(a >= b),
                BinOp::Leq => Bool(a <= b),
                BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor | BinOp::BitShr | BinOp::BitShl => {
                    unreachable!()
                }
                BinOp::And | BinOp::Or => unreachable!(),
            }),
            (Bool(a), Bool(b)) => Some(Bool(match self {
                BinOp::Eq => a == b,
                BinOp::Neq => a != b,
                // We don't care about evaluation order, we're not doing any side effects
                BinOp::BitAnd => a & b,
                BinOp::BitOr => a | b,
                BinOp::BitXor => a ^ b,
                BinOp::And => a & b,
                BinOp::Or => a | b,
                _ => unreachable!(),
            })),
            (a @ (String(_) | Null(_)), b @ (String(_) | Null(_))) => Some(Bool(match self {
                BinOp::Eq => a == b,
                BinOp::Neq => a != b,
                _ => return None,
            })),
            (a, b) => {
                let a = a.to_term_partial()?;
                let b = b.to_term_partial()?;
                Some(CVal::Term(JTerm::BinOp(self, Box::new(a), Box::new(b))))
            } // _ => None,
        }
    }
}

impl JTerm {
    pub(super) fn prop(&mut self, env: &mut Env) -> Option<CVal> {
        let r = match self {
            JTerm::Var(v, _) => match env.var(*v) {
                Some(val) => {
                    if let Some(t) = val.to_term(env) {
                        *self = t;
                    }
                    Some(val)
                }
                None => None,
            },
            JTerm::Lit(l) => Some(match l {
                JLit::Int(i) => CVal::Int(*i),
                JLit::Long(i) => CVal::Long(*i),
                JLit::Float(i) => CVal::Float(*i),
                JLit::Double(i) => CVal::Double(*i),
                JLit::Str(s) => CVal::String(*s),
                JLit::Bool(b) => CVal::Bool(*b),
            }),
            JTerm::Cast(x, ty) => match x.prop(env)? {
                CVal::Int(i) => match ty {
                    JTy::I32 => Some(CVal::Int(i as i32)),
                    JTy::I64 => Some(CVal::Long(i as i64)),
                    JTy::F32 => Some(CVal::Float(i as f32)),
                    JTy::F64 => Some(CVal::Double(i as f64)),
                    _ => None,
                },
                CVal::Long(i) => match ty {
                    JTy::I32 => Some(CVal::Int(i as i32)),
                    JTy::I64 => Some(CVal::Long(i as i64)),
                    JTy::F32 => Some(CVal::Float(i as f32)),
                    JTy::F64 => Some(CVal::Double(i as f64)),
                    _ => None,
                },
                CVal::Float(i) => match ty {
                    JTy::I32 => Some(CVal::Int(i as i32)),
                    JTy::I64 => Some(CVal::Long(i as i64)),
                    JTy::F32 => Some(CVal::Float(i as f32)),
                    JTy::F64 => Some(CVal::Double(i as f64)),
                    _ => None,
                },
                CVal::Double(i) => match ty {
                    JTy::I32 => Some(CVal::Int(i as i32)),
                    JTy::I64 => Some(CVal::Long(i as i64)),
                    JTy::F32 => Some(CVal::Float(i as f32)),
                    JTy::F64 => Some(CVal::Double(i as f64)),
                    _ => None,
                },
                _ => None,
            },
            // Ignoring some terms, like the object of a method call, is fine
            // They won't be optimized, but complicated expressions shouldn't be method receivers anyway
            JTerm::Call(_, _, args, _) => {
                for i in args {
                    i.prop(env);
                }
                env.clobber_globals();
                // TODO check for side effects
                None
            }
            JTerm::Prop(x, p, _) => match x.prop(env) {
                Some(CVal::Class(m)) => m.get(p).cloned(),
                Some(CVal::Array { len, .. }) => {
                    assert_eq!(*p, Prop::Raw(env.bindings.get_raw("length").unwrap()));
                    len.map(|i| CVal::Int(i as i32))
                }
                _ => None,
            },
            JTerm::BinOp(op, a, b) => {
                // Make sure we apply constprop to both before returning early
                let a = a.prop(env);
                let b = b.prop(env)?;
                let a = a?;
                let o = op.prop(a, b)?;
                if let Some(o) = o.to_term(env) {
                    *self = o;
                }
                Some(o)
            }
            JTerm::Variant(c, v) => Some(CVal::Variant(*c, *v)),
            JTerm::Array(x, _) => {
                let mut idxs = HashMap::new();
                let mut end = Vec::new();
                for (i, x) in x.iter_mut().enumerate() {
                    match x.prop(env) {
                        Some(t) => {
                            idxs.insert(i, t.clone());
                            end.push(t);
                        }
                        None => end.clear(),
                    }
                }
                Some(CVal::Array {
                    idxs,
                    end,
                    len: Some(if x.len() == 0 { 8 } else { x.len() }),
                })
            }
            JTerm::ArrayNew(len, _) => match len.prop(env)? {
                CVal::Int(i) => Some(CVal::Array {
                    // TODO fill with nulls for reference types?
                    idxs: HashMap::new(),
                    end: Vec::new(),
                    len: Some(i as usize),
                }),
                _ => None,
            },
            JTerm::ClassNew(_, args) => {
                for i in args {
                    i.prop(env);
                }
                Some(CVal::Class(HashMap::new()))
            }
            JTerm::Index(arr, idx, _) => {
                let arr = arr.prop(env);
                let idx = idx.prop(env)?;
                match arr? {
                    CVal::Array { idxs, .. } => match idx {
                        CVal::Int(i) => idxs.get(&(i as usize)).cloned(),
                        _ => None,
                    },
                    _ => None,
                }
            }
            JTerm::SIndex(arr, idx) => match idx.prop(env) {
                Some(CVal::Int(i)) => {
                    if i as usize > arr.len() || i < 0 {
                        return None;
                    }
                    *self = arr.swap_remove(i as usize);
                    self.prop(env)
                }
                _ => {
                    for i in arr {
                        i.prop(env);
                    }
                    None
                }
            },
            JTerm::Not(x) => match x.prop(env)? {
                CVal::Bool(b) => Some(CVal::Bool(!b)),
                _ => None,
            },
            JTerm::Null(ty) => Some(CVal::Null(ty.clone())),
            // TODO `this` in constprop for members and stuff
            JTerm::This(_) => None,
            JTerm::InlineJava(_, _) => {
                // Inline java could do anything to public variables
                env.clobber_public();
                None
            }
        };
        match r {
            Some(x) => Some(x),
            // TODO what constant makes sense here? A store and load takes 2
            None => {
                if self.ops() < 2 && self.start_valid(env) {
                    Some(CVal::Term(self.clone()))
                } else {
                    None
                }
            }
        }
    }
}
