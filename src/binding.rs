use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU32,
};

use crate::term::RawPath;

/// Represents an interned string directly
///
/// Same size properties as Sym
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct RawSym(NonZeroU32);
impl RawSym {
    fn new(idx: usize) -> Self {
        RawSym(NonZeroU32::new(idx as u32 + 1).expect("unreachable"))
    }
    fn idx(self) -> usize {
        self.0.get() as usize - 1
    }
}

/// An index into the pool of interned strings held by a `Bindings` object
///
/// It's the size of a u32 but is optimized for things like `Option<Sym>` (because it has a `NonZeroU32` inside)
/// The 18 least significant bits represent the raw symbol (interned string), the top 14 the instance of that symbol
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
pub struct Sym(NonZeroU32);
impl Sym {
    // fn from_parts(raw: RawSym, num: u32) -> Self {
    //     let idx = raw.idx();
    //     if idx >= 1 << 18 {
    //         panic!("Too many unique identifiers!");
    //     }
    //     if num >= 1 << 14 {
    //         panic!("Too many instances of identifier {}!", idx);
    //     }
    //     Sym(NonZeroU32::new((num << 18) | idx as u32 + 1).expect("unreachable"))
    // }
    // fn with_num(self, num: u32) -> Self {
    //     Sym::from_parts(self.raw(), num)
    // }

    // /// Gets the number corresponding to this symbol, so we can show the user that two symbols with the same name are distinct
    // pub fn num(self) -> u32 {
    //     (self.0.get() - 1) >> 18
    // }

    // /// Gets the raw symbol corresponding to this symbol
    // /// This can be used for comparing identifiers directly, as in record labels
    // pub fn raw(self) -> RawSym {
    //     RawSym(NonZeroU32::new(((self.0.get() - 1) & ((1 << 18) - 1)) + 1).expect("unreachable"))
    // }
}

/// Like a Sym, but it identifies a type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
pub struct TypeId(NonZeroU32);
impl TypeId {
    pub fn num(self) -> u32 {
        self.0.get() - 1
    }
}

/// Like a Sym, but it identifies a function
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
pub struct FnId(NonZeroU32);
impl FnId {
    pub fn num(self) -> u32 {
        self.0.get() - 1
    }
}

/// Implements globally-unique binding - every bound variable is unique, and we can freshen them if we copy it
/// That means we don't ever have to worry about capture-avoidance outside of this module and `Value::cloned()`
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Bindings {
    /// It's possible this is a memory problem (storing each string twice), but if so we'll deal with it then
    strings: HashMap<String, RawSym>,
    string_pool: Vec<String>,
    nums: HashMap<RawSym, u32>,
    pubs: HashSet<Sym>,
    types: Vec<RawPath>,
    fns: Vec<RawPath>,
    syms: Vec<RawPath>,
}
impl Bindings {
    /// Don't do this if you're holding symbols somewhere!
    pub fn reset(&mut self) {
        let mut b = Bindings::default();
        std::mem::swap(&mut b, self);
        // We want the RawSyms to be the same
        let Bindings {
            strings,
            string_pool,
            ..
        } = b;
        self.strings = strings;
        self.string_pool = string_pool;
    }

    pub fn type_name(&self, t: TypeId) -> RawPath {
        self.types[(t.0.get() - 1) as usize].clone()
    }

    pub fn add_type(&mut self, raw: RawPath) -> TypeId {
        self.types.push(raw);
        TypeId(NonZeroU32::new(self.types.len() as u32).unwrap())
    }

    pub fn fn_name(&self, t: FnId) -> RawPath {
        self.fns[(t.0.get() - 1) as usize].clone()
    }

    pub fn add_fn(&mut self, raw: RawPath) -> FnId {
        self.fns.push(raw);
        FnId(NonZeroU32::new(self.fns.len() as u32).unwrap())
    }

    /// Interns a string (or gets it if it's already interned), returning the RawSym to it
    pub fn raw(&mut self, s: impl Into<String>) -> RawSym {
        let s = s.into();
        self.strings.get(&s).copied().unwrap_or_else(|| {
            let i = RawSym::new(self.string_pool.len());
            self.strings.insert(s.clone(), i);
            self.string_pool.push(s);
            i
        })
    }

    /// Creates a new symbol with the same name as `s`, but a fresh value
    pub fn fresh(&mut self, s: Sym) -> Sym {
        let raw = self.syms[s.0.get() as usize - 1].clone();
        self.syms.push(raw);
        let s2 = Sym(NonZeroU32::new(self.syms.len() as u32).unwrap());
        if self.public(s) {
            self.pubs.insert(s2);
        }
        s2
    }

    pub fn public(&self, s: Sym) -> bool {
        self.pubs.contains(&s)
    }

    /// Create a new symbol. It's guaranteed to be unique to all other symbols created with create()
    pub fn create(&mut self, raw: RawPath, public: bool) -> Sym {
        self.syms.push(raw);
        let s = Sym(NonZeroU32::new(self.syms.len() as u32).unwrap());
        if public {
            self.pubs.insert(s);
        }
        s
    }

    pub fn sym_path(&self, s: Sym) -> RawPath {
        self.syms[s.0.get() as usize - 1].clone()
    }

    /// This doesn't return an Option, because only the Bindings can create symbols, and it adds them to `self.bindings`
    /// Therefore, if you pass a symbol created by another Bindings instance, this may panic
    pub fn resolve_spath(&self, s: Sym) -> String {
        let mut raw = &self.syms[s.0.get() as usize - 1];
        let mut s = String::new();
        for i in &raw.0 {
            s.push_str(self.resolve_raw(**i));
            s.push('.');
        }
        s.push_str(self.resolve_raw(*raw.1));
        s
    }

    pub fn resolve_path(&self, raw: &RawPath) -> String {
        let mut s = String::new();
        for i in &raw.0 {
            s.push_str(self.resolve_raw(**i));
            s.push('.');
        }
        s.push_str(self.resolve_raw(*raw.1));
        s
    }

    pub fn resolve_local(&self, s: Sym) -> &str {
        let mut raw = &self.syms[s.0.get() as usize - 1];
        self.resolve_raw(*raw.1)
    }

    pub fn resolve_raw(&self, s: RawSym) -> &str {
        self.string_pool
            .get(s.idx())
            .expect("String referred to by symbol not in Bindings interned string table!")
    }
}
