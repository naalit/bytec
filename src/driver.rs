use std::path::PathBuf;

use crate::term::*;

pub struct ModStatus {
    pub pre_items: Vec<PreItem>,
    pub items: Vec<Item>,
    pub ty: ModType,
    pub file: FileId,
    pub input_path: PathBuf,
}
pub trait Pass {
    fn run(
        &self,
        mods: &[(RawSym, ModType)],
        bindings: &mut Bindings,
        module: ModStatus,
    ) -> Result<ModStatus, Error>;
}
struct DeclareP1;
impl Pass for DeclareP1 {
    fn run(
        &self,
        _mods: &[(RawSym, ModType)],
        bindings: &mut Bindings,
        module: ModStatus,
    ) -> Result<ModStatus, Error> {
        let (ty, items) =
            crate::elaborate::declare_mod_p1(&module.pre_items, bindings, module.file)?;
        Ok(ModStatus {
            ty,
            items,
            ..module
        })
    }
}
struct DeclareP2;
impl Pass for DeclareP2 {
    fn run(
        &self,
        mods: &[(RawSym, ModType)],
        bindings: &mut Bindings,
        module: ModStatus,
    ) -> Result<ModStatus, Error> {
        let (ty, items) = crate::elaborate::declare_mod_p2(
            &module.pre_items,
            module.ty,
            mods,
            module.items,
            bindings,
            module.file,
        )?;
        Ok(ModStatus {
            ty,
            items,
            ..module
        })
    }
}
struct DeclareP3;
impl Pass for DeclareP3 {
    fn run(
        &self,
        mods: &[(RawSym, ModType)],
        bindings: &mut Bindings,
        module: ModStatus,
    ) -> Result<ModStatus, Error> {
        let (ty, items) = crate::elaborate::declare_mod_p3(
            &module.pre_items,
            module.ty,
            mods,
            module.items,
            bindings,
            module.file,
        )?;
        Ok(ModStatus {
            ty,
            items,
            ..module
        })
    }
}
struct DeclareP4;
impl Pass for DeclareP4 {
    fn run(
        &self,
        mods: &[(RawSym, ModType)],
        bindings: &mut Bindings,
        module: ModStatus,
    ) -> Result<ModStatus, Error> {
        let (ty, items) = crate::elaborate::declare_mod_p4(
            &module.pre_items,
            module.ty,
            mods,
            module.items,
            bindings,
            module.file,
        )?;
        Ok(ModStatus {
            ty,
            items,
            ..module
        })
    }
}
struct ElabPass;
impl Pass for ElabPass {
    fn run(
        &self,
        mods: &[(RawSym, ModType)],
        bindings: &mut Bindings,
        module: ModStatus,
    ) -> Result<ModStatus, Error> {
        let items = crate::elaborate::elab_mod(
            &module.pre_items,
            module.ty.clone(),
            mods,
            module.items,
            bindings,
            module.file,
        )?;
        Ok(ModStatus {
            pre_items: Vec::new(),
            items,
            ..module
        })
    }
}

pub struct Driver {
    mods_pre: Vec<(RawSym, ModType)>,
    pub mods: Vec<ModStatus>,
    pub errors: Vec<(Error, FileId)>,
}
impl Driver {
    pub fn new(parsed: Vec<(FileId, Vec<PreItem>, PathBuf)>) -> Self {
        Driver {
            mods_pre: Vec::new(),
            mods: parsed
                .into_iter()
                .map(|(file, pre_items, input_path)| ModStatus {
                    pre_items,
                    items: Vec::new(),
                    ty: ModType::default(),
                    file,
                    input_path,
                })
                .collect(),
            errors: Vec::new(),
        }
    }
    fn run_pass(&mut self, bindings: &mut Bindings, pass: &dyn Pass) {
        let mut mods2 = Vec::new();
        for i in self.mods.split_off(0) {
            let file = i.file;
            match pass.run(&self.mods_pre, bindings, i) {
                Ok(m) => mods2.push(m),
                Err(e) => self.errors.push((e, file)),
            }
        }
        self.mods = mods2;
        self.mods_pre = self.mods.iter().map(|m| (m.file.1, m.ty.clone())).collect();
    }
    pub fn run_all(&mut self, bindings: &mut Bindings) {
        for pass in passes() {
            self.run_pass(bindings, pass);
        }
    }
    pub fn run_no_elab(&mut self, bindings: &mut Bindings) {
        for pass in passes_no_elab() {
            self.run_pass(bindings, pass);
        }
    }
}

fn passes() -> Vec<&'static dyn Pass> {
    return vec![&DeclareP1, &DeclareP2, &DeclareP3, &DeclareP4, &ElabPass];
}
fn passes_no_elab() -> Vec<&'static dyn Pass> {
    return vec![&DeclareP1, &DeclareP2, &DeclareP3, &DeclareP4];
}
