use std::{fs::File, path::PathBuf};

use ropey::Rope;

use crate::{parser::Parser, pretty::Style, term::*};

pub struct ModStatusBase {
    pub pre_items: Vec<PreItem>,
    pub ty: ModType,
    pub file: FileId,
    pub input_path: PathBuf,
    pub mod_path: RawPath,
}
impl From<&ModStatusBase> for ModStatus {
    fn from(value: &ModStatusBase) -> Self {
        let ModStatusBase {
            pre_items,
            ty,
            file,
            input_path,
            mod_path,
        } = value;
        ModStatus {
            pre_items: pre_items.clone(),
            items: Vec::new(),
            ty: ty.clone(),
            file: *file,
            input_path: input_path.clone(),
            mod_path: mod_path.clone(),
        }
    }
}
pub struct ModStatus {
    pub pre_items: Vec<PreItem>,
    pub items: Vec<Item>,
    pub ty: ModType,
    pub file: FileId,
    pub input_path: PathBuf,
    pub mod_path: RawPath,
}
pub trait Pass {
    fn run(
        &self,
        bindings: &mut Bindings,
        module: ModStatus,
        driver: &mut Driver,
    ) -> (ModStatus, Vec<Error>);
}
struct DeclareP1;
impl Pass for DeclareP1 {
    fn run(
        &self,
        bindings: &mut Bindings,
        module: ModStatus,
        driver: &mut Driver,
    ) -> (ModStatus, Vec<Error>) {
        let (ty, items, errors) = crate::elaborate::declare_mod_p1(
            &module.pre_items,
            bindings,
            module.file,
            module.mod_path.clone(),
            driver,
        );
        (
            ModStatus {
                ty,
                items,
                ..module
            },
            errors,
        )
    }
}
struct DeclareP2;
impl Pass for DeclareP2 {
    fn run(
        &self,
        bindings: &mut Bindings,
        module: ModStatus,
        driver: &mut Driver,
    ) -> (ModStatus, Vec<Error>) {
        let (ty, items, errors) = crate::elaborate::declare_mod_p2(
            &module.pre_items,
            module.ty,
            module.items,
            bindings,
            module.file,
            module.mod_path.clone(),
            driver,
        );
        (
            ModStatus {
                ty,
                items,
                ..module
            },
            errors,
        )
    }
}
struct DeclareP3;
impl Pass for DeclareP3 {
    fn run(
        &self,
        bindings: &mut Bindings,
        module: ModStatus,
        driver: &mut Driver,
    ) -> (ModStatus, Vec<Error>) {
        let (ty, items, errors) = crate::elaborate::declare_mod_p3(
            &module.pre_items,
            module.ty,
            module.items,
            bindings,
            module.file,
            module.mod_path.clone(),
            driver,
        );
        (
            ModStatus {
                ty,
                items,
                ..module
            },
            errors,
        )
    }
}
struct DeclareP4;
impl Pass for DeclareP4 {
    fn run(
        &self,
        bindings: &mut Bindings,
        module: ModStatus,
        driver: &mut Driver,
    ) -> (ModStatus, Vec<Error>) {
        let (ty, items, errors) = crate::elaborate::declare_mod_p4(
            &module.pre_items,
            module.ty,
            module.items,
            bindings,
            module.file,
            module.mod_path.clone(),
            driver,
        );
        (
            ModStatus {
                ty,
                items,
                ..module
            },
            errors,
        )
    }
}
struct ElabPass;
impl Pass for ElabPass {
    fn run(
        &self,
        bindings: &mut Bindings,
        module: ModStatus,
        driver: &mut Driver,
    ) -> (ModStatus, Vec<Error>) {
        let (items, errors) = crate::elaborate::elab_mod(
            &module.pre_items,
            module.ty.clone(),
            module.items,
            bindings,
            module.file,
            module.mod_path.clone(),
            driver,
        );
        (
            ModStatus {
                pre_items: Vec::new(),
                items,
                ..module
            },
            errors,
        )
    }
}

pub struct Driver {
    pub mods_pre: Vec<(RawPath, ModType)>,
    pub mods_base: Vec<ModStatusBase>,
    pub mods: Vec<ModStatus>,
    pub errors: Vec<(Error, FileId)>,
    pub stage: usize,
    pub nfiles: usize,
    pub root: Option<PathBuf>,
}
impl Driver {
    pub fn module(&mut self, r: &RawPath, bindings: &mut Bindings) -> Option<&ModType> {
        if self.mods_base.iter().all(|m| m.mod_path != *r) {
            // Look for modules in the file system
            let mut path = self.root.clone()?;
            for i in &r.0 {
                path.push(bindings.resolve_raw(**i));
            }
            path.push(format!("{}.bt", bindings.resolve_raw(*r.1)));
            if path.exists() {
                eprintln!("Found module on disk: {}", path.display());
                self.add_mod_file(path, bindings);
            }
        }
        self.mods_pre.iter().find(|(k, _)| k == r).map(|(_, v)| v)
    }

    pub fn new(paths: Vec<String>, bindings: &mut Bindings) -> Self {
        let mut files = Vec::new();
        for input in paths {
            let input: PathBuf = input.into();
            if input.is_file() {
                files.push(input.clone());
            } else {
                for i in input.read_dir().unwrap() {
                    let i = i.unwrap();
                    if i.file_name().to_str().unwrap().ends_with(".bt") {
                        files.push(i.path());
                    }
                }
            }
        }

        let mut nfiles = 0;
        let mut parsed = Vec::new();
        let mut errors = Vec::new();
        for input in files {
            let (file_id, v, e) = parse_file(&input, &mut nfiles, bindings);
            parsed.push((file_id, v, input));
            errors.extend(e.into_iter().map(|e| (e, file_id)));
        }

        let root = parsed
            .first()
            .and_then(|(_, _, path)| {
                path.canonicalize()
                    .unwrap()
                    .ancestors()
                    .find(|x| x.file_name().unwrap().to_string_lossy() == "bytec")
                    .map(|x| x.to_owned())
            })
            .or_else(|| {
                parsed
                    .iter()
                    .map(|(_, _, p)| p.canonicalize().unwrap().parent().unwrap().to_owned())
                    .reduce(|mut x, mut y| {
                        while x != y {
                            if x.components().count() < y.components().count() {
                                y = y.parent().unwrap().to_owned();
                            } else {
                                x = x.parent().unwrap().to_owned();
                            }
                        }
                        x
                    })
            });
        let mut driver = Driver {
            mods_pre: Vec::new(),
            stage: 0,
            nfiles,
            mods: Vec::new(),
            mods_base: Vec::new(),
            errors,
            root,
        };
        for (i, (file, pre_items, input_path)) in parsed.into_iter().enumerate() {
            driver.add_pre_mod(input_path, bindings, i, pre_items, file);
        }

        driver
    }
    pub fn add_const_overrides(
        &mut self,
        c: impl IntoIterator<Item = (RawPath, SPre)>,
    ) -> Result<(), RawPath> {
        for (path, value) in c {
            let m = path.stem();
            let n = path.last();
            *self
                .mods_base
                .iter_mut()
                .find(|s| s.mod_path == m)
                .ok_or(path.clone())?
                .pre_items
                .iter_mut()
                .find_map(|i| match i {
                    PreItem::Let(x, true, _, body, _, _) if *x == n => Some(body),
                    _ => None,
                })
                .ok_or(path)? = Some(value);
        }
        Ok(())
    }
    fn run_pass(&mut self, bindings: &mut Bindings, pass: &dyn Pass) {
        for i in self.mods.split_off(0) {
            let file = i.file;
            let (m, e) = pass.run(bindings, i, self);
            self.mods.push(m);
            self.errors.extend(e.into_iter().map(|e| (e, file)));
        }
        self.mods_pre = self
            .mods
            .iter()
            .map(|m| (m.mod_path.clone(), m.ty.clone()))
            .collect();
    }
    pub fn run_all(&mut self, bindings: &mut Bindings) {
        self.stage = 0;
        self.mods = self.mods_base.iter().map(|x| x.into()).collect();
        self.mods_pre = Vec::new();
        for pass in passes() {
            self.stage += 1;
            self.run_pass(bindings, pass);
        }
    }
    pub fn run_no_elab(&mut self, bindings: &mut Bindings) {
        for pass in passes_no_elab() {
            self.run_pass(bindings, pass);
        }
    }

    // May not work if used on an existing file while running `run_all`
    pub fn update_mod_src(&mut self, file_id: FileId, bindings: &mut Bindings, source: &Rope) {
        let mut parser = Parser::new(source.slice(..), bindings);
        let (v, e) = parser.top_level();

        self.errors.extend(e.into_iter().map(|x| (x, file_id)));
        self.mods_base.retain(|m| m.file != file_id);
        self.mods.retain(|m| m.file != file_id);

        self.add_pre_mod(
            crate::term::INPUT_PATH
                .read()
                .unwrap()
                .get(&file_id)
                .unwrap()
                .clone(),
            bindings,
            1,
            v,
            file_id,
        );
        if self.stage != 0 {
            self.mods.push(self.mods_base.last().unwrap().into());
            let passes = passes();
            for i in 0..self.stage {
                let (new, e) = passes[i].run(bindings, self.mods.pop().unwrap(), self);
                self.errors.extend(e.into_iter().map(|x| (x, file_id)));

                self.mods_pre.retain(|(k, _)| *k != new.mod_path);
                self.mods_pre.push((new.mod_path.clone(), new.ty.clone()));

                self.mods.push(new);
            }
        }
    }

    fn add_mod_file(&mut self, path: PathBuf, bindings: &mut Bindings) {
        let file = File::open(&path).unwrap_or_else(|_| {
            Doc::start("error")
                .style(Style::BoldRed)
                .add(": File not found: ")
                .add(&path.as_os_str().to_str().unwrap())
                .style(Style::Bold)
                .emit();
            std::process::exit(1)
        });

        let file_id = crate::term::FileId(self.nfiles);
        self.nfiles += 1;
        let rope = Rope::from_reader(file).unwrap();
        {
            crate::term::INPUT_SOURCE
                .write()
                .unwrap()
                .insert(file_id, rope.clone());
            crate::term::INPUT_PATH
                .write()
                .unwrap()
                .insert(file_id, path.clone());
        }

        self.update_mod_src(file_id, bindings, &rope);
    }

    fn add_pre_mod(
        &mut self,
        input_path: PathBuf,
        bindings: &mut Bindings,
        i: usize,
        pre_items: Vec<PreItem>,
        file: FileId,
    ) {
        if self.root.is_none() {
            self.root = input_path
                .canonicalize()
                .unwrap()
                .ancestors()
                .find(|x| x.file_name().unwrap().to_string_lossy() == "bytec")
                .map(|x| x.to_owned());
        }
        let mod_path = {
            let mut p = input_path.clone().canonicalize().unwrap();
            p.set_file_name(
                p.file_name()
                    .unwrap()
                    .to_string_lossy()
                    .into_owned()
                    .trim_end_matches(".bt"),
            );
            let mut p = &p as &std::path::Path;
            let mut v = Vec::new();
            while &p != self.root.as_ref().unwrap() {
                if p.components().count() < self.root.as_ref().unwrap().components().count() {
                    panic!(
                        "path '{}' not in bytec directory '{}'",
                        p.display(),
                        self.root.as_ref().unwrap().display()
                    )
                }
                v.insert(
                    0,
                    Spanned::hack(bindings.raw(p.file_name().unwrap().to_string_lossy())),
                );
                p = p.parent().unwrap();
            }
            let name = v.pop().unwrap();
            RawPath(v, name)
        };
        eprintln!(
            "Found module {} at {} (root: {})",
            bindings.resolve_path_o(&mod_path),
            input_path.display(),
            self.root.as_ref().unwrap().display()
        );
        if i == 0 {
            bindings.root_mod_path = Some(mod_path.clone());
        }
        self.mods_base.push(ModStatusBase {
            pre_items,
            ty: ModType::default(),
            file,
            input_path,
            mod_path,
        });
    }
}

fn parse_file(
    input: &PathBuf,
    nfiles: &mut usize,
    bindings: &mut Bindings,
) -> (FileId, Vec<PreItem>, Vec<Spanned<Doc>>) {
    let file = File::open(input).unwrap_or_else(|_| {
        Doc::start("error")
            .style(Style::BoldRed)
            .add(": File not found: ")
            .add(&input.as_os_str().to_str().unwrap())
            .style(Style::Bold)
            .emit();
        std::process::exit(1)
    });

    let file_id = crate::term::FileId(*nfiles);
    *nfiles += 1;
    let rope = Rope::from_reader(file).unwrap();
    {
        crate::term::INPUT_SOURCE
            .write()
            .unwrap()
            .insert(file_id, rope.clone());
        crate::term::INPUT_PATH
            .write()
            .unwrap()
            .insert(file_id, input.clone());
    }

    let mut parser = Parser::new(rope.slice(..), bindings);
    let (v, e) = parser.top_level();
    (file_id, v, e)
}

fn passes() -> Vec<&'static dyn Pass> {
    return vec![&DeclareP1, &DeclareP2, &DeclareP3, &DeclareP4, &ElabPass];
}
fn passes_no_elab() -> Vec<&'static dyn Pass> {
    return vec![&DeclareP1, &DeclareP2, &DeclareP3, &DeclareP4];
}
