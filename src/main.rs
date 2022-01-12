mod backend;
mod binding;
mod elaborate;
mod parser;
mod pretty;
mod term;
use std::path::PathBuf;
use std::{fs::File, io::Read};

use crate::parser::Parser;
use crate::pretty::{Doc, Style};

fn main() {
    let mut args = std::env::args();
    let _exe = args.next();
    let mut paths: Vec<_> = args.collect();

    let output: PathBuf = paths.pop().unwrap_or_else(|| {
        Doc::start("error")
            .style(Style::BoldRed)
            .add(": No output directory given")
            .style(Style::Bold)
            .emit();
        std::process::exit(1)
    }).into();

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
    let package = if output.ends_with(".java") {
            output.parent().unwrap().file_name().unwrap()
        } else {
            output.file_name().unwrap()
        }.to_str().unwrap();

    let mut nfiles = 0;
    let mut mods = Vec::new();
    let mut p1 = Vec::new();
    let mut bindings = crate::binding::Bindings::default();
    for input in files {
        let mut file = File::open(&input).unwrap_or_else(|_| {
            Doc::start("error")
                .style(Style::BoldRed)
                .add(": File not found: ")
                .add(&input.as_os_str().to_str().unwrap())
                .style(Style::Bold)
                .emit();
            std::process::exit(1)
        });

        let mod_name = bindings.raw(input.file_stem().unwrap().to_str().unwrap());

        let file_id = term::FileId(nfiles, mod_name);
        nfiles += 1;
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        {
            term::INPUT_SOURCE
                .write()
                .unwrap()
                .insert(file_id, buf.clone());
            term::INPUT_PATH
                .write()
                .unwrap()
                .insert(file_id, input.clone());
        }

        let mut parser = Parser::new(&buf, bindings);
        let v = parser.top_level();
        bindings = parser.finish();
        let v = v.and_then(|v| {
            let (t, i) = crate::elaborate::declare_mod_p1(&v, &mut bindings, file_id)?;
            Ok((t, i, v))
        });
        match v {
            Ok((t, items, v)) => {
                mods.push((file_id.1, t.clone()));
                p1.push((v, t, items, file_id, input));
            }
            Err(x) => x.emit(term::Severity::Error, file_id),
        }
    }
    let mut p2 = Vec::new();
    let mut mods2 = Vec::new();
    for (v, t, items, file_id, input_path) in p1 {
        let (t, items) =
            match crate::elaborate::declare_mod_p2(&v, t, &mods, items, &mut bindings, file_id) {
                Ok(x) => x,
                Err(x) => {
                    x.emit(term::Severity::Error, file_id);
                    continue;
                }
            };
        mods2.push((file_id.1, t.clone()));
        p2.push((v, t, items, file_id, input_path));
    }
    let mut p3 = Vec::new();
    let mut mods3 = Vec::new();
    for (v, t, items, file_id, input_path) in p2 {
        let (t, items) =
            match crate::elaborate::declare_mod_p3(&v, t, &mods2, items, &mut bindings, file_id) {
                Ok(x) => x,
                Err(x) => {
                    x.emit(term::Severity::Error, file_id);
                    continue;
                }
            };
        mods3.push((file_id.1, t.clone()));
        p3.push((v, t, items, file_id, input_path));
    }
    let mut p4 = Vec::new();
    let mut mods4 = Vec::new();
    for (v, t, items, file_id, input_path) in p3 {
        let (t, items) =
            match crate::elaborate::declare_mod_p4(&v, t, &mods3, items, &mut bindings, file_id) {
                Ok(x) => x,
                Err(x) => {
                    x.emit(term::Severity::Error, file_id);
                    continue;
                }
            };
        mods4.push((file_id.1, t.clone()));
        p4.push((v, t, items, file_id, input_path));
    }
    let mut elabed = Vec::new();
    for (v, t, items, file_id, input_path) in p4 {
        let v = match crate::elaborate::elab_mod(&v, t, &mods4, items, &mut bindings, file_id) {
            Ok(v) => v,
            Err(x) => {
                x.emit(term::Severity::Error, file_id);
                continue;
            }
        };

        if output.ends_with(".java") {
            output
                .parent()
                .map(|p| std::fs::create_dir_all(p).unwrap());
        } else if !output.exists() {
            std::fs::create_dir_all(&output).unwrap();
        }

        let out_path = if !output.is_dir() {
            if nfiles == 1 {
                output.clone()
            } else {
                Doc::start("error")
                    .style(Style::BoldRed)
                    .add(": Output path is not a directory: ")
                    .add(&output.as_os_str().to_str().unwrap())
                    .style(Style::Bold)
                    .emit();
                std::process::exit(1)
            }
        } else {
            output.join(format!(
                "{}.java",
                input_path.file_stem().unwrap().to_str().unwrap()
            ))
        };

        eprintln!(
            "Compiling {} to {}",
            input_path.to_str().unwrap(),
            out_path.to_str().unwrap()
        );

        elabed.push((v, out_path))
    }
    let mut ir_mods = Vec::new();
    let mut cxt = backend::Cxt::new(&mut bindings, package);
    for (v, _) in &elabed {
        crate::backend::declare_p1(v, &mut cxt);
    }
    for (v, out_path) in elabed {
        ir_mods.push((
            crate::backend::declare_p2(
                v,
                &mut cxt,
                out_path.file_stem().unwrap().to_str().unwrap(),
            ),
            out_path,
        ));
    }
    for (m, out_path) in &ir_mods {
        use std::io::Write;

        let java = m.codegen(&mut cxt, &ir_mods);
        let mut out_file = File::create(out_path).unwrap();
        write!(out_file, "{}", java).unwrap();
    }
}
