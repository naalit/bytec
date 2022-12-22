mod backend;
mod binding;
mod driver;
mod elaborate;
mod parser;
mod pretty;
mod server;
mod term;
use std::collections::HashMap;
use std::path::PathBuf;
use std::{fs::File, io::Read};

use crate::parser::Parser;
use crate::pretty::{Doc, Style};

use ropey::Rope;
use term::*;

use driver::Driver;

fn main() {
    if std::env::args().nth(1).as_deref() == Some("server") {
        let mut server = server::Server::start();
        server.main_loop();
        server.shutdown();
        return;
    }

    let mut args = std::env::args();
    let _exe = args.next();
    let mut paths = Vec::new();
    let mut bindings = crate::binding::Bindings::default();
    let mut defs = HashMap::new();
    for i in args {
        if i.starts_with("-D") {
            if let Some(idx) = i.find('=') {
                let k = &i[2..idx];
                let v = &i[idx + 1..];
                let v = crate::parser::lex_one(Rope::from(v).slice(..), &mut bindings)
                    .unwrap_or_else(|| {
                        Doc::start("error")
                            .style(Style::BoldRed)
                            .add(": Invalid token is definition argument: '")
                            .add(v)
                            .add("'")
                            .style(Style::Bold)
                            .emit();
                        std::process::exit(1)
                    });
                defs.insert(bindings.raw(k), Some(v));
            } else {
                defs.insert(bindings.raw(&i[2..]), None);
            }
        } else {
            paths.push(i);
        }
    }

    let output: PathBuf = paths
        .pop()
        .unwrap_or_else(|| {
            Doc::start("error")
                .style(Style::BoldRed)
                .add(": No output directory given")
                .style(Style::Bold)
                .emit();
            std::process::exit(1)
        })
        .into();

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
    }
    .to_str()
    .unwrap();

    let mut nfiles = 0;
    let mut had_err = false;
    let mut parsed = Vec::new();
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
        let rope = Rope::from_reader(file).unwrap();
        {
            term::INPUT_SOURCE
                .write()
                .unwrap()
                .insert(file_id, rope.clone());
            term::INPUT_PATH
                .write()
                .unwrap()
                .insert(file_id, input.clone());
        }

        let mut parser = Parser::new(rope.slice(..), &mut bindings, &mut defs);
        let v = parser.top_level();
        match v {
            Ok(v) => {
                parsed.push((file_id, v, input));
            }
            Err(x) => {
                had_err = true;
                x.emit(term::Severity::Error, file_id)
            }
        }
    }
    if had_err {
        std::process::exit(1)
    }

    let mut driver = Driver::new(parsed);
    driver.run_all(&mut bindings);
    let mut elabed = Vec::new();
    for module in driver.mods {
        if output.ends_with(".java") {
            output.parent().map(|p| std::fs::create_dir_all(p).unwrap());
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
                module.input_path.file_stem().unwrap().to_str().unwrap()
            ))
        };

        eprintln!(
            "Compiling {} to {}",
            module.input_path.to_str().unwrap(),
            out_path.to_str().unwrap()
        );

        elabed.push((module.items, out_path, module.file))
    }
    if !driver.errors.is_empty() {
        for (e, file) in driver.errors {
            e.emit(Severity::Error, file);
        }
        std::process::exit(1)
    }
    let mut ir_mods = Vec::new();
    let mut cxt = backend::Cxt::new(&mut bindings, package);
    for (v, _, _) in &elabed {
        crate::backend::declare_p1(v, &mut cxt);
    }
    for (v, out_path, file) in elabed {
        ir_mods.push((
            crate::backend::declare_p2(
                v,
                &mut cxt,
                out_path.file_stem().unwrap().to_str().unwrap(),
            )
            .unwrap_or_else(|e| {
                e.emit(Severity::Error, file);
                std::process::exit(1);
            }),
            (out_path, file),
        ));
    }
    for (m, (out_path, file)) in &ir_mods {
        use std::io::Write;

        let java = m.codegen(&mut cxt, &ir_mods).unwrap_or_else(|e| {
            e.emit(Severity::Error, *file);
            std::process::exit(1);
        });
        let mut out_file = File::create(out_path).unwrap();
        write!(out_file, "{}", java).unwrap();
    }
}
