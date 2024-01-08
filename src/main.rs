mod backend;
mod binding;
mod driver;
mod elaborate;
mod parser;
mod pretty;
mod server;
mod term;

use std::fs::File;
use std::path::PathBuf;

use crate::pretty::{Doc, Style};

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
    let mut consts = Vec::new();
    let mut throws = Vec::new();
    for i in args {
        if i.starts_with("-C") {
            if let Some(idx) = i.find('=') {
                let k = &i[2..idx];
                let v = &i[idx + 1..];
                let mut split: Vec<_> = k
                    .split("::")
                    .map(|x| Spanned::hack(bindings.raw(x)))
                    .collect();
                if split.len() < 2 {
                    Doc::start("error")
                        .style(Style::BoldRed)
                        .add(": Invalid path for overriden constant: '")
                        .add(k)
                        .add("'")
                        .style(Style::Bold)
                        .emit();
                    std::process::exit(1)
                }
                let name = split.pop().unwrap();
                let path = RawPath(split, name);
                consts.push((
                    path,
                    crate::parser::quick_parse_term(v, &mut bindings).unwrap_or_else(|| {
                        Doc::start("error")
                            .style(Style::BoldRed)
                            .add(": Invalid value for overriden constant: '")
                            .add(v)
                            .add("'")
                            .style(Style::Bold)
                            .emit();
                        std::process::exit(1)
                    }),
                ));
            } else {
                Doc::start("error")
                    .style(Style::BoldRed)
                    .add(": Constant override must have a value (e.g. `-CMain::MAX_VALUE=4`)")
                    .style(Style::Bold)
                    .emit();
                std::process::exit(1)
            }
        } else if i.starts_with("-T") {
            let rest = &i[2..];
            throws.push(bindings.raw(rest));
        } else if i.starts_with('-') {
            Doc::start("error")
                .style(Style::BoldRed)
                .add(": Unrecognized option: ")
                .add(i)
                .style(Style::Bold)
                .emit();
            std::process::exit(1)
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
    let package = if output.ends_with(".java") {
        output.parent().unwrap().file_name().unwrap()
    } else {
        output.file_name().unwrap()
    }
    .to_str()
    .unwrap();

    let mut had_err = false;

    let mut driver = Driver::new(paths, &mut bindings);
    driver.add_const_overrides(consts).unwrap_or_else(|path| {
        Doc::start("error")
            .style(Style::BoldRed)
            .add(": Constant to override not found: ")
            .add(&bindings.resolve_path_o(&path))
            .style(Style::Bold)
            .emit();
        had_err = true;
    });
    driver.run_all(&mut bindings);
    let mut elabed = Vec::new();
    let nmods = driver.mods.len();
    for module in driver.mods {
        if output.ends_with(".java") {
            output.parent().map(|p| std::fs::create_dir_all(p).unwrap());
        } else if !output.exists() {
            std::fs::create_dir_all(&output).unwrap();
        }

        let out_path = if !output.is_dir() {
            if nmods == 1 {
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
                bindings.resolve_path_jm(&module.mod_path)
            ))
        };

        eprintln!(
            "Compiling {} to {}",
            module.input_path.to_str().unwrap(),
            out_path.to_str().unwrap()
        );

        elabed.push((module.items, module.mod_path, out_path, module.file))
    }
    if !driver.errors.is_empty() || had_err {
        for (e, file) in driver.errors {
            e.emit(Severity::Error, file);
        }
        std::process::exit(1)
    }
    let mut ir_mods = Vec::new();
    let mut cxt = backend::Cxt::new(&mut bindings, package);
    for (v, _, _, _) in &elabed {
        crate::backend::declare_p1(v, &mut cxt);
    }
    for (v, mod_path, out_path, file) in elabed {
        ir_mods.push((
            crate::backend::declare_p2(
                v,
                &mut cxt,
                mod_path,
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

        let java = m
            .codegen(&mut cxt, &ir_mods, throws.clone())
            .unwrap_or_else(|e| {
                e.emit(Severity::Error, *file);
                std::process::exit(1);
            });
        let mut out_file = File::create(out_path).unwrap();
        write!(out_file, "{}", java).unwrap();
    }
}
