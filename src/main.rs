mod backend;
mod binding;
mod elaborate;
mod parser;
mod pretty;
mod term;
use std::path::Path;
use std::{fs::File, io::Read};

use crate::parser::Parser;
use crate::pretty::{Doc, Style};

fn main() {
    let mut args = std::env::args();
    let _exe = args.next();
    let input = args.next().unwrap_or_else(|| {
        Doc::start("error")
            .style(Style::BoldRed)
            .add(": No input file given")
            .style(Style::Bold)
            .emit();
        std::process::exit(1)
    });
    let output = args.next().unwrap_or_else(|| {
        Doc::start("error")
            .style(Style::BoldRed)
            .add(": No output file given")
            .style(Style::Bold)
            .emit();
        std::process::exit(1)
    });

    let mut file = File::open(&input).unwrap_or_else(|_| {
        Doc::start("error")
            .style(Style::BoldRed)
            .add(": File not found: ")
            .add(&input)
            .style(Style::Bold)
            .emit();
        std::process::exit(1)
    });

    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();
    {
        *term::INPUT_SOURCE.write().unwrap() = buf.clone();
        *term::INPUT_PATH.write().unwrap() = input.into();
    }

    let mut parser = Parser::new(&buf);
    let v = parser.top_level().and_then(|v| {
        let mut bindings = parser.finish();
        let v = crate::elaborate::elab_mod(&v, &mut bindings)?;
        Ok((v, bindings))
    });
    match v {
        Ok((v, mut bindings)) => {
            use std::io::Write;

            let out_path: &std::path::Path = output.as_ref();
            out_path
                .parent()
                .map(|p| std::fs::create_dir_all(p).unwrap());

            // for i in &v {
            //     i.pretty(&bindings).line().emit();
            // }

            let java = crate::backend::codegen(
                &v,
                &mut bindings,
                out_path.file_stem().unwrap().to_str().unwrap(),
            );
            let mut out_file = File::create(out_path).unwrap();
            write!(out_file, "{}", java).unwrap();
        }
        Err(x) => x.emit(term::Severity::Error),
    }
}
