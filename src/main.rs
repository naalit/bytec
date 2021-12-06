mod backend;
mod parser;
mod pretty;
mod term;
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
            .add(input)
            .style(Style::Bold)
            .emit();
        std::process::exit(1)
    });

    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();

    let mut parser = Parser::new(&buf);
    let v = parser.top_level();
    match v {
        Ok(v) => {
            use std::io::Write;

            let out_path: &std::path::Path = output.as_ref();
            out_path
                .parent()
                .map(|p| std::fs::create_dir_all(p).unwrap());
            let java = backend::codegen(&v, out_path.file_stem().unwrap().to_str().unwrap());
            let mut out_file = File::create(out_path).unwrap();
            write!(out_file, "{}", java).unwrap();
        }
        Err(x) => {
            // An extremely simple copy of Rust's error message design
            // TODO show the whole span, show secondary message
            let (mut line, mut col) = (0, x.span.0);
            let mut line_str = None;
            for l in buf.lines() {
                if col <= l.len() {
                    line_str = Some(l);
                    break;
                } else {
                    line += 1;
                    col -= l.len() + 1;
                }
            }
            Doc::start("error")
                .style(Style::BoldRed)
                .add(": Syntax error: ")
                .add(&*x)
                .style(Style::Bold)
                .hardline()
                .chain(
                    Doc::start("    --> ")
                        .style(Style::Special)
                        .add(line)
                        .chain(Doc::start(":"))
                        .add(col),
                )
                .hardline()
                .chain(Doc::start("     |").style(Style::Special))
                .hardline()
                .chain(
                    Doc::start(format!("{:4} |", line))
                        .style(Style::Special)
                        .space()
                        .add(line_str.unwrap_or("")),
                )
                .hardline()
                .chain(Doc::start("     |").style(Style::Special).space().chain(
                    Doc::start(format!("{:>width$}", "^", width = col + 1)).style(Style::BoldRed),
                ))
                .emit();
        }
    }
}
