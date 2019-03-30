//! This "main.rs" file is extremely experimental and is currently making OX just a one-off compiler,
//! when that is really not the case. OX will become a library in the future. This file will be trashed
//! someday -- it is just for testing the compiler.

// Made by following along with https://norasandler.com/2017/11/29/Write-a-Compiler.html
#![allow(unused_imports)]

extern crate peek_nth;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;
use std::path::Path;
use std::time::Instant;

mod scanner;
mod ast;
mod generator;

use scanner::*;
use ast::*;
use generator::generate;

fn help() {
    println!("Usage: oxc{} <FILE> [OPTIONS]\n", if cfg!(target_os = "windows") { ".exe" } else { "" });
    println!("OPTIONS:\n\tlex\tOnly lexically analyze the file.");
    println!("\tparse\tOnly lex and generate an abstract syntax tree for the file.");
}

fn main() {
    let argv: Vec<String> = env::args().collect();

    let mut to_lex = false;
    let mut to_parse = false;

    for arg in &argv {
        match &arg[..] {
            "help" => {
                help();
                return;
            }
            "lex" => to_lex = true,
            "parse" => to_parse = true,
            _ => {},
        }
    }

    let mut contents = String::new();
    match argv.get(1) {
        Some(file) => { File::open(&file).unwrap().read_to_string(&mut contents).unwrap(); },
        None => {
            println!("Error: <FILE> not given. See help:\n");
            help();
            return;
        }
    }

    // Comment the following line if you don't want the source file to be printed
    println!("{}:\n{}\n", &argv[1], contents);

    let started_processing = Instant::now();

    let tokens = Lexer::new(&contents).collect::<Vec<TokenData>>();
    let lexing_time = {
        let duration = started_processing.elapsed();
        duration.as_secs() as f64 + duration.subsec_nanos() as f64 * 1e-9
    };
    if to_lex {
        println!("Lexically analyzed in {}s:\n{:#?}\n", lexing_time, tokens);
    } else if to_parse {
        println!("Lexically analyzed in {}s: option 'lex' to print tokens\n", lexing_time);
        let ast = parse(&tokens[..]);
        let parsing_time = {
            let duration = started_processing.elapsed();
            duration.as_secs() as f64 + duration.subsec_nanos() as f64 * 1e-9
        } - lexing_time;
        println!("Parsed in {}s:\n{:#?}\n", parsing_time, ast);
    } else {
        println!("Lexically analyzed in {}s: option 'lex' to print tokens\n", lexing_time);
        let ast = parse(&tokens[..]);
        let parsing_time = {
            let duration = started_processing.elapsed();
            duration.as_secs() as f64 + duration.subsec_nanos() as f64 * 1e-9
        } - lexing_time;
        println!("Parsed in {}s:\n{:#?}\n", parsing_time, ast);

        // Comment out everything below this line to disable code generation
        let generated = generate(&ast);
        let generation_time = {
            let duration = started_processing.elapsed();
            duration.as_secs() as f64 + duration.subsec_nanos() as f64 * 1e-9
        } - (lexing_time + parsing_time);
        println!("Generated assembly in {}s:\n{}", generation_time, generated);

        let file_name = Path::new(&argv[1]).file_stem().unwrap().to_str().unwrap();

        let mut output_file = File::create(&format!("{}.s", file_name)).unwrap();
        output_file.write_all(generated.as_bytes()).unwrap();

        let output_name = if cfg!(target_os = "windows") {
            format!("{}.exe", file_name)
        } else {
            file_name.to_owned()
        };

        println!("Assembling...");
        Command::new("gcc")
            .args(&["-m32", "-Wall", &format!("{}.s", file_name), "-o", &output_name])
            .spawn()
            .unwrap()
            .wait()
            .unwrap();
        let assembly_time = {
            let duration = started_processing.elapsed();
            duration.as_secs() as f64 + duration.subsec_nanos() as f64 * 1e-9
        } - (lexing_time + parsing_time + generation_time);
        println!("Assembled in {}s", assembly_time);
    }
}
