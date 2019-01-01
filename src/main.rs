// Made by following along with https://norasandler.com/2017/11/29/Write-a-Compiler.html
#![allow(unused_imports)]

extern crate peek_nth;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;
use std::path::Path;

mod scanner;
mod ast;
mod generator;

use scanner::lex;
use ast::*;
use generator::generate;

fn main() {
    let argv: Vec<String> = env::args().collect();

    let mut contents = String::new();
    File::open(&argv[1]).unwrap().read_to_string(&mut contents).unwrap();

    // Comment the following line if you don't want the source file to be printed
    println!("{}:\n{}\n", &argv[1], contents);

    let tokens = lex(&contents);
    println!("Scanner production:\n{:?}\n", tokens);

    let ast = parse(&tokens[..]);
    println!("Abstract syntax tree:\n{:#?}\n", ast);

    // Comment out everything below this line to disable code generation
    let generated = generate(&ast);
    println!("Generated assembly:\n{}", generated);

    // let file_name = Path::new(&argv[1]).file_stem().unwrap().to_str().unwrap();

    // let mut output_file = File::create(&format!("{}.s", file_name)).unwrap();
    // output_file.write_all(generated.as_bytes()).unwrap();

    // Command::new("gcc")
    //     .args(&["-m32", &format!("{}.s", file_name), "-o", "out"])
    //     .spawn()
    //     .unwrap()
    //     .wait()
    //     .unwrap();
}
