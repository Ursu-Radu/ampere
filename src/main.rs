#![deny(unused_must_use)]
#![allow(clippy::result_large_err)]

use std::{fs, io::Write, path::PathBuf};

use ahash::AHashMap;
use lasso::Rodeo;

use crate::{
    parsing::parser::Parser,
    runtime::interpreter::{Interpreter, Scope},
    sources::AmpereSource,
};

mod errors;
mod parsing;
mod runtime;
mod sources;

fn main() {
    print!("\x1B[2J\x1B[1;1H");

    std::io::stdout().flush().unwrap();
    let path = PathBuf::from("test.amp");

    let code = fs::read_to_string(&path).unwrap();
    let src = AmpereSource::File(path);
    let interner = Rodeo::new();
    let mut parser = Parser::new(&code, &src, interner);

    match parser.parse() {
        Ok(e) => {
            let mut interpreter = Interpreter::new(&src, parser.interner);
            let global_scope = interpreter.scopes.insert(Scope {
                vars: AHashMap::new(),
                parent: None,
            });
            match interpreter.execute_list(&e, global_scope) {
                Ok(k) => {
                    println!("-> {}", interpreter.value_str(k))
                }
                Err(err) => {
                    let err = err.to_report(&interpreter);
                    println!("{:#?}", err)
                }
            }
            // println!("{:#?}", e)
        }
        Err(err) => {
            let err = err.to_report();
            println!("{:#?}", err)
        }
    }
}
