#![deny(unused_must_use)]
#![allow(clippy::result_large_err)]

use std::{fs, io::Write, path::PathBuf};

use crate::{errors::ToReport, parsing::parser::Parser, sources::AmpereSource};

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
    let mut parser = Parser::new(&code, &src);

    match parser.parse_stmts() {
        Ok(e) => {
            println!("{:#?}", e)
        }
        Err(err) => {
            let err = err.to_report();
            println!("{:#?}", err)
        }
    }
}
