#![deny(unused_must_use)]
#![allow(clippy::result_large_err)]

use std::{fs, io::Write, path::PathBuf};

use ahash::AHashMap;
use lasso::Rodeo;

use crate::{
    parsing::parser::Parser,
    runtime::{
        builtins::Builtin,
        error::RuntimeError,
        interpreter::{Halt, Interpreter, Jump, Scope},
        value::ValueType,
    },
    sources::{AmpereSource, SourceMap},
};

mod errors;
mod parsing;
mod runtime;
mod sources;

fn main() {
    print!("\x1B[2J\x1B[1;1H");
    std::io::stdout().flush().unwrap();

    let path = PathBuf::from("test.amp");

    let mut source_map = SourceMap::new();

    let src = source_map.insert(AmpereSource::File(path));
    let code = source_map[src].src.read().unwrap();

    let mut interner = Rodeo::new();
    let mut parser = Parser::new(&code, src, &mut interner);

    match parser.parse() {
        Ok(e) => {
            let mut interpreter = Interpreter::new(&mut interner, source_map);
            let global_scope = interpreter.scopes.insert(Scope {
                vars: AHashMap::new(),
                parent: None,
            });
            ValueType::populate_scope(&mut interpreter, global_scope);
            Builtin::populate_scope(&mut interpreter, global_scope);

            interpreter.src_stack.push(src);

            match interpreter.execute_list(&e, global_scope) {
                Ok(_) => {
                    interpreter.src_stack.pop();
                }
                Err(err) => {
                    interpreter.src_stack.pop();
                    let err = match err {
                        Halt::Error(err) => err,
                        Halt::Jump(Jump::Return(_, area)) => RuntimeError::ReturnOutside { area },
                        Halt::Jump(Jump::Break(_, area)) => RuntimeError::BreakOutside { area },
                        Halt::Jump(Jump::Continue(area)) => RuntimeError::ContinueOutside { area },
                    };
                    err.to_report(&interpreter).display(interpreter.source_map)
                }
            }
            // println!("{:#?}", e)
        }
        Err(err) => err.to_report().display(source_map),
    }
}
