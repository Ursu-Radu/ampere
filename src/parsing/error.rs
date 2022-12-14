use crate::{error_maker, sources::CodeArea};

use super::lexer::Token;

error_maker! {
    Title: "Syntax Error"
    Extra: {}
    pub enum SyntaxError {
        #[
            Message: "Unexpected token",
            Labels: [
                area => "Expected {}, found {}": expected, found.name();
            ]
        ]
        UnexpectedToken {
            found: Token,
            expected: String,
            area: CodeArea,
        }
        #[
            Message: "No matching bracket found",
            Labels: [
                area => "No matching `{}` found for this `{}`": close.name(), open.name();
            ]
        ]
        NoMatching {
            close: Token,
            open: Token,
            area: CodeArea,
        }
    }
}
