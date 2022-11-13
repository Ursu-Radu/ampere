use crate::{error_maker, parsing::lexer::Token, sources::CodeArea};

use super::value::ValueType;

error_maker! {
    Title: "Runtime Error";
    pub enum RuntimeError {
        #[
            Message: "Invalid operands",
            Labels: [
                area => "Cannot apply operator `{}` to {} and {}": op.name(), left.0.name(), right.0.name();
                left.1 => "This is of type {}": left.0.name();
                right.1 => "This is of type {}": right.0.name();
            ]
        ]
        InvalidOperands {
            left: (ValueType, CodeArea),
            right: (ValueType, CodeArea),
            op: Token,
            area: CodeArea,
        }
        #[
            Message: "Invalid unary operand",
            Labels: [
                area => "Cannot apply unary operator `{}` to {}": op.name(), value.0.name();
                value.1 => "This is of type {}": value.0.name();
            ]
        ]
        InvalidUnaryOperand {
            value: (ValueType, CodeArea),
            op: Token,
            area: CodeArea,
        }
        #[
            Message: "Nonexistent variable",
            Labels: [
                area => "Variable {} does not exist": name;
            ]
        ]
        NonexistentVariable {
            name: String,
            area: CodeArea,
        }
    }
}
