use crate::{
    error_maker,
    parsing::lexer::Token,
    sources::{CodeArea, CodeSpan},
};

use super::{
    interpreter::Interpreter,
    value::{Value, ValueType},
};

error_maker! {
    Title: "Runtime Error"
    Extra: {
        interpreter: &Interpreter,
    }
    pub enum RuntimeError {
        #[
            Message: "Invalid operands",
            Labels: [
                interpreter.make_area(*span) => "Cannot apply operator `{}` to {} and {}": op.name(), left.0.name(), right.0.name();
                interpreter.make_area(left.1) => "This is of type {}": left.0.name();
                interpreter.make_area(right.1) => "This is of type {}": right.0.name();
            ]
        ]
        InvalidOperands {
            left: (ValueType, CodeSpan),
            right: (ValueType, CodeSpan),
            op: Token,
            span: CodeSpan,
        }
        #[
            Message: "Invalid unary operand",
            Labels: [
                interpreter.make_area(*span) => "Cannot apply unary operator `{}` to {}": op.name(), v.0.name();
                interpreter.make_area(v.1) => "This is of type {}": v.0.name();
            ]
        ]
        InvalidUnaryOperand {
            v: (ValueType, CodeSpan),
            op: Token,
            span: CodeSpan,
        }
        #[
            Message: "Nonexistent variable",
            Labels: [
                interpreter.make_area(*span) => "Variable {} does not exist": name;
            ]
        ]
        NonexistentVariable {
            name: String,
            span: CodeSpan,
        }
        #[
            Message: "Cannot convert to boolean",
            Labels: [
                interpreter.make_area(*span) => "Cannot convert {} to a boolean": value.name();
            ]
        ]
        BooleanConversion {
            value: ValueType,
            span: CodeSpan,
        }
    }
}
