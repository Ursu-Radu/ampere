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
        #[
            Message: "Invalid index for base",
            Labels: [
                interpreter.make_area(*span) => "Cannot index {} with {}": base.0.name(), index.0.name();
                interpreter.make_area(base.1) => "This is of type {}": base.0.name();
                interpreter.make_area(index.1) => "This is of type {}": index.0.name();
            ]
        ]
        CannotIndex {
            base: (ValueType, CodeSpan),
            index: (ValueType, CodeSpan),
            span: CodeSpan,
        }
        #[
            Message: "Index out of bounds",
            Labels: [
                interpreter.make_area(*span) => "Index {} is out of bounds": index;
            ]
        ]
        IndexOutOfBounds {
            index: i64,
            span: CodeSpan,
        }
        #[
            Message: "Nonexistent key",
            Labels: [
                interpreter.make_area(*span) => r#"Key "{}" does not exist"#: key;
            ]
        ]
        NonexistentKey {
            key: String,
            span: CodeSpan,
        }
        #[
            Message: "Nonexistent member",
            Labels: [
                interpreter.make_area(*span) => r#"Member "{}" does not exist on type {}"#: member, base.0.name();
                interpreter.make_area(base.1) => r#"This is of type {}"#: base.0.name();
            ]
        ]
        NonexistentMember {
            base: (ValueType, CodeSpan),
            member: String,
            span: CodeSpan,
        }
        #[
            Message: "Mismatched type",
            Labels: [
                interpreter.make_area(*span) => r#"Found {}, expected {}"#: found.name(), expected;
            ]
        ]
        MismatchedType {
            found: ValueType,
            expected: String,
            span: CodeSpan,
        }
        #[
            Message: "Cannot convert between types",
            Labels: [
                interpreter.make_area(*span) => r#"Type {} cannot be converted to {}"#: typ.name(), to.name();
            ]
        ]
        CannotConvert {
            typ: ValueType,
            to: ValueType,
            span: CodeSpan,
        }
        #[
            Message: "Conversion error",
            Labels: [
                interpreter.make_area(*span) => r#"Couldn't convert value to {}"#: to.name();
            ]
        ]
        ConversionError {
            to: ValueType,
            span: CodeSpan,
        }
    }
}
