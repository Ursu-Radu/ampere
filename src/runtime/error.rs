use crate::{
    error_maker,
    parsing::lexer::Token,
    sources::{CodeArea, CodeSpan},
};

use super::{
    interpreter::{Halt, Interpreter},
    value::{Pattern, Value, ValueType},
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
                area => "Cannot apply unary operator `{}` to {}": op.name(), v.0.name();
                v.1 => "This is of type {}": v.0.name();
            ]
        ]
        InvalidUnaryOperand {
            v: (ValueType, CodeArea),
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
        #[
            Message: "Cannot convert to boolean",
            Labels: [
                area => "Cannot convert {} to a boolean": value.name();
            ]
        ]
        BooleanConversion {
            value: ValueType,
            area: CodeArea,
        }
        #[
            Message: "Invalid index for base",
            Labels: [
                area => "Cannot index {} with {}": base.0.name(), index.0.name();
                base.1 => "This is of type {}": base.0.name();
                index.1 => "This is of type {}": index.0.name();
            ]
        ]
        CannotIndex {
            base: (ValueType, CodeArea),
            index: (ValueType, CodeArea),
            area: CodeArea,
        }
        #[
            Message: "Index out of bounds",
            Labels: [
                area => "Index {} is out of bounds": index;
            ]
        ]
        IndexOutOfBounds {
            index: i64,
            area: CodeArea,
        }
        #[
            Message: "Nonexistent key",
            Labels: [
                area => r#"Key "{}" does not exist"#: key;
            ]
        ]
        NonexistentKey {
            key: String,
            area: CodeArea,
        }
        #[
            Message: "Nonexistent member",
            Labels: [
                area => r#"Member "{}" does not exist on type {}"#: member, base.0.name();
                base.1 => r#"This is of type {}"#: base.0.name();
            ]
        ]
        NonexistentMember {
            base: (ValueType, CodeArea),
            member: String,
            area: CodeArea,
        }
        #[
            Message: "Mismatched type",
            Labels: [
                area => r#"Found {}, expected {}"#: found.name(), expected;
            ]
        ]
        MismatchedType {
            found: ValueType,
            expected: String,
            area: CodeArea,
        }
        #[
            Message: "Cannot convert between types",
            Labels: [
                area => r#"Type {} cannot be converted to {}"#: typ.name(), to.name();
            ]
        ]
        CannotConvert {
            typ: ValueType,
            to: ValueType,
            area: CodeArea,
        }
        #[
            Message: "Conversion error",
            Labels: [
                area => r#"Couldn't convert value to {}"#: to.name();
            ]
        ]
        ConversionError {
            to: ValueType,
            area: CodeArea,
        }
        #[
            Message: "Incorrect number of arguments",
            Labels: [
                area => r#"Expected {} arguments, found {}"#: expected, found;
            ]
        ]
        ArgumentAmount {
            expected: usize,
            found: usize,
            area: CodeArea,
        }
        #[
            Message: "Argument pattern mismatch",
            Labels: [
                area => r#"Argument "{}" defined as {}, found {}"#: name, expected.to_str(), found.name();
            ]
        ]
        ArgPatternMismatch {
            name: String,
            expected: Pattern,
            found: ValueType,
            area: CodeArea,
        }
        #[
            Message: "Return pattern mismatch",
            Labels: [
                area => r#"Return type defined as {}, found {}"#: expected.to_str(), found.name();
            ]
        ]
        RetPatternMismatch {
            expected: Pattern,
            found: ValueType,
            area: CodeArea,
        }
        #[
            Message: "Return used outside of function",
            Labels: [
                area => r#"This return was used outside of a function"#;
            ]
        ]
        ReturnOutside {
            area: CodeArea,
        }
        #[
            Message: "Break used outside of loop",
            Labels: [
                area => r#"This break was used outside of a loop"#;
            ]
        ]
        BreakOutside {
            area: CodeArea,
        }
        #[
            Message: "Continue used outside of loop",
            Labels: [
                area => r#"This continue was used outside of a loop"#;
            ]
        ]
        ContinueOutside {
            area: CodeArea,
        }
        #[
            Message: "Cannot iterate",
            Labels: [
                area => "Cannot iterate over a {}": value.name();
            ]
        ]
        CannotIterate {
            value: ValueType,
            area: CodeArea,
        }
        #[
            Message: "Range already has a step size different from 1",
            Labels: [
                area => "This range already has a step size different from 1";
            ]
        ]
        RangeStepSize {
            area: CodeArea,
        }
        #[
            Message: "Range cannot have a step smaller or equal to 0",
            Labels: [
                area => "This range has a step smaller or equal to 0";
            ]
        ]
        RangeNegativeStep {
            area: CodeArea,
        }
    }
}

impl RuntimeError {
    pub fn into_halt(self) -> Halt {
        Halt::Error(self)
    }
}

impl From<RuntimeError> for Halt {
    fn from(e: RuntimeError) -> Self {
        e.into_halt()
    }
}
// impl<T> From<Result<T, RuntimeError>> for Result<T, Halt> {
//     fn from(e: Result<T, RuntimeError>) -> Self {
//         e.map_err(|e| e.into_halt())
//     }
// }

// pub trait HaltResult<T> {
//     fn err_halt(self) -> Result<T, Halt>;
// }

// impl<T> HaltResult<T> for Result<T, RuntimeError> {
//     fn err_halt(self) -> Result<T, Halt> {
//         self.map_err(|e| e.into_halt())
//     }
// }
