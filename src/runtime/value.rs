use lasso::Spur;

use crate::sources::CodeSpan;

use super::interpreter::{self, Interpreter, ValueKey};

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Tuple(Vec<ValueKey>),
}
#[derive(Debug, Clone, Copy)]
pub enum ValueType {
    Int,
    Float,
    String,
    Bool,
    Tuple,
}

impl Value {
    pub fn unit() -> Self {
        Value::Tuple(vec![])
    }
    pub fn to_type(&self) -> ValueType {
        match self {
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::String(_) => ValueType::String,
            Value::Bool(_) => ValueType::Bool,
            Value::Tuple(_) => ValueType::Tuple,
        }
    }
}

impl ValueType {
    pub fn name(self) -> String {
        match self {
            ValueType::Int => "int",
            ValueType::Float => "float",
            ValueType::String => "string",
            ValueType::Bool => "bool",
            ValueType::Tuple => "tuple",
        }
        .into()
    }
}

pub mod value_ops {
    use crate::{
        parsing::lexer::Token,
        runtime::{error::RuntimeError, interpreter::Interpreter},
        sources::CodeSpan,
    };

    use super::Value;

    pub fn equality(a: &Value, b: &Value, interpreter: &Interpreter) -> bool {
        match (a, b) {
            (Value::Int(v1), Value::Int(v2)) => v1 == v2,
            (Value::Float(v1), Value::Float(v2)) => v1 == v2,
            (Value::Int(v1), Value::Float(v2)) => *v1 as f64 == *v2,
            (Value::Float(v1), Value::Int(v2)) => *v1 == *v2 as f64,
            (Value::String(v1), Value::String(v2)) => v1 == v2,
            (Value::Bool(v1), Value::Bool(v2)) => v1 == v2,
            (Value::Tuple(v1), Value::Tuple(v2)) => {
                if v1.len() != v2.len() {
                    false
                } else {
                    v1.iter().zip(v2).all(|(e1, e2)| {
                        equality(
                            &interpreter.memory[*e1],
                            &interpreter.memory[*e2],
                            interpreter,
                        )
                    })
                }
            }
            _ => false,
        }
    }

    pub fn to_bool(
        v: &Value,
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<bool, RuntimeError> {
        todo!()
    }

    pub fn eq_op(
        a: (&Value, CodeSpan),
        b: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        Ok(Value::Bool(equality(a.0, b.0, interpreter)))
    }
    pub fn not_eq_op(
        a: (&Value, CodeSpan),
        b: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        Ok(Value::Bool(!equality(a.0, b.0, interpreter)))
    }

    pub fn plus(
        a: (&Value, CodeSpan),
        b: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (a.0, b.0) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(*v1 + *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(*v1 + *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Float(*v1 as f64 + *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Float(*v1 + *v2 as f64)),

            (Value::String(v1), Value::String(v2)) => Ok(Value::String(v1.clone() + v2)),
            _ => Err(RuntimeError::InvalidOperands {
                left: (a.0.to_type(), a.1),
                right: (b.0.to_type(), b.1),
                op: Token::Plus,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn minus(
        a: (&Value, CodeSpan),
        b: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (a.0, b.0) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(*v1 - *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(*v1 - *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Float(*v1 as f64 - *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Float(*v1 - *v2 as f64)),

            _ => Err(RuntimeError::InvalidOperands {
                left: (a.0.to_type(), a.1),
                right: (b.0.to_type(), b.1),
                op: Token::Minus,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn mult(
        a: (&Value, CodeSpan),
        b: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (a.0, b.0) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(*v1 * *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(*v1 * *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Float(*v1 as f64 * *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Float(*v1 * *v2 as f64)),

            (Value::Int(v1), Value::String(v2)) => Ok(Value::String(v2.repeat(if *v1 < 0 {
                0
            } else {
                *v1 as usize
            }))),
            (Value::String(v1), Value::Int(v2)) => Ok(Value::String(v1.repeat(if *v2 < 0 {
                0
            } else {
                *v2 as usize
            }))),
            _ => Err(RuntimeError::InvalidOperands {
                left: (a.0.to_type(), a.1),
                right: (b.0.to_type(), b.1),
                op: Token::Mult,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn div(
        a: (&Value, CodeSpan),
        b: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (a.0, b.0) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(*v1 / *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(*v1 / *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Float(*v1 as f64 / *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Float(*v1 / *v2 as f64)),

            _ => Err(RuntimeError::InvalidOperands {
                left: (a.0.to_type(), a.1),
                right: (b.0.to_type(), b.1),
                op: Token::Div,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn modulo(
        a: (&Value, CodeSpan),
        b: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (a.0, b.0) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(v1.rem_euclid(*v2))),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(v1.rem_euclid(*v2))),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Float((*v1 as f64).rem_euclid(*v2))),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Float(v1.rem_euclid(*v2 as f64))),

            _ => Err(RuntimeError::InvalidOperands {
                left: (a.0.to_type(), a.1),
                right: (b.0.to_type(), b.1),
                op: Token::Mod,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn pow(
        a: (&Value, CodeSpan),
        b: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (a.0, b.0) {
            (Value::Int(v1), Value::Int(v2)) => {
                Ok(Value::Int((*v1 as f64).powf(*v2 as f64).floor() as i64))
            }
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(v1.powf(*v2))),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Float((*v1 as f64).powf(*v2))),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Float(v1.powf(*v2 as f64))),

            _ => Err(RuntimeError::InvalidOperands {
                left: (a.0.to_type(), a.1),
                right: (b.0.to_type(), b.1),
                op: Token::Pow,
                area: interpreter.make_area(span),
            }),
        }
    }

    pub fn greater(
        a: (&Value, CodeSpan),
        b: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (a.0, b.0) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 > *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 > *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 as f64 > *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 > *v2 as f64)),

            _ => Err(RuntimeError::InvalidOperands {
                left: (a.0.to_type(), a.1),
                right: (b.0.to_type(), b.1),
                op: Token::Greater,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn greater_eq(
        a: (&Value, CodeSpan),
        b: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (a.0, b.0) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 >= *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 >= *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 as f64 >= *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 >= *v2 as f64)),

            _ => Err(RuntimeError::InvalidOperands {
                left: (a.0.to_type(), a.1),
                right: (b.0.to_type(), b.1),
                op: Token::Greater,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn lesser(
        a: (&Value, CodeSpan),
        b: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (a.0, b.0) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 < *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 < *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Bool((*v1 as f64) < *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 < *v2 as f64)),

            _ => Err(RuntimeError::InvalidOperands {
                left: (a.0.to_type(), a.1),
                right: (b.0.to_type(), b.1),
                op: Token::Greater,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn lesser_eq(
        a: (&Value, CodeSpan),
        b: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (a.0, b.0) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 <= *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 <= *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 as f64 <= *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 <= *v2 as f64)),

            _ => Err(RuntimeError::InvalidOperands {
                left: (a.0.to_type(), a.1),
                right: (b.0.to_type(), b.1),
                op: Token::Greater,
                area: interpreter.make_area(span),
            }),
        }
    }

    pub fn unary_minus(
        a: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match a.0 {
            Value::Int(v1) => Ok(Value::Int(-*v1)),
            Value::Float(v1) => Ok(Value::Float(-*v1)),
            _ => Err(RuntimeError::InvalidUnaryOperand {
                v: (a.0.to_type(), a.1),
                op: Token::Minus,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn unary_not(
        a: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match a.0 {
            Value::Bool(v1) => Ok(Value::Bool(!*v1)),
            _ => Err(RuntimeError::InvalidUnaryOperand {
                v: (a.0.to_type(), a.1),
                op: Token::ExclMark,
                area: interpreter.make_area(span),
            }),
        }
    }
}
