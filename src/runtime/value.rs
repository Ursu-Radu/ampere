use ahash::AHashMap;
use lasso::Spur;

use crate::sources::CodeSpan;

use super::interpreter::{self, Interpreter, ScopeKey, ValueKey};

macro_rules! values {
    (
        $(
            $name:ident $data:tt: $str:literal,
        )*
    ) => {
        #[derive(Debug, Clone, PartialEq)]
        pub enum Value {
            $(
                $name $data,
            )*
        }
        #[derive(Debug, Clone, PartialEq, Copy)]
        pub enum ValueType {
            $(
                $name,
            )*
        }

        impl Value {
            pub fn to_type(&self) -> ValueType {
                match self {
                    $(
                        Value::$name {..} => ValueType::$name,
                    )*
                }
            }
        }

        impl ValueType {
            pub fn name(self) -> String {
                match self {
                    $(
                        ValueType::$name => $str,
                    )*
                }
                .into()
            }
            pub fn populate_scope(interpreter: &mut Interpreter, scope: ScopeKey) {
                let scope = &mut interpreter.scopes[scope];

                $(
                    scope.vars.insert(
                        interpreter.interner.get_or_intern($str),
                        interpreter.memory.insert(Value::Type(ValueType::$name)),
                    );
                )*
            }
        }
    };
}

values! {
    Int(i64): "int",
    Float(f64): "float",
    String(String): "string",
    Bool(bool): "bool",
    Tuple(Vec<ValueKey>): "tuple",
    Array(Vec<ValueKey>): "array",
    Dict(AHashMap<String, ValueKey>): "dict",
    Type(ValueType): "type",
}

impl Value {
    pub fn unit() -> Self {
        Value::Tuple(vec![])
    }
}

pub mod value_ops {
    use crate::{
        parsing::lexer::Token,
        runtime::{
            error::RuntimeError,
            interpreter::{Interpreter, ValueKey},
        },
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
        match v {
            Value::Bool(b) => Ok(*b),
            _ => Err(RuntimeError::BooleanConversion {
                value: v.to_type(),
                span,
            }),
        }
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
                span,
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
                span,
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
                span,
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
                span,
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
                span,
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
                span,
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
                span,
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
                span,
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
                span,
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
                span,
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
                span,
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
                span,
            }),
        }
    }

    pub fn index(
        base: (&Value, CodeSpan),
        index: (&Value, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<ValueKey, RuntimeError> {
        match (base.0, index.0) {
            (Value::Array(arr), Value::Int(n)) => {
                let idx = if *n < 0 { arr.len() as i64 + *n } else { *n } as usize;
                match arr.get(idx) {
                    Some(k) => Ok(*k),
                    None => Err(RuntimeError::IndexOutOfBounds {
                        index: *n,
                        span: index.1,
                    }),
                }
            }
            (Value::String(s), Value::Int(n)) => {
                let idx = if *n < 0 { s.len() as i64 + *n } else { *n } as usize;
                match s.chars().nth(idx) {
                    Some(k) => Ok(interpreter.memory.insert(Value::String(k.to_string()))),
                    None => Err(RuntimeError::IndexOutOfBounds {
                        index: *n,
                        span: index.1,
                    }),
                }
            }
            (Value::Dict(map), Value::String(k)) => match map.get(k) {
                Some(k) => Ok(*k),
                None => Err(RuntimeError::NonexistentKey {
                    key: k.clone(),
                    span: index.1,
                }),
            },
            _ => Err(RuntimeError::CannotIndex {
                base: (base.0.to_type(), base.1),
                index: (index.0.to_type(), index.1),
                span,
            }),
        }
    }
}
