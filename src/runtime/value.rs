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
    pub fn into_stored(self, def_span: CodeSpan) -> StoredValue {
        StoredValue {
            value: self,
            def_span,
        }
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

#[derive(Debug, Clone)]
pub struct StoredValue {
    pub value: Value,
    pub def_span: CodeSpan,
}
impl StoredValue {
    pub fn clone_redef(&self, span: CodeSpan) -> Self {
        StoredValue {
            value: self.value.clone(),
            def_span: span,
        }
    }
}

pub mod value_ops {
    use crate::{
        parsing::lexer::Token,
        runtime::{error::RuntimeError, interpreter::Interpreter},
        sources::CodeSpan,
    };

    use super::{StoredValue, Value};

    pub fn plus(
        a: &StoredValue,
        b: &StoredValue,
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(*v1 + *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(*v1 + *v2)),
            (Value::String(v1), Value::String(v2)) => Ok(Value::String(v1.clone() + v2)),
            _ => Err(RuntimeError::InvalidOperands {
                left: (a.value.to_type(), interpreter.make_area(a.def_span)),
                right: (b.value.to_type(), interpreter.make_area(b.def_span)),
                op: Token::Plus,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn minus(
        a: &StoredValue,
        b: &StoredValue,
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(*v1 - *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(*v1 - *v2)),
            _ => Err(RuntimeError::InvalidOperands {
                left: (a.value.to_type(), interpreter.make_area(a.def_span)),
                right: (b.value.to_type(), interpreter.make_area(b.def_span)),
                op: Token::Minus,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn mult(
        a: &StoredValue,
        b: &StoredValue,
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(*v1 * *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(*v1 * *v2)),
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
                left: (a.value.to_type(), interpreter.make_area(a.def_span)),
                right: (b.value.to_type(), interpreter.make_area(b.def_span)),
                op: Token::Mult,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn div(
        a: &StoredValue,
        b: &StoredValue,
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(*v1 / *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(*v1 / *v2)),
            _ => Err(RuntimeError::InvalidOperands {
                left: (a.value.to_type(), interpreter.make_area(a.def_span)),
                right: (b.value.to_type(), interpreter.make_area(b.def_span)),
                op: Token::Div,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn modulo(
        a: &StoredValue,
        b: &StoredValue,
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(*v1 % *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(*v1 % *v2)),
            _ => Err(RuntimeError::InvalidOperands {
                left: (a.value.to_type(), interpreter.make_area(a.def_span)),
                right: (b.value.to_type(), interpreter.make_area(b.def_span)),
                op: Token::Mod,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn pow(
        a: &StoredValue,
        b: &StoredValue,
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match (&a.value, &b.value) {
            (Value::Int(v1), Value::Int(v2)) => {
                Ok(Value::Int((*v1 as f64).powf(*v2 as f64).floor() as i64))
            }
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(v1.powf(*v2))),
            _ => Err(RuntimeError::InvalidOperands {
                left: (a.value.to_type(), interpreter.make_area(a.def_span)),
                right: (b.value.to_type(), interpreter.make_area(b.def_span)),
                op: Token::Pow,
                area: interpreter.make_area(span),
            }),
        }
    }

    pub fn unary_minus(
        a: &StoredValue,
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match &a.value {
            Value::Int(v1) => Ok(Value::Int(-*v1)),
            Value::Float(v1) => Ok(Value::Float(-*v1)),
            _ => Err(RuntimeError::InvalidUnaryOperand {
                value: (a.value.to_type(), interpreter.make_area(a.def_span)),
                op: Token::Minus,
                area: interpreter.make_area(span),
            }),
        }
    }
    pub fn unary_not(
        a: &StoredValue,
        span: CodeSpan,
        interpreter: &Interpreter,
    ) -> Result<Value, RuntimeError> {
        match &a.value {
            Value::Bool(v1) => Ok(Value::Bool(!*v1)),
            _ => Err(RuntimeError::InvalidUnaryOperand {
                value: (a.value.to_type(), interpreter.make_area(a.def_span)),
                op: Token::ExclMark,
                area: interpreter.make_area(span),
            }),
        }
    }
}
