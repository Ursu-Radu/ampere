use ahash::AHashMap;
use lasso::Spur;

use crate::{parsing::ast::ExprNode, sources::CodeSpan};

use super::{
    builtins::Builtin,
    interpreter::{self, Interpreter, ScopeKey, ValueKey},
};

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
    Pattern(Pattern): "pattern",
    Func {
        args: Vec<(Spur, Option<Pattern>)>,
        code: ExprNode,
        parent_scope: ScopeKey,
        ret: Option<Pattern>,
    }: "func",
    Builtin(Builtin): "builtin",
}

impl Value {
    pub fn unit() -> Self {
        Value::Tuple(vec![])
    }
    pub fn matches_pat(&self, pat: &Pattern, interpreter: &Interpreter) -> bool {
        match pat {
            Pattern::Type(t) => self.to_type() == *t,
            Pattern::Either(a, b) => {
                self.matches_pat(a, interpreter) || self.matches_pat(b, interpreter)
            }
            Pattern::Not(p) => !self.matches_pat(p, interpreter),
        }
    }
}

impl ValueType {
    pub fn print_str(&self) -> String {
        format!("<type: {}>", self.name())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Type(ValueType),
    Either(Box<Pattern>, Box<Pattern>),
    Not(Box<Pattern>),
}

impl Pattern {
    pub fn to_str(&self) -> String {
        match self {
            Pattern::Type(t) => t.name(),
            Pattern::Either(a, b) => format!("({} | {})", a.to_str(), b.to_str()),
            Pattern::Not(p) => format!("!{}", p.to_str()),
        }
    }
}

pub mod value_ops {
    use ahash::AHashMap;

    use crate::{
        parsing::lexer::Token,
        runtime::{
            error::RuntimeError,
            interpreter::{Interpreter, ValueKey},
        },
        sources::CodeSpan,
    };

    use super::{Pattern, Value, ValueType};

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
            (Value::Array(v1), Value::Array(v2)) => {
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
            (Value::Dict(v1), Value::Dict(v2)) => {
                if v1.len() != v2.len() {
                    false
                } else {
                    for (k, e1) in v1 {
                        match v2.get(k) {
                            Some(e2) => {
                                if !equality(
                                    &interpreter.memory[*e1],
                                    &interpreter.memory[*e2],
                                    interpreter,
                                ) {
                                    return false;
                                }
                            }
                            None => return false,
                        }
                    }
                    true
                }
            }

            (Value::Type(v1), Value::Type(v2)) => v1 == v2,
            (Value::Pattern(v1), Value::Pattern(v2)) => v1 == v2,

            (v1 @ Value::Func { .. }, v2 @ Value::Func { .. }) => v1 == v2,

            (Value::Builtin(v1), Value::Builtin(v2)) => v1 == v2,

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
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        Ok(Value::Bool(equality(v1, v2, interpreter)))
    }
    pub fn not_eq_op(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        Ok(Value::Bool(!equality(v1, v2, interpreter)))
    }

    pub fn plus(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        match (v1, v2) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(*v1 + *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(*v1 + *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Float(*v1 as f64 + *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Float(*v1 + *v2 as f64)),

            (Value::String(v1), Value::String(v2)) => Ok(Value::String(v1.clone() + v2)),

            (Value::Array(v1), Value::Array(v2)) => Ok({
                let mut elems = vec![];
                let (v1, v2) = (v1.clone(), v2.clone());
                for k in v1 {
                    elems.push(interpreter.clone_key(k))
                }
                for k in v2 {
                    elems.push(interpreter.clone_key(k))
                }
                Value::Array(elems)
            }),
            (Value::Dict(v1), Value::Dict(v2)) => Ok({
                let mut map = AHashMap::new();
                let (v1, v2) = (v1.clone(), v2.clone());
                for (k, v) in v1 {
                    map.insert(k, v);
                }
                for (k, v) in v2 {
                    map.insert(k, v);
                }
                Value::Dict(map)
            }),

            _ => Err(RuntimeError::InvalidOperands {
                left: (v1.to_type(), a.1),
                right: (v2.to_type(), b.1),
                op: Token::Plus,
                span,
            }),
        }
    }
    pub fn minus(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        match (v1, v2) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(*v1 - *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(*v1 - *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Float(*v1 as f64 - *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Float(*v1 - *v2 as f64)),

            _ => Err(RuntimeError::InvalidOperands {
                left: (v1.to_type(), a.1),
                right: (v2.to_type(), b.1),
                op: Token::Minus,
                span,
            }),
        }
    }
    pub fn mult(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        match (v1, v2) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(*v1 * *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(*v1 * *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Float(*v1 as f64 * *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Float(*v1 * *v2 as f64)),

            (Value::Int(n), Value::String(s)) | (Value::String(s), Value::Int(n)) => {
                Ok(Value::String(s.repeat(if *n < 0 {
                    0
                } else {
                    *n as usize
                })))
            }

            (Value::Array(arr), Value::Int(n)) | (Value::Int(n), Value::Array(arr)) => Ok({
                let arr = arr.clone();
                let mut elems = vec![];
                for _ in 0..(*n.max(&0) as usize) {
                    for k in &arr {
                        elems.push(interpreter.clone_key(*k))
                    }
                }
                Value::Array(elems)
            }),

            _ => Err(RuntimeError::InvalidOperands {
                left: (v1.to_type(), a.1),
                right: (v2.to_type(), b.1),
                op: Token::Mult,
                span,
            }),
        }
    }
    pub fn div(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        match (v1, v2) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(*v1 / *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(*v1 / *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Float(*v1 as f64 / *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Float(*v1 / *v2 as f64)),

            _ => Err(RuntimeError::InvalidOperands {
                left: (v1.to_type(), a.1),
                right: (v2.to_type(), b.1),
                op: Token::Div,
                span,
            }),
        }
    }
    pub fn modulo(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        match (v1, v2) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Int(v1.rem_euclid(*v2))),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(v1.rem_euclid(*v2))),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Float((*v1 as f64).rem_euclid(*v2))),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Float(v1.rem_euclid(*v2 as f64))),

            _ => Err(RuntimeError::InvalidOperands {
                left: (v1.to_type(), a.1),
                right: (v2.to_type(), b.1),
                op: Token::Mod,
                span,
            }),
        }
    }
    pub fn pow(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        match (v1, v2) {
            (Value::Int(v1), Value::Int(v2)) => {
                Ok(Value::Int((*v1 as f64).powf(*v2 as f64).floor() as i64))
            }
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Float(v1.powf(*v2))),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Float((*v1 as f64).powf(*v2))),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Float(v1.powf(*v2 as f64))),

            _ => Err(RuntimeError::InvalidOperands {
                left: (v1.to_type(), a.1),
                right: (v2.to_type(), b.1),
                op: Token::Pow,
                span,
            }),
        }
    }

    pub fn greater(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        match (v1, v2) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 > *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 > *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 as f64 > *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 > *v2 as f64)),

            _ => Err(RuntimeError::InvalidOperands {
                left: (v1.to_type(), a.1),
                right: (v2.to_type(), b.1),
                op: Token::Greater,
                span,
            }),
        }
    }
    pub fn greater_eq(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        match (v1, v2) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 >= *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 >= *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 as f64 >= *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 >= *v2 as f64)),

            _ => Err(RuntimeError::InvalidOperands {
                left: (v1.to_type(), a.1),
                right: (v2.to_type(), b.1),
                op: Token::Greater,
                span,
            }),
        }
    }
    pub fn lesser(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        match (v1, v2) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 < *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 < *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Bool((*v1 as f64) < *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 < *v2 as f64)),

            _ => Err(RuntimeError::InvalidOperands {
                left: (v1.to_type(), a.1),
                right: (v2.to_type(), b.1),
                op: Token::Greater,
                span,
            }),
        }
    }
    pub fn lesser_eq(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        match (v1, v2) {
            (Value::Int(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 <= *v2)),
            (Value::Float(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 <= *v2)),

            (Value::Int(v1), Value::Float(v2)) => Ok(Value::Bool(*v1 as f64 <= *v2)),
            (Value::Float(v1), Value::Int(v2)) => Ok(Value::Bool(*v1 <= *v2 as f64)),

            _ => Err(RuntimeError::InvalidOperands {
                left: (v1.to_type(), a.1),
                right: (v2.to_type(), b.1),
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
            Value::Type(v1) => Ok(Value::Pattern(Pattern::Not(Box::new(Pattern::Type(*v1))))),
            Value::Pattern(v1) => Ok(Value::Pattern(Pattern::Not(Box::new(v1.clone())))),
            _ => Err(RuntimeError::InvalidUnaryOperand {
                v: (a.0.to_type(), a.1),
                op: Token::ExclMark,
                span,
            }),
        }
    }

    pub fn index(
        base: (ValueKey, CodeSpan),
        index: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<ValueKey, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[base.0], &interpreter.memory[index.0]);
        match (v1, v2) {
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
                base: (v1.to_type(), base.1),
                index: (v2.to_type(), index.1),
                span,
            }),
        }
    }

    pub fn is_op(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        match (v1, v2) {
            (_, Value::Type(t)) => Ok(Value::Bool(v1.to_type() == *t)),
            (_, Value::Pattern(p)) => Ok(Value::Bool(v1.matches_pat(p, interpreter))),

            _ => Err(RuntimeError::MismatchedType {
                found: v2.to_type(),
                expected: "type or pattern".into(),
                span: b.1,
            }),
        }
    }
    pub fn pipe(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        match (v1, v2) {
            (Value::Type(a), Value::Type(b)) => Ok(Value::Pattern(Pattern::Either(
                Box::new(Pattern::Type(*a)),
                Box::new(Pattern::Type(*b)),
            ))),
            (Value::Type(a), Value::Pattern(b)) => Ok(Value::Pattern(Pattern::Either(
                Box::new(Pattern::Type(*a)),
                Box::new(b.clone()),
            ))),
            (Value::Pattern(a), Value::Type(b)) => Ok(Value::Pattern(Pattern::Either(
                Box::new(a.clone()),
                Box::new(Pattern::Type(*b)),
            ))),
            (Value::Pattern(a), Value::Pattern(b)) => Ok(Value::Pattern(Pattern::Either(
                Box::new(a.clone()),
                Box::new(b.clone()),
            ))),

            _ => Err(RuntimeError::InvalidOperands {
                left: (v1.to_type(), a.1),
                right: (v2.to_type(), b.1),
                op: Token::Pipe,
                span,
            }),
        }
    }
    pub fn as_op(
        a: (ValueKey, CodeSpan),
        b: (ValueKey, CodeSpan),
        span: CodeSpan,
        interpreter: &mut Interpreter,
    ) -> Result<Value, RuntimeError> {
        let (v1, v2) = (&interpreter.memory[a.0], &interpreter.memory[b.0]);
        match (v1, v2) {
            (v, Value::Type(t)) => Ok(match (v, t) {
                (_, ValueType::String) => Value::String(interpreter.value_str(a.0)),
                (_, ValueType::Type) => Value::Type(v.to_type()),

                (Value::Int(n), ValueType::Float) => Value::Float(*n as f64),
                (Value::Float(n), ValueType::Int) => Value::Int(n.floor() as i64),

                (Value::Bool(b), ValueType::Int) => Value::Int(if *b { 1 } else { 0 }),
                (Value::Bool(b), ValueType::Float) => Value::Float(if *b { 1.0 } else { 0.0 }),

                (Value::String(s), ValueType::Int) => Value::Int(
                    s.parse::<i64>()
                        .map_err(|_| RuntimeError::ConversionError { to: *t, span })?,
                ),
                (Value::String(s), ValueType::Float) => Value::Float(
                    s.parse::<f64>()
                        .map_err(|_| RuntimeError::ConversionError { to: *t, span })?,
                ),
                (Value::String(s), ValueType::Bool) => Value::Bool(
                    s.parse::<bool>()
                        .map_err(|_| RuntimeError::ConversionError { to: *t, span })?,
                ),

                _ => {
                    if v.to_type() == *t {
                        v.clone()
                    } else {
                        return Err(RuntimeError::CannotConvert {
                            typ: v.to_type(),
                            to: *t,
                            span,
                        });
                    }
                }
            }),

            _ => Err(RuntimeError::MismatchedType {
                found: v2.to_type(),
                expected: "type".into(),
                span: b.1,
            }),
        }
    }
}
