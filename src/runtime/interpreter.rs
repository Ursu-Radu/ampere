use ahash::AHashMap;
use lasso::{Rodeo, Spur};
use slotmap::{new_key_type, SlotMap};

use crate::{
    parsing::{
        ast::{ExprNode, Expression, ListNode, Statement, StatementList, StmtNode},
        lexer::Token,
    },
    sources::{AmpereSource, CodeArea, CodeSpan},
};

use super::{
    error::RuntimeError,
    value::{value_ops, Value},
};

new_key_type! {
   pub struct ValueKey;
   pub struct ScopeKey;
}

pub struct Scope {
    pub vars: AHashMap<Spur, ValueKey>,
    pub parent: Option<ScopeKey>,
}

pub struct Interpreter {
    pub interner: Rodeo,
    pub source: AmpereSource,

    pub memory: SlotMap<ValueKey, Value>,
    pub scopes: SlotMap<ScopeKey, Scope>,
}

impl Interpreter {
    pub fn new(source: &AmpereSource, interner: Rodeo) -> Self {
        Self {
            interner,
            source: source.clone(),
            memory: SlotMap::default(),
            scopes: SlotMap::default(),
        }
    }
    pub fn get_var(&self, scope: ScopeKey, var: Spur) -> Option<ValueKey> {
        let scope = &self.scopes[scope];
        match scope.vars.get(&var) {
            Some(k) => Some(*k),
            None => match scope.parent {
                Some(p) => self.get_var(p, var),
                None => None,
            },
        }
    }

    pub fn make_area(&self, span: CodeSpan) -> CodeArea {
        CodeArea {
            span,
            source: self.source.clone(),
        }
    }
    pub fn new_unit(&mut self, span: CodeSpan) -> ValueKey {
        self.memory.insert(Value::unit())
    }
    pub fn clone_value(&mut self, k: ValueKey) -> Value {
        self.memory[k].clone()
    }
    pub fn clone_key(&mut self, k: ValueKey) -> ValueKey {
        self.memory.insert(self.memory[k].clone())
    }

    pub fn derive_scope(&mut self, scope: ScopeKey) -> ScopeKey {
        let new_scope = Scope {
            vars: AHashMap::new(),
            parent: Some(scope),
        };
        self.scopes.insert(new_scope)
    }

    pub fn value_str(&self, key: ValueKey) -> String {
        let mut passed = vec![];
        fn inner<'a>(
            interpreter: &'a Interpreter,
            key: ValueKey,
            passed: &mut Vec<&'a Value>,
        ) -> String {
            // passed.push(key);

            let val = &interpreter.memory[key];
            let contains = if passed.is_empty() {
                false
            } else {
                passed.contains(&val)
            };
            passed.push(val);

            let s = match val {
                Value::Int(v) => v.to_string(),
                Value::Float(v) => v.to_string(),
                Value::String(v) => v.to_string(),
                Value::Bool(v) => v.to_string(),
                Value::Tuple(v) => {
                    if contains {
                        "(...)".into()
                    } else {
                        format!(
                            "({})",
                            v.iter()
                                .map(|k| inner(interpreter, *k, passed))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    }
                }
                Value::Array(v) => {
                    if contains {
                        "[...]".into()
                    } else {
                        format!(
                            "[{}]",
                            v.iter()
                                .map(|k| inner(interpreter, *k, passed))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    }
                }
                Value::Dict(v) => {
                    if contains {
                        "{...}".into()
                    } else {
                        format!(
                            "{{{}}}",
                            v.iter()
                                .map(|(k, v)| format!("{}: {}", k, inner(interpreter, *v, passed)))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    }
                }
                Value::Type(t) => format!("<type: {}>", t.name()),
            };
            passed.pop();
            s
        }
        inner(self, key, &mut passed)
    }
    pub fn execute_expr(
        &mut self,
        node: &ExprNode,
        scope: ScopeKey,
    ) -> Result<ValueKey, RuntimeError> {
        match &*node.expr {
            Expression::Int(n) => Ok(self.memory.insert(Value::Int(*n))),
            Expression::Float(f) => Ok(self.memory.insert(Value::Float(*f))),
            Expression::String(s) => Ok(self.memory.insert(Value::String(s.clone()))),
            Expression::Bool(b) => Ok(self.memory.insert(Value::Bool(*b))),
            Expression::Op(left, op, right) => {
                let left_key = self.execute_expr(left, scope)?;
                let right_key = self.execute_expr(right, scope)?;

                let result = match op {
                    Token::Plus => value_ops::plus(
                        (&self.memory[left_key], left.span),
                        (&self.memory[right_key], right.span),
                        node.span,
                        self,
                    ),
                    Token::Minus => value_ops::minus(
                        (&self.memory[left_key], left.span),
                        (&self.memory[right_key], right.span),
                        node.span,
                        self,
                    ),
                    Token::Mult => value_ops::mult(
                        (&self.memory[left_key], left.span),
                        (&self.memory[right_key], right.span),
                        node.span,
                        self,
                    ),
                    Token::Div => value_ops::div(
                        (&self.memory[left_key], left.span),
                        (&self.memory[right_key], right.span),
                        node.span,
                        self,
                    ),
                    Token::Mod => value_ops::modulo(
                        (&self.memory[left_key], left.span),
                        (&self.memory[right_key], right.span),
                        node.span,
                        self,
                    ),
                    Token::Pow => value_ops::pow(
                        (&self.memory[left_key], left.span),
                        (&self.memory[right_key], right.span),
                        node.span,
                        self,
                    ),
                    Token::Eq => value_ops::eq_op(
                        (&self.memory[left_key], left.span),
                        (&self.memory[right_key], right.span),
                        node.span,
                        self,
                    ),
                    Token::NotEq => value_ops::not_eq_op(
                        (&self.memory[left_key], left.span),
                        (&self.memory[right_key], right.span),
                        node.span,
                        self,
                    ),
                    Token::Greater => value_ops::greater(
                        (&self.memory[left_key], left.span),
                        (&self.memory[right_key], right.span),
                        node.span,
                        self,
                    ),
                    Token::GreaterEq => value_ops::greater_eq(
                        (&self.memory[left_key], left.span),
                        (&self.memory[right_key], right.span),
                        node.span,
                        self,
                    ),
                    Token::Lesser => value_ops::lesser(
                        (&self.memory[left_key], left.span),
                        (&self.memory[right_key], right.span),
                        node.span,
                        self,
                    ),
                    Token::LesserEq => value_ops::lesser_eq(
                        (&self.memory[left_key], left.span),
                        (&self.memory[right_key], right.span),
                        node.span,
                        self,
                    ),
                    Token::Assign => {
                        let v = self.clone_value(right_key);
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    Token::PlusEq => {
                        let v = value_ops::plus(
                            (&self.memory[left_key], left.span),
                            (&self.memory[right_key], right.span),
                            node.span,
                            self,
                        )?;
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    Token::MinusEq => {
                        let v = value_ops::minus(
                            (&self.memory[left_key], left.span),
                            (&self.memory[right_key], right.span),
                            node.span,
                            self,
                        )?;
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    Token::MultEq => {
                        let v = value_ops::mult(
                            (&self.memory[left_key], left.span),
                            (&self.memory[right_key], right.span),
                            node.span,
                            self,
                        )?;
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    Token::DivEq => {
                        let v = value_ops::div(
                            (&self.memory[left_key], left.span),
                            (&self.memory[right_key], right.span),
                            node.span,
                            self,
                        )?;
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    Token::PowEq => {
                        let v = value_ops::pow(
                            (&self.memory[left_key], left.span),
                            (&self.memory[right_key], right.span),
                            node.span,
                            self,
                        )?;
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    Token::ModEq => {
                        let v = value_ops::modulo(
                            (&self.memory[left_key], left.span),
                            (&self.memory[right_key], right.span),
                            node.span,
                            self,
                        )?;
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    _ => unreachable!(),
                }?;

                Ok(self.memory.insert(result))
            }
            Expression::Unary(op, v) => {
                let value_key = self.execute_expr(v, scope)?;

                let result = match op {
                    Token::Minus => {
                        value_ops::unary_minus((&self.memory[value_key], v.span), node.span, self)
                    }
                    Token::ExclMark => {
                        value_ops::unary_not((&self.memory[value_key], v.span), node.span, self)
                    }
                    _ => unreachable!(),
                }?;

                Ok(self.memory.insert(result))
            }
            Expression::Block(stmts) => {
                let derived = self.derive_scope(scope);
                self.execute_list(stmts, derived)
            }
            Expression::If {
                branches,
                else_branch,
            } => {
                for (cond, code) in branches {
                    let k = self.execute_expr(cond, scope)?;
                    if value_ops::to_bool(&self.memory[k], cond.span, self)? {
                        let derived = self.derive_scope(scope);
                        return self.execute_list(code, derived);
                    }
                }
                if let Some(code) = else_branch {
                    let derived = self.derive_scope(scope);
                    self.execute_list(code, derived)
                } else {
                    Ok(self.new_unit(node.span))
                }
            }
            Expression::Var(v) => match self.get_var(scope, *v) {
                Some(k) => Ok(k),
                None => Err(RuntimeError::NonexistentVariable {
                    name: self.interner.resolve(v).into(),
                    span: node.span,
                }),
            },
            Expression::While { cond, code } => {
                let mut ret = self.new_unit(node.span);
                loop {
                    let k = self.execute_expr(cond, scope)?;
                    if value_ops::to_bool(&self.memory[k], cond.span, self)? {
                        let derived = self.derive_scope(scope);
                        ret = self.execute_list(code, derived)?;
                    } else {
                        return Ok(ret);
                    }
                }
            }
            Expression::Array(elems) => {
                let mut arr = vec![];
                for i in elems {
                    arr.push(self.execute_expr(i, scope)?);
                }
                Ok(self.memory.insert(Value::Array(arr)))
            }
            Expression::Tuple(elems) => {
                let mut arr = vec![];
                for i in elems {
                    arr.push(self.execute_expr(i, scope)?);
                }
                Ok(self.memory.insert(Value::Tuple(arr)))
            }
            Expression::Dict(map) => {
                let mut dict = AHashMap::new();
                for (k, v) in map {
                    dict.insert(
                        self.interner.resolve(k).into(),
                        self.execute_expr(v, scope)?,
                    );
                }
                Ok(self.memory.insert(Value::Dict(dict)))
            }
            Expression::Index(base, index) => {
                let base_key = self.execute_expr(base, scope)?;
                let index_key = self.execute_expr(index, scope)?;

                value_ops::index(
                    (&self.memory[base_key].clone(), base.span),
                    (&self.memory[index_key].clone(), index.span),
                    node.span,
                    self,
                )
            }
            Expression::Member(base, s) => {
                let base_key = self.execute_expr(base, scope)?;
                match (&self.memory[base_key], self.interner.resolve(s)) {
                    (Value::String(s), "length") => {
                        Ok(self.memory.insert(Value::Int(s.chars().count() as i64)))
                    }
                    (Value::Array(arr), "length") => {
                        Ok(self.memory.insert(Value::Int(arr.len() as i64)))
                    }
                    (Value::Dict(map), "length") => {
                        Ok(self.memory.insert(Value::Int(map.len() as i64)))
                    }
                    (v, m) => Err(RuntimeError::NonexistentMember {
                        base: (v.to_type(), base.span),
                        member: m.into(),
                        span: node.span,
                    }),
                }
            }
        }
    }
    pub fn execute_stmt(
        &mut self,
        node: &StmtNode,
        scope: ScopeKey,
    ) -> Result<ValueKey, RuntimeError> {
        match &*node.stmt {
            Statement::Expr(e) => self.execute_expr(e, scope),
            Statement::Let(name, e) => {
                let k = self.execute_expr(e, scope)?;
                let k = self.clone_key(k);
                self.scopes[scope].vars.insert(*name, k);
                Ok(self.new_unit(node.span))
            }
            Statement::Print(e) => {
                let k = self.execute_expr(e, scope)?;
                println!("{}", self.value_str(k));

                Ok(self.new_unit(node.span))
            }
        }
    }
    pub fn execute_list(
        &mut self,
        node: &ListNode,
        scope: ScopeKey,
    ) -> Result<ValueKey, RuntimeError> {
        match &*node.list {
            StatementList::Normal(stmts) => {
                for stmt in stmts {
                    self.execute_stmt(stmt, scope)?;
                }
                Ok(self.new_unit(node.span))
            }
            StatementList::Ret(stmts, ret) => {
                for stmt in stmts {
                    self.execute_stmt(stmt, scope)?;
                }
                Ok(self.execute_stmt(ret, scope)?)
            }
        }
    }
}
