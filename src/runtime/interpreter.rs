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
    value::{value_ops, StoredValue, Value},
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

    pub memory: SlotMap<ValueKey, StoredValue>,
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
        self.memory.insert(Value::unit().into_stored(span))
    }
    pub fn clone_value(&mut self, k: ValueKey, span: CodeSpan) -> StoredValue {
        self.memory[k].clone_redef(span)
    }
    pub fn clone_key(&mut self, k: ValueKey, span: CodeSpan) -> ValueKey {
        self.memory.insert(self.memory[k].clone_redef(span))
    }
    pub fn value_str(&self, key: ValueKey) -> String {
        let mut passed = vec![];
        fn inner(interpreter: &Interpreter, key: ValueKey, passed: &mut Vec<ValueKey>) -> String {
            passed.push(key);

            let s = match &interpreter.memory[key].value {
                Value::Int(v) => v.to_string(),
                Value::Float(v) => v.to_string(),
                Value::String(v) => v.to_string(),
                Value::Bool(v) => v.to_string(),
                Value::Tuple(v) => {
                    if passed[0..(passed.len() - 1)].contains(&key) {
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
            Expression::Int(n) => Ok(self.memory.insert(Value::Int(*n).into_stored(node.span))),
            Expression::Float(f) => Ok(self.memory.insert(Value::Float(*f).into_stored(node.span))),
            Expression::String(s) => Ok(self
                .memory
                .insert(Value::String(s.clone()).into_stored(node.span))),
            Expression::Bool(b) => Ok(self.memory.insert(Value::Bool(*b).into_stored(node.span))),
            Expression::Op(left, op, right) => {
                let left_key = self.execute_expr(left, scope)?;
                let right_key = self.execute_expr(right, scope)?;

                let result = match op {
                    Token::Plus => value_ops::plus(
                        &self.memory[left_key],
                        &self.memory[right_key],
                        node.span,
                        self,
                    ),
                    Token::Minus => value_ops::minus(
                        &self.memory[left_key],
                        &self.memory[right_key],
                        node.span,
                        self,
                    ),
                    Token::Mult => value_ops::mult(
                        &self.memory[left_key],
                        &self.memory[right_key],
                        node.span,
                        self,
                    ),
                    Token::Div => value_ops::div(
                        &self.memory[left_key],
                        &self.memory[right_key],
                        node.span,
                        self,
                    ),
                    Token::Mod => value_ops::modulo(
                        &self.memory[left_key],
                        &self.memory[right_key],
                        node.span,
                        self,
                    ),
                    Token::Assign => {
                        let v = self.clone_value(right_key, right.span);
                        let ret = v.value.clone();
                        self.memory[left_key] = v;
                        Ok(ret)
                    }
                    _ => unreachable!(),
                }?;

                Ok(self.memory.insert(result.into_stored(node.span)))
            }
            Expression::Unary(op, v) => {
                let value_key = self.execute_expr(v, scope)?;

                let result = match op {
                    Token::Minus => {
                        value_ops::unary_minus(&self.memory[value_key], node.span, self)
                    }
                    Token::ExclMark => {
                        value_ops::unary_not(&self.memory[value_key], node.span, self)
                    }
                    _ => unreachable!(),
                }?;

                Ok(self.memory.insert(result.into_stored(node.span)))
            }
            Expression::If {
                branches,
                else_branch,
            } => todo!(),
            Expression::Var(v) => match self.get_var(scope, *v) {
                Some(k) => Ok(k),
                None => Err(RuntimeError::NonexistentVariable {
                    name: self.interner.resolve(v).into(),
                    area: self.make_area(node.span),
                }),
            },
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
                let k = self.clone_key(k, e.span);
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
