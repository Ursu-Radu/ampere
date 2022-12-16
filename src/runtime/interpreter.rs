use std::path::PathBuf;

use ahash::AHashMap;
use lasso::{Rodeo, Spur};
use slotmap::{new_key_type, SecondaryMap, SlotMap};

use crate::{
    parsing::{
        ast::{ExprNode, Expression, ListNode, Statement, StatementList, StmtNode},
        lexer::Token,
        parser::Parser,
    },
    sources::{AmpereSource, CodeArea, CodeSpan, SourceKey, SourceMap},
};

use super::{
    builtins::Builtin,
    error::RuntimeError,
    value::{value_ops, Pattern, Value, ValueType},
};

new_key_type! {
   pub struct ValueKey;
   pub struct ScopeKey;
}

pub struct Scope {
    pub vars: AHashMap<Spur, ValueKey>,
    pub parent: Option<ScopeKey>,
}

pub struct Interpreter<'a> {
    pub interner: &'a mut Rodeo,
    pub source_map: SourceMap,

    pub memory: SlotMap<ValueKey, Value>,
    pub scopes: SlotMap<ScopeKey, Scope>,

    pub exports: SecondaryMap<SourceKey, AHashMap<Spur, ValueKey>>,

    pub src_stack: Vec<SourceKey>,
}

macro_rules! func_exec {
    ($interpreter:expr, $args:expr, $ret:expr, $scope:expr => $arg_exec:ident, $ret_out:ident) => {
        let mut $arg_exec = vec![];

        let mut expr_option_pattern = |node: &Option<ExprNode>| {
            Ok(match node {
                Some(e) => {
                    let k = $interpreter.execute_expr(e, $scope)?;
                    Some(match &$interpreter.memory[k] {
                        Value::Type(t) => Pattern::Type(*t),
                        Value::Pattern(p) => p.clone(),
                        v => {
                            return Err(RuntimeError::MismatchedType {
                                found: v.to_type(),
                                expected: "type or pattern".into(),
                                area: e.area,
                            }
                            .into_halt())
                        }
                    })
                }
                None => None,
            })
        };

        for (s, t) in $args {
            $arg_exec.push((*s, expr_option_pattern(t)?))
        }
        let $ret_out = expr_option_pattern($ret)?;
    };
}

#[derive(Clone)]
pub enum Jump {
    Return(ValueKey, CodeArea),
    Break(ValueKey, CodeArea),
    Continue(CodeArea),
}

#[derive(Clone)]
pub enum Halt {
    Error(RuntimeError),
    Jump(Jump),
}

impl<'a> Interpreter<'a> {
    pub fn new(interner: &'a mut Rodeo, source_map: SourceMap) -> Self {
        Self {
            source_map,
            interner,
            memory: SlotMap::default(),
            scopes: SlotMap::default(),
            exports: SecondaryMap::default(),
            src_stack: vec![],
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

    // pub fn make_area(&self, span: CodeSpan) -> CodeArea {
    //     CodeArea {
    //         span,
    //         source: self.source.clone(),
    //     }
    // }
    pub fn new_unit(&mut self) -> ValueKey {
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
                Value::Type(t) => t.print_str(),
                Value::Pattern(p) => p.to_str(),
                Value::Func { args, .. } => {
                    format!(
                        "({}) => ...",
                        args.iter()
                            .map(|(name, typ)| if let Some(v) = typ {
                                format!("{}: {}", interpreter.interner.resolve(name), v.to_str())
                            } else {
                                interpreter.interner.resolve(name).into()
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
                Value::Builtin(b) => b.print_str(),
                Value::Range { start, end, step } => {
                    if *step == 1 {
                        format!("{}..{}", start, end)
                    } else {
                        format!("{}..{}..{}", start, step, end)
                    }
                }
            };
            passed.pop();
            s
        }
        inner(self, key, &mut passed)
    }
    pub fn execute_expr(&mut self, node: &ExprNode, scope: ScopeKey) -> Result<ValueKey, Halt> {
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
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::Minus => value_ops::minus(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::Mult => value_ops::mult(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::Div => value_ops::div(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::Mod => value_ops::modulo(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::Pow => value_ops::pow(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::Eq => value_ops::eq_op(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::NotEq => value_ops::not_eq_op(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::Greater => value_ops::greater(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::GreaterEq => value_ops::greater_eq(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::Lesser => value_ops::lesser(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::LesserEq => value_ops::lesser_eq(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::Assign => {
                        let v = self.clone_value(right_key);
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    Token::PlusEq => {
                        let v = value_ops::plus(
                            (left_key, left.area),
                            (right_key, right.area),
                            node.area,
                            self,
                        )?;
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    Token::MinusEq => {
                        let v = value_ops::minus(
                            (left_key, left.area),
                            (right_key, right.area),
                            node.area,
                            self,
                        )?;
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    Token::MultEq => {
                        let v = value_ops::mult(
                            (left_key, left.area),
                            (right_key, right.area),
                            node.area,
                            self,
                        )?;
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    Token::DivEq => {
                        let v = value_ops::div(
                            (left_key, left.area),
                            (right_key, right.area),
                            node.area,
                            self,
                        )?;
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    Token::PowEq => {
                        let v = value_ops::pow(
                            (left_key, left.area),
                            (right_key, right.area),
                            node.area,
                            self,
                        )?;
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    Token::ModEq => {
                        let v = value_ops::modulo(
                            (left_key, left.area),
                            (right_key, right.area),
                            node.area,
                            self,
                        )?;
                        self.memory[left_key] = v.clone();
                        Ok(v)
                    }
                    Token::Is => value_ops::is_op(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::As => value_ops::as_op(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::Pipe => value_ops::pipe(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    Token::DoubleDot => value_ops::range_op(
                        (left_key, left.area),
                        (right_key, right.area),
                        node.area,
                        self,
                    ),
                    _ => unreachable!(),
                }?;

                Ok(self.memory.insert(result))
            }
            Expression::Unary(op, v) => {
                let value_key = self.execute_expr(v, scope)?;

                let result = match op {
                    Token::Minus => {
                        value_ops::unary_minus((&self.memory[value_key], v.area), node.area, self)
                    }
                    Token::ExclMark => {
                        value_ops::unary_not((&self.memory[value_key], v.area), node.area, self)
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
                    if value_ops::to_bool(&self.memory[k], cond.area, self)? {
                        let derived = self.derive_scope(scope);
                        return self.execute_list(code, derived);
                    }
                }
                if let Some(code) = else_branch {
                    let derived = self.derive_scope(scope);
                    self.execute_list(code, derived)
                } else {
                    Ok(self.new_unit())
                }
            }
            Expression::Var(v) => match self.get_var(scope, *v) {
                Some(k) => Ok(k),
                None => Err(RuntimeError::NonexistentVariable {
                    name: self.interner.resolve(v).into(),
                    area: node.area,
                }
                .into_halt()),
            },
            Expression::While { cond, code } => {
                let mut ret = self.new_unit();
                loop {
                    let k = self.execute_expr(cond, scope)?;
                    if value_ops::to_bool(&self.memory[k], cond.area, self)? {
                        let derived = self.derive_scope(scope);

                        ret = match self.execute_list(code, derived) {
                            Ok(k) => k,
                            h @ (Err(Halt::Error(_)) | Err(Halt::Jump(Jump::Return(_, _)))) => {
                                return h
                            }

                            Err(Halt::Jump(Jump::Break(k, _))) => return Ok(k),
                            Err(Halt::Jump(Jump::Continue(_))) => continue,
                        };
                    } else {
                        return Ok(ret);
                    }
                }
            }
            Expression::For {
                iterator,
                expr,
                code,
            } => {
                let k = self.execute_expr(expr, scope)?;
                let mut ret = self.new_unit();
                for v in value_ops::to_iter(&self.memory[k], expr.area, self)? {
                    let derived = self.derive_scope(scope);
                    let k = self.memory.insert(v);
                    self.scopes[scope].vars.insert(*iterator, k);

                    ret = match self.execute_list(code, derived) {
                        Ok(k) => k,
                        h @ (Err(Halt::Error(_)) | Err(Halt::Jump(Jump::Return(_, _)))) => {
                            return h
                        }

                        Err(Halt::Jump(Jump::Break(k, _))) => return Ok(k),
                        Err(Halt::Jump(Jump::Continue(_))) => continue,
                    };
                }
                Ok(ret)
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
                    (base_key, base.area),
                    (index_key, index.area),
                    node.area,
                    self,
                )
                .map_err(|e| e.into_halt())
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

                    (Value::Dict(map), s) if map.contains_key(s) => Ok(map[s]),
                    (v, m) => Err(RuntimeError::NonexistentMember {
                        base: (v.to_type(), base.area),
                        member: m.into(),
                        area: node.area,
                    }
                    .into_halt()),
                }
            }
            Expression::Func { args, code, ret } => {
                func_exec!(self, args, ret, scope => arg_exec, ret);
                Ok(self.memory.insert(Value::Func {
                    args: arg_exec,
                    code: code.clone(),
                    parent_scope: scope,
                    ret,
                }))
            }
            Expression::Call(base, args) => {
                let base_key = self.execute_expr(base, scope)?;
                match self.memory[base_key].clone() {
                    Value::Func {
                        args: func_args,
                        code,
                        parent_scope,
                        ret,
                    } => {
                        if args.len() != func_args.len() {
                            return Err(RuntimeError::ArgumentAmount {
                                expected: func_args.len(),
                                found: args.len(),
                                area: node.area,
                            }
                            .into_halt());
                        }
                        let derived = self.derive_scope(parent_scope);

                        for (e, (name, typ)) in args.iter().zip(func_args) {
                            let k = self.execute_expr(e, scope)?;

                            if let Some(p) = typ {
                                let val = &self.memory[k];
                                if !val.matches_pat(&p, self) {
                                    return Err(RuntimeError::ArgPatternMismatch {
                                        name: self.interner.resolve(&name).into(),
                                        expected: p,
                                        found: val.to_type(),
                                        area: e.area,
                                    }
                                    .into_halt());
                                }
                            }

                            let k = self.clone_key(k);
                            self.scopes[derived].vars.insert(name, k);
                        }

                        let ret_key = match self.execute_expr(&code, derived) {
                            Ok(k) => k,
                            Err(h @ Halt::Error(_)) => return Err(h),

                            Err(Halt::Jump(Jump::Return(k, _))) => k,
                            Err(Halt::Jump(Jump::Break(_, area))) => {
                                return Err(RuntimeError::BreakOutside { area }.into_halt())
                            }
                            Err(Halt::Jump(Jump::Continue(area))) => {
                                return Err(RuntimeError::ContinueOutside { area }.into_halt())
                            }
                        };
                        if let Some(p) = ret {
                            let val = &self.memory[ret_key];
                            if !val.matches_pat(&p, self) {
                                return Err(RuntimeError::RetPatternMismatch {
                                    expected: p,
                                    found: val.to_type(),
                                    area: node.area,
                                }
                                .into_halt());
                            }
                        }
                        Ok(ret_key)
                    }
                    Value::Type(t) => {
                        if args.len() != 1 {
                            return Err(RuntimeError::ArgumentAmount {
                                expected: 1,
                                found: args.len(),
                                area: node.area,
                            }
                            .into_halt());
                        }
                        let arg_key = self.execute_expr(&args[0], scope)?;

                        let result = value_ops::as_op(
                            (arg_key, args[0].area),
                            (base_key, base.area),
                            node.area,
                            self,
                        )?;

                        Ok(self.memory.insert(result))
                    }
                    Value::Builtin(b) => {
                        let result = b.run(args, node.area, scope, self)?;
                        Ok(self.memory.insert(result))
                    }
                    v => Err(RuntimeError::MismatchedType {
                        found: v.to_type(),
                        expected: "function, builtin, or type".into(),
                        area: base.area,
                    }
                    .into_halt()),
                }
            }
            Expression::Return(value) => {
                let k = if let Some(v) = value {
                    self.execute_expr(v, scope)?
                } else {
                    self.new_unit()
                };
                Err(Halt::Jump(Jump::Return(k, node.area)))
            }
            Expression::Break(value) => {
                let k = if let Some(v) = value {
                    self.execute_expr(v, scope)?
                } else {
                    self.new_unit()
                };
                Err(Halt::Jump(Jump::Break(k, node.area)))
            }
            Expression::Continue => Err(Halt::Jump(Jump::Continue(node.area))),
            Expression::Import(value) => {
                let k = self.execute_expr(value, scope)?;
                let path = match &self.memory[k] {
                    Value::String(s) => s,
                    v => {
                        return Err(RuntimeError::MismatchedType {
                            found: v.to_type(),
                            expected: "string".into(),
                            area: value.area,
                        }
                        .into_halt())
                    }
                };

                let path = PathBuf::from(path);
                let src = AmpereSource::File(path);

                let (code, src) = if let Some(c) = src.read() {
                    (c, self.source_map.insert(src))
                } else {
                    return Err(RuntimeError::ImportReadFailed { area: node.area }.into_halt());
                };

                let parse_result = {
                    let mut parser = Parser::new(&code, src, self.interner);
                    parser.parse()
                };

                match parse_result {
                    Ok(e) => {
                        let global_scope = self.scopes.insert(Scope {
                            vars: AHashMap::new(),
                            parent: None,
                        });
                        ValueType::populate_scope(self, global_scope);
                        Builtin::populate_scope(self, global_scope);

                        self.src_stack.push(src);

                        match self.execute_list(&e, global_scope) {
                            Ok(_) => {
                                self.src_stack.pop();
                            }
                            Err(err) => {
                                self.src_stack.pop();
                                let err = match err {
                                    Halt::Error(err) => err,
                                    Halt::Jump(Jump::Return(_, area)) => {
                                        RuntimeError::ReturnOutside { area }
                                    }
                                    Halt::Jump(Jump::Break(_, area)) => {
                                        RuntimeError::BreakOutside { area }
                                    }
                                    Halt::Jump(Jump::Continue(area)) => {
                                        RuntimeError::ContinueOutside { area }
                                    }
                                };
                                return Err(err.into_halt());
                            }
                        }
                    }
                    Err(err) => {
                        return Err(RuntimeError::ImportParse {
                            error: err,
                            area: node.area,
                        }
                        .into_halt());
                    }
                }

                let mut map = AHashMap::new();
                for (k, v) in &self.source_map[src].exports {
                    map.insert(self.interner.resolve(k).to_string(), *v);
                }

                Ok(self.memory.insert(Value::Dict(map)))
            }
        }
    }
    pub fn execute_stmt(&mut self, node: &StmtNode, scope: ScopeKey) -> Result<ValueKey, Halt> {
        match &*node.stmt {
            Statement::Expr(e) => self.execute_expr(e, scope),
            Statement::Let(name, e, export) => {
                let k = self.execute_expr(e, scope)?;
                let k = self.clone_key(k);
                self.scopes[scope].vars.insert(*name, k);

                if *export {
                    self.source_map[*self.src_stack.last().unwrap()]
                        .exports
                        .insert(*name, k);
                }

                Ok(self.new_unit())
            }
            Statement::Func {
                name,
                args,
                code,
                ret,
                export,
            } => {
                func_exec!(self, args, ret, scope => arg_exec, ret);
                let k = self.memory.insert(Value::Func {
                    args: arg_exec,
                    code: code.clone(),
                    parent_scope: scope,
                    ret,
                });
                self.scopes[scope].vars.insert(*name, k);

                if *export {
                    self.source_map[*self.src_stack.last().unwrap()]
                        .exports
                        .insert(*name, k);
                }

                Ok(self.new_unit())
            }
        }
    }
    pub fn execute_list(&mut self, node: &ListNode, scope: ScopeKey) -> Result<ValueKey, Halt> {
        match &*node.list {
            StatementList::Normal(stmts) => {
                for stmt in stmts {
                    self.execute_stmt(stmt, scope)?;
                }
                Ok(self.new_unit())
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
