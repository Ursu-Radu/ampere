use lasso::{Rodeo, Spur};
use logos::{Lexer, Logos};

use crate::sources::{AmpereSource, CodeArea, CodeSpan};

use super::{
    ast::{ExprNode, Expression, ListNode, Statement, StatementList, StmtNode},
    error::SyntaxError,
    lexer::Token,
    utils::operators::{self, unary_prec},
};

pub struct Parser<'a> {
    pub lexer: Lexer<'a, Token>,
    pub source: AmpereSource,
    pub interner: Rodeo,
}

impl<'a> Parser<'a> {
    pub fn new(code: &'a str, source: &AmpereSource, interner: Rodeo) -> Self {
        let lexer = Token::lexer(code);
        Parser {
            lexer,
            source: source.clone(),
            interner,
        }
    }

    pub fn next(&mut self) -> Token {
        self.lexer.next().unwrap_or(Token::Eof)
    }
    pub fn span(&self) -> CodeSpan {
        self.lexer.span().into()
    }
    pub fn slice(&self) -> &str {
        self.lexer.slice()
    }
    pub fn slice_interned(&mut self) -> Spur {
        let s = self.lexer.slice();
        self.interner.get_or_intern(s)
    }
    pub fn peek(&self) -> Token {
        let mut peek = self.lexer.clone();
        peek.next().unwrap_or(Token::Eof)
    }
    pub fn peek_span(&self) -> CodeSpan {
        let mut peek = self.lexer.clone();
        peek.next();
        peek.span().into()
    }

    pub fn make_area(&self, span: CodeSpan) -> CodeArea {
        CodeArea {
            span,
            source: self.source.clone(),
        }
    }

    pub fn expect_tok_named(&mut self, expect: Token, name: &str) -> Result<(), SyntaxError> {
        let next = self.next();
        if next != expect {
            return Err(SyntaxError::UnexpectedToken {
                found: next,
                expected: name.to_string(),
                area: self.make_area(self.span()),
            });
        }
        Ok(())
    }
    pub fn expect_tok(&mut self, expect: Token) -> Result<(), SyntaxError> {
        self.expect_tok_named(expect, expect.name())
    }

    pub fn parse_unit(&mut self) -> Result<ExprNode, SyntaxError> {
        let unary;
        match self.next() {
            Token::Int => {
                Ok(Expression::Int(self.slice().parse::<i64>().unwrap()).to_node(self.span()))
            }

            Token::Float => {
                Ok(Expression::Float(self.slice().parse::<f64>().unwrap()).to_node(self.span()))
            }

            Token::String => Ok(Expression::String(self.slice().into()).to_node(self.span())),

            Token::True => Ok(Expression::Bool(true).to_node(self.span())),
            Token::False => Ok(Expression::Bool(false).to_node(self.span())),
            Token::OpenParen => {
                let start = self.span();
                let mut inner = self.parse_expr()?;
                self.expect_tok(Token::ClosedParen)?;
                inner.span = start.extend(self.span());
                Ok(inner)
            }
            Token::Ident => Ok(Expression::Var(self.slice_interned()).to_node(self.span())),
            Token::If => {
                let start = self.span();

                let mut branches = vec![];
                let cond = self.parse_expr()?;
                self.expect_tok(Token::OpenBracket)?;
                let code = self.parse_stmts()?;
                self.expect_tok(Token::ClosedBracket)?;
                branches.push((cond, code));

                while self.peek() == Token::Elif {
                    self.next();
                    let cond = self.parse_expr()?;
                    self.expect_tok(Token::OpenBracket)?;
                    let code = self.parse_stmts()?;
                    self.expect_tok(Token::ClosedBracket)?;
                    branches.push((cond, code));
                }
                let else_branch = if self.peek() == Token::Else {
                    self.next();
                    self.expect_tok(Token::OpenBracket)?;
                    let code = self.parse_stmts()?;
                    self.expect_tok(Token::ClosedBracket)?;
                    Some(code)
                } else {
                    None
                };

                Ok(Expression::If {
                    branches,
                    else_branch,
                }
                .to_node(start.extend(self.span())))
            }
            unary_op
                if {
                    unary = unary_prec(unary_op);
                    unary.is_some()
                } =>
            {
                let start = self.span();
                let unary_prec = unary.unwrap();
                let next_prec = operators::next_infix(unary_prec);
                let val = match next_prec {
                    Some(next_prec) => self.parse_op(next_prec)?,
                    None => self.parse_unit()?,
                };
                Ok(Expression::Unary(unary_op, val).to_node(start.extend(self.span())))
            }
            other => Err(SyntaxError::UnexpectedToken {
                found: other,
                expected: "expression".into(),
                area: self.make_area(self.span()),
            }),
        }
    }

    pub fn parse_op(&mut self, prec: usize) -> Result<ExprNode, SyntaxError> {
        let next_prec = operators::next_infix(prec);

        let mut left = match next_prec {
            Some(next_prec) => self.parse_op(next_prec)?,
            None => self.parse_unit()?,
        };
        while operators::is_infix_prec(self.peek(), prec) {
            let op = self.next();
            let right = if operators::prec_type(prec) == operators::OpType::Left {
                match next_prec {
                    Some(next_prec) => self.parse_op(next_prec)?,
                    None => self.parse_unit()?,
                }
            } else {
                self.parse_op(prec)?
            };
            let new_span = left.span.extend(right.span);
            left = Expression::Op(left, op, right).to_node(new_span)
        }
        Ok(left)
    }

    pub fn parse_expr(&mut self) -> Result<ExprNode, SyntaxError> {
        self.parse_op(0)
    }

    pub fn parse_stmt(&mut self) -> Result<(StmtNode, bool), SyntaxError> {
        let start = self.peek_span();
        let stmt = match self.peek() {
            Token::Let => {
                self.next();
                self.expect_tok_named(Token::Ident, "variable name")?;
                let s = self.slice_interned();
                self.expect_tok(Token::Assign)?;
                let value = self.parse_expr()?;
                Statement::Let(s, value)
            }
            Token::Print => {
                self.next();
                let value = self.parse_expr()?;
                Statement::Print(value)
            }
            _ => {
                let expr = self.parse_expr()?;
                Statement::Expr(expr)
            }
        };
        let is_ret = if self.peek() == Token::Semicolon {
            self.next();
            false
        } else {
            true
        };
        Ok((stmt.to_node(start.extend(self.span())), is_ret))
    }

    pub fn parse_stmts(&mut self) -> Result<ListNode, SyntaxError> {
        let mut stmts = vec![];
        let mut span = self.span();
        (span.start, span.end) = (span.start + 1, span.start + 1);
        while ![Token::ClosedBracket, Token::Eof].contains(&self.peek()) {
            let (stmt, is_ret) = self.parse_stmt()?;
            if is_ret {
                let span = stmt.span;
                return Ok(StatementList::Ret(stmts, stmt).to_node(span));
            } else {
                span = stmt.span;
                stmts.push(stmt);
            }
        }
        Ok(StatementList::Normal(stmts).to_node(span))
    }

    pub fn parse(&mut self) -> Result<ListNode, SyntaxError> {
        let list = self.parse_stmts()?;
        self.expect_tok(Token::Eof)?;
        Ok(list)
    }
}
