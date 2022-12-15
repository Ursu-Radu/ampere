use ahash::AHashMap;
use lasso::{Rodeo, Spur};
use logos::{Lexer, Logos};

use crate::sources::{AmpereSource, CodeArea, CodeSpan, SourceKey};

use super::{
    ast::{ExprNode, Expression, ListNode, Statement, StatementList, StmtNode},
    error::SyntaxError,
    lexer::Token,
    utils::operators::{self, unary_prec},
};

pub struct Parser<'a> {
    pub lexer: Lexer<'a, Token>,
    pub src: SourceKey,
    pub interner: Rodeo,
}

macro_rules! func_parser {
    ($parser:expr => $args:ident, $ret:ident) => {
        let mut $args = vec![];

        while $parser.peek() != Token::ClosedParen {
            $parser.expect_tok_named(Token::Ident, "argument name")?;
            let arg_name = $parser.slice_interned();

            let typ = if $parser.next_is(Token::Colon) {
                $parser.next();
                Some($parser.parse_expr()?)
            } else {
                None
            };

            $args.push((arg_name, typ));
            if !$parser.skip_tok(Token::Comma) {
                break;
            }
        }
        $parser.expect_tok(Token::ClosedParen)?;

        let $ret = if $parser.next_is(Token::Arrow) {
            $parser.next();
            Some($parser.parse_expr()?)
        } else {
            None
        };
    };
}

impl<'a> Parser<'a> {
    pub fn new(code: &'a str, src: SourceKey, interner: Rodeo) -> Self {
        let lexer = Token::lexer(code);
        Parser {
            lexer,
            src,
            interner,
        }
    }

    pub fn next(&mut self) -> Token {
        self.lexer.next().unwrap_or(Token::Eof)
    }
    pub fn span(&self) -> CodeSpan {
        self.lexer.span().into()
    }
    pub fn peek_span(&self) -> CodeSpan {
        let mut peek = self.lexer.clone();
        peek.next();
        peek.span().into()
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
    pub fn next_is(&self, tok: Token) -> bool {
        self.peek() == tok
    }
    pub fn next_are(&self, toks: &[Token]) -> bool {
        let mut peek = self.lexer.clone();
        for tok in toks {
            if peek.next().unwrap_or(Token::Eof) != *tok {
                return false;
            }
        }
        true
    }

    pub fn make_area(&self, span: CodeSpan) -> CodeArea {
        CodeArea {
            span,
            src: self.src,
        }
    }
    pub fn skip_tok(&mut self, skip: Token) -> bool {
        if self.peek() == skip {
            self.next();
            true
        } else {
            false
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

    pub fn could_be_expr(&mut self) -> bool {
        use Token::*;
        let unary;
        match self.peek() {
            Int | Float | String | True | False | Ident | OpenParen | OpenSqBracket | If
            | While | OpenBracket | Return | Break | Continue => true,
            unary_op
                if {
                    unary = unary_prec(unary_op);
                    unary.is_some()
                } =>
            {
                true
            }
            _ => false,
        }
    }

    pub fn parse_unit(&mut self) -> Result<ExprNode, SyntaxError> {
        let unary;
        let start = self.peek_span();
        match self.next() {
            Token::Int => Ok(Expression::Int(self.slice().parse::<i64>().unwrap())
                .into_node(self.span(), self.src)),

            Token::Float => Ok(Expression::Float(self.slice().parse::<f64>().unwrap())
                .into_node(self.span(), self.src)),

            Token::String => Ok(Expression::String({
                let s = self.slice();
                s[1..s.len() - 1].into()
            })
            .into_node(self.span(), self.src)),

            Token::True => Ok(Expression::Bool(true).into_node(self.span(), self.src)),
            Token::False => Ok(Expression::Bool(false).into_node(self.span(), self.src)),
            Token::Ident => {
                Ok(Expression::Var(self.slice_interned()).into_node(self.span(), self.src))
            }
            Token::OpenParen => {
                let mut peek = self.lexer.clone();
                let mut indent = 1;

                let after_close = loop {
                    match peek.next().unwrap_or(Token::Eof) {
                        Token::OpenParen => indent += 1,
                        Token::Eof => {
                            return Err(SyntaxError::NoMatching {
                                close: Token::ClosedParen,
                                open: Token::OpenParen,
                                area: self.make_area(start),
                            })
                        }
                        Token::ClosedParen => {
                            indent -= 1;
                            if indent == 0 {
                                break peek.next().unwrap_or(Token::Eof);
                            }
                        }
                        _ => (),
                    }
                };

                if after_close == Token::LargeArrow || after_close == Token::Arrow {
                    func_parser!(self => args, ret);

                    self.expect_tok(Token::LargeArrow)?;

                    let code = self.parse_expr()?;

                    Ok(Expression::Func { args, code, ret }
                        .into_node(start.extend(self.span()), self.src))
                } else {
                    if self.next_is(Token::ClosedParen) {
                        self.next();
                        return Ok(Expression::Tuple(vec![])
                            .into_node(start.extend(self.span()), self.src));
                    }
                    let mut inner = self.parse_expr()?;
                    if self.next_is(Token::Comma) {
                        self.next();
                        let mut elems = vec![inner];
                        while self.peek() != Token::ClosedParen {
                            elems.push(self.parse_expr()?);
                            if !self.skip_tok(Token::Comma) {
                                break;
                            }
                        }
                        self.expect_tok(Token::ClosedParen)?;

                        Ok(Expression::Tuple(elems).into_node(start.extend(self.span()), self.src))
                    } else {
                        self.expect_tok(Token::ClosedParen)?;
                        inner.area.span = start.extend(self.span());
                        Ok(inner)
                    }
                }
            }
            Token::OpenSqBracket => {
                let mut elems = vec![];
                while self.peek() != Token::ClosedSqBracket {
                    elems.push(self.parse_expr()?);
                    if !self.skip_tok(Token::Comma) {
                        break;
                    }
                }
                self.expect_tok(Token::ClosedSqBracket)?;

                Ok(Expression::Array(elems).into_node(start.extend(self.span()), self.src))
            }
            Token::If => {
                let mut branches = vec![];
                let cond = self.parse_expr()?;
                self.expect_tok(Token::OpenBracket)?;
                let code = self.parse_stmts()?;
                self.expect_tok(Token::ClosedBracket)?;
                branches.push((cond, code));

                while self.next_is(Token::Elif) {
                    self.next();
                    let cond = self.parse_expr()?;
                    self.expect_tok(Token::OpenBracket)?;
                    let code = self.parse_stmts()?;
                    self.expect_tok(Token::ClosedBracket)?;
                    branches.push((cond, code));
                }
                let else_branch = if self.next_is(Token::Else) {
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
                .into_node(start.extend(self.span()), self.src))
            }
            Token::While => {
                let cond = self.parse_expr()?;
                self.expect_tok(Token::OpenBracket)?;
                let code = self.parse_stmts()?;
                self.expect_tok(Token::ClosedBracket)?;

                Ok(Expression::While { code, cond }.into_node(start.extend(self.span()), self.src))
            }
            Token::For => {
                self.expect_tok_named(Token::Ident, "iterator name")?;
                let iterator = self.slice_interned();
                self.expect_tok(Token::In)?;
                let expr = self.parse_expr()?;

                self.expect_tok(Token::OpenBracket)?;
                let code = self.parse_stmts()?;
                self.expect_tok(Token::ClosedBracket)?;

                Ok(Expression::For {
                    iterator,
                    code,
                    expr,
                }
                .into_node(start.extend(self.span()), self.src))
            }
            Token::OpenBracket => {
                if self.next_are(&[Token::Ident, Token::Colon]) {
                    let mut map = AHashMap::new();

                    while self.peek() != Token::ClosedBracket {
                        self.expect_tok_named(Token::Ident, "key name")?;
                        let k = self.slice_interned();
                        self.expect_tok(Token::Colon)?;

                        map.insert(k, self.parse_expr()?);
                        if !self.skip_tok(Token::Comma) {
                            break;
                        }
                    }
                    self.expect_tok(Token::ClosedBracket)?;

                    Ok(Expression::Dict(map).into_node(start.extend(self.span()), self.src))
                } else {
                    let code = self.parse_stmts()?;
                    self.expect_tok(Token::ClosedBracket)?;
                    Ok(Expression::Block(code).into_node(start.extend(self.span()), self.src))
                }
            }
            Token::Return => {
                let value = if self.could_be_expr() {
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                Ok(Expression::Return(value).into_node(start.extend(self.span()), self.src))
            }
            Token::Break => {
                let value = if self.could_be_expr() {
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                Ok(Expression::Break(value).into_node(start.extend(self.span()), self.src))
            }
            Token::Continue => {
                Ok(Expression::Continue.into_node(start.extend(self.span()), self.src))
            }
            unary_op
                if {
                    unary = unary_prec(unary_op);
                    unary.is_some()
                } =>
            {
                let unary_prec = unary.unwrap();
                let next_prec = operators::next_infix(unary_prec);
                let val = match next_prec {
                    Some(next_prec) => self.parse_op(next_prec)?,
                    None => self.parse_value()?,
                };
                Ok(Expression::Unary(unary_op, val).into_node(start.extend(self.span()), self.src))
            }
            other => Err(SyntaxError::UnexpectedToken {
                found: other,
                expected: "expression".into(),
                area: self.make_area(self.span()),
            }),
        }
    }

    pub fn parse_value(&mut self) -> Result<ExprNode, SyntaxError> {
        let mut value = self.parse_unit()?;
        loop {
            match self.peek() {
                Token::OpenSqBracket => {
                    self.next();
                    let index = self.parse_expr()?;
                    self.expect_tok(Token::ClosedSqBracket)?;

                    let new_span = value.area.span.extend(self.span());
                    value = Expression::Index(value, index).into_node(new_span, self.src);
                }
                Token::Dot => {
                    self.next();
                    self.expect_tok_named(Token::Ident, "member name")?;
                    let s = self.slice_interned();

                    let new_span = value.area.span.extend(self.span());
                    value = Expression::Member(value, s).into_node(new_span, self.src);
                }
                Token::OpenParen => {
                    self.next();

                    let mut args = vec![];
                    while self.peek() != Token::ClosedParen {
                        args.push(self.parse_expr()?);
                        if !self.skip_tok(Token::Comma) {
                            break;
                        }
                    }
                    self.expect_tok(Token::ClosedParen)?;

                    let new_span = value.area.span.extend(self.span());
                    value = Expression::Call(value, args).into_node(new_span, self.src);
                }
                _ => break,
            }
        }
        Ok(value)
    }

    pub fn parse_op(&mut self, prec: usize) -> Result<ExprNode, SyntaxError> {
        let next_prec = operators::next_infix(prec);

        let mut left = match next_prec {
            Some(next_prec) => self.parse_op(next_prec)?,
            None => self.parse_value()?,
        };
        while operators::is_infix_prec(self.peek(), prec) {
            let op = self.next();
            let right = if operators::prec_type(prec) == operators::OpType::Left {
                match next_prec {
                    Some(next_prec) => self.parse_op(next_prec)?,
                    None => self.parse_value()?,
                }
            } else {
                self.parse_op(prec)?
            };
            let new_span = left.area.span.extend(right.area.span);
            left = Expression::Op(left, op, right).into_node(new_span, self.src)
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
            Token::Func => {
                self.next();

                self.expect_tok_named(Token::Ident, "function name")?;
                let s = self.slice_interned();
                self.expect_tok(Token::OpenParen)?;

                func_parser!(self => args, ret);

                let code = self.parse_expr()?;

                Statement::Func {
                    name: s,
                    args,
                    code,
                    ret,
                }
            }
            _ => {
                let expr = self.parse_expr()?;
                Statement::Expr(expr)
            }
        };
        let is_ret = if self.next_is(Token::Semicolon) {
            self.next();
            false
        } else {
            true
        };
        Ok((stmt.into_node(start.extend(self.span()), self.src), is_ret))
    }

    pub fn parse_stmts(&mut self) -> Result<ListNode, SyntaxError> {
        let mut stmts = vec![];
        let mut span = self.span();
        (span.start, span.end) = (span.start + 1, span.start + 1);
        while ![Token::ClosedBracket, Token::Eof].contains(&self.peek()) {
            let (stmt, is_ret) = self.parse_stmt()?;
            if is_ret {
                let span = stmt.area.span;
                return Ok(StatementList::Ret(stmts, stmt).into_node(span, self.src));
            } else {
                span = stmt.area.span;
                stmts.push(stmt);
            }
        }
        Ok(StatementList::Normal(stmts).into_node(span, self.src))
    }

    pub fn parse(&mut self) -> Result<ListNode, SyntaxError> {
        let list = self.parse_stmts()?;
        self.expect_tok(Token::Eof)?;
        Ok(list)
    }
}
