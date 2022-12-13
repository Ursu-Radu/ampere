use ahash::AHashMap;
use lasso::Spur;

use crate::sources::CodeSpan;

use super::lexer::Token;

#[derive(Clone, Debug)]
pub struct ExprNode {
    pub span: CodeSpan,
    pub expr: Box<Expression>,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),

    Op(ExprNode, Token, ExprNode),
    Unary(Token, ExprNode),

    Var(Spur),

    Block(ListNode),

    Array(Vec<ExprNode>),
    Tuple(Vec<ExprNode>),

    Dict(AHashMap<Spur, ExprNode>),

    If {
        branches: Vec<(ExprNode, ListNode)>,
        else_branch: Option<ListNode>,
    },

    While {
        cond: ExprNode,
        code: ListNode,
    },

    Index(ExprNode, ExprNode),
    Member(ExprNode, Spur),
}
impl Expression {
    pub fn into_node(self, span: CodeSpan) -> ExprNode {
        ExprNode {
            span,
            expr: Box::new(self),
        }
    }
}

#[derive(Clone, Debug)]
pub struct StmtNode {
    pub span: CodeSpan,
    pub stmt: Box<Statement>,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expr(ExprNode),
    Let(Spur, ExprNode),
    Print(ExprNode),
}
impl Statement {
    pub fn to_node(self, span: CodeSpan) -> StmtNode {
        StmtNode {
            span,
            stmt: Box::new(self),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ListNode {
    pub span: CodeSpan,
    pub list: Box<StatementList>,
}

#[derive(Clone, Debug)]
pub enum StatementList {
    Normal(Vec<StmtNode>),
    Ret(Vec<StmtNode>, StmtNode),
}
impl StatementList {
    pub fn to_node(self, span: CodeSpan) -> ListNode {
        ListNode {
            span,
            list: Box::new(self),
        }
    }
}
