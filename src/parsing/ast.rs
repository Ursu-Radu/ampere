use ahash::AHashMap;
use lasso::Spur;

use crate::sources::{CodeArea, CodeSpan, SourceKey};

use super::lexer::Token;

#[derive(Clone, Debug, PartialEq)]
pub struct ExprNode {
    pub area: CodeArea,
    pub expr: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
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
    For {
        iterator: Spur,
        expr: ExprNode,
        code: ListNode,
    },

    Index(ExprNode, ExprNode),
    Member(ExprNode, Spur),

    Func {
        args: Vec<(Spur, Option<ExprNode>)>,
        code: ExprNode,
        ret: Option<ExprNode>,
    },
    Call(ExprNode, Vec<ExprNode>),

    Return(Option<ExprNode>),
    Break(Option<ExprNode>),
    Continue,

    Import(ExprNode),
}
impl Expression {
    pub fn into_node(self, span: CodeSpan, src: SourceKey) -> ExprNode {
        ExprNode {
            area: CodeArea { span, src },
            expr: Box::new(self),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StmtNode {
    pub area: CodeArea,
    pub stmt: Box<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Expr(ExprNode),
    Let(Spur, ExprNode, bool),
    Func {
        name: Spur,
        args: Vec<(Spur, Option<ExprNode>)>,
        code: ExprNode,
        ret: Option<ExprNode>,
        export: bool,
    },
}
impl Statement {
    pub fn into_node(self, span: CodeSpan, src: SourceKey) -> StmtNode {
        StmtNode {
            area: CodeArea { span, src },
            stmt: Box::new(self),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ListNode {
    pub area: CodeArea,
    pub list: Box<StatementList>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementList {
    Normal(Vec<StmtNode>),
    Ret(Vec<StmtNode>, StmtNode),
}
impl StatementList {
    pub fn into_node(self, span: CodeSpan, src: SourceKey) -> ListNode {
        ListNode {
            area: CodeArea { span, src },
            list: Box::new(self),
        }
    }
}
