use logos::Logos;

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token {
    #[regex(r#"\d+"#, priority = 2)]
    Int,
    #[regex(r#"\d+(\.[\d]+)?"#)]
    Float,
    #[regex(r#""(?:\\.|[^\\"])*"|'(?:\\.|[^\\'])*'"#)]
    String,

    #[token("true")]
    True,
    #[token("false")]
    False,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Mult,
    #[token("/")]
    Div,
    #[token("%")]
    Mod,
    #[token("^")]
    Pow,

    #[token("!")]
    ExclMark,

    #[token("=")]
    Assign,
    #[token("==")]
    Eq,
    #[token("!=")]
    NotEq,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEq,
    #[token("<")]
    Lesser,
    #[token("<=")]
    LesserEq,

    #[token("|")]
    Pipe,

    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    MultEq,
    #[token("/=")]
    DivEq,
    #[token("%=")]
    ModEq,
    #[token("^=")]
    PowEq,

    #[token("(")]
    OpenParen,
    #[token(")")]
    ClosedParen,
    #[token("[")]
    OpenSqBracket,
    #[token("]")]
    ClosedSqBracket,
    #[token("{")]
    OpenBracket,
    #[token("}")]
    ClosedBracket,

    #[token("let")]
    Let,

    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("elif")]
    Elif,

    #[token("while")]
    While,
    #[token("for")]
    For,

    #[token("func")]
    Func,

    #[token("return")]
    Return,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,

    #[token("import")]
    Import,
    #[token("export")]
    Export,

    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,

    #[token(".")]
    Dot,
    #[token("..")]
    DoubleDot,

    #[token("=>")]
    LargeArrow,
    #[token("->")]
    Arrow,

    #[token("is")]
    Is,
    #[token("as")]
    As,
    #[token("in")]
    In,

    #[regex(r"[a-zA-Z_][a-zA-Z_0-9]*")]
    Ident,

    #[error]
    #[regex(r"[ \t\f\n\r]+|/\*[^*]*\*(([^/\*][^\*]*)?\*)*/|//[^\n]*", logos::skip)]
    Error,

    Eof,
}

impl Token {
    pub fn name(self) -> &'static str {
        match self {
            Token::Int => "int literal",
            Token::Float => "float literal",
            Token::String => "string literal",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Mult => "*",
            Token::Div => "/",
            Token::Mod => "%",
            Token::Pow => "^",
            Token::ExclMark => "!",
            Token::Assign => "=",
            Token::Eq => "==",
            Token::NotEq => "!=",
            Token::Greater => ">",
            Token::GreaterEq => ">=",
            Token::Lesser => "<",
            Token::LesserEq => "<=",
            Token::PlusEq => "+=",
            Token::MinusEq => "-=",
            Token::MultEq => "*=",
            Token::DivEq => "/=",
            Token::ModEq => "%=",
            Token::PowEq => "^=",
            Token::Pipe => "|",
            Token::Ident => "identifier",
            Token::Error => "unexpected character",
            Token::Eof => "end of file",
            Token::True => "true",
            Token::False => "false",
            Token::OpenParen => "(",
            Token::ClosedParen => ")",
            Token::OpenSqBracket => "[",
            Token::ClosedSqBracket => "]",
            Token::OpenBracket => "{",
            Token::ClosedBracket => "}",
            Token::Semicolon => ";",
            Token::Colon => ":",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::DoubleDot => "..",
            Token::LargeArrow => "=>",
            Token::Arrow => "->",
            Token::Is => "is",
            Token::As => "as",
            Token::In => "in",
            Token::Let => "let",
            Token::If => "if",
            Token::Else => "else",
            Token::Elif => "elif",
            Token::While => "while",
            Token::For => "for",
            Token::Func => "func",
            Token::Break => "break",
            Token::Continue => "continue",
            Token::Return => "return",
            Token::Import => "import",
            Token::Export => "export",
        }
    }
}
