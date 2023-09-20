use std::borrow::Cow;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Identifier(String),
    IntegerLiteral(String),
    StringLiteral(String),

    /// \n
    NewLine,

    NL,

    Indent,

    Dedent,

    /// "("
    LParen,

    /// ")"
    RParen,

    /// "["
    LBracket,

    /// "]"
    RBracket,

    /// "{"
    LBrace,

    /// "}"
    RBrace,

    /// ":"
    Colon,

    /// ";"
    Semi,

    /// "."
    Dot,

    /// ","
    Comma,

    /// "+"
    Plus,

    /// "-"
    Minus,

    /// "*"
    Star,

    /// "/"
    Slash,

    /// "%"
    Percent,

    /// "^"
    Caret,

    /// "&"
    Amper,

    /// "|"
    Pipe,

    /// "~"
    Tilde,

    /// "="
    Equals,

    /// "<"
    Less,

    /// ">"
    Greater,

    /// "!"
    Not,

    /// "@"
    At,

    /// "->"
    RArrow,

    /// "=="
    EqualsEquals,

    /// "!="
    NotEquals,

    /// "<="
    LessEquals,

    /// ">="
    GreaterEquals,

    /// "<<"
    LShift,

    /// ">>"
    RShift,

    /// "**"
    StarStar,

    /// "+="
    PlusEquals,

    /// "-="
    MinusEquals,

    /// "*="
    StarEquals,

    /// "/="
    SlashEquals,

    /// "%="
    PercentEquals,

    /// "&="
    AmperEquals,

    /// "|="
    PipeEquals,

    /// "^="
    CaretEquals,

    /// "<<="
    LShiftEquals,

    /// ">>="
    RShiftEquals,

    /// "**="
    StarStarEquals,

    Unknown,
}
