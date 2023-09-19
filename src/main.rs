mod token;

use exp_parser_1::parse;
use exp_parser_1::token::{Token, TokenKind};

fn tokens_from(token_kinds: Vec<TokenKind>) -> Vec<Token> {
    token_kinds.into_iter().map(|kind| Token { kind }).collect()
}

fn main() {
    let tokens = tokens_from(vec![
        TokenKind::Identifier("foo".into()),
        TokenKind::Equals,
        TokenKind::IntegerLiteral("0".into()),
        TokenKind::NewLine,
        TokenKind::Identifier("bar".into()),
        TokenKind::Equals,
        TokenKind::IntegerLiteral("5".into()),
        TokenKind::NewLine,
        TokenKind::IntegerLiteral("5".into()),
        TokenKind::NewLine,
        TokenKind::Identifier("return".into()),
        TokenKind::IntegerLiteral("5".into()),
        TokenKind::NewLine,
    ]);

    let res = parse(&tokens);

    println!("{:#?}", res);
}
