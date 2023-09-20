// EBNF notation:
//
// Program ::= { Statement } ;
//
//
// Statements ::= Statement , { Statement } ;
// Statement ::= SimpleStatement , NewLine | CompoundStatement ;
//
// SimpleStatement ::= Assign | Return | ExprStatement ;
// CompoundStatement ::= IfStatement | WhileStatement ;
//
// Assign ::= Ident , [ ":" , Expr ] , "=" , Expr ;
// Return ::= "return" , Expr ;
// ExprStatement ::= Expr ;
//
// IfStatement ::= "if" , Expr , ":" , Block , ( ElifStatement | [ ElseBlock ] ) ;
// ElifStatement ::= "elif" , Expr , ":" , Block ( ElifStatement | [ ElseBlock ] ) ;
// ElseBlock ::= "else" , ":" , Block ;
// WhileStatement ::= "while" , Expr , ":" , Block ;
//
// Block ::= NewLine , Indent , Statements , Dedent ;
//
//
// Expr ::= Identifier | IntegerLiteral | StringLiteral ;
// Identifier ::= ( letter | "_" ) , { letter | "_" | digit } ;
// IntegerLiteral ::= Digit , { Digit } ;
//
//
// NewLine ::= "\n" ;
// Digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
// Letter = "A" | "B" | "C" | "D" | "E" | "F" | "G"
//        | "H" | "I" | "J" | "K" | "L" | "M" | "N"
//        | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
//        | "V" | "W" | "X" | "Y" | "Z" | "a" | "b"
//        | "c" | "d" | "e" | "f" | "g" | "h" | "i"
//        | "j" | "k" | "l" | "m" | "n" | "o" | "p"
//        | "q" | "r" | "s" | "t" | "u" | "v" | "w"
//        | "x" | "y" | "z" ;

pub mod token;
use token::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Node {
    kind: Box<NodeKind>,
}

impl Node {
    fn from(kind: NodeKind) -> Self {
        Node {
            kind: Box::new(kind),
        }
    }
}

fn new_node_vec(kind: NodeKind) -> Vec<Node> {
    vec![Node::from(kind)]
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Stmt(StmtKind),
    Expr(ExprKind),
    Program(Vec<Node>),
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Assignment {
        name: String,
        expr: Node,
        type_annotation: Option<Node>,
    },
    Return {
        expr: Node,
    },
    ExprStmt {
        expr: Node,
    },
    While {
        expr: Node,
        block: Node,
    },
    If {
        condition: Node,
        consequent: Node,
        alternative: Option<Node>,
    },
    Elif {
        condition: Node,
        consequent: Node,
        alternative: Option<Node>,
    },
    Else {
        block: Node,
    },
    Block {
        statements: Vec<Node>,
    },
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Identifier(String),
    IntLiteral(u32),
    StrLiteral(String),
}

type RawParserRule = fn(input: &[Token]) -> Option<(Vec<Node>, &[Token])>;
type ParserRule<'a> = Box<dyn Fn(&[Token]) -> Option<(Vec<Node>, &[Token])> + 'a>;

pub fn parse(input: &[Token]) -> Option<Node> {
    Some(Node {
        kind: Box::new(NodeKind::Program(
            collect_until_no_match(wrap(stmt))(input)?.0,
        )),
    })
}

fn stmt(input: &[Token]) -> Option<(Vec<Node>, &[Token])> {
    let Some((nodes, input)) = sequence(vec![
        any(vec![
            sequence(vec![
                any(wrap_many(SIMPLE_STMTS)),
                token(TokenKind::NewLine)
            ]),
            any(wrap_many(COMPOUND_STMTS))
        ]),
        optional(collect_until_no_match(token(TokenKind::NL)))
    ])(input)
    else { return None; };

    Some((vec![nodes.into_iter().nth(0)?], input))
}

const SIMPLE_STMTS: &[RawParserRule] = &[stmt_assignment, stmt_return, stmt_expr];
const COMPOUND_STMTS: &[RawParserRule] = &[stmt_while, stmt_if];

fn stmt_return(input: &[Token]) -> Option<(Vec<Node>, &[Token])> {
    let Some((nodes, input)) = sequence(vec![
        named_ident("return"),
        wrap(expr)
      ]
    )(input)
    else { return None; };

    Some((
        new_node_vec(NodeKind::Stmt(StmtKind::Return {
            expr: nodes.into_iter().nth(1)?,
        })),
        input,
    ))
}

fn stmt_assignment(input: &[Token]) -> Option<(Vec<Node>, &[Token])> {
    let Some((nodes, input)) = sequence(vec![
        wrap(expr_ident),
        optional(sequence(vec![
            token(TokenKind::Colon),
            wrap(expr)
        ])),
        token(TokenKind::Equals),
        wrap(expr)
    ])(input)
    else { return None; };

    let name = match *nodes.clone().into_iter().nth(0)?.kind {
        NodeKind::Expr(kind) => match kind {
            ExprKind::Identifier(name) => name,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    }
    .to_string();

    let type_annotation = if nodes.len() == 3 {
        Some(nodes.clone().into_iter().nth(1)?)
    } else {
        None
    };

    Some((
        new_node_vec(NodeKind::Stmt(StmtKind::Assignment {
            name,
            expr: nodes.into_iter().last()?,
            type_annotation,
        })),
        input,
    ))
}

fn stmt_expr(input: &[Token]) -> Option<(Vec<Node>, &[Token])> {
    let Some((expr, input)) = expr(input)
    else { return None; };

    Some((
        new_node_vec(NodeKind::Stmt(StmtKind::ExprStmt {
            expr: expr.into_iter().nth(0)?,
        })),
        input,
    ))
}

fn stmt_while(input: &[Token]) -> Option<(Vec<Node>, &[Token])> {
    let Some((nodes, input)) = sequence(vec![
        named_ident("while"),
        wrap(expr),
        token(TokenKind::Colon),
        wrap(stmt_block),
    ])(input)
    else { return None; };

    Some((
        new_node_vec(NodeKind::Stmt(StmtKind::While {
            expr: nodes.clone().into_iter().nth(1)?,
            block: nodes.into_iter().nth(2)?,
        })),
        input,
    ))
}

fn stmt_if(input: &[Token]) -> Option<(Vec<Node>, &[Token])> {
    // TODO: Add support for `elif`
    let Some((nodes, input)) = sequence(vec![
        named_ident("if"),
        wrap(expr),
        token(TokenKind::Colon),
        wrap(stmt_block),
        optional(sequence(vec![
            named_ident("else"),
            token(TokenKind::Colon),
            wrap(stmt_block),
        ]))
    ])(input)
    else { return None; };

    Some((
        new_node_vec(NodeKind::Stmt(StmtKind::If {
            condition: nodes.clone().into_iter().nth(1)?,
            consequent: nodes.clone().into_iter().nth(2)?,
            alternative: nodes.into_iter().nth(4),
        })),
        input,
    ))
}

fn stmt_block(input: &[Token]) -> Option<(Vec<Node>, &[Token])> {
    let Some((nodes, input)) = sequence(vec![
        token(TokenKind::NewLine),
        token(TokenKind::Indent),
        collect_until_no_match(wrap(stmt)),
        token(TokenKind::Dedent),
    ])(input)
    else { return None; };

    Some((
        new_node_vec(NodeKind::Stmt(StmtKind::Block { statements: nodes })),
        input,
    ))
}

fn expr(input: &[Token]) -> Option<(Vec<Node>, &[Token])> {
    any(wrap_many(EXPRS))(input)
}

const EXPRS: &[RawParserRule] = &[expr_ident, expr_intlit, expr_strlit];

fn expr_ident(input: &[Token]) -> Option<(Vec<Node>, &[Token])> {
    let TokenKind::Identifier(name) = &input.first()?.kind
    else { return None; };
    let input = consume_first(input);

    Some((
        new_node_vec(NodeKind::Expr(ExprKind::Identifier(name.to_string()))),
        input,
    ))
}

fn expr_strlit(input: &[Token]) -> Option<(Vec<Node>, &[Token])> {
    let TokenKind::StringLiteral(string) = &input.first()?.kind
    else { return None; };
    let input = consume_first(input);

    Some((
        new_node_vec(NodeKind::Expr(ExprKind::StrLiteral(string.to_string()))),
        input,
    ))
}

fn expr_intlit(input: &[Token]) -> Option<(Vec<Node>, &[Token])> {
    let TokenKind::IntegerLiteral(int) = &input.first()?.kind
    else { return None; };
    let input = consume_first(input);

    let number: u32 = int.parse().unwrap();
    Some((
        new_node_vec(NodeKind::Expr(ExprKind::IntLiteral(number))),
        input,
    ))
}

fn any<'a>(rules: Vec<ParserRule<'a>>) -> ParserRule<'a> {
    Box::new(move |input| Some(rules.iter().find_map(|rule| rule(input))?))
}

fn token<'a>(token_kind: TokenKind) -> ParserRule<'a> {
    Box::new(move |input| {
        if token_kind != input.first()?.kind {
            return None;
        }
        let input = consume_first(input);

        Some((Vec::new(), input))
    })
}

fn sequence<'a>(rules: Vec<ParserRule<'a>>) -> ParserRule<'a> {
    Box::new(move |input| sequence_body(input, &rules, Vec::new()))
}

fn sequence_body<'a>(
    input: &'a [Token],
    rules: &[ParserRule],
    nodes: Vec<Node>,
) -> Option<(Vec<Node>, &'a [Token])> {
    if rules.len() == 0 {
        return Some((nodes, input));
    }

    let Some((new_nodes, input)) = rules.first()?(input)
    else { return None; };

    let rules = consume_first(rules);
    let nodes = [nodes, new_nodes].concat();

    sequence_body(input, rules, nodes)
}

fn optional(rule: ParserRule) -> ParserRule {
    Box::new(move |input| Some(rule(input).unwrap_or((Vec::new(), input))))
}

fn named_ident(str: &str) -> ParserRule {
    Box::new(move |input| {
        let Some((expr, input)) = expr_ident(input)
        else { return None; };

        let NodeKind::Expr(expr_kind) = &(*expr.first()?.clone().kind)
        else { unreachable!() };

        let ExprKind::Identifier(ident_str) = expr_kind
        else { unreachable!() };

        if !str.eq(ident_str) {
            return None;
        }

        Some((expr, input))
    })
}

fn collect_until_no_match(rule: ParserRule) -> ParserRule {
    Box::new(move |input| collect_until_no_match_body(input, &rule, Vec::new()))
}

fn collect_until_no_match_body<'a>(
    input: &'a [Token],
    rule: &ParserRule,
    nodes: Vec<Node>,
) -> Option<(Vec<Node>, &'a [Token])> {
    if input.len() == 0 {
        return Some((nodes, input));
    }

    let Some((new_nodes, input)) = rule(input)
    else { return Some((nodes, input)); };

    let nodes = [nodes, new_nodes].concat();

    collect_until_no_match_body(input, rule, nodes)
}

fn consume_first<T>(arr: &[T]) -> &[T] {
    if let Some((_, arr)) = arr.split_first() {
        arr
    } else {
        &[]
    }
}

/// Wrap a RawParserRule into ParserRule
fn wrap<'a>(rule: RawParserRule) -> ParserRule<'a> {
    Box::new(rule)
}

fn wrap_many(rules: &'static [RawParserRule]) -> Vec<ParserRule<'static>> {
    rules.into_iter().map(map_for_wrap_many).collect::<Vec<_>>()
}

fn map_for_wrap_many(rule: &'static RawParserRule) -> ParserRule<'static> {
    Box::new(rule)
}
