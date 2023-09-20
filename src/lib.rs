// EBNF notation:
//
// Program ::= { Statement } ;
//
// Statement ::= ( Assign | Return | ExprStatement ) , NewLine ;
// Assign ::= Ident , "=" , Expr ;
// Return ::= "return" , Expr ;
// ExprStatement ::= Expr ;
//
// Expr ::= Identifier | IntegerLiteral | StringLiteral ;
// Identifier ::= ( letter | "_" ) , { letter | "_" | digit } ;
// IntegerLiteral ::= Digit , { Digit } ;
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

#[derive(Debug, Clone)]
pub enum NodeKind {
    Stmt(StmtKind),
    Expr(ExprKind),
    Program(Vec<Node>),
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Assignment { name: String, expr: Node },
    Return { expr: Node },
    ExprStmt { expr: Node },
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Identifier(String),
    IntLiteral(u32),
    StrLiteral(String),
}

type RawParserRule = fn(input: &[Token]) -> Option<(Node, &[Token])>;
type ParserRule<'a> = Box<dyn Fn(&[Token]) -> Option<(Node, &[Token])> + 'a>;

pub fn parse(input: &[Token]) -> Node {
    Node {
        kind: Box::new(NodeKind::Program(parse_body(input, Vec::new()))),
    }
}

fn parse_body<'a>(input: &'a [Token], nodes: Vec<Node>) -> Vec<Node> {
    if input.len() == 0 {
        return nodes;
    }

    let stmt_res = rule_stmt(input);

    if stmt_res.is_none() {
        return nodes;
    }

    let (node, input) = stmt_res.unwrap();

    let nodes = [nodes, vec![node]].concat();

    parse_body(input, nodes)
}

fn rule_stmt(input: &[Token]) -> Option<(Node, &[Token])> {
    let Some((stmt, input)) = any(box_rules(STMT_RULES))(input)
    else { return None; };

    let TokenKind::NewLine = input[0].kind
    else { return None; };
    let input = consume_first(input);

    Some((stmt, input))
}

const STMT_RULES: &[RawParserRule] = &[rule_assignment, rule_return, rule_expr_stmt];

fn rule_return(input: &[Token]) -> Option<(Node, &[Token])> {
    let Some((_, input)) = ident_of_str("return")(input)
    else { return None; };

    let Some((expr, input)) = rule_expr(input)
    else { return None; };

    Some((Node::from(NodeKind::Stmt(StmtKind::Return { expr })), input))
}

fn rule_assignment(input: &[Token]) -> Option<(Node, &[Token])> {
    let TokenKind::Identifier(name) = &input[0].kind
    else { return None; };
    let input = consume_first(input);

    let TokenKind::Equals = input[0].kind
    else { return None; };
    let input = consume_first(input);

    let Some((expr, input)) = rule_expr(input)
    else { return None; };

    Some((
        Node::from(NodeKind::Stmt(StmtKind::Assignment {
            name: name.to_string(),
            expr,
        })),
        input,
    ))
}

fn rule_expr_stmt(input: &[Token]) -> Option<(Node, &[Token])> {
    let Some((expr, input)) = rule_expr(input)
    else { return None; };

    Some((
        Node::from(NodeKind::Stmt(StmtKind::ExprStmt { expr })),
        input,
    ))
}

fn rule_expr(input: &[Token]) -> Option<(Node, &[Token])> {
    any(box_rules(EXPR_RULES))(input)
}

const EXPR_RULES: &[RawParserRule] = &[rule_ident, rule_intliteral, rule_stringliteral];

fn rule_ident(input: &[Token]) -> Option<(Node, &[Token])> {
    let TokenKind::Identifier(name) = &input[0].kind
    else { return None; };
    let input = consume_first(input);

    Some((
        Node::from(NodeKind::Expr(ExprKind::Identifier(name.to_string()))),
        input,
    ))
}

fn rule_stringliteral(input: &[Token]) -> Option<(Node, &[Token])> {
    let TokenKind::StringLiteral(string) = &input[0].kind
    else { return None; };
    let input = consume_first(input);

    Some((
        Node::from(NodeKind::Expr(ExprKind::StrLiteral(string.to_string()))),
        input,
    ))
}

fn rule_intliteral(input: &[Token]) -> Option<(Node, &[Token])> {
    let TokenKind::IntegerLiteral(int) = &input[0].kind
    else { return None; };
    let input = consume_first(input);

    let number: u32 = int.parse().unwrap();
    Some((
        Node::from(NodeKind::Expr(ExprKind::IntLiteral(number))),
        input,
    ))
}

fn any<'a>(rules: Vec<ParserRule<'a>>) -> ParserRule<'a> {
    Box::new(move |input| Some(rules.iter().find_map(|rule| rule(input))?))
}

fn ident_of_str(str: &str) -> ParserRule {
    Box::new(move |input| {
        let Some((expr, input)) = rule_ident(input)
        else { return None; };

        let NodeKind::Expr(expr_kind) = &(*expr.kind)
        else { unreachable!() };

        let ExprKind::Identifier(ident_str) = expr_kind
        else { unreachable!() };

        if !str.eq(ident_str) {
            return None;
        }

        Some((expr, input))
    })
}

fn consume_first<T>(arr: &[T]) -> &[T] {
    if let Some((_, arr)) = arr.split_first() {
        arr
    } else {
        &[]
    }
}

fn box_rule(rule: &'static RawParserRule) -> ParserRule<'static> {
    Box::new(|input| rule(input))
}

fn box_rules(rules: &'static [RawParserRule]) -> Vec<ParserRule<'static>> {
    rules.into_iter().map(box_rule).collect::<Vec<_>>()
}
