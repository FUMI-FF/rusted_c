use crate::parser::{ParseError, Parser};
use crate::tokenizer::Token;

#[derive(Debug, PartialEq, Eq)]
pub enum Node {
    Int(i32),
    Binary {
        op: String,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Ident(String),
    Return(Box<Node>),
    CompoundStmt(Vec<Node>),
    ExprStmt(Box<Node>),
    IfStmt {
        cond: Box<Node>,
        then: Box<Node>,
    },
}

pub fn ast_parser<'a>() -> Parser<'a, Token, Node> {
    compound_stmt()
}

// compound_stmt := stmt{1,n}
fn compound_stmt<'a>() -> Parser<'a, Token, Node> {
    stmt().repeat1().map(|nodes| Node::CompoundStmt(nodes))
}

// stmt := return_stmt | expr_stmt | if_stmt
fn stmt<'a>() -> Parser<'a, Token, Node> {
    return_stmt() | expr_stmt() | if_stmt()
}

// return_stmt := "return" assign ";"
fn return_stmt<'a>() -> Parser<'a, Token, Node> {
    (expect_keyword("return") & assign() & expect_symbol(";"))
        .map(|((_, node), _)| Node::Return(Box::new(node)))
}

// expr_stmt := assign ";"
fn expr_stmt<'a>() -> Parser<'a, Token, Node> {
    (assign() & expect_symbol(";")).map(|(node, _)| Node::ExprStmt(Box::new(node)))
}

// if_stmt := if_stmt_terminal | if_stmt_non_terminal
fn if_stmt<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|input| {
        if let Ok(val) = if_stmt_terminal().parse(input) {
            return Ok(val);
        }
        if matches!(input.get(0), Some(Token::Keyword(val)) if val == "if") {
            return if_stmt_non_terminal().parse(input);
        }
        Err(ParseError::new(
            "Expected valid if-statement.".to_string(),
            input,
        ))
    })
}

// if_stmt_terminal := "if" "(" expr ")" return_stmt | expr_stmt
fn if_stmt_terminal<'a>() -> Parser<'a, Token, Node> {
    expect_keyword("if")
        .ignore_then(expect_symbol("("))
        .ignore_then(expr())
        .then_ignore(expect_symbol(")"))
        .then(return_stmt() | expr_stmt())
        .map(|(cond, then)| Node::IfStmt {
            cond: Box::new(cond),
            then: Box::new(then),
        })
}

// if_stmt_non_terminal := "if" "(" expr ")" stmt
fn if_stmt_non_terminal<'a>() -> Parser<'a, Token, Node> {
    expect_keyword("if")
        .ignore_then(expect_symbol("("))
        .ignore_then(expr())
        .then_ignore(expect_symbol(")"))
        .then(stmt())
        .map(|(cond, then)| Node::IfStmt {
            cond: Box::new(cond),
            then: Box::new(then),
        })
}

fn expect_symbol<'a>(expected: &str) -> Parser<'a, Token, ()> {
    expect(Token::Symbol(expected.to_string()))
}

fn expect_keyword<'a>(expected: &str) -> Parser<'a, Token, ()> {
    expect(Token::Keyword(expected.to_string()))
}

fn expect<'a>(expected: Token) -> Parser<'a, Token, ()> {
    Parser::new(move |tokens: &[Token]| match tokens.get(0) {
        Some(tk) if *tk == expected => Ok((&tokens[1..], ())),
        Some(tk) => Err(ParseError::new(
            format!("{:?} is expected, but got {:?}", expected, tk),
            tokens,
        )),
        None => Err(ParseError::new(
            format!("{:?} is expected but got nothing", expected),
            tokens,
        )),
    })
}

// assign := expr | expr "=" expr ;
fn assign<'a>() -> Parser<'a, Token, Node> {
    (expr() & symbol(vec!["=".to_string()]) & expr()).map(|((lhs, op), rhs)| Node::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }) | expr()
}

// expr := mul ((+|-) mul)
fn expr<'a>() -> Parser<'a, Token, Node> {
    (mul() & (symbol(vec!["+".to_string(), "-".to_string()]) & mul()).repeat0()).map(
        |(mut lhs, op_and_nums)| {
            for (op, rhs) in op_and_nums {
                lhs = Node::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }
            lhs
        },
    )
}

// mul := term ((*|/) term)*
fn mul<'a>() -> Parser<'a, Token, Node> {
    (term() & (symbol(vec!["*".to_string(), "/".to_string()]) & term()).repeat0()).map(
        |(mut lhs, op_and_nums)| {
            for (op, rhs) in op_and_nums {
                lhs = Node::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }
            lhs
        },
    )
}

// term := number | ident | "(" assign ")"
fn term<'a>() -> Parser<'a, Token, Node> {
    let paren = Parser::new(move |tokens: &[Token]| match tokens.get(0) {
        Some(Token::Symbol(sym)) if sym == "(" => {
            let parser = assign() & expect_symbol(")");
            return parser.map(|(node, _)| node).parse(&tokens[1..]);
        }
        _ => Err(ParseError::new("( is expected".to_string(), tokens)),
    });

    paren | number() | ident()
}

// number := [1-9][0-9]*
fn number<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|tokens: &[Token]| match tokens.get(0) {
        Some(Token::Int(val)) => Ok((&tokens[1..], Node::Int(*val))),
        Some(token) => Err(ParseError::new(
            format!("number is expected but got {:?}", token),
            tokens,
        )),
        None => Err(ParseError::new(
            "number is expected but got nothing".to_string(),
            tokens,
        )),
    })
}

fn symbol<'a>(set: Vec<String>) -> Parser<'a, Token, String> {
    Parser::new(move |tokens: &[Token]| match tokens.get(0) {
        Some(Token::Symbol(op)) if set.contains(op) => Ok((&tokens[1..], op.to_owned())),
        _ => Err(ParseError::new(format!("{:?} is expected", set), tokens)),
    })
}

fn ident<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|tokens: &[Token]| match tokens.get(0) {
        Some(Token::Ident(id)) => Ok((&tokens[1..], Node::Ident(id.to_string()))),
        Some(token) => Err(ParseError::new(
            format!("ident is expected but got {:?}", token),
            tokens,
        )),
        None => Err(ParseError::new(
            "ident is expected but got nothing".to_string(),
            tokens,
        )),
    })
}

#[test]
fn test_number() {
    let number = number();
    let empty: &[Token] = &[];
    assert_eq!(number.parse(&[Token::Int(10)]), Ok((empty, Node::Int(10))));
}

#[test]
fn test_ident() {
    let ident = ident();
    let empty: &[Token] = &[];
    assert_eq!(
        ident.parse(&[Token::Ident("a".to_string())]),
        Ok((empty, Node::Ident("a".to_string())))
    )
}
