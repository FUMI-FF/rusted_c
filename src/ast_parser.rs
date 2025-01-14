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
        els: Box<Option<Node>>,
    },
    Call {
        ident: String,
        args: Option<Vec<Node>>,
    },
}

// BNF
// compound_stmt := stmt (stmt)*
// stmt := return_stmt | expr_stmt | if_stmt
// atom_stmt := return_stmt | expr_stmt
// return_stmt := "return" assign ";"
// expr_stmt := assign ";"
// if_stmt := if_stmt_atom | if_stmt_non_atom
// if_stmt_atom := "if" "(" expr ")" atom_stmt "else" atom_stmt
// if_stmt_non_atom := "if" "(" expr ")" stmt "else" stmt
//
// assign := expr ("=" expr ";")?
// expr := mul (("+" | "-") mul)*
// mul := term (("*" | "/") term)*
// term := atom | non_atom
// non_atom := "(" expr ")" | call
// atom := number | ident
// call := ident "(" call_args? ")"
// call_args := expr ("," expr)*
// number := [1-9][0-9]*
// ident := [a-zA-Z][a-zA-Z0-9]*
pub fn ast_parser<'a>() -> Parser<'a, Token, Node> {
    compound_stmt()
}

// compound_stmt := stmt (stmt)*
fn compound_stmt<'a>() -> Parser<'a, Token, Node> {
    stmt().repeat1().map(|nodes| Node::CompoundStmt(nodes))
}

// stmt := return_stmt | expr_stmt | if_stmt
fn stmt<'a>() -> Parser<'a, Token, Node> {
    return_stmt() | expr_stmt() | if_stmt()
}

// atom_stmt := return_stmt | expr_stmt
fn atom_stmt<'a>() -> Parser<'a, Token, Node> {
    return_stmt() | expr_stmt()
}

// return_stmt := "return" assign ";"
fn return_stmt<'a>() -> Parser<'a, Token, Node> {
    keyword("return")
        .ignore_then(assign())
        .then_ignore(sym(";"))
        .map(|node| Node::Return(Box::new(node)))
}

// expr_stmt := assign ";"
fn expr_stmt<'a>() -> Parser<'a, Token, Node> {
    assign()
        .then_ignore(sym(";"))
        .map(|node| Node::ExprStmt(Box::new(node)))
}

// if_stmt := if_stmt_atom | if_stmt_non_atom
fn if_stmt<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|input| {
        if let Ok(val) = if_stmt_atom().parse(input) {
            return Ok(val);
        }
        if matches!(input.get(0), Some(Token::Keyword(val)) if val == "if") {
            return if_stmt_non_atom().parse(input);
        }
        Err(ParseError::new(
            "Expected valid if-statement.".to_string(),
            input,
        ))
    })
}

// if_stmt_atom := "if" "(" expr ")" atom_stmt "else" atom_stmt
fn if_stmt_atom<'a>() -> Parser<'a, Token, Node> {
    keyword("if")
        .ignore_then(keyword("("))
        .ignore_then(expr())
        .then_ignore(keyword(")"))
        .then(atom_stmt())
        .then((keyword("else").ignore_then(atom_stmt())).opt())
        .map(|((cond, then), els)| Node::IfStmt {
            cond: Box::new(cond),
            then: Box::new(then),
            els: Box::new(els),
        })
}

// if_stmt_non_atom := "if" "(" expr ")" stmt "else" stmt
fn if_stmt_non_atom<'a>() -> Parser<'a, Token, Node> {
    keyword("if")
        .ignore_then(sym("("))
        .ignore_then(expr())
        .then_ignore(sym(")"))
        .then(stmt())
        .then((keyword("else").ignore_then(stmt())).opt())
        .map(|((cond, then), els)| Node::IfStmt {
            cond: Box::new(cond),
            then: Box::new(then),
            els: Box::new(els),
        })
}

// assign := expr ("=" expr ";")?
fn assign<'a>() -> Parser<'a, Token, Node> {
    let assignment = expr()
        .then(sym("="))
        .then(expr())
        .map(|((lhs, op), rhs)| Node::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        });

    assignment | expr()
}

// expr := mul (("+" | "-") mul)*
fn expr<'a>() -> Parser<'a, Token, Node> {
    mul()
        .then(one_of_syms(vec!["+", "-"]).then(mul()).repeat0())
        .map(|(mut lhs, op_and_rhs)| {
            for (op, rhs) in op_and_rhs {
                lhs = Node::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }
            lhs
        })
}

// mul := term (("*" | "/") term)*
fn mul<'a>() -> Parser<'a, Token, Node> {
    term()
        .then(one_of_syms(vec!["*", "/"]).then(term()).repeat0())
        .map(|(mut lhs, op_and_rhs)| {
            for (op, rhs) in op_and_rhs {
                lhs = Node::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }
            lhs
        })
}

// term := atom | non_atom
fn term<'a>() -> Parser<'a, Token, Node> {
    atom() | non_atom()
}

// atom := number | ident
fn atom<'a>() -> Parser<'a, Token, Node> {
    number() | ident()
}

// non_atom := "(" expr ")"
fn non_atom<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|input| {
        let ret = sym("(").parse(input);
        if let Err(err) = ret {
            return Err(err);
        }
        expr().then_ignore(sym(")")).parse(ret.unwrap().0)
    })
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

fn sym<'a>(expected: &'a str) -> Parser<'a, Token, String> {
    Parser::new(move |tokens: &[Token]| match tokens.get(0) {
        Some(Token::Symbol(op)) if op == expected => Ok((&tokens[1..], op.to_owned())),
        _ => Err(ParseError::new(
            format!("{:?} is expected", expected),
            tokens,
        )),
    })
}

fn one_of_syms<'a>(syms: Vec<&'a str>) -> Parser<'a, Token, String> {
    Parser::new(move |tokens: &[Token]| match tokens.get(0) {
        Some(Token::Symbol(op)) if syms.iter().any(|&s| s == op.as_str()) => {
            Ok((&tokens[1..], op.to_owned()))
        }
        _ => Err(ParseError::new(format!("{:?} is expected", syms), tokens)),
    })
}

fn keyword<'a>(expected: &'a str) -> Parser<'a, Token, String> {
    Parser::new(move |tokens: &[Token]| match tokens.get(0) {
        Some(Token::Keyword(op)) if op == expected => Ok((&tokens[1..], op.to_owned())),
        _ => Err(ParseError::new(
            format!("{:?} is expected", expected),
            tokens,
        )),
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
