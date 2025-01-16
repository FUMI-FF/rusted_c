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
    Variable(String),
    Return(Box<Node>),
    CompoundStmt(Vec<Node>),
    ExprStmt(Box<Node>),
    IfStmt {
        cond: Box<Node>,
        then: Box<Node>,
        els: Box<Option<Node>>,
    },
    Call {
        name: String,
        args: Option<Vec<Node>>,
    },
}

// BNF
// compound_stmt := stmt (stmt)*
// stmt := return_stmt | expr_stmt | if_stmt atom_stmt := return_stmt | expr_stmt return_stmt := "return" assign ";"
// expr_stmt := assign ";"
// if_stmt := "if" "(" expr ")" stmt ("else" stmt)?
// assign := expr ("=" expr ";")?
// expr := mul (("+" | "-") mul)*
// mul := term (("*" | "/") term)*
// term := atom | non_atom
// non_atom := "(" expr ")" | call
// atom := number | variable
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

// return_stmt := "return" assign ";"
fn return_stmt<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|input| {
        let (next, _) = keyword("return").parse(input)?;
        let (next, _assign) = assign().parse(next)?;
        let (next, _) = sym(";").parse(next)?;
        Ok((next, Node::Return(Box::new(_assign))))
    })
}

// expr_stmt := assign ";"
fn expr_stmt<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|input| {
        let (next, _assign) = assign().parse(input)?;
        let (next, _) = sym(";").parse(next)?;
        Ok((next, Node::ExprStmt(Box::new(_assign))))
    })
}

// if_stmt :=
fn if_stmt<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|input| {
        let (next, _) = keyword("if").parse(input)?;
        let (next, _) = sym("(").parse(next)?;
        let (next, cond) = expr().parse(next)?;
        let (next, _) = sym(")").parse(next)?;
        let (next, then) = stmt().parse(next)?;
        let (next, _else) = keyword("else").ignore_then(stmt()).opt().parse(next)?;
        Ok((
            next,
            Node::IfStmt {
                cond: Box::new(cond),
                then: Box::new(then),
                els: Box::new(_else),
            },
        ))
    })
}

// assign := expr ("=" expr)?
fn assign<'a>() -> Parser<'a, Token, Node> {
    let _assign = Parser::new(|input| {
        let (next, lhs) = expr().parse(input)?;
        let (next, op) = sym("=").parse(next)?;
        let (next, rhs) = expr().parse(next)?;
        Ok((
            next,
            Node::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        ))
    });

    _assign | expr()
}

// expr := mul (("+" | "-") mul)*
fn expr<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|input| {
        let (next, fst) = mul().parse(input)?;
        let (next, rest) = one_of_syms(vec!["+", "-"])
            .then(mul())
            .repeat0()
            .parse(next)?;
        let node = fold_nodes(fst, rest);
        Ok((next, node))
    })
}

// mul := term (("*" | "/") term)*
fn mul<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|input| {
        let (next, fst) = term().parse(input)?;
        let (next, rest) = one_of_syms(vec!["*", "/"])
            .then(term())
            .repeat0()
            .parse(next)?;
        let node = fold_nodes(fst, rest);
        Ok((next, node))
    })
}

// term := atom | non_atom
fn term<'a>() -> Parser<'a, Token, Node> {
    atom() | non_atom()
}

// atom := number | ident
fn atom<'a>() -> Parser<'a, Token, Node> {
    number() | variable()
}

// non_atom := "(" expr ")" | call
fn non_atom<'a>() -> Parser<'a, Token, Node> {
    let paren = Parser::new(|input| {
        let (next, _) = sym("(").parse(input)?;
        let (next, node) = expr().parse(next)?;
        let (next, _) = sym(")").parse(next)?;
        Ok((next, node))
    });
    paren | call()
}

// call := ident "(" call_args? ")"
fn call<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|input| {
        let (next, name) = ident().parse(input)?;
        let (next, _) = sym("(").parse(next)?;
        let (next, args) = call_args().opt().parse(next)?;
        let (next, _) = sym(")").parse(next)?;
        Ok((next, Node::Call { name, args }))
    })
}

// call_args := expr ("," expr)*
fn call_args<'a>() -> Parser<'a, Token, Vec<Node>> {
    Parser::new(|input| {
        let (next, fst) = expr().parse(input)?;
        let (next, mut rest) = sym(",").ignore_then(expr()).repeat0().parse(next)?;
        rest.insert(0, fst);
        Ok((next, rest))
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

fn variable<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|input| {
        let (next, name) = ident().parse(input)?;
        match next.get(0) {
            Some(Token::Symbol(sym)) if sym == "(" => Err(ParseError::new(
                "variable should not end with paren".to_string(),
                next,
            )),
            _ => Ok((next, Node::Variable(name))),
        }
    })
}

// ident := [a-zA-Z][a-zA-Z0-9]*
fn ident<'a>() -> Parser<'a, Token, String> {
    Parser::new(|tokens: &[Token]| match tokens.get(0) {
        Some(Token::Ident(id)) => Ok((&tokens[1..], id.to_string())),
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

fn fold_nodes(fst: Node, rest: Vec<(String, Node)>) -> Node {
    rest.into_iter().fold(fst, |fst, (op, node)| Node::Binary {
        op,
        lhs: Box::new(fst),
        rhs: Box::new(node),
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
        Ok((empty, "a".to_string()))
    )
}

#[test]
fn test_non_atom() {
    let parser = non_atom();
    let empty: &[Token] = &[];
    assert_eq!(
        parser.parse(&[
            Token::Symbol("(".to_string()),
            Token::Ident("a".to_string()),
            Token::Symbol("+".to_string()),
            Token::Int(1),
            Token::Symbol(")".to_string()),
        ]),
        Ok((
            empty,
            Node::Binary {
                op: "+".to_string(),
                lhs: Box::new(Node::Variable("a".to_string())),
                rhs: Box::new(Node::Int(1))
            }
        ))
    )
}

#[test]
fn test_call_args() {
    let parser = call_args();
    let empty: &[Token] = &[];
    assert_eq!(
        parser.parse(&[
            Token::Ident("x".to_string()),
            Token::Symbol(",".to_string()),
            Token::Int(1),
        ]),
        Ok((empty, vec![Node::Variable("x".to_string()), Node::Int(1)]))
    )
}

#[test]
fn test_call() {
    let parser = call();
    let empty: &[Token] = &[];
    assert_eq!(
        parser.parse(&[
            Token::Ident("add".to_string()),
            Token::Symbol("(".to_string()),
            Token::Int(1),
            Token::Symbol(",".to_string()),
            Token::Int(2),
            Token::Symbol(")".to_string()),
        ]),
        Ok((
            empty,
            Node::Call {
                name: "add".to_string(),
                args: Some(vec![Node::Int(1), Node::Int(2)])
            }
        ))
    )
}
