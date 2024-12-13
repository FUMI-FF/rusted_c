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
}

pub fn ast_parser<'a>() -> Parser<'a, Token, Node> {
    stmt()
}

// stmt := (assign ";")* "return" assign ";"
fn stmt<'a>() -> Parser<'a, Token, Node> {
    let expr_stmt =
        (assign() & expect(Token::Symbol(b';'))).map(|(node, _)| Node::ExprStmt(Box::new(node)));
    let ret_stmt =
        (expect(Token::Keyword("return".to_string())) & assign() & expect(Token::Symbol(b';')))
            .map(|((_, node), _)| Node::Return(Box::new(node)));

    (expr_stmt.repeat0() & ret_stmt).map(|(mut nodes, ret_node)| {
        nodes.push(ret_node);
        Node::CompoundStmt(nodes)
    })
}

fn expect<'a>(expected: Token) -> Parser<'a, Token, ()> {
    Parser::new(move |tokens: &[Token]| match tokens.get(0) {
        Some(tk) if *tk == expected => Ok((&tokens[1..], ())),
        Some(tk) => Err(ParseError::new(format!(
            "{:?} is expected, but got {:?}",
            expected, tk
        ))),
        None => Err(ParseError::new(format!(
            "{:?} is expected but got nothing",
            expected
        ))),
    })
}

// assign := expr | expr "=" expr ;
fn assign<'a>() -> Parser<'a, Token, Node> {
    (expr() & symbol(b"=") & expr()).map(|((lhs, op), rhs)| Node::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }) | expr()
}

// expr := mul ((+|-) mul)
fn expr<'a>() -> Parser<'a, Token, Node> {
    (mul() & (symbol(b"+-") & mul()).repeat0()).map(|(mut lhs, op_and_nums)| {
        for (op, rhs) in op_and_nums {
            lhs = Node::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        }
        lhs
    })
}

// mul := term ((*|/) term)*
fn mul<'a>() -> Parser<'a, Token, Node> {
    (term() & (symbol(b"*/") & term()).repeat0()).map(|(mut lhs, op_and_nums)| {
        for (op, rhs) in op_and_nums {
            lhs = Node::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        }
        lhs
    })
}

// term := number | ident | "(" assign ")"
fn term<'a>() -> Parser<'a, Token, Node> {
    let paren = Parser::new(move |tokens: &[Token]| {
        if let Some(Token::Symbol(b'(')) = tokens.get(0) {
            let parser = assign() & expect(Token::Symbol(b')'));
            return parser.map(|(node, _)| node).parse(&tokens[1..]);
        }
        Err(ParseError::new("( is expected".to_string()))
    });

    paren | number() | ident()
}

fn symbol<'a>(set: &'a [u8]) -> Parser<'a, Token, String> {
    Parser::new(move |tokens: &[Token]| match tokens.get(0) {
        Some(Token::Symbol(op)) if set.contains(op) => {
            Ok((&tokens[1..], (*op as char).to_string()))
        }
        _ => Err(ParseError::new(format!("{:?} is expected", set))),
    })
}

// number := [1-9][0-9]*
fn number<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|tokens: &[Token]| match tokens.get(0) {
        Some(Token::Int(val)) => Ok((&tokens[1..], Node::Int(*val))),
        Some(token) => Err(ParseError::new(format!(
            "number is expected but got {:?}",
            token
        ))),
        None => Err(ParseError::new(
            "number is expected but got nothing".to_string(),
        )),
    })
}

fn ident<'a>() -> Parser<'a, Token, Node> {
    Parser::new(|tokens: &[Token]| match tokens.get(0) {
        Some(Token::Ident(id)) => Ok((&tokens[1..], Node::Ident(id.to_string()))),
        Some(token) => Err(ParseError::new(format!(
            "ident is expected but got {:?}",
            token
        ))),
        None => Err(ParseError::new(
            "ident is expected but got nothing".to_string(),
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

#[test]
fn test_mul() {
    let mul = mul();
    let empty: &[Token] = &[];
    assert_eq!(
        mul.parse(&[Token::Int(10), Token::Symbol(b'*'), Token::Int(5)]),
        Ok((
            empty,
            Node::Binary {
                op: "*".to_string(),
                lhs: Box::new(Node::Int(10)),
                rhs: Box::new(Node::Int(5))
            }
        ))
    );
}
