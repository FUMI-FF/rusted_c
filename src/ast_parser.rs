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
}

pub type ASTResult<'a> = Result<(&'a [Token], Node), String>;

// expr := mul ((+|-) mul)
pub fn expr<'a>() -> Parser<'a, Token, Node> {
    (mul() & (operator(b"+-") & mul()).repeat0()).map(|(mut lhs, op_and_nums)| {
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

// mul := number ((*|/) number)*
pub fn mul<'a>() -> Parser<'a, Token, Node> {
    (number() & (operator(b"*/") & number()).repeat0()).map(|(mut lhs, op_and_nums)| {
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

fn operator<'a>(set: &'a [u8]) -> Parser<'a, Token, String> {
    Parser::new(move |tokens: &[Token]| match tokens.get(0) {
        Some(Token::Symbol(op)) if set.contains(op) => {
            Ok((&tokens[1..], (*op as char).to_string()))
        }
        _ => Err(ParseError::new(format!("{:?} is expected", set))),
    })
}

// number := [1-9][0-9]*
pub fn number<'a>() -> Parser<'a, Token, Node> {
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

#[test]
fn test_number() {
    let number = number();
    let empty: &[Token] = &[];
    assert_eq!(number.parse(&[Token::Int(10)]), Ok((empty, Node::Int(10))));
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

#[test]
fn test_expr() {
    let expr = expr();
    let empty: &[Token] = &[];
    assert_eq!(
        expr.parse(&[
            Token::Int(2),
            Token::Symbol(b'+'),
            Token::Int(3),
            Token::Symbol(b'*'),
            Token::Int(4)
        ]),
        Ok((
            empty,
            Node::Binary {
                op: "+".to_string(),
                lhs: Box::new(Node::Int(2)),
                rhs: Box::new(Node::Binary {
                    op: "*".to_string(),
                    lhs: Box::new(Node::Int(3)),
                    rhs: Box::new(Node::Int(4))
                })
            }
        ))
    );
    assert_eq!(
        expr.parse(&[
            Token::Int(2),
            Token::Symbol(b'*'),
            Token::Int(3),
            Token::Symbol(b'+'),
            Token::Int(4)
        ]),
        Ok((
            empty,
            Node::Binary {
                op: "+".to_string(),
                lhs: Box::new(Node::Binary {
                    op: "*".to_string(),
                    lhs: Box::new(Node::Int(2)),
                    rhs: Box::new(Node::Int(3))
                }),
                rhs: Box::new(Node::Int(4)),
            }
        ))
    );
}
