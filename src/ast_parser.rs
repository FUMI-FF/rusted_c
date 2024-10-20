use crate::tokenizer::Token;

#[derive(Debug)]
pub enum Node {
    Number(i32),
    Binary {
        op: String,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
}

pub type ASTResult<'a> = Result<(&'a [Token], Node), String>;

pub fn number(tokens: &[Token]) -> ASTResult {
    match tokens[0] {
        Token::Int(val) => Ok((&tokens[1..], Node::Number(val))),
        _ => Err(format!("number expected, but got {:?}", tokens[0])),
    }
}

pub fn expr<'a>(tokens: &'a [Token]) -> ASTResult<'a> {
    let (mut cur, mut lhs) = number(tokens).expect("must be a number");

    while cur.len() > 0 {
        let op = match &cur[0] {
            Token::Symbol(op) => op,
            _ => return Err(format!("operator exepcted, but got {:?}", tokens[0])),
        };
        let (next, number) = number(&cur[1..]).expect("must be a number");
        lhs = Node::Binary {
            op: op.clone(),
            lhs: Box::new(lhs),
            rhs: Box::new(number),
        };
        cur = next;
    }

    Ok((cur, lhs))
}
