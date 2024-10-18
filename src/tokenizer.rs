use crate::parser::{digit, one_of, whitespaces, Parser};

#[derive(Debug, PartialEq)]
pub enum Token {
    Int(i32),
    Symbol(char),
}

fn digit_token<'a>() -> Parser<'a, Token> {
    (whitespaces() & digit()).snd().map(Token::Int)
}

fn operator_token<'a>() -> Parser<'a, Token> {
    (whitespaces() & one_of("+-")).snd().map(Token::Symbol)
}

pub fn tokenizer<'a>() -> Parser<'a, Vec<Token>> {
    let parser = digit_token() & (operator_token() & digit_token()).repeat0();
    parser.map(|(num, rest)| {
        std::iter::once(num)
            .chain(rest.into_iter().flat_map(|(op, dig)| vec![op, dig]))
            .collect()
    })
}

#[test]
fn test_tokenize() {
    let tokenizer = tokenizer();
    assert_eq!(tokenizer.parse("1x"), Ok(("x", vec![Token::Int(1)])));
}
