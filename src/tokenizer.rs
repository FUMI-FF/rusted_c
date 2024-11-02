use crate::parser::{digit, one_of, whitespaces, Parser};

#[derive(Debug, PartialEq)]
pub enum Token {
    Int(i32),
    Symbol(u8),
}

fn digit_token<'a>() -> Parser<'a, u8, Token> {
    (whitespaces() & digit()).map(|(_, val)| Token::Int(val))
}

fn operator_token<'a>() -> Parser<'a, u8, Token> {
    (whitespaces() & one_of(b"+-*")).map(|(_, op)| Token::Symbol(op))
}

pub fn tokenizer<'a>() -> Parser<'a, u8, Vec<Token>> {
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
    assert_eq!(
        tokenizer.parse(b"1x"),
        Ok(("x".as_bytes(), vec![Token::Int(1)]))
    );
}
