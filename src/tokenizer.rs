use crate::parser::{digit, one_of, Parser};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Int(i32),
    Symbol(char),
}

fn digit_t<'a>() -> Parser<'a, Token> {
    digit().map(Token::Int)
}

pub fn tokenizer<'a>() -> Parser<'a, Vec<Token>> {
    let operator: Parser<Token> = one_of("+-").map(Token::Symbol);
    let parser = digit_t() & (operator & digit_t()).repeat0();
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
