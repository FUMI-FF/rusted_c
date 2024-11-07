use crate::parser::{digit, literal, one_of, symbol, whitespaces, Parser};

#[derive(Debug, PartialEq)]
pub enum Token {
    Int(i32),
    Symbol(u8),
    Keyword(String),
}

fn digit_token<'a>() -> Parser<'a, u8, Token> {
    (whitespaces() & digit()).map(|(_, val)| Token::Int(val))
}

fn operator_token<'a>() -> Parser<'a, u8, Token> {
    (whitespaces() & one_of(b"+-*/")).map(|(_, op)| Token::Symbol(op))
}

fn keyword_token<'a>(keyword: &'a [u8]) -> Parser<'a, u8, Token> {
    (whitespaces() & literal(keyword)).map(|(_, keyword)| Token::Keyword(keyword))
}

fn symbol_token<'a>(expected: u8) -> Parser<'a, u8, Token> {
    (whitespaces() & symbol(expected)).map(|(_, ch)| Token::Symbol(ch))
}

// "return" digit ([+-*/] digit)* ';'
pub fn tokenizer<'a>() -> Parser<'a, u8, Vec<Token>> {
    let parser = (keyword_token(b"return") & digit_token())
        & (operator_token() & digit_token()).repeat0()
        & symbol_token(b';');
    parser.map(|(((keyword, num), op_and_nums), semi)| {
        let mut vec = vec![keyword, num];
        vec.extend(op_and_nums.into_iter().flat_map(|(op, num)| vec![op, num]));
        vec.push(semi);
        vec
    })
}

#[test]
fn test_tokenize() {
    let tokenizer = tokenizer();
    assert_eq!(
        tokenizer.parse(b"return 1 + 1;"),
        Ok((
            "".as_bytes(),
            vec![
                Token::Keyword("return".to_string()),
                Token::Int(1),
                Token::Symbol(b'+'),
                Token::Int(1),
                Token::Symbol(b';'),
            ]
        ))
    );
}
