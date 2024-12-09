use crate::parser::{digit, ident, one_of, whitespace_wrap, Parser};

#[derive(Debug, PartialEq)]
pub enum Token {
    Int(i32),
    Symbol(u8),
    Keyword(String),
    Ident(String),
}

fn digit_token<'a>() -> Parser<'a, u8, Token> {
    whitespace_wrap(digit()).map(|val| Token::Int(val))
}

fn symbol_token<'a>() -> Parser<'a, u8, Token> {
    whitespace_wrap(one_of(b"+-*/;=()")).map(|op| Token::Symbol(op))
}

fn ident_or_keyword_token<'a>() -> Parser<'a, u8, Token> {
    whitespace_wrap(ident()).map(|val| {
        if val == "return" {
            Token::Keyword(val)
        } else {
            Token::Ident(val)
        }
    })
}

pub fn tokenizer<'a>() -> Parser<'a, u8, Vec<Token>> {
    (digit_token() | symbol_token() | ident_or_keyword_token()).repeat1()
}

#[test]
fn test_tokenize() {
    let tokenizer = tokenizer();
    assert_eq!(
        tokenizer.parse(b"x=10;return 1 + 1;"),
        Ok((
            "".as_bytes(),
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(b'='),
                Token::Int(10),
                Token::Symbol(b';'),
                Token::Keyword("return".to_string()),
                Token::Int(1),
                Token::Symbol(b'+'),
                Token::Int(1),
                Token::Symbol(b';'),
            ]
        ))
    );
}
