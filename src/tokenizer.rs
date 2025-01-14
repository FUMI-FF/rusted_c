use crate::parser::{digit, ident, one_of, whitespace_wrap, ParseError, Parser};
use std::collections::HashSet;
use std::str;

#[derive(Debug, PartialEq)]
pub enum Token {
    Int(i32),
    Symbol(String),
    Keyword(String),
    Ident(String),
}

fn digit_token<'a>() -> Parser<'a, u8, Token> {
    whitespace_wrap(digit()).map(|val| Token::Int(val))
}

fn symbol_token<'a>() -> Parser<'a, u8, Token> {
    whitespace_wrap(one_of(b"+-*/;=(),"))
        .map(|op| Token::Symbol(str::from_utf8(&[op]).unwrap().to_string()))
}

fn ident_token<'a>() -> Parser<'a, u8, Token> {
    whitespace_wrap(ident()).map(|val| Token::Ident(val))
}

fn keyword_token<'a>() -> Parser<'a, u8, Token> {
    let set = HashSet::from(["return".to_string(), "if".to_string(), "else".to_string()]);

    Parser::new(move |input: &[u8]| {
        whitespace_wrap(ident())
            .parse(input)
            .and_then(|(rest, val)| {
                if set.contains(&val) {
                    return Ok((rest, Token::Keyword(val)));
                }
                return Err(ParseError::new(
                    format!("unexpected keyword: {}", val),
                    input,
                ));
            })
    })
}

pub fn tokenizer<'a>() -> Parser<'a, u8, Vec<Token>> {
    (digit_token() | symbol_token() | keyword_token() | ident_token()).repeat1()
}

#[test]
fn test_digit_token() {
    let digit = digit_token();
    assert_eq!(digit.parse(b"10"), Ok(("".as_bytes(), Token::Int(10))));
}

#[test]
fn test_symbol_token() {
    let symbol = symbol_token().repeat0();
    assert_eq!(
        symbol.parse(b"+-*/;=()"),
        Ok((
            "".as_bytes(),
            vec![
                Token::Symbol("+".to_string()),
                Token::Symbol("-".to_string()),
                Token::Symbol("*".to_string()),
                Token::Symbol("/".to_string()),
                Token::Symbol(";".to_string()),
                Token::Symbol("=".to_string()),
                Token::Symbol("(".to_string()),
                Token::Symbol(")".to_string()),
            ]
        ))
    )
}

#[test]
fn test_ident_token() {
    let ident = ident_token();
    assert_eq!(
        ident.parse(b"xyz"),
        Ok(("".as_bytes(), Token::Ident("xyz".to_string())))
    )
}

#[test]
fn test_keyword_token() {
    let keyword = keyword_token().repeat0();
    assert_eq!(
        keyword.parse(b"return if"),
        Ok((
            "".as_bytes(),
            vec![
                Token::Keyword("return".to_string()),
                Token::Keyword("if".to_string())
            ]
        ))
    )
}
