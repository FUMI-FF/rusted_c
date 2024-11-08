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

// digit ([+-*/] digit)* ';'
fn expr_tokens<'a>() -> Parser<'a, u8, Vec<Token>> {
    (digit_token() & (operator_token() & digit_token()).repeat0() & symbol_token(b';')).map(
        |((num, pairs), sym)| {
            let mut vec = vec![num];
            vec.extend(pairs.into_iter().flat_map(|(fst, snd)| vec![fst, snd]));
            vec.push(sym);
            vec
        },
    )
}

// "return" digit ([+-*/] digit)* ';'
fn return_tokens<'a>() -> Parser<'a, u8, Vec<Token>> {
    (keyword_token(b"return") & expr_tokens()).map(|(keyword, tokens)| {
        let mut vec = vec![keyword];
        vec.extend(tokens);
        vec
    })
}

// expr_tokens* return_tokens
pub fn tokenizer<'a>() -> Parser<'a, u8, Vec<Token>> {
    (expr_tokens().repeat0() & return_tokens()).map(|(tokens, ret_tokens)| {
        let mut vec: Vec<Token> = tokens.into_iter().flat_map(|vec| vec).collect();
        vec.extend(ret_tokens);
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
