use std::ops::{BitAnd, BitOr};

// parser implementation
pub type ParseResult<'a, O> = Result<(&'a str, O), &'a str>;
pub type ParseFn<'a, O> = dyn Fn(&'a str) -> ParseResult<'a, O> + 'a;

pub struct Parser<'a, O> {
    parser: Box<ParseFn<'a, O>>,
}

impl<'a, O: 'a> Parser<'a, O> {
    pub fn new<P>(parser: P) -> Self
    where
        P: Fn(&'a str) -> Result<(&'a str, O), &str> + 'a,
    {
        Self {
            parser: Box::new(parser),
        }
    }

    pub fn parse(&self, input: &'a str) -> ParseResult<'a, O> {
        (self.parser)(input)
    }

    pub fn repeat0(self) -> Parser<'a, Vec<O>> {
        Parser::new(move |input: &'a str| {
            let mut vec: Vec<O> = vec![];
            let mut cur = input;
            while let Ok((next, ret)) = self.parse(cur) {
                vec.push(ret);
                cur = next;
            }
            Ok((cur, vec))
        })
    }

    pub fn repeat1(self) -> Parser<'a, Vec<O>> {
        Parser::new(move |input: &'a str| {
            let mut vec: Vec<O> = vec![];
            let mut cur = input;

            match self.parse(cur) {
                Ok((next, ret)) => {
                    vec.push(ret);
                    cur = next;
                }
                Err(err) => return Err(err),
            }

            while let Ok((next, ret)) = self.parse(cur) {
                vec.push(ret);
                cur = next;
            }
            Ok((cur, vec))
        })
    }

    pub fn map<U, F>(self, f: F) -> Parser<'a, U>
    where
        F: Fn(O) -> U + 'a,
        U: 'a,
    {
        Parser::new(move |input: &str| self.parse(input).map(|(next, ret)| (next, f(ret))))
    }

    pub fn is_a<F>(self, pred: F) -> Parser<'a, O>
    where
        F: Fn(&O) -> bool + 'a,
    {
        Parser::new(move |input| match self.parse(input) {
            Ok((next, ret)) if pred(&ret) => Ok((next, ret)),
            _ => Err(input),
        })
    }
}

impl<'a, A: 'a, B: 'a> Parser<'a, (A, B)> {
    pub fn fst(self) -> Parser<'a, A> {
        self.map(|ret| ret.0)
    }

    pub fn snd(self) -> Parser<'a, B> {
        self.map(|ret| ret.1)
    }
}

impl<'a, A: 'a, B: 'a> BitAnd<Parser<'a, B>> for Parser<'a, A> {
    type Output = Parser<'a, (A, B)>;

    // pair: A & B
    fn bitand(self, rhs: Parser<'a, B>) -> Parser<(A, B)> {
        Parser::new(move |input: &str| match self.parse(input) {
            Ok((next, ret1)) => match rhs.parse(next) {
                Ok((_final, ret2)) => Ok((_final, (ret1, ret2))),
                Err(err) => Err(err),
            },
            Err(err) => Err(err),
        })
    }
}

impl<'a, A: 'a> BitOr<Parser<'a, A>> for Parser<'a, A> {
    type Output = Parser<'a, A>;

    // either: A | B
    fn bitor(self, rhs: Parser<'a, A>) -> Parser<'a, A> {
        Parser::new(move |input: &str| match self.parse(input) {
            Ok((next, ret)) => Ok((next, ret)),
            Err(_) => rhs.parse(input),
        })
    }
}

pub fn symbol<'a>(expected: char) -> Parser<'a, char> {
    Parser::new(move |input: &str| match input.chars().next() {
        Some(ch) if ch == expected => Ok((&input[ch.len_utf8()..], ch)),
        _ => Err(input),
    })
}

pub fn one_of<'a>(set: &'static str) -> Parser<'a, char> {
    Parser::new(move |input: &str| match input.chars().next() {
        Some(ch) if set.contains(ch) => Ok((&input[ch.len_utf8()..], ch)),
        _ => Err(input),
    })
}

pub fn digit<'a>() -> Parser<'a, i32> {
    (one_of("123456789") & one_of("0123456789").repeat0()).map(|(fst, mut rest)| {
        rest.insert(0, fst);
        let s: String = rest.into_iter().collect();
        s.parse::<i32>().unwrap()
    }) | symbol('0').map(|_| 0)
}

pub fn any_char<'a>() -> Parser<'a, char> {
    Parser::new(move |input: &str| match input.chars().next() {
        Some(ch) => Ok((&input[ch.len_utf8()..], ch)),
        _ => Err(input),
    })
}

pub fn whitespaces<'a>() -> Parser<'a, Vec<char>> {
    any_char().is_a(|ch| ch.is_whitespace()).repeat0()
}

#[test]
fn test_symbol() {
    let symbol_parser = symbol('*');
    assert_eq!(symbol_parser.parse("*10"), Ok(("10", '*')));
    assert_eq!(symbol_parser.parse("1+1"), Err("1+1"));
}

#[test]
fn test_one_of() {
    let one_of_parser = one_of("0123456789");
    assert_eq!(one_of_parser.parse("10"), Ok(("0", '1')));
    assert_eq!(one_of_parser.parse("xx"), Err("xx"));
}

#[test]
fn test_repeat0() {
    let repeat0_parser = one_of("0123456789").repeat0();
    assert_eq!(repeat0_parser.parse("123x"), Ok(("x", vec!['1', '2', '3'])));
    assert_eq!(repeat0_parser.parse("xxx"), Ok(("xxx", vec![])));
}

#[test]
fn test_repeat1() {
    let repeat0_parser = one_of("0123456789").repeat1();
    assert_eq!(repeat0_parser.parse("123x"), Ok(("x", vec!['1', '2', '3'])));
    assert_eq!(repeat0_parser.parse("xxx"), Err("xxx"));
}

#[test]
fn test_pair() {
    let p1 = symbol('1');
    let p2 = symbol('x');
    let paired = p1 & p2;
    assert_eq!(paired.parse("1xy"), Ok(("y", ('1', 'x'))));
}

#[test]
fn test_either() {
    let p1 = symbol('x');
    let p2 = symbol('y');
    let either = p1 | p2;
    assert_eq!(either.parse("x1"), Ok(("1", 'x')));
    assert_eq!(either.parse("y1"), Ok(("1", 'y')));
    assert_eq!(either.parse("10"), Err("10"));
}

#[test]
fn test_digit() {
    let digit = digit();
    assert_eq!(digit.parse("0x"), Ok(("x", 0)));
    assert_eq!(digit.parse("1x"), Ok(("x", 1)));
    assert_eq!(digit.parse("23x"), Ok(("x", 23)));
    assert_eq!(digit.parse("459x"), Ok(("x", 459)));
    assert_eq!(digit.parse("xxx"), Err("xxx"));
}

#[test]
fn test_whitespaces() {
    let whitespaces = whitespaces();
    assert_eq!(whitespaces.parse(" x"), Ok(("x", vec![' '])));
    assert_eq!(
        whitespaces.parse(" \t\nx"),
        Ok(("x", vec![' ', '\t', '\n']))
    );
}
