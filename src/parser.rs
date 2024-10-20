use std::ops::{BitAnd, BitOr};

// parser implementation
#[derive(Debug, PartialEq)]
pub struct ParseError;
pub type ParseResult<'a, I, O> = Result<(&'a [I], O), ParseError>;
pub type ParseFn<'a, I, O> = dyn Fn(&'a [I]) -> ParseResult<'a, I, O> + 'a;

pub struct Parser<'a, I, O> {
    parser: Box<ParseFn<'a, I, O>>,
}

impl<'a, I, O: 'a> Parser<'a, I, O> {
    pub fn new<P>(parser: P) -> Self
    where
        P: Fn(&'a [I]) -> Result<(&'a [I], O), ParseError> + 'a,
    {
        Self {
            parser: Box::new(parser),
        }
    }

    pub fn parse(&self, input: &'a [I]) -> ParseResult<'a, I, O> {
        (self.parser)(input)
    }

    pub fn repeat0(self) -> Parser<'a, I, Vec<O>> {
        Parser::new(move |input: &'a [I]| {
            let mut vec: Vec<O> = vec![];
            let mut cur = input;
            while let Ok((next, ret)) = self.parse(cur) {
                vec.push(ret);
                cur = next;
            }
            Ok((cur, vec))
        })
    }

    pub fn repeat1(self) -> Parser<'a, I, Vec<O>> {
        Parser::new(move |input: &'a [I]| {
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

    pub fn map<U, F>(self, f: F) -> Parser<'a, I, U>
    where
        F: Fn(O) -> U + 'a,
        U: 'a,
    {
        Parser::new(move |input: &[I]| self.parse(input).map(|(next, ret)| (next, f(ret))))
    }

    pub fn is_a<F>(self, pred: F) -> Parser<'a, I, O>
    where
        F: Fn(&O) -> bool + 'a,
    {
        Parser::new(move |input: &[I]| match self.parse(input) {
            Ok((next, ret)) if pred(&ret) => Ok((next, ret)),
            _ => Err(ParseError),
        })
    }
}

impl<'a, I, A: 'a, B: 'a> BitAnd<Parser<'a, I, B>> for Parser<'a, I, A> {
    type Output = Parser<'a, I, (A, B)>;

    // pair: A & B
    fn bitand(self, rhs: Parser<'a, I, B>) -> Parser<'a, I, (A, B)> {
        Parser::new(move |input: &[I]| match self.parse(input) {
            Ok((next, ret1)) => match rhs.parse(next) {
                Ok((_final, ret2)) => Ok((_final, (ret1, ret2))),
                Err(err) => Err(err),
            },
            Err(err) => Err(err),
        })
    }
}

impl<'a, I, A: 'a> BitOr<Parser<'a, I, A>> for Parser<'a, I, A> {
    type Output = Parser<'a, I, A>;

    // either: A | B
    fn bitor(self, rhs: Parser<'a, I, A>) -> Parser<'a, I, A> {
        Parser::new(move |input: &[I]| match self.parse(input) {
            Ok((next, ret)) => Ok((next, ret)),
            Err(_) => rhs.parse(input),
        })
    }
}

pub fn symbol<'a>(expected: u8) -> Parser<'a, u8, u8> {
    Parser::new(move |input: &[u8]| match input.get(0) {
        Some(ch) if *ch == expected => Ok((&input[1..], *ch)),
        _ => Err(ParseError),
    })
}

pub fn one_of<'a>(set: &'a [u8]) -> Parser<'a, u8, u8> {
    Parser::new(move |input: &[u8]| match input.get(0) {
        Some(ch) if set.contains(ch) => Ok((&input[1..], *ch)),
        _ => Err(ParseError),
    })
}

pub fn digit<'a>() -> Parser<'a, u8, i32> {
    (one_of(b"123456789") & one_of(b"0123456789").repeat0()).map(|(fst, mut rest)| {
        rest.insert(0, fst);
        let s = String::from_utf8(rest).expect("must be string");
        s.parse::<i32>().unwrap()
    }) | symbol(b'0').map(|_| 0)
}

pub fn any_char<'a>() -> Parser<'a, u8, u8> {
    Parser::new(move |input: &[u8]| match input.get(0) {
        Some(ch) => Ok((&input[1..], *ch)),
        _ => Err(ParseError),
    })
}

pub fn whitespaces<'a>() -> Parser<'a, u8, Vec<u8>> {
    any_char().is_a(|ch| ch.is_ascii_whitespace()).repeat0()
}

#[test]
fn test_symbol() {
    let symbol_parser = symbol(b'*');
    assert_eq!(
        symbol_parser.parse("*10".as_bytes()),
        Ok(("10".as_bytes(), b'*'))
    );
    assert_eq!(symbol_parser.parse(b"1+1"), Err(ParseError));
}

#[test]
fn test_one_of() {
    let one_of_parser = one_of(b"0123456789");
    assert_eq!(one_of_parser.parse(b"10"), Ok(("0".as_bytes(), b'1')));
    assert_eq!(one_of_parser.parse(b"xx"), Err(ParseError));
}

#[test]
fn test_repeat0() {
    let repeat0_parser = one_of(b"0123456789").repeat0();
    assert_eq!(
        repeat0_parser.parse(b"123x"),
        Ok(("x".as_bytes(), vec![b'1', b'2', b'3']))
    );
    assert_eq!(repeat0_parser.parse(b"xxx"), Ok(("xxx".as_bytes(), vec![])));
}

#[test]
fn test_repeat1() {
    let repeat0_parser = one_of(b"0123456789").repeat1();
    assert_eq!(
        repeat0_parser.parse(b"123x"),
        Ok(("x".as_bytes(), vec![b'1', b'2', b'3']))
    );
    assert_eq!(repeat0_parser.parse(b"xxx"), Err(ParseError));
}

#[test]
fn test_pair() {
    let p1 = symbol(b'1');
    let p2 = symbol(b'x');
    let paired = p1 & p2;
    assert_eq!(paired.parse(b"1xy"), Ok(("y".as_bytes(), (b'1', b'x'))));
}

#[test]
fn test_either() {
    let p1 = symbol(b'x');
    let p2 = symbol(b'y');
    let either = p1 | p2;
    assert_eq!(either.parse(b"x1"), Ok(("1".as_bytes(), b'x')));
    assert_eq!(either.parse(b"y1"), Ok(("1".as_bytes(), b'y')));
    assert_eq!(either.parse(b"10"), Err(ParseError));
}

#[test]
fn test_digit() {
    let digit = digit();
    assert_eq!(digit.parse(b"0x"), Ok(("x".as_bytes(), 0)));
    assert_eq!(digit.parse(b"1x"), Ok(("x".as_bytes(), 1)));
    assert_eq!(digit.parse(b"23x"), Ok(("x".as_bytes(), 23)));
    assert_eq!(digit.parse(b"459x"), Ok(("x".as_bytes(), 459)));
    assert_eq!(digit.parse(b"xxx"), Err(ParseError));
}

#[test]
fn test_whitespaces() {
    let whitespaces = whitespaces();
    assert_eq!(whitespaces.parse(b" x"), Ok(("x".as_bytes(), vec![b' '])));
    assert_eq!(
        whitespaces.parse(b" \t\nx"),
        Ok(("x".as_bytes(), vec![b' ', b'\t', b'\n']))
    );
}
