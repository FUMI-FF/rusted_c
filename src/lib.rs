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
