//! Provides functions for the tokenization of SAILAR assembly.

use crate::ast;
use logos::Logos;

pub struct OffsetMapBuilder<'s> {
    lookup: Vec<(usize, ast::LocationNumber, &'s str)>,
    input: &'s str,
    previous_offset: usize,
    next_line_number: ast::LocationNumber,
}

impl<'s> OffsetMapBuilder<'s> {
    fn new(input: &'s str) -> Self {
        Self {
            lookup: Vec::with_capacity(16),
            input,
            previous_offset: 0,
            next_line_number: ast::LocationNumber::new(2).unwrap(),
        }
    }

    fn push_new_line(&mut self, offset: usize) {
        self.lookup
            .push((offset, self.next_line_number, &self.input[self.previous_offset..offset]));
        self.previous_offset = offset;
        self.next_line_number = ast::LocationNumber::new(self.next_line_number.get() + 1).unwrap();
    }

    fn finish(mut self) -> OffsetMap<'s> {
        let last_offset = self.input.len();

        match self.lookup.last() {
            Some((last, _, _)) if *last < last_offset => self.push_new_line(self.input.len()),
            _ => (),
        };

        OffsetMap { lookup: self.lookup }
    }
}

/// Maps byte offsets into the input file into line and column numbers.
pub struct OffsetMap<'s> {
    lookup: Vec<(usize, ast::LocationNumber, &'s str)>,
}

impl<'s> OffsetMap<'s> {
    pub fn get_location(&self, offset: usize) -> Option<ast::Location> {
        if !self.lookup.is_empty() {
            match self.lookup.binary_search_by_key(&offset, |(o, _, _)| *o) {
                Ok(exact) => Some(ast::Location::new(self.lookup[exact].1, ast::LOCATION_NUMBER_START)),
                Err(0) => unreachable!("missing first line lookup entry"),
                Err(index) => {
                    let (_, line_number, line) = self.lookup[index];
                    let (line_offset, _, _) = self.lookup[index - 1];
                    let column_number =
                        ast::LocationNumber::new(line[0..offset - line_offset].chars().count()).expect("valid column number");
                    Some(ast::Location::new(line_number, column_number))
                }
            }
        } else {
            None
        }
    }
}

fn literal_string_contents<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> &'s str {
    let token: &'s str = lex.slice();
    &token[0..token.len() - 1]
}

#[derive(Logos, Debug, Eq, PartialEq)]
#[logos(extras = OffsetMapBuilder<'s>)]
pub enum Token<'s> {
    #[token(".format")]
    FormatDirective,
    #[token(".identifier")]
    IdentifierDirective,
    #[regex(r"[a-zA-Z][a-zA-Z_0-9]*")]
    Word(&'s str),
    #[regex("\"[a-zA-Z0-9_\\?\\\\/!\\*\\+\\.]*\"@#;", literal_string_contents)]
    LiteralString(&'s str),
    #[regex(r"\n|\r|(\r\n)")]
    Newline,
    #[error]
    #[regex(r"[ \t]+", logos::skip)]
    #[regex(r";[ \t\w\d;\?!\\/\.\*\+-=#]*", logos::skip)]
    Unknown,
}

pub fn tokenize(mut input: &str) -> (Vec<(Token<'_>, std::ops::Range<usize>)>, OffsetMap) {
    let mut tokens = Vec::default();
    let mut lexer = Token::lexer_with_extras(&mut input, OffsetMapBuilder::new(input));
    while let Some(token) = lexer.next() {
        tokens.push((token, lexer.span()));
    }

    (tokens, lexer.extras.finish())
}
