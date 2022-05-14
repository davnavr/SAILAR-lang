//! Provides functions for the tokenization of SAILAR assembly.

use logos::Logos;

fn literal_string_contents<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> &'s str {
    let token: &'s str = lex.slice();
    &token[0..token.len() - 1]
}

#[derive(Logos, Debug, Eq, PartialEq)]
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

pub fn tokenizer(mut input: &str) -> impl std::iter::Iterator<Item = (Token<'_>, std::ops::Range<usize>)> {
    Token::lexer(&mut input).spanned()
}

//pub fn tokenize(mut input: &str) -> (Vec<(Token<'_>, std::ops::Range<usize>)>, OffsetMap)
