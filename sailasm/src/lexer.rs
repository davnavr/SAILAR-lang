//! Provides functions for the tokenization of SAILAR assembly.

use std::borrow::Cow;

const DIRECTIVES: phf::Set<&'static str> = phf::phf_set! {
    "format"
};

fn get_directive_name(lex: &mut logos::Lexer<Token>) -> Cow<'static, str> {
    let name = &lex.slice()[1..];
    DIRECTIVES
        .get_key(name)
        .map(|known| Cow::Borrowed(*known))
        .unwrap_or_else(|| Cow::Owned(name.to_string()))
}

#[derive(logos::Logos, Debug, Eq, PartialEq)]
pub enum Token {
    #[regex(r"\.[a-zA-Z]+", get_directive_name)]
    Directive(Cow<'static, str>),
    #[regex(r"\n|\r|(\r\n)")]
    Newline,
    #[error]
    #[regex(r"[ \t]+", logos::skip)]
    #[regex(r";[ \t\w\d;\?!\\/\.\*\+-=#]*", logos::skip)]
    Unknown,
}
