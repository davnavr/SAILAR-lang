//! Showcases the parser API, by parsing format versions.

fn main() {
    let input = ".format major 0\n.format minor 12\n";
    let tokens = sailasm::lexer::tokenize(input);
    let tree = sailasm::parser::parse(&tokens);

    dbg!(tree.tree());
}
