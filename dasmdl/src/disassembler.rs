use registir::format;
use std::io::{Result, Write};

#[derive(Debug, Clone, Copy)]
pub enum Indentation {
    Tabs,
    Spaces(u8),
}

impl Default for Indentation {
    fn default() -> Self {
        Indentation::Spaces(4)
    }
}

mod output {
    use crate::disassembler::{Indentation, Result, Write};

    #[derive(Debug)]
    pub struct Output<'a, W> {
        writer: &'a mut W,
        indentation: String,
        indented: bool,
        indentation_level: u32,
    }

    impl<'a, W: Write> Output<'a, W> {
        pub fn new(writer: &'a mut W, indentation: Indentation) -> Self {
            Self {
                writer,
                indentation: match indentation {
                    Indentation::Tabs => String::from('\t'),
                    Indentation::Spaces(count) => unsafe {
                        String::from_utf8_unchecked(vec![b' '; count.into()])
                    },
                },
                indented: false,
                indentation_level: 0,
            }
        }

        fn write_indent(&mut self) -> Result<()> {
            if !self.indented {
                self.indented = true;
                let indentation = self.indentation.as_bytes();
                for _ in 0..self.indentation_level {
                    self.writer.write_all(indentation)?;
                }
            }
            Ok(())
        }

        pub fn write_str(&mut self, s: &str) -> Result<()> {
            self.write_indent()?;
            self.writer.write_all(s.as_bytes())
        }

        pub fn write_ln(&mut self) -> Result<()> {
            self.indented = false;
            self.writer.write_all(&[b'\n'])
        }

        pub fn write_str_ln(&mut self, s: &str) -> Result<()> {
            self.write_str(s)?;
            self.write_ln()
        }

        pub fn write_char(&mut self, c: char) -> Result<()> {
            let mut buffer = [0u8, 4];
            self.write_str(c.encode_utf8(&mut buffer))
        }

        pub fn write_char_ln(&mut self, c: char) -> Result<()> {
            self.write_char(c)?;
            self.write_ln()
        }

        pub fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> Result<()> {
            if let Some(s) = args.as_str() {
                self.write_str(s)
            } else {
                self.write_str(&args.to_string())
            }
        }

        pub fn write_fmt_ln(&mut self, args: std::fmt::Arguments<'_>) -> Result<()> {
            self.write_fmt(args)?;
            self.write_ln()
        }

        pub fn write_iter<
            I: std::iter::IntoIterator,
            F: FnMut(&mut Self, &I::Item) -> Result<()>,
        >(
            &mut self,
            iter: I,
            mut writer: F,
        ) -> Result<()> {
            for i in iter {
                writer(self, &i)?;
            }
            Ok(())
        }

        pub fn write_join<
            I: std::iter::IntoIterator,
            F: FnMut(&mut Self, &I::Item) -> Result<()>,
        >(
            &mut self,
            separator: &str,
            iter: I,
            mut writer: F,
        ) -> Result<()> {
            self.write_iter(iter.into_iter().enumerate(), |out, (index, i)| {
                if *index > 0 {
                    out.write_str(separator)?;
                }
                writer(out, i)
            })
        }

        pub fn indent(&mut self) {
            self.indentation_level += 1
        }

        pub fn dedent(&mut self) {
            self.indentation_level -= 1
        }
    }
}

use output::Output;

fn directive<W: FnOnce(&mut Output<'_, O>) -> Result<()>, O: Write>(
    out: &mut Output<'_, O>,
    name: &'static str,
    writer: W,
) -> Result<()> {
    out.write_char('.')?;
    out.write_str(name)?;
    out.write_char(' ')?;
    writer(out)?;
    out.write_char_ln(';')
}

fn quoted_identifier<O: Write>(out: &mut Output<'_, O>, id: &format::Identifier) -> Result<()> {
    out.write_char('\"')?;
    for c in id.as_str().chars() {
        match c {
            '\n' => out.write_str("\\n")?,
            '\t' => out.write_str("\\t")?,
            '\"' => out.write_str("\\\"")?,
            '\\' => out.write_str("\\\\")?,
            _ if c.is_control() => out.write_fmt(format_args!("\\u{:04X}", u32::from(c)))?,
            _ => out.write_char(c)?,
        }
    }
    out.write_char('\"')
}

fn version_numbers<O: Write>(
    out: &mut Output<'_, O>,
    format::VersionNumbers(ref version): &format::VersionNumbers,
) -> Result<()> {
    out.write_join(" ", &version.0, |out, n| {
        out.write_fmt(format_args!("{}", n))
    })
}

fn module_header<O: Write>(out: &mut Output<'_, O>, header: &format::ModuleHeader) -> Result<()> {
    out.write_str_ln(".module {")?;
    out.indent();
    out.write_fmt_ln(format_args!("// FieldCount: {}", header.field_count()))?;
    directive(out, "name", |out| {
        quoted_identifier(out, &header.identifier.name)
    })?;
    directive(out, "version", |out| {
        version_numbers(out, &header.identifier.version)
    })?;
    out.dedent();
    out.write_str_ln("}")
}

// TODO: Pad indices.
fn indexed<I: TryInto<usize>, T, W: FnOnce(&mut Output<'_, O>, &T) -> Result<()>, O: Write>(
    out: &mut Output<'_, O>,
    index: I,
    items: &[T],
    writer: W,
) -> Result<()>
where
    <I as TryInto<usize>>::Error: std::fmt::Debug,
{
    let i = index.try_into().unwrap();
    out.write_fmt(format_args!("/* {} */ ", i))?;
    writer(out, &items[i]) // TODO: Print error message if index is invalid.
}

fn commented_module_data<T, P: FnMut(&mut Output<'_, O>, &T) -> Result<()>, O: Write>(
    out: &mut Output<'_, O>,
    items: &[T],
    header: &str,
    mut printer: P,
) -> Result<()> {
    out.write_str("// ")?;
    out.write_str_ln(header)?;
    out.write_fmt_ln(format_args!("// Length = {}", items.len()))?;
    // TODO: Pad index integers.
    for (index, i) in items.iter().enumerate() {
        out.write_fmt(format_args!("// {} - ", index))?;
        printer(out, i)?;
        out.write_ln()?;
    }
    Ok(())
}

fn indexed_identifier<O: Write>(
    out: &mut Output<'_, O>,
    index: format::indices::Identifier,
    identifiers: &[format::Identifier],
) -> Result<()> {
    indexed(out, index, identifiers, |out, id| {
        quoted_identifier(out, id)
    })
}

fn quoted_namespace<O: Write>(
    out: &mut Output<'_, O>,
    identifiers: &[format::Identifier],
    namespace: &format::structures::LengthEncodedVector<format::indices::Identifier>,
) -> Result<()> {
    out.write_join(", ", &namespace.0, |out, id| {
        indexed_identifier(out, **id, identifiers)
    })
}

const BLOCK_NAME_PREFIX: &str = "BLOCK_";
const ENTRY_BLOCK_NAME: &str = "ENTRY";

fn code_block<O: Write>(
    out: &mut Output<'_, O>,
    block: &format::CodeBlock,
    index: Option<usize>,
) -> Result<()> {
    out.write_str(".block $")?;
    out.write_str(BLOCK_NAME_PREFIX)?;
    if let Some(i) = index {
        out.write_fmt(format_args!("{}", i))?;
    } else {
        out.write_str(ENTRY_BLOCK_NAME)?;
    }
    out.write_str(" (")?;
    for _ in 0..block.input_register_count.0 {

    }
    out.write_str_ln(") {")?;
    out.indent();
    out.dedent();
    out.write_str_ln("};")
}

fn module_method_bodies<O: Write>(
    out: &mut Output<'_, O>,
    method_bodies: &[format::Code],
) -> Result<()> {
    for (index, body) in method_bodies.iter().enumerate() {
        out.write_fmt(format_args!(".code @code_{} {{", index))?;
        out.indent();
        out.write_str(".entry $")?;
        out.write_str(BLOCK_NAME_PREFIX)?;
        out.write_str(ENTRY_BLOCK_NAME)?;
        out.write_char_ln(';')?;
        code_block(out, &body.entry_block, None)?;
        out.dedent();
        out.write_str_ln("};")?;
    }
    Ok(())
}

fn indexed_namespace<O: Write>(
    out: &mut Output<'_, O>,
    index: format::indices::Namespace,
    identifiers: &Vec<format::Identifier>,
    namespaces: &Vec<format::structures::LengthEncodedVector<format::indices::Identifier>>,
) -> Result<()> {
    indexed(out, index, &namespaces, |out, ns| {
        quoted_namespace(out, identifiers, ns)
    })
}

pub fn disassemble<O: Write>(
    output: &mut O,
    module: &format::Module,
    indentation: Indentation,
    // TODO: Have option to have indices be in base 10 or hex.
) -> Result<()> {
    let mut out = Output::new(output, indentation);
    out.write_str_ln("// Disassembled by dasmdl")?;
    out.write_fmt_ln(format_args!(
        "// IntegerSize: {} bytes",
        module.integer_size
    ))?;
    out.write_ln()?;
    out.write_fmt_ln(format_args!(
        ".format {{ .major {}; .minor {}; }};",
        module.format_version.major, module.format_version.minor
    ))?;
    out.write_ln()?;
    module_header(&mut out, &module.header.0)?;
    out.write_ln()?;
    // TODO: Option to omit identifiers.
    commented_module_data(&mut out, &module.identifiers, "Identifiers", |out, name| {
        quoted_identifier(out, name)
    })?;
    out.write_ln()?;
    // TODO: Option to omit namespaces.
    commented_module_data(&mut out, &module.namespaces, "Namespaces", |out, ns| {
        quoted_namespace(out, &module.identifiers, ns)
    })?;
    out.write_ln()?;
    // TODO: Print type signatures.
    out.write_ln()?;
    // TODO: Print method signatures.
    out.write_ln()?;
    module_method_bodies(&mut out, &module.method_bodies)?;
    out.write_ln()
}
