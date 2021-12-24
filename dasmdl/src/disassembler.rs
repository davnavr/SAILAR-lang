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
    out.write_char_ln('}')
}

// TODO: Pad indices.
fn indexed<I: TryInto<usize>, T, W: FnOnce(&mut Output<'_, O>, &T) -> Result<()>, O: Write>(
    out: &mut Output<'_, O>,
    index: I,
    items: &format::structures::LengthEncodedVector<T>,
    writer: W,
) -> Result<()>
where
    <I as TryInto<usize>>::Error: std::fmt::Debug,
{
    let i = index.try_into().unwrap();
    out.write_fmt(format_args!("/* {} */ ", i))?;
    writer(out, &items.0[i])
}

fn indexed_identifier<O: Write>(
    out: &mut Output<'_, O>,
    id: format::indices::Identifier,
    identifiers: &format::structures::LengthEncodedVector<format::Identifier>,
) -> Result<()> {
    indexed(out, id, identifiers, |out, id| {
        out.write_char('\"')?;
        quoted_identifier(out, id)?;
        out.write_char('\"')
    })
}

fn unsigned_length<O: Write>(out: &mut Output<'_, O>, length: usize) -> Result<()> {
    out.write_fmt_ln(format_args!("// Length = {}", length))
}

fn module_identifiers<O: Write>(
    out: &mut Output<'_, O>,
    identifiers: &format::structures::LengthEncodedVector<format::Identifier>,
) -> Result<()> {
    out.write_str_ln("// Identifiers")?;
    unsigned_length(out, identifiers.0.len())?;
    // TODO: Pad index integers so first character of identifier is aligned with others.
    for (index, name) in identifiers.0.iter().enumerate() {
        out.write_fmt(format_args!("// {} ", index))?;
        quoted_identifier(out, name)?;
        out.write_ln()?;
    }
    Ok(())
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
        ".format {{ .major {}; .minor {}; }}",
        module.format_version.major, module.format_version.minor
    ))?;
    out.write_ln()?;
    module_header(&mut out, &module.header.0)?;
    out.write_ln()?;
    // TODO: Option to omit identifiers.
    module_identifiers(&mut out, &module.identifiers.0)?;
    out.write_ln()?;

    out.write_ln()
}
