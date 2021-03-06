use sailar::format;
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
        format_writer: String,
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
                format_writer: String::new(),
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
                self.format_writer.clear();
                std::fmt::Write::write_fmt(&mut self.format_writer, args).unwrap();
                let formatted = std::mem::take(&mut self.format_writer);
                self.write_str(&formatted)?;
                self.format_writer = formatted;
                Ok(())
            }
        }

        pub fn write_fmt_ln(&mut self, args: std::fmt::Arguments<'_>) -> Result<()> {
            self.write_fmt(args)?;
            self.write_ln()
        }

        pub fn write_iter<
            I: std::iter::IntoIterator,
            F: FnMut(&mut Self, I::Item) -> Result<()>,
        >(
            &mut self,
            iter: I,
            mut writer: F,
        ) -> Result<()> {
            for i in iter {
                writer(self, i)?;
            }
            Ok(())
        }

        pub fn write_join<
            I: std::iter::IntoIterator,
            F: FnMut(&mut Self, I::Item) -> Result<()>,
        >(
            &mut self,
            separator: &str,
            iter: I,
            mut writer: F,
        ) -> Result<()> {
            self.write_iter(iter.into_iter().enumerate(), |out, (index, i)| {
                if index > 0 {
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

fn data_header<O: Write>(out: &mut Output<'_, O>, header: &str, length: usize) -> Result<()> {
    out.write_str("// ")?;
    out.write_str_ln(header)?;
    out.write_fmt_ln(format_args!("// Length = {}", length))
}

fn commented_module_data<T, P: FnMut(&mut Output<'_, O>, &T) -> Result<()>, O: Write>(
    out: &mut Output<'_, O>,
    items: &[T],
    header: &str,
    mut printer: P,
) -> Result<()> {
    data_header(out, header, items.len())?;
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
        indexed_identifier(out, *id, identifiers)
    })
}

fn primitive_type<O: Write>(
    out: &mut Output<'_, O>,
    t: format::type_system::PrimitiveType,
) -> Result<()> {
    out.write_fmt(format_args!("{}", t))
}

fn type_signature<O: Write>(
    out: &mut Output<'_, O>,
    signature: &format::type_system::AnyType,
) -> Result<()> {
    use format::type_system::*;

    match signature {
        AnyType::Heap(HeapType::Val(SimpleType::Primitive(t))) => primitive_type(out, *t),
        _ => todo!(),
    }
}

// method_signature

const BLOCK_NAME_PREFIX: &str = "BLOCK_";
const ENTRY_BLOCK_NAME: &str = "ENTRY";

fn code_register<O: Write>(
    out: &mut Output<'_, O>,
    register: format::indices::Register,
) -> Result<()> {
    match register {
        format::indices::Register::Input(index) => out.write_fmt(format_args!("%a{}", index)),
        format::indices::Register::Temporary(index) => out.write_fmt(format_args!("%t{}", index)),
    }
}

fn numeric_type<O: Write>(
    out: &mut Output<'_, O>,
    t: &format::instruction_set::NumericType,
) -> Result<()> {
    use format::instruction_set::NumericType;
    match t {
        NumericType::Primitive(t) => primitive_type(out, *t),
    }
}

fn overflow_behavior<O: Write>(
    out: &mut Output<'_, O>,
    behavior: format::instruction_set::OverflowBehavior,
) -> Result<()> {
    use format::instruction_set::OverflowBehavior;

    match behavior {
        OverflowBehavior::Ignore => Ok(()),
        OverflowBehavior::Halt => out.write_str(" ovf.halt"),
        OverflowBehavior::Flag => out.write_str(" ovf.flag"),
    }
}

fn basic_arithmetic_instruction<O: Write>(
    out: &mut Output<'_, O>,
    name: &str,
    operation: &format::instruction_set::BasicArithmeticOperation,
    separator: &str,
) -> Result<()> {
    out.write_str(name)?;
    out.write_char(' ')?;
    numeric_type(out, &operation.return_type)?;
    out.write_char(' ')?;
    code_register(out, operation.x)?;
    out.write_char(' ')?;
    out.write_str(separator)?;
    out.write_char(' ')?;
    code_register(out, operation.y)?;

    overflow_behavior(out, operation.overflow)
}

fn division_instruction<O: Write>(
    out: &mut Output<'_, O>,
    name: &str,
    operation: &format::instruction_set::DivisionOperation,
) -> Result<()> {
    use format::instruction_set::DivideByZeroBehavior;

    out.write_str(name)?;
    out.write_char(' ')?;
    numeric_type(out, &operation.return_type)?;
    out.write_char(' ')?;
    code_register(out, operation.numerator)?;
    out.write_str(" over ")?;
    code_register(out, operation.denominator)?;

    match operation.divide_by_zero {
        DivideByZeroBehavior::Halt => out.write_str(" zeroed.halt")?,
        DivideByZeroBehavior::Return(return_index) => {
            out.write_str(" or ")?;
            code_register(out, return_index)?;
        }
    }

    overflow_behavior(out, operation.overflow)
}

fn bitwise_instruction<O: Write>(
    out: &mut Output<'_, O>,
    name: &str,
    operation: &format::instruction_set::BitwiseOperation,
    separator: Option<&str>,
) -> Result<()> {
    out.write_str(name)?;
    out.write_char(' ')?;
    numeric_type(out, &operation.result_type)?;
    out.write_char(' ')?;
    code_register(out, operation.x)?;
    out.write_char(' ')?;
    if let Some(separator) = separator {
        out.write_str(separator)?;
        out.write_char(' ')?;
    }
    code_register(out, operation.y)
}

fn bitwise_shift_instruction<O: Write>(
    out: &mut Output<'_, O>,
    name: &str,
    operation: &format::instruction_set::BitwiseShiftOperation,
) -> Result<()> {
    bitwise_instruction(out, name, &operation.0, Some("by"))
}

fn jump_target<O: Write>(
    out: &mut Output<'_, O>,
    target: format::instruction_set::JumpTarget,
) -> Result<()> {
    out.write_str(BLOCK_NAME_PREFIX)?;
    out.write_fmt(format_args!("{}", target.0))
}

fn many_code_registers<O: Write>(
    out: &mut Output<'_, O>,
    registers: &Vec<format::instruction_set::RegisterIndex>,
) -> Result<()> {
    if !registers.is_empty() {
        out.write_char(' ')?;
        out.write_join(", ", registers, |out, &index| code_register(out, index))?;
    }
    Ok(())
}

const METHOD_DEFINITION_NAME_PREFIX: &str = "method_definition_";
const METHOD_IMPORT_NAME_PREFIX: &str = "method_import_";

fn method_index<O: Write>(
    out: &mut Output<'_, O>,
    index: format::instruction_set::MethodIndex,
) -> Result<()> {
    out.write_char('@')?;
    match index {
        format::instruction_set::MethodIndex::Defined(definition_index) => {
            out.write_str(METHOD_DEFINITION_NAME_PREFIX)?;
            out.write_fmt(format_args!("{}", definition_index))
        }
        format::instruction_set::MethodIndex::Imported(import_index) => {
            out.write_str(METHOD_IMPORT_NAME_PREFIX)?;
            out.write_fmt(format_args!("{}", import_index))
        }
    }
}

fn code_block<O: Write, R: Fn(format::instruction_set::MethodIndex) -> u8>(
    out: &mut Output<'_, O>,
    block: &format::CodeBlock,
    index: Option<usize>,
    method_return_count: &R,
) -> Result<()> {
    out.write_str(".block $")?;
    out.write_str(BLOCK_NAME_PREFIX)?;
    if let Some(i) = index {
        out.write_fmt(format_args!("{}", i - 1))?;
    } else {
        out.write_str(ENTRY_BLOCK_NAME)?;
    }
    out.write_str(" (")?;
    out.write_join(", ", 0..block.input_register_count.0, |out, i| {
        out.write_fmt(format_args!("%a{}", i))
    })?;
    out.write_str_ln(") {")?;
    out.indent();

    let mut temporary_count = 0usize;

    for instruction in &block.instructions.0 .0 {
        use format::instruction_set::{Instruction, TailCall};

        let return_count = instruction.return_count(method_return_count);
        if return_count > 0 {
            out.write_join(", ", 0..return_count, |out, index| {
                out.write_fmt(format_args!("%t{}", temporary_count + usize::from(index)))
            })?;
            out.write_str(" = ")?;
        }

        temporary_count += usize::from(return_count);

        match instruction {
            Instruction::Nop => out.write_str("nop")?,
            Instruction::Ret(values) => {
                out.write_str("ret")?;
                many_code_registers(out, &values.0)?;
            }
            Instruction::Br(target, input_registers) => {
                out.write_str("br ")?;
                jump_target(out, *target)?;
                many_code_registers(out, &input_registers.0)?
            }
            Instruction::BrIf {
                condition,
                true_branch,
                false_branch,
                input_registers,
            } => {
                out.write_str("br.if ")?;
                code_register(out, *condition)?;
                out.write_str(" then ")?;
                jump_target(out, *true_branch)?;
                out.write_str(" else ")?;
                jump_target(out, *false_branch)?;
                many_code_registers(out, &input_registers.0)?
            }
            Instruction::Call(call) => {
                out.write_str("call ");
                match call.tail_call {
                    TailCall::Allowed => (),
                    TailCall::Required => out.write_str("tail.required ")?,
                    TailCall::Prohibited => out.write_str("tail.prohibited ")?,
                }
                method_index(out, call.method)?;
                many_code_registers(out, &call.arguments.0)?;
            }
            Instruction::Add(operation) => {
                basic_arithmetic_instruction(out, "add", operation, "and")?
            }
            Instruction::Sub(operation) => {
                basic_arithmetic_instruction(out, "sub", operation, "from")?
            }
            Instruction::Mul(operation) => {
                basic_arithmetic_instruction(out, "mul", operation, "by")?
            }
            Instruction::Div(operation) => division_instruction(out, "div", operation)?,
            Instruction::And(operation) => bitwise_instruction(out, "and", operation, None)?,
            Instruction::Or(operation) => bitwise_instruction(out, "or", operation, None)?,
            Instruction::Not(result_type, value) => {
                out.write_str("not ")?;
                numeric_type(out, result_type)?;
                out.write_char(' ')?;
                code_register(out, *value)?;
            }
            Instruction::Xor(operation) => bitwise_instruction(out, "xor", operation, None)?,
            Instruction::ShL(operation) => bitwise_shift_instruction(out, "sh.l", operation)?,
            Instruction::ShR(operation) => bitwise_shift_instruction(out, "sh.r", operation)?,
            Instruction::RotL(operation) => bitwise_shift_instruction(out, "rot.l", operation)?,
            Instruction::RotR(operation) => bitwise_shift_instruction(out, "rot.r", operation)?,
            Instruction::ConstI(constant) => out.write_fmt(format_args!(
                "const.i {} {}",
                constant.integer_type(),
                constant.value()
            ))?,
            Instruction::Break => out.write_str("break")?,
        }

        out.write_char_ln(';')?;
    }

    out.dedent();
    out.write_str_ln("};")
}

fn module_method_bodies<O: Write, R: Fn(format::instruction_set::MethodIndex) -> u8>(
    out: &mut Output<'_, O>,
    method_bodies: &[format::Code],
    method_return_count: &R,
) -> Result<()> {
    for (index, body) in method_bodies.iter().enumerate() {
        out.write_fmt_ln(format_args!(".code @code_{} {{", index))?;
        out.indent();
        out.write_str(".entry $")?;
        out.write_str(BLOCK_NAME_PREFIX)?;
        out.write_str(ENTRY_BLOCK_NAME)?;
        out.write_char_ln(';')?;
        code_block(out, &body.entry_block, None, method_return_count)?;
        out.write_ln()?;
        for (block_index, block) in body.blocks.iter().enumerate() {
            code_block(out, block, Some(block_index + 1), method_return_count)?;
            out.write_ln()?;
        }
        out.dedent();
        out.write_str_ln("};")?;
    }
    Ok(())
}

fn indexed_namespace<O: Write>(
    out: &mut Output<'_, O>,
    index: format::indices::Namespace,
    identifiers: &[format::Identifier],
    namespaces: &[format::Namespace],
) -> Result<()> {
    indexed(out, index, &namespaces, |out, ns| {
        quoted_namespace(out, identifiers, ns)
    })
}

fn visibility<O: Write>(out: &mut Output<'_, O>, visibility: format::Visibility) -> Result<()> {
    out.write_str(match visibility {
        format::Visibility::Unspecified => " ",
        format::Visibility::Public => " public",
        format::Visibility::Private => " private",
    })
}

fn module_definitions<O: Write>(
    out: &mut Output<'_, O>,
    identifiers: &[format::Identifier],
    namespaces: &[format::Namespace],
    definitions: &format::ModuleDefinitions,
) -> Result<()> {
    for (index, type_definition) in definitions.defined_types.iter().enumerate() {
        out.write_fmt(format_args!(".type @type_{}", index))?;
        visibility(out, type_definition.visibility)?;
        // TODO: Print other type attributes.
        out.write_str_ln(" {")?;
        out.indent();

        directive(out, "name", |out| {
            indexed_identifier(out, type_definition.name, identifiers)
        })?;
        directive(out, "namespace", |out| {
            indexed_namespace(out, type_definition.namespace, identifiers, namespaces)
        })?;
        out.write_ln()?;

        data_header(out, "Fields", type_definition.fields.len())?;
        out.write_ln()?;

        data_header(out, "Methods", type_definition.methods.len())?;
        for method_index in type_definition.methods.iter() {
            // TODO: Check that method index is in bounds, and that owner is correct.
            let method = &definitions.defined_methods[usize::try_from(*method_index).unwrap()];
            out.write_str(".method @")?;
            out.write_str(METHOD_DEFINITION_NAME_PREFIX)?;
            out.write_fmt(format_args!("{} (", method_index))?;
            // TODO: Write signature things.
            out.write_str(") returns (")?;
            out.write_str(") ")?;
            visibility(out, method.visibility)?;
            // TODO: Write other method attributes.
            out.write_str_ln(" {")?;
            out.indent();

            directive(out, "name", |out| {
                indexed_identifier(out, method.name, identifiers)
            })?;

            out.dedent();
            out.write_str_ln("};")?;
        }
        out.write_ln()?;

        out.dedent();
        out.write_str_ln("};")?;
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
    commented_module_data(
        &mut out,
        &module.type_signatures,
        "Type Signatures",
        |out, signature| type_signature(out, signature),
    )?;
    out.write_ln()?;
    // TODO: Print method signatures.
    out.write_ln()?;
    {
        let method_return_count = |index: format::instruction_set::MethodIndex| {
            // TODO: Print error comments if method or signature is not found while retrieving return value count.
            let method = &module.definitions.0.defined_methods.0[usize::try_from(index).unwrap()];
            let signature = &module.method_signatures.0[usize::try_from(method.signature).unwrap()];
            signature.return_types.0.len().try_into().unwrap()
        };
        module_method_bodies(&mut out, &module.method_bodies, &method_return_count)?;
    }
    out.write_ln()?;
    // TODO: Print data arrays.
    out.write_ln()?;
    // TODO: Print imports.
    data_header(
        &mut out,
        "Module Imports",
        module.imports.imported_modules.len(),
    )?;
    out.write_ln()?;
    data_header(
        &mut out,
        "Type Imports",
        module.imports.imported_types.len(),
    )?;
    out.write_ln()?;
    data_header(
        &mut out,
        "Type Definitions",
        module.definitions.defined_types.len(),
    )?;
    module_definitions(
        &mut out,
        &module.identifiers,
        &module.namespaces,
        &module.definitions,
    )?;
    out.write_ln()
}
