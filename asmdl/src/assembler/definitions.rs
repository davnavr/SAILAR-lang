use crate::assembler::*;

#[derive(Debug)]
pub struct FunctionAssembler<'a> {
    pub location: &'a ast::Position,
    pub is_export: bool,
    pub symbol: &'a ast::Identifier,
    pub parameter_types: signatures::ParameterSet<'a>,
    pub return_types: signatures::ParameterSet<'a>,
    pub declarations: &'a [ast::Positioned<ast::FunctionDeclaration>],
}

pub type FunctionLookup<'a> = lookup::IndexedMap<
    format::indices::FunctionDefinition,
    &'a ast::Identifier,
    FunctionAssembler<'a>,
>;

impl<'a> FunctionAssembler<'a> {
    pub(crate) fn assemble(
        &self,
        errors: &mut error::Builder,
        _symbols: &mut SymbolLookup<'a>,
        identifiers: &mut IdentifierLookup,
        code_lookup: &mut code_gen::FunctionCodeLookup<'a>,
        type_signatures: &mut signatures::TypeLookup<'a>,
        function_signatures: &mut signatures::FunctionLookup<'a>,
    ) -> Option<format::Function> {
        let mut function_name = None;
        let mut function_body = None;

        for declaration in self.declarations {
            match &declaration.0 {
                ast::FunctionDeclaration::Name((name, _)) => {
                    if function_name.is_none() {
                        function_name = Some(identifiers.insert_or_get(name.clone()))
                    } else {
                        errors.push_with_location(
                            ErrorKind::DuplicateDirective,
                            declaration.1.clone(),
                        )
                    }
                }
                ast::FunctionDeclaration::Body(body) => {
                    if function_body.is_none() {
                        function_body = Some(match body {
                            ast::FunctionBodyDeclaration::Defined(body_symbol) => code_lookup
                                .get_index(body_symbol.identifier())
                                .map(format::FunctionBody::Defined),
                            ast::FunctionBodyDeclaration::External { .. } => {
                                todo!("external function bodies are not yet supported")
                            }
                        });
                    } else {
                        errors.push_with_location(
                            ErrorKind::DuplicateDirective,
                            declaration.1.clone(),
                        )
                    }
                }
            }
        }

        if function_name.is_none() {
            errors.push_with_location(
                ErrorKind::MissingDirective("function name"),
                self.location.clone(),
            )
        }

        if function_body.is_none() {
            errors.push_with_location(
                ErrorKind::MissingDirective("function body"),
                self.location.clone(),
            )
        }

        if let (Some(name), Some(Some(body))) = (function_name, function_body) {
            Some(format::Function {
                name,
                is_export: self.is_export,
                symbol: identifiers.insert_or_get(self.symbol.clone()),
                signature: function_signatures.get(
                    type_signatures,
                    self.parameter_types,
                    self.return_types,
                ),
                body,
            })
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct FieldAssembler<'a> {
    owner: format::indices::StructDefinition,
    location: &'a ast::Position,
    is_export: bool,
    symbol: &'a ast::Identifier,
    value_type: &'a ast::Positioned<ast::Type>,
    declarations: &'a [ast::Positioned<ast::FieldDeclaration>],
}

pub type FieldLookup<'a> = lookup::IndexedMap<
    format::indices::FieldDefinition,
    (&'a ast::Identifier, &'a ast::Identifier),
    FieldAssembler<'a>,
>;

impl<'a> FieldAssembler<'a> {
    pub(crate) fn assemble(
        &self,
        errors: &mut error::Builder,
        _symbols: &mut SymbolLookup<'a>,
        identifiers: &mut IdentifierLookup,
        type_signatures: &mut signatures::TypeLookup<'a>,
    ) -> Option<format::Field> {
        let mut field_name = None;

        for declaration in self.declarations {
            match &declaration.0 {
                ast::FieldDeclaration::Name(name) => {
                    if field_name.is_none() {
                        field_name = Some(identifiers.insert_or_get(name.0.clone()))
                    } else {
                        errors.push_with_location(
                            ErrorKind::DuplicateDirective,
                            declaration.1.clone(),
                        )
                    }
                }
            }
        }

        if field_name.is_none() {
            errors.push_with_location(
                ErrorKind::MissingDirective("field name"),
                self.location.clone(),
            )
        }

        Some(format::Field {
            owner: self.owner,
            name: field_name?,
            is_export: self.is_export,
            symbol: identifiers.insert_or_get(self.symbol.clone()),
            signature: type_signatures.get(self.value_type),
        })
    }
}

#[derive(Debug)]
pub struct StructAssembler<'a> {
    pub index: format::indices::StructDefinition,
    pub location: &'a ast::Position,
    pub is_export: bool,
    pub symbol: &'a ast::Identifier,
    pub declarations: &'a [ast::Positioned<ast::StructDeclaration>],
}

pub type StructLookup<'a> =
    lookup::IndexedMap<format::indices::StructDefinition, &'a ast::Identifier, StructAssembler<'a>>;

pub type StructLayoutLookup<'a> =
    lookup::IndexedSet<format::indices::StructLayout, format::StructLayout>;

impl<'a> StructAssembler<'a> {
    pub(crate) fn assemble(
        &self,
        errors: &mut error::Builder,
        _symbols: &mut SymbolLookup<'a>,
        identifiers: &mut IdentifierLookup,
        layout_lookup: &mut StructLayoutLookup<'a>,
        field_lookup: &mut FieldLookup<'a>,
    ) -> Option<format::Struct> {
        let mut struct_name = None;
        let mut struct_layout = None;
        let mut field_indices = Vec::with_capacity(self.declarations.len());

        for declaration in self.declarations {
            match &declaration.0 {
                ast::StructDeclaration::Name(name) => {
                    if struct_name.is_none() {
                        struct_name = Some(identifiers.insert_or_get(name.0.clone()))
                    } else {
                        errors.push_with_location(
                            ErrorKind::DuplicateDirective,
                            declaration.1.clone(),
                        )
                    }
                }
                ast::StructDeclaration::Layout(layout) => match layout {
                    _ if struct_layout.is_some() => errors
                        .push_with_location(ErrorKind::DuplicateDirective, declaration.1.clone()),
                    ast::StructLayoutDeclaration::Unspecified => {
                        struct_layout =
                            Some(layout_lookup.insert_or_get(format::StructLayout::Unspecified))
                    }
                },
                ast::StructDeclaration::Field {
                    symbol,
                    value_type,
                    is_export,
                    declarations,
                } => {
                    let field_symbol = symbol.identifier();
                    let insertion_result = field_lookup.insert(
                        (self.symbol, field_symbol),
                        FieldAssembler {
                            owner: self.index,
                            declarations,
                            value_type,
                            is_export: *is_export,
                            location: &declaration.1,
                            symbol: field_symbol,
                        },
                    );

                    match insertion_result {
                        Ok(index) => field_indices.push(index),
                        Err(error) => errors.push_with_location(
                            ErrorKind::DuplicateDeclaration {
                                name: field_symbol.clone(),
                                kind: "field definition",
                                original: error.location.clone(),
                            },
                            declaration.1.clone(),
                        ),
                    }
                }
            }
        }

        if struct_name.is_none() {
            errors.push_with_location(
                ErrorKind::MissingDirective("struct name"),
                self.location.clone(),
            )
        }

        Some(format::Struct {
            name: struct_name?,
            is_export: self.is_export,
            symbol: identifiers.insert_or_get(self.symbol.clone()),
            layout: struct_layout?,
            fields: format::LenVec(field_indices),
        })
    }
}
