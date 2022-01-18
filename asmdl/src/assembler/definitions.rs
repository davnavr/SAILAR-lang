use crate::assembler::*;

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
        symbols: &mut SymbolLookup<'a>,
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
