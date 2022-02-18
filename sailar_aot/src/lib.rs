use inkwell::context::Context;
use sailar_get::loader;
use std::cell::RefCell;
use std::collections::hash_map;

mod code_gen;
mod error;

pub use error::{Error, Result};

struct ComparableRef<'a, T>(&'a T);

impl<T> std::cmp::PartialEq for ComparableRef<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0 as *const T, other.0 as *const T)
    }
}

impl<T> std::cmp::Eq for ComparableRef<'_, T> {}

impl<T> std::hash::Hash for ComparableRef<'_, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hasher::write_usize(state, self.0 as *const T as usize)
    }
}

impl<'a, T> std::fmt::Debug for ComparableRef<'a, T>
where
    &'a T: std::fmt::Debug,
{
    fn fmt(&self, format: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, format)
    }
}

struct TypeLookup<'c, 'l> {
    context: &'c Context,
    loader: &'l loader::Loader<'l>,
    type_lookup: RefCell<
        hash_map::HashMap<&'l loader::TypeSignature<'l>, inkwell::types::BasicTypeEnum<'c>>,
    >,
    function_lookup: RefCell<
        hash_map::HashMap<
            ComparableRef<'l, loader::FunctionSignature<'l>>,
            inkwell::types::FunctionType<'c>,
        >,
    >,
}

impl<'c, 'l> TypeLookup<'c, 'l> {
    fn get_integer_type<T: Into<sailar::format::type_system::Int>>(
        &self,
        integer_type: T,
    ) -> inkwell::types::IntType<'c> {
        use sailar::format::type_system::FixedInt;

        match self
            .loader
            .native_integer_type()
            .expect("todo: how to handle invalid native integer type?")
            .convert_integer_type(integer_type.into())
        {
            FixedInt::U8 | FixedInt::S8 => self.context.i8_type(),
            FixedInt::U16 | FixedInt::S16 => self.context.i16_type(),
            FixedInt::U32 | FixedInt::S32 => self.context.i32_type(),
            FixedInt::U64 | FixedInt::S64 => self.context.i64_type(),
        }
    }

    fn get_type(
        &self,
        signature: &'l loader::TypeSignature<'l>,
    ) -> inkwell::types::BasicTypeEnum<'c> {
        use inkwell::types::BasicTypeEnum;
        use sailar::format::type_system as sailar_types;

        match self.type_lookup.borrow_mut().entry(signature) {
            hash_map::Entry::Occupied(occupied) => *occupied.get(),
            hash_map::Entry::Vacant(vacant) => *vacant.insert(match signature.as_raw() {
                sailar_types::Any::Primitive(sailar_types::Primitive::Int(int)) => {
                    BasicTypeEnum::IntType(self.get_integer_type(*int))
                }
                sailar_types::Any::Primitive(sailar_types::Primitive::Real(real)) => {
                    BasicTypeEnum::FloatType(match real {
                        sailar_types::Real::F32 => self.context.f32_type(),
                        sailar_types::Real::F64 => self.context.f64_type(),
                    })
                }
                bad => todo!("unsupported type {}", bad),
            }),
        }
    }

    fn get_function(
        &self,
        signature: &'l loader::FunctionSignature<'l>,
    ) -> inkwell::types::FunctionType<'c> {
        // TODO: Make custom enum that is like AnyTypeEnum, but excludes FunctionType.
        use inkwell::types::{AnyType, AnyTypeEnum};

        match self
            .function_lookup
            .borrow_mut()
            .entry(ComparableRef(signature))
        {
            hash_map::Entry::Occupied(occupied) => *occupied.get(),
            hash_map::Entry::Vacant(vacant) => *vacant.insert({
                let return_type = match signature.return_types().first() {
                    Some(return_type) if signature.return_types().len() == 1 => {
                        self.get_type(return_type).as_any_type_enum()
                    }
                    Some(_) => todo!(
                        "multiple return types {} are not yet supported",
                        signature.return_types().len()
                    ),
                    None => AnyTypeEnum::VoidType(self.context.void_type()),
                };

                let parameter_types = signature
                    .parameter_types()
                    .iter()
                    .map(|parameter_type| {
                        inkwell::types::BasicMetadataTypeEnum::from(self.get_type(parameter_type))
                    })
                    .collect::<Vec<_>>();

                match return_type {
                    AnyTypeEnum::ArrayType(array_type) => {
                        array_type.fn_type(parameter_types.as_slice(), false)
                    }
                    AnyTypeEnum::FloatType(float_type) => {
                        float_type.fn_type(parameter_types.as_slice(), false)
                    }
                    AnyTypeEnum::FunctionType(_) => {
                        todo!("cannot return function type, return a function pointer instead")
                    }
                    AnyTypeEnum::IntType(integer_type) => {
                        integer_type.fn_type(parameter_types.as_slice(), false)
                    }
                    AnyTypeEnum::PointerType(pointer_type) => {
                        pointer_type.fn_type(parameter_types.as_slice(), false)
                    }
                    AnyTypeEnum::StructType(struct_type) => {
                        struct_type.fn_type(parameter_types.as_slice(), false)
                    }
                    AnyTypeEnum::VectorType(vector_type) => {
                        vector_type.fn_type(parameter_types.as_slice(), false)
                    }
                    AnyTypeEnum::VoidType(void_type) => {
                        void_type.fn_type(parameter_types.as_slice(), false)
                    }
                }
            }),
        }
    }
}

struct DataLookup<'b, 'c, 'l> {
    context: &'c Context,
    module: &'b inkwell::module::Module<'c>,
    lookup: RefCell<
        hash_map::HashMap<ComparableRef<'l, loader::Data<'l>>, inkwell::values::PointerValue<'c>>,
    >,
    buffer: RefCell<Vec<inkwell::values::IntValue<'c>>>,
}

impl<'b, 'c, 'l> DataLookup<'b, 'c, 'l> {
    fn get(&self, data: &'l loader::Data<'l>) -> inkwell::values::PointerValue<'c> {
        match self.lookup.borrow_mut().entry(ComparableRef(data)) {
            hash_map::Entry::Occupied(occupied) => *occupied.get(),
            hash_map::Entry::Vacant(vacant) => {
                let data_type = self.context.i8_type();
                let mut buffer = self.buffer.borrow_mut();
                buffer.clear();
                buffer.extend(
                    data.bytes()
                        .iter()
                        .map(|byte| data_type.const_int(u64::from(*byte), false)),
                );

                let data_value = data_type.const_array(&buffer);
                let constant = self.module.add_global(data_value.get_type(), None, "");
                constant.set_initializer(&data_value);
                *vacant.insert(constant.as_pointer_value())
            }
        }
    }
}

struct NameLookup<'l> {
    module_prefixes: RefCell<hash_map::HashMap<ComparableRef<'l, loader::Module<'l>>, String>>,
}

impl<'l> NameLookup<'l> {
    fn get(&self, function: &'l loader::Function<'l>) -> Result<String> {
        let mut prefixes = self.module_prefixes.borrow_mut();
        let module_name = prefixes
            .entry(ComparableRef(function.declaring_module()))
            .or_insert_with_key(|module| {
                use std::fmt::Write;

                let identifier = module.0.identifier();
                let mut name = identifier.name.to_string();

                for number in identifier.version.0.iter() {
                    write!(&mut name, "_{}", number).unwrap();
                }

                name
            });

        Ok(format!("{}_{}", module_name, function.symbol()?))
    }
}

struct FunctionLookup<'b, 'c, 'l> {
    lookup: RefCell<
        hash_map::HashMap<
            ComparableRef<'l, loader::Function<'l>>,
            inkwell::values::FunctionValue<'c>,
        >,
    >,
    undefined: RefCell<Vec<&'l loader::Function<'l>>>,
    module: &'b inkwell::module::Module<'c>,
    function_names: &'b NameLookup<'l>,
    type_lookup: &'b TypeLookup<'c, 'l>,
}

impl<'b, 'c, 'l> FunctionLookup<'b, 'c, 'l> {
    fn get(
        &self,
        function: &'l loader::Function<'l>,
    ) -> Result<inkwell::values::FunctionValue<'c>> {
        match self.lookup.borrow_mut().entry(ComparableRef(function)) {
            hash_map::Entry::Occupied(occupied) => Ok(*occupied.get()),
            hash_map::Entry::Vacant(vacant) => {
                let linkage: inkwell::module::Linkage;
                let constructed_name;
                let name: &str;

                if let Some(external) = function.as_external()? {
                    linkage = inkwell::module::Linkage::External;
                    name = external.symbol().as_str()
                } else {
                    linkage = if function.is_export() {
                        inkwell::module::Linkage::External
                    } else {
                        inkwell::module::Linkage::Private
                    };

                    constructed_name = self.function_names.get(function)?;
                    name = constructed_name.as_str()
                };

                let defined = self.module.add_function(
                    name,
                    self.type_lookup.get_function(function.signature()?),
                    Some(linkage),
                );

                self.undefined.borrow_mut().push(function);
                Ok(*vacant.insert(defined))
            }
        }
    }
}

/// Compiles the specified SAILAR module with its dependencies into an LLVM module.
pub fn compile<'c>(
    application: sailar::format::Module,
    resolver: &mut dyn loader::ReferenceResolver,
    context: &'c Context,
    target: &inkwell::targets::TargetMachine,
) -> Result<inkwell::module::Module<'c>> {
    let pointer_byte_size = target.get_target_data().get_pointer_byte_size(None);

    let mut loader = None;
    let (loader, application) = loader::Loader::initialize(
        &mut loader,
        loader::PointerSize::try_from(std::num::NonZeroU32::try_from(pointer_byte_size).unwrap())
            .unwrap(),
        resolver,
        application,
    );

    let module = context.create_module(&application.identifier().name);

    let type_lookup = TypeLookup {
        context,
        loader,
        type_lookup: RefCell::default(),
        function_lookup: RefCell::default(),
    };

    let data_lookup = DataLookup {
        context,
        module: &module,
        lookup: RefCell::default(),
        buffer: RefCell::default(),
    };

    let function_names = NameLookup {
        module_prefixes: RefCell::default(),
    };

    let function_lookup = FunctionLookup {
        lookup: RefCell::new(hash_map::HashMap::with_capacity(
            application.source().definitions.0.defined_functions.0.len(),
        )),
        undefined: RefCell::new(
            application
                .iter_defined_functions()
                .filter(|function| function.is_export())
                .collect::<Vec<_>>(),
        ),
        module: &module,
        type_lookup: &type_lookup,
        function_names: &function_names,
    };

    // Include entry point in list of functions to compile, if it is defined.
    let application_entry_point = application.entry_point()?;
    if let Some(entry_point_function) = application_entry_point {
        function_lookup
            .undefined
            .borrow_mut()
            .push(entry_point_function);
    }

    let code_builder = context.create_builder();
    let code = code_gen::Cache::new(&code_builder, &function_lookup, &type_lookup, &data_lookup);

    while let Some(function) = {
        // Avoids a mutable borrow error.
        let mut undefined = function_lookup.undefined.borrow_mut();
        undefined.pop().as_ref().copied()
    } {
        let definition = function_lookup.get(function)?;
        if definition.count_basic_blocks() == 0u32 {
            code_gen::generate(context, function, definition, &code)?;
        }
    }

    if let Some(entry_point_function) = application_entry_point {
        let entry_point_value = function_lookup.get(entry_point_function)?;

        // TODO: Add parameters for application arguments when necessary.
        let main = module.add_function(
            "main",
            context.i32_type().fn_type(&[], false),
            Some(inkwell::module::Linkage::External),
        );

        let main_block = context.append_basic_block(main, "entry");
        code_builder.position_at_end(main_block);

        let entry_point_result = code_builder
            .build_call(entry_point_value, &[], "exit_code")
            .try_as_basic_value()
            .left();

        // TODO: Check that the entry point returns an INTEGER exit code.
        let zero_exit_code = context.i32_type().const_zero();

        code_builder.build_return(Some(
            entry_point_result
                .as_ref()
                .map(|exit_code| exit_code as &dyn inkwell::values::BasicValue)
                .unwrap_or(&zero_exit_code),
        ));
    }

    Ok(module)
}
