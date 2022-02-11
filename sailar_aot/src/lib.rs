use inkwell::context::Context;
use inkwell::targets;
use sailar::format;
use sailar_get::loader;

mod error;

pub use error::{Error, Result};

/// Compiles the specified SAILAR module with its dependencies into an LLVM module.
pub fn compile<'c>(
    application: format::Module,
    resolver: &mut dyn loader::ReferenceResolver,
    context: &'c Context,
    target: &targets::TargetMachine,
) -> Result<inkwell::module::Module<'c>> {
    let mut loader = None;
    let (loader, application) = loader::Loader::initialize(
        &mut loader,
        loader::PointerSize::try_from(
            std::num::NonZeroU32::try_from(target.get_target_data().get_pointer_byte_size(None))
                .unwrap(),
        )
        .unwrap(),
        resolver,
        application,
    );

    let entry_point = application.entry_point()?.ok_or(Error::MissingEntryPoint);

    let module = context.create_module(&application.identifier().name);
    Ok(module)
}
