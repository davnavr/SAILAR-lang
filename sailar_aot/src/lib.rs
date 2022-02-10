use inkwell::context::Context;
use inkwell::targets;
use sailar::format;
use sailar_get::loader;

/// Compiles the specified SAILAR module with its dependencies into an LLVM module.
pub fn compile(
    application: format::Module,
    resolver: &mut dyn loader::ReferenceResolver,
    target: &targets::TargetMachine,
) {
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

    let context = Context::create();
    let module = context.create_module(&application.identifier().name);
    todo!()
}
