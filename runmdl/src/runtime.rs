use getmdl::loader;

pub struct Runtime<'l> {
    loader: &'l loader::Loader<'l>,
    program: &'l loader::Module<'l>,
}

#[derive(Default)]
pub struct RuntimeInitializer<'l> {
    runtime: Option<Runtime<'l>>,
    loader: Option<loader::Loader<'l>>,
}

impl<'l> RuntimeInitializer<'l> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<'l> Runtime<'l> {
    pub fn initialize(
        initializer: &'l mut RuntimeInitializer<'l>,
        application: registir::format::Module,
    ) -> &'l Self {
        let (loader, program) = loader::Loader::initialize(&mut initializer.loader, application);
        initializer.runtime.insert(Self { loader, program })
    }

    /// Interprets the entry point of the program, supplying the specified arguments.
    pub fn invoke_entry_point(&'l self, #[allow(unused_variables)] argv: &[&str]) -> i32 {
        todo!()
    }
}
