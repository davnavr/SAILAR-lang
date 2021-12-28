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

#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    MissingEntryPoint,
    External(Box<dyn std::error::Error>),
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
    pub fn invoke_entry_point(
        &'l self,
        argv: &[&str],
        //max_stack_capacity: usize,
    ) -> Result<i32, Error> {
        if !argv.is_empty() {
            todo!("Command line arguments are not yet supported")
        }
        todo!()
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingEntryPoint => {
                f.write_str("The entry point method of the module is not defined")
            }
            Self::External(error) => std::fmt::Display::fmt(error, f),
        }
    }
}

impl std::error::Error for Error {}
