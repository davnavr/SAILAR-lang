use getmdl::loader;
use registir::format;

pub use format::type_system::PrimitiveType;

#[derive(Debug, Copy, Clone)]
enum RegisterType {
    Primitive(PrimitiveType),
    //Pointer(usize),
    //Object
}

#[derive(Copy, Clone)]
#[repr(C)]
union RegisterValue {
    sint: i32,
    ulong: u64,
}

#[derive(Copy, Clone)]
pub struct Register {
    value: RegisterValue,
    value_type: RegisterType,
}

#[derive(Debug, Clone)]
pub struct TryFromRegisterValueError {
    expected: RegisterType,
    actual: RegisterType,
}

impl std::fmt::Display for TryFromRegisterValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "expected register to contain a value of type {:?} but got {:?}",
            self.expected, self.actual
        )
    }
}

impl std::error::Error for TryFromRegisterValueError {}

impl TryFrom<&Register> for i32 {
    type Error = TryFromRegisterValueError;

    fn try_from(register: &Register) -> Result<i32, Self::Error> {
        match register.value_type {
            RegisterType::Primitive(PrimitiveType::S32) => unsafe { Ok(register.value.sint) },
            actual => Err(Self::Error {
                expected: RegisterType::Primitive(PrimitiveType::S32),
                actual,
            }),
        }
    }
}

type LoadedMethod<'l> = &'l loader::Method<'l>;

#[derive(Clone)]
pub struct StackFrame<'l> {
    current_method: LoadedMethod<'l>,
}

impl<'l> StackFrame<'l> {
    pub fn current_method(&'l self) -> LoadedMethod<'l> {
        &self.current_method
    }
}

struct Interpreter<'l> {
    loader: &'l loader::Loader<'l>,
    stack_frames: Vec<StackFrame<'l>>,
}

impl<'l> Interpreter<'l> {
    fn initialize(loader: &'l loader::Loader<'l>) -> Self {
        Self {
            loader,
            stack_frames: Vec::new(),
        }
    }
}

pub fn run<'l>(loader: &'l loader::Loader<'l>, arguments: &[Register], entry_point: LoadedMethod<'l>) -> Vec<Register> {
    let mut interpreter = Interpreter::initialize(loader);
    todo!()
}
