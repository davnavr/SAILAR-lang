use sailar::format::{Module, ModuleIdentifier};

type Result = std::result::Result<Option<Box<Module>>, Box<dyn std::error::Error>>;

pub trait ReferenceResolver {
    fn resolve(&self, name: &ModuleIdentifier) -> Result;
}

impl<R: ReferenceResolver + ?Sized> ReferenceResolver for &'_ R {
    fn resolve(&self, name: &ModuleIdentifier) -> Result {
        (*self).resolve(name)
    }
}

impl<R: ReferenceResolver> ReferenceResolver for Option<R> {
    fn resolve(&self, name: &ModuleIdentifier) -> Result {
        match self {
            Some(resolver) => resolver.resolve(name),
            None => Ok(None),
        }
    }
}

#[derive(Clone)]
pub struct ResolverClosure<R>(pub R);

impl<R: Fn(&ModuleIdentifier) -> Result> ReferenceResolver for ResolverClosure<R> {
    fn resolve(&self, name: &ModuleIdentifier) -> Result {
        (self.0)(name)
    }
}

impl ReferenceResolver for () {
    fn resolve(&self, _: &ModuleIdentifier) -> Result {
        Ok(None)
    }
}
