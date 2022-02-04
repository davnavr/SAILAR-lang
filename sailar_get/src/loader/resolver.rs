use sailar::format::{Module, ModuleIdentifier};

type Result = std::result::Result<Option<Box<Module>>, Box<dyn std::error::Error>>;

pub trait ReferenceResolver {
    fn resolve(&mut self, name: &ModuleIdentifier) -> Result;
}

impl<R: ReferenceResolver + ?Sized> ReferenceResolver for &'_ mut R {
    fn resolve(&mut self, name: &ModuleIdentifier) -> Result {
        (*self).resolve(name)
    }
}

impl<R: ReferenceResolver> ReferenceResolver for Option<R> {
    fn resolve(&mut self, name: &ModuleIdentifier) -> Result {
        match self {
            Some(resolver) => resolver.resolve(name),
            None => Ok(None),
        }
    }
}

#[derive(Clone)]
pub struct ResolverClosure<R>(pub R);

impl<R: FnMut(&ModuleIdentifier) -> Result> ReferenceResolver for ResolverClosure<R> {
    fn resolve(&mut self, name: &ModuleIdentifier) -> Result {
        (self.0)(name)
    }
}

impl ReferenceResolver for () {
    fn resolve(&mut self, _: &ModuleIdentifier) -> Result {
        Ok(None)
    }
}
