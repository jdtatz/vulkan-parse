mod typed_registry;
pub use crate::typed_registry::*;

mod parse_xml;
pub use crate::parse_xml::*;

// TODO
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Expression<'a>(std::marker::PhantomData<&'a ()>);
