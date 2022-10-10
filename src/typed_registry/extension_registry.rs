use std::borrow::Cow;

use super::common::*;
use super::feature_registry::Require;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExtensionKind {
    Instance,
    Device,
    // PhysicalDevice,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExtensionSupport {
    Vulkan,
    Disabled,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExtensionPromotion<'a> {
    Core(SemVarVersion),
    Extension(Cow<'a, str>),
}

#[derive(Debug)]
pub struct Extension<'a> {
    pub name: Cow<'a, str>,
    pub number: u32,
    pub kind: Option<ExtensionKind>,
    pub supported: ExtensionSupport,
    pub requires_core: SemVarVersion,
    pub requires_depencies: Box<[Cow<'a, str>]>,

    pub author: Cow<'a, str>,
    pub contact: Cow<'a, str>,
    pub promoted_to: ExtensionPromotion<'a>,

    pub comment: Option<Cow<'a, str>>,
    pub requires: Box<[Require<'a>]>,
}
