use std::borrow::Cow;

use super::commands_registry::*;
use super::common::*;
use super::enums_registry::*;
use super::extension_registry::*;
use super::feature_registry::*;
use super::format_registry::*;
use super::spirv_registry::*;
use super::types_registry::*;

#[derive(Debug)]
pub struct Registry<'a>(pub CommentendChildren<'a, RegistryChild<'a>>);

#[derive(Debug)]
pub enum RegistryChild<'a> {
    Platforms(Box<[Platform<'a>]>, Option<Comment<'a>>),
    Tags(Box<[Tag<'a>]>, Option<Comment<'a>>),
    Types(CommentendChildren<'a, Type<'a>>, Option<Comment<'a>>),
    Enums(Enums<'a>),
    Commands(
        CommentendChildren<'a, DefinitionOrAlias<'a, Command<'a>>>,
        Option<Comment<'a>>,
    ),
    Features(Feature<'a>),
    Extensions(Box<[Extension<'a>]>, Option<Comment<'a>>),
    Formats(Box<[Format<'a>]>),
    SpirvExtensions(Box<[SpirvExtension<'a>]>, Option<Comment<'a>>),
    SpirvCapabilities(Box<[SpirvCapability<'a>]>, Option<Comment<'a>>),
}

#[derive(Debug)]
pub struct Platform<'a> {
    pub name: Cow<'a, str>,
    pub protect: Cow<'a, str>,
    pub comment: Cow<'a, str>,
}

#[derive(Debug)]
pub struct Tag<'a> {
    pub name: Cow<'a, str>,
    pub author: Cow<'a, str>,
    pub contact: Cow<'a, str>,
}
