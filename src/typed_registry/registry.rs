use std::borrow::Cow;

use roxmltree::Node;
use serde::Serialize;

use crate::attribute;
use crate::try_attribute;
use crate::Parse;
use crate::ParseElements;
use crate::ParseResult;

use super::commands_registry::*;
use super::common::*;
use super::enums_registry::*;
use super::extension_registry::*;
use super::feature_registry::*;
use super::format_registry::*;
use super::spirv_registry::*;
use super::types_registry::*;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Registry<'a>(pub CommentendChildren<'a, RegistryChild<'a>>);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum RegistryChild<'a> {
    Platforms(Box<[Platform<'a>]>, Option<Cow<'a, str>>),
    Tags(Box<[Tag<'a>]>, Option<Cow<'a, str>>),
    Types(CommentendChildren<'a, Type<'a>>, Option<Cow<'a, str>>),
    Enums(Enums<'a>),
    Commands(
        CommentendChildren<'a, DefinitionOrAlias<'a, Command<'a>>>,
        Option<Cow<'a, str>>,
    ),
    Features(Feature<'a>),
    Extensions(Box<[WrappedExtension<'a>]>, Option<Cow<'a, str>>),
    Formats(Box<[Format<'a>]>),
    SpirvExtensions(Box<[SpirvExtension<'a>]>, Option<Cow<'a, str>>),
    SpirvCapabilities(Box<[SpirvCapability<'a>]>, Option<Cow<'a, str>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Platform<'a> {
    pub name: Cow<'a, str>,
    pub protect: Cow<'a, str>,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Tag<'a> {
    pub name: Cow<'a, str>,
    pub author: Cow<'a, str>,
    pub contact: Cow<'a, str>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum WrappedExtension<'a> {
    Extension(Extension<'a>),
    PseudoExtension(PseudoExtension<'a>),
}

impl<'a> FromIterator<MaybeComment<'a, RegistryChild<'a>>> for Registry<'a> {
    fn from_iter<T: IntoIterator<Item = MaybeComment<'a, RegistryChild<'a>>>>(iter: T) -> Self {
        Self(CommentendChildren(iter.into_iter().collect()))
    }
}

impl<'a, 'input: 'a> ParseElements<'a, 'input> for Registry<'a> {
    type Item = MaybeComment<'a, RegistryChild<'a>>;

    type NodeIter = roxmltree::Children<'a, 'input>;

    fn get_nodes(node: Node<'a, 'input>) -> ParseResult<Option<Self::NodeIter>> {
        if node.has_tag_name("registry") {
            Ok(Some(node.children()))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for RegistryChild<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        match node.tag_name().name() {
            "platforms" => Ok(Some(RegistryChild::Platforms(
                Box::parse(node)?,
                try_attribute(node, "comment")?,
            ))),
            "tags" => Ok(Some(RegistryChild::Tags(
                Box::parse(node)?,
                try_attribute(node, "comment")?,
            ))),
            "types" => Ok(Some(RegistryChild::Types(
                CommentendChildren::parse(node)?,
                try_attribute(node, "comment")?,
            ))),
            "enums" => Ok(Some(RegistryChild::Enums(Parse::parse(node)?))),
            "commands" => Ok(Some(RegistryChild::Commands(
                Parse::parse(node)?,
                try_attribute(node, "comment")?,
            ))),
            "feature" => Ok(Some(RegistryChild::Features(Parse::parse(node)?))),
            "extensions" => Ok(Some(RegistryChild::Extensions(
                Parse::parse(node)?,
                try_attribute(node, "comment")?,
            ))),
            "formats" => Ok(Some(RegistryChild::Formats(Parse::parse(node)?))),
            "spirvextensions" => Ok(Some(RegistryChild::SpirvExtensions(
                Parse::parse(node)?,
                try_attribute(node, "comment")?,
            ))),
            "spirvcapabilities" => Ok(Some(RegistryChild::SpirvCapabilities(
                Parse::parse(node)?,
                try_attribute(node, "comment")?,
            ))),
            _ => Ok(None),
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for Platform<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("platform") {
            Ok(Some(Platform {
                name: attribute(node, "name")?,
                protect: attribute(node, "protect")?,
                comment: try_attribute(node, "comment")?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for Tag<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("tag") {
            Ok(Some(Tag {
                name: attribute(node, "name")?,
                author: attribute(node, "author")?,
                contact: attribute(node, "contact")?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for WrappedExtension<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(e) = Extension::try_parse(node)? {
            Ok(Some(Self::Extension(e)))
        } else if let Some(e) = PseudoExtension::try_parse(node)? {
            Ok(Some(Self::PseudoExtension(e)))
        } else {
            Ok(None)
        }
    }
}
