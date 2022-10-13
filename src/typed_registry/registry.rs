use std::borrow::Cow;

use roxmltree::Node;

use crate::get_req_attr;
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
                node.attribute("comment").map(Cow::Borrowed).map(Comment),
            ))),
            "tags" => Ok(Some(RegistryChild::Tags(
                Box::parse(node)?,
                node.attribute("comment").map(Cow::Borrowed).map(Comment),
            ))),
            "types" => Ok(Some(RegistryChild::Types(
                CommentendChildren::parse(node)?,
                node.attribute("comment").map(Cow::Borrowed).map(Comment),
            ))),
            "enums" => Ok(Some(RegistryChild::Enums(Parse::parse(node)?))),
            "commands" => Ok(Some(RegistryChild::Commands(
                Parse::parse(node)?,
                node.attribute("comment").map(Cow::Borrowed).map(Comment),
            ))),
            "feature" => todo!(),
            "extensions" => todo!(),
            "formats" => todo!(),
            "spirvextensions" => todo!(),
            "spirvcapabilities" => todo!(),
            _ => Ok(None),
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for Platform<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("platform") {
            Ok(Some(Platform {
                name: Cow::Borrowed(get_req_attr(node, "name")?),
                protect: Cow::Borrowed(get_req_attr(node, "protect")?),
                comment: Cow::Borrowed(get_req_attr(node, "comment")?),
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
                name: Cow::Borrowed(get_req_attr(node, "name")?),
                author: Cow::Borrowed(get_req_attr(node, "author")?),
                contact: Cow::Borrowed(get_req_attr(node, "contact")?),
            }))
        } else {
            Ok(None)
        }
    }
}
