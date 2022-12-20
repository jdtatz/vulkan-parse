use roxmltree::Node;

use super::{
    commands_registry::Command,
    common::{CommentendChildren, DefinitionOrAlias, MaybeComment},
    enums_registry::Enums,
    extension_registry::{Extension, PseudoExtension},
    feature_registry::Feature,
    format_registry::Format,
    spirv_registry::{SpirvCapability, SpirvExtension},
    types_registry::Type,
};
use crate::{attribute, parse_children, try_attribute, Parse, ParseResult};

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Registry<'a>(pub CommentendChildren<'a, Items<'a>>);

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum Items<'a> {
    Platforms {
        platforms: Vec<Platform<'a>>,
        comment: Option<&'a str>,
    },
    Tags {
        tags: Vec<Tag<'a>>,
        comment: Option<&'a str>,
    },
    Types {
        types: CommentendChildren<'a, Type<'a>>,
        comment: Option<&'a str>,
    },
    Enums(Enums<'a>),
    Commands {
        commands: CommentendChildren<'a, DefinitionOrAlias<'a, Command<'a>>>,
        comment: Option<&'a str>,
    },
    Features(Feature<'a>),
    Extensions {
        extensions: Vec<WrappedExtension<'a>>,
        comment: Option<&'a str>,
    },
    Formats(Vec<Format<'a>>),
    SpirvExtensions {
        extensions: Vec<SpirvExtension<'a>>,
        comment: Option<&'a str>,
    },
    SpirvCapabilities {
        capabilities: Vec<SpirvCapability<'a>>,
        comment: Option<&'a str>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct Platform<'a> {
    /// name of the platform, used as part of extension names
    pub name: &'a str,
    /// preprocessor symbol to include platform headers from <vulkan.h>
    pub protect: &'a str,
    /// platform description
    pub comment: Option<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Tag<'a> {
    pub name: &'a str,
    /// name of the author (usually a company or project name)
    pub author: &'a str,
    /// contact responsible for the tag (name and contact information)
    pub contact: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum WrappedExtension<'a> {
    Extension(Extension<'a>),
    PseudoExtension(PseudoExtension<'a>),
}

impl<'a> FromIterator<MaybeComment<'a, Items<'a>>> for Registry<'a> {
    fn from_iter<T: IntoIterator<Item = MaybeComment<'a, Items<'a>>>>(iter: T) -> Self {
        Self(CommentendChildren(iter.into_iter().collect()))
    }
}

impl<'a> Parse<'a> for Registry<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("registry") {
            parse_children(node).map(Self).map(Some)
        } else {
            Ok(None)
        }
    }
}

impl<'a> Parse<'a> for Items<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        match node.tag_name().name() {
            "platforms" => Ok(Some(Items::Platforms {
                platforms: parse_children(node)?,
                comment: try_attribute(node, "comment")?,
            })),
            "tags" => Ok(Some(Items::Tags {
                tags: parse_children(node)?,
                comment: try_attribute(node, "comment")?,
            })),
            "types" => Ok(Some(Items::Types {
                types: parse_children(node)?,
                comment: try_attribute(node, "comment")?,
            })),
            "enums" => Ok(Some(Items::Enums(Parse::parse(node)?))),
            "commands" => Ok(Some(Items::Commands {
                commands: parse_children(node)?,
                comment: try_attribute(node, "comment")?,
            })),
            "feature" => Ok(Some(Items::Features(Parse::parse(node)?))),
            "extensions" => Ok(Some(Items::Extensions {
                extensions: parse_children(node)?,
                comment: try_attribute(node, "comment")?,
            })),
            "formats" => Ok(Some(Items::Formats(parse_children(node)?))),
            "spirvextensions" => Ok(Some(Items::SpirvExtensions {
                extensions: parse_children(node)?,
                comment: try_attribute(node, "comment")?,
            })),
            "spirvcapabilities" => Ok(Some(Items::SpirvCapabilities {
                capabilities: parse_children(node)?,
                comment: try_attribute(node, "comment")?,
            })),
            _ => Ok(None),
        }
    }
}

impl<'a> Parse<'a> for Platform<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
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

impl<'a> Parse<'a> for Tag<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
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

impl<'a> Parse<'a> for WrappedExtension<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(e) = Extension::try_parse(node)? {
            Ok(Some(Self::Extension(e)))
        } else if let Some(e) = PseudoExtension::try_parse(node)? {
            Ok(Some(Self::PseudoExtension(e)))
        } else {
            Ok(None)
        }
    }
}
