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
use crate::{attribute, try_attribute, Parse, ParseElements, ParseResult};

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

impl<'a, 'input: 'a> ParseElements<'a, 'input> for Registry<'a> {
    type Item = MaybeComment<'a, Items<'a>>;

    type NodeIter = roxmltree::Children<'a, 'input>;

    fn get_nodes(node: Node<'a, 'input>) -> ParseResult<Option<Self::NodeIter>> {
        if node.has_tag_name("registry") {
            Ok(Some(node.children()))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for Items<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        match node.tag_name().name() {
            "platforms" => Ok(Some(Items::Platforms {
                platforms: Parse::parse(node)?,
                comment: try_attribute(node, "comment")?,
            })),
            "tags" => Ok(Some(Items::Tags {
                tags: Parse::parse(node)?,
                comment: try_attribute(node, "comment")?,
            })),
            "types" => Ok(Some(Items::Types {
                types: Parse::parse(node)?,
                comment: try_attribute(node, "comment")?,
            })),
            "enums" => Ok(Some(Items::Enums(Parse::parse(node)?))),
            "commands" => Ok(Some(Items::Commands {
                commands: Parse::parse(node)?,
                comment: try_attribute(node, "comment")?,
            })),
            "feature" => Ok(Some(Items::Features(Parse::parse(node)?))),
            "extensions" => Ok(Some(Items::Extensions {
                extensions: Parse::parse(node)?,
                comment: try_attribute(node, "comment")?,
            })),
            "formats" => Ok(Some(Items::Formats(Parse::parse(node)?))),
            "spirvextensions" => Ok(Some(Items::SpirvExtensions {
                extensions: Parse::parse(node)?,
                comment: try_attribute(node, "comment")?,
            })),
            "spirvcapabilities" => Ok(Some(Items::SpirvCapabilities {
                capabilities: Parse::parse(node)?,
                comment: try_attribute(node, "comment")?,
            })),
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
