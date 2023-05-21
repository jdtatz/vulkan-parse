use roxmltree::Node;

use super::{
    commands_registry::Command,
    common::{CommentendChildren, DefinitionOrAlias},
    enums_registry::Enums,
    extension_registry::{Extension, PseudoExtension},
    feature_registry::Feature,
    format_registry::Format,
    spirv_registry::{SpirvCapability, SpirvExtension},
    types_registry::Type,
};

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[xml(tag = "registry")]
pub struct Registry<'a> {
    #[xml(child)]
    pub registry: CommentendChildren<'a, Items<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum Items<'a> {
    #[xml(tag = "platforms")]
    Platforms {
        #[xml(child)]
        platforms: Vec<Platform<'a>>,
        #[xml(attribute())]
        comment: Option<&'a str>,
    },
    #[xml(tag = "tags")]
    Tags {
        #[xml(child)]
        tags: Vec<Tag<'a>>,
        #[xml(attribute())]
        comment: Option<&'a str>,
    },
    #[xml(tag = "types")]
    Types {
        #[xml(child)]
        types: CommentendChildren<'a, Type<'a>>,
        #[xml(attribute())]
        comment: Option<&'a str>,
    },
    Enums(Enums<'a>),
    #[xml(tag = "commands")]
    Commands {
        #[xml(child)]
        commands: CommentendChildren<'a, DefinitionOrAlias<'a, Command<'a>>>,
        #[xml(attribute())]
        comment: Option<&'a str>,
    },
    Features(Feature<'a>),
    #[xml(tag = "extensions")]
    Extensions {
        #[xml(child)]
        extensions: Vec<WrappedExtension<'a>>,
        #[xml(attribute())]
        comment: Option<&'a str>,
    },
    #[xml(tag = "formats")]
    Formats {
        #[xml(child)]
        formats: Vec<Format<'a>>,
    },
    #[xml(tag = "spirvextensions")]
    SpirvExtensions {
        #[xml(child)]
        extensions: Vec<SpirvExtension<'a>>,
        #[xml(attribute())]
        comment: Option<&'a str>,
    },
    #[xml(tag = "spirvcapabilities")]
    SpirvCapabilities {
        #[xml(child)]
        capabilities: Vec<SpirvCapability<'a>>,
        #[xml(attribute())]
        comment: Option<&'a str>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "platform")]
pub struct Platform<'a> {
    /// name of the platform, used as part of extension names
    #[xml(attribute())]
    pub name: &'a str,
    /// preprocessor symbol to include platform headers from <vulkan.h>
    #[xml(attribute())]
    pub protect: &'a str,
    /// platform description
    #[xml(attribute())]
    pub comment: Option<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[xml(tag = "tag")]
pub struct Tag<'a> {
    #[xml(attribute())]
    pub name: &'a str,
    /// name of the author (usually a company or project name)
    #[xml(attribute())]
    pub author: &'a str,
    /// contact responsible for the tag (name and contact information)
    #[xml(attribute())]
    pub contact: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[xml(tag = "extension")]
pub enum WrappedExtension<'a> {
    #[xml(discriminant(attr = "number"))]
    Extension(Extension<'a>),
    PseudoExtension(PseudoExtension<'a>),
}
