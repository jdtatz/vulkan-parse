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
use crate::UnescapedStr;

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "registry")]
pub struct Registry<'a> {
    #[vkxml(child)]
    pub registry: CommentendChildren<'a, Items<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum Items<'a> {
    #[vkxml(tag = "platforms")]
    Platforms {
        #[vkxml(child)]
        platforms: Vec<Platform<'a>>,
        #[vkxml(attribute)]
        comment: Option<UnescapedStr<'a>>,
    },
    #[vkxml(tag = "tags")]
    Tags {
        #[vkxml(child)]
        tags: Vec<Tag<'a>>,
        #[vkxml(attribute)]
        comment: Option<UnescapedStr<'a>>,
    },
    #[vkxml(tag = "types")]
    Types {
        #[vkxml(child)]
        types: CommentendChildren<'a, Type<'a>>,
        #[vkxml(attribute)]
        comment: Option<UnescapedStr<'a>>,
    },
    Enums(Enums<'a>),
    #[vkxml(tag = "commands")]
    Commands {
        #[vkxml(child)]
        commands: CommentendChildren<'a, DefinitionOrAlias<'a, Command<'a>>>,
        #[vkxml(attribute)]
        comment: Option<UnescapedStr<'a>>,
    },
    Features(Feature<'a>),
    #[vkxml(tag = "extensions")]
    Extensions {
        #[vkxml(child)]
        extensions: Vec<WrappedExtension<'a>>,
        #[vkxml(attribute)]
        comment: Option<UnescapedStr<'a>>,
    },
    #[vkxml(tag = "formats")]
    Formats {
        #[vkxml(child)]
        formats: Vec<Format<'a>>,
    },
    #[vkxml(tag = "spirvextensions")]
    SpirvExtensions {
        #[vkxml(child)]
        extensions: Vec<SpirvExtension<'a>>,
        #[vkxml(attribute)]
        comment: Option<UnescapedStr<'a>>,
    },
    #[vkxml(tag = "spirvcapabilities")]
    SpirvCapabilities {
        #[vkxml(child)]
        capabilities: Vec<SpirvCapability<'a>>,
        #[vkxml(attribute)]
        comment: Option<UnescapedStr<'a>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[vkxml(tag = "platform")]
pub struct Platform<'a> {
    /// name of the platform, used as part of extension names
    #[vkxml(attribute)]
    pub name: &'a str,
    /// preprocessor symbol to include platform headers from <vulkan.h>
    #[vkxml(attribute)]
    pub protect: &'a str,
    /// platform description
    #[vkxml(attribute)]
    pub comment: Option<UnescapedStr<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "tag")]
pub struct Tag<'a> {
    #[vkxml(attribute)]
    pub name: &'a str,
    /// name of the author (usually a company or project name)
    #[vkxml(attribute)]
    pub author: &'a str,
    /// contact responsible for the tag (name and contact information)
    #[vkxml(attribute)]
    pub contact: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "extension")]
pub enum WrappedExtension<'a> {
    #[vkxml(discriminant = "number")]
    Extension(Extension<'a>),
    PseudoExtension(PseudoExtension<'a>),
}
