use core::fmt;

use roxmltree::Node;

use super::common::{CommentendChildren, SemVarVersion};
use crate::{
    attribute, parse_children, try_attribute, AliasDeprecationKind, Expression, Parse, ParseResult,
    StdVersion,
};

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum VulkanApi {
    #[strum(serialize = "vulkan")]
    Vulkan,
    #[strum(serialize = "vulkansc")]
    SafetyCriticalVulkan,
}

// TODO
// "VK_KHR_get_physical_device_properties2,VK_VERSION_1_1"
// "VK_KHR_get_physical_device_properties2+VK_KHR_storage_buffer_storage_class"
// "VK_KHR_swapchain+VK_KHR_get_surface_capabilities2+(VK_KHR_get_physical_device_properties2,VK_VERSION_1_1)"
// "VK_KHR_swapchain+(VK_KHR_maintenance2,VK_VERSION_1_1)+(VK_KHR_image_format_list,VK_VERSION_1_2)"
/// list of versions / extension names seperated by '+', ',', and/or paranthized
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct VulkanDependencies<'a>(&'a str);

impl<'a> From<&'a str> for VulkanDependencies<'a> {
    fn from(value: &'a str) -> Self {
        VulkanDependencies(value)
    }
}

impl fmt::Display for VulkanDependencies<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct Feature<'a> {
    /// version C preprocessor name
    pub name: &'a str,
    /// API tag used internally, not necessarily an actual API name
    pub api: enumflags2::BitFlags<VulkanApi>,
    /// version number
    pub number: SemVarVersion,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
    /// features to require/remove in this version
    pub children: Vec<FeatureChild<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum FeatureChild<'a> {
    /// features to require in this version
    Require(Require<'a>),
    /// features to remove in this version
    Remove(Remove<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct Require<'a> {
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
    /// API feature name
    pub feature: Option<StdVersion>,
    pub api: Option<VulkanApi>,
    pub dependencies: Option<VulkanDependencies<'a>>,
    pub values: CommentendChildren<'a, RequireValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct Remove<'a> {
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
    /// API feature name
    pub feature: Option<StdVersion>,
    pub api: Option<VulkanApi>,
    pub dependencies: Option<VulkanDependencies<'a>>,
    pub values: CommentendChildren<'a, RequireValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum RequireValue<'a> {
    Type {
        name: &'a str,
        /// descriptive text with no semantic meaning
        comment: Option<&'a str>,
    },
    Command {
        name: &'a str,
        /// descriptive text with no semantic meaning
        comment: Option<&'a str>,
    },
    Enum(RequireEnum<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct RequireEnum<'a> {
    pub name: Option<&'a str>,
    /// name of a separately defined enumerated type to which the extension enumerant is added
    pub extends: Option<&'a str>,
    /// If `None` then it's a Refrence Enum, otherwise it's an Exstension Enum
    pub value: Option<RequireValueEnum<'a>>,
    /// preprocessor protection symbol for the enum
    pub protect: Option<&'a str>,
    pub api: Option<VulkanApi>,
    pub deprecated: Option<AliasDeprecationKind>,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum OffsetDirection {
    #[strum(serialize = "-")]
    Negative,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum RequireValueEnum<'a> {
    Value(Expression<'a>),
    Alias(&'a str),
    Offset {
        // Required for <feature><require>, but optional in <extension><require>
        // extnumber: u32,
        /// extension number. The extension number in turn specifies the starting value of a block (range) of values reserved for enumerants
        extnumber: Option<u32>,
        /// the offset within an extension block
        offset: u32,
        /// if present, the calculated enumerant value will be negative, instead of positive. Negative enumerant values are normally used only for Vulkan error codes
        direction: Option<OffsetDirection>,
    },
    Bitpos(u8),
}

impl<'a> Parse<'a> for Feature<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("feature") {
            Ok(Some(Feature {
                name: attribute(node, "name")?,
                api: attribute::<_, false>(node, "api").map(crate::CommaSeperated::into)?,
                number: attribute(node, "number")?,
                comment: try_attribute(node, "comment")?,
                children: parse_children(node)?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a> Parse<'a> for FeatureChild<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(if let Some(v) = Parse::try_parse(node)? {
            Some(FeatureChild::Require(v))
        } else if let Some(v) = Parse::try_parse(node)? {
            Some(FeatureChild::Remove(v))
        } else {
            None
        })
    }
}

impl<'a> Parse<'a> for Require<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("require") {
            Ok(Some(Require {
                feature: try_attribute(node, "feature")?,
                dependencies: try_attribute(node, "depends")?,
                api: try_attribute::<_, false>(node, "api")?,
                comment: try_attribute(node, "comment")?,
                values: parse_children(node)?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a> Parse<'a> for Remove<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("remove") {
            Ok(Some(Remove {
                feature: try_attribute(node, "feature")?,
                dependencies: try_attribute(node, "depends")?,
                api: try_attribute::<_, false>(node, "api")?,
                comment: try_attribute(node, "comment")?,
                values: parse_children(node)?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a> Parse<'a> for RequireValue<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        match node.tag_name().name() {
            "type" => Ok(Some(RequireValue::Type {
                name: attribute(node, "name")?,
                comment: try_attribute(node, "comment")?,
            })),
            "command" => Ok(Some(RequireValue::Command {
                name: attribute(node, "name")?,
                comment: try_attribute(node, "comment")?,
            })),
            "enum" => Ok(Some(RequireValue::Enum(RequireEnum {
                name: try_attribute(node, "name")?,
                extends: try_attribute(node, "extends")?,
                protect: try_attribute(node, "protect")?,
                value: Parse::try_parse(node)?,
                api: try_attribute::<_, false>(node, "api")?,
                deprecated: try_attribute::<_, false>(node, "deprecated")?,
                comment: try_attribute(node, "comment")?,
            }))),
            _ => Ok(None),
        }
    }
}

impl<'a> Parse<'a> for RequireValueEnum<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(value) = try_attribute(node, "value")? {
            Ok(Some(RequireValueEnum::Value(value)))
        } else if let Some(alias) = try_attribute(node, "alias")? {
            Ok(Some(RequireValueEnum::Alias(alias)))
        } else if let Some(offset) = try_attribute(node, "offset")? {
            Ok(Some(RequireValueEnum::Offset {
                // extnumber: attribute(node, "extnumber")?,
                extnumber: try_attribute(node, "extnumber")?,
                offset,
                direction: try_attribute::<_, false>(node, "dir")?,
            }))
        } else if let Some(bitpos) = try_attribute(node, "bitpos")? {
            Ok(Some(RequireValueEnum::Bitpos(bitpos)))
        } else {
            Ok(None)
        }
    }
}
