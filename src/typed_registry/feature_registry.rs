use core::fmt;

use roxmltree::Node;

use super::common::{AliasDeprecationKind, CommentendChildren, SemVarVersion};
use crate::{try_attribute, Expression, ParseResult, StdVersion, XMLElementBuilder};

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    strum::EnumString,
    strum::Display,
    TryFromEscapedStr,
    DisplayEscaped,
)]
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
#[derive(Debug, Clone, PartialEq, Eq, TryFromEscapedStr, DisplayEscaped)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct VulkanDependencies<'a>(&'a str);

impl<'a, 'de: 'a> From<&'de str> for VulkanDependencies<'a> {
    fn from(value: &'de str) -> Self {
        VulkanDependencies(value)
    }
}

impl fmt::Display for VulkanDependencies<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "feature")]
pub struct Feature<'a> {
    /// version C preprocessor name
    #[xml(attribute())]
    pub name: &'a str,
    /// API tag used internally, not necessarily an actual API name
    #[xml(attribute(seperator = "crate::CommaSeperator"))]
    pub api: enumflags2::BitFlags<VulkanApi>,
    /// version number
    #[xml(attribute())]
    pub number: SemVarVersion,
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<&'a str>,
    /// features to require/remove in this version
    #[xml(child)]
    pub children: Vec<FeatureChild<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum FeatureChild<'a> {
    /// features to require in this version
    Require(Require<'a>),
    /// features to remove in this version
    Remove(Remove<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "require")]
pub struct Require<'a> {
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<&'a str>,
    /// API feature name
    #[xml(attribute())]
    pub feature: Option<StdVersion>,
    #[xml(attribute())]
    pub api: Option<VulkanApi>,
    #[xml(attribute(rename = "depends"))]
    pub dependencies: Option<VulkanDependencies<'a>>,
    #[xml(child)]
    pub values: CommentendChildren<'a, RequireValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "remove")]
pub struct Remove<'a> {
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<&'a str>,
    /// API feature name
    #[xml(attribute())]
    pub feature: Option<StdVersion>,
    #[xml(attribute())]
    pub api: Option<VulkanApi>,
    #[xml(attribute(rename = "depends"))]
    pub dependencies: Option<VulkanDependencies<'a>>,
    #[xml(child)]
    pub values: CommentendChildren<'a, RequireValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum RequireValue<'a> {
    #[xml(tag = "type")]
    Type {
        #[xml(attribute())]
        name: &'a str,
        /// descriptive text with no semantic meaning
        #[xml(attribute())]
        comment: Option<&'a str>,
    },
    #[xml(tag = "command")]
    Command {
        #[xml(attribute())]
        name: &'a str,
        /// descriptive text with no semantic meaning
        #[xml(attribute())]
        comment: Option<&'a str>,
    },
    Enum(RequireEnum<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "enum")]
pub struct RequireEnum<'a> {
    #[xml(attribute())]
    pub name: Option<&'a str>,
    /// name of a separately defined enumerated type to which the extension enumerant is added
    #[xml(attribute())]
    pub extends: Option<&'a str>,
    /// If `None` then it's a Refrence Enum, otherwise it's an Extension Enum
    #[xml(attribute(flattened))]
    pub value: Option<RequireValueEnum<'a>>,
    /// preprocessor protection symbol for the enum
    #[xml(attribute())]
    pub protect: Option<&'a str>,
    #[xml(attribute())]
    pub api: Option<VulkanApi>,
    #[xml(attribute())]
    pub deprecated: Option<AliasDeprecationKind>,
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<&'a str>,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    strum::EnumString,
    strum::Display,
    TryFromEscapedStr,
    DisplayEscaped,
)]
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

impl<'a, 'xml: 'a> crate::FromAttributes<'xml> for RequireValueEnum<'a> {
    fn from_attributes<'input: 'xml>(
        node: Node<'xml, 'input>,
    ) -> ParseResult<Result<Self, &'static [&'static str]>> {
        if let Some(value) = try_attribute(node, "value")? {
            Ok(Ok(RequireValueEnum::Value(value)))
        } else if let Some(alias) = try_attribute(node, "alias")? {
            Ok(Ok(RequireValueEnum::Alias(alias)))
        } else if let Some(offset) = try_attribute(node, "offset")? {
            Ok(Ok(RequireValueEnum::Offset {
                // extnumber: attribute(node, "extnumber")?,
                extnumber: try_attribute(node, "extnumber")?,
                offset,
                direction: try_attribute(node, "dir")?,
            }))
        } else if let Some(bitpos) = try_attribute(node, "bitpos")? {
            Ok(Ok(RequireValueEnum::Bitpos(bitpos)))
        } else {
            Ok(Err(&["value", "alias", "offset", "bitpos"]))
        }
    }
}

impl<'a> crate::IntoXMLAttributes for RequireValueEnum<'a> {
    fn write_attributes<'t, 'w, W: ?Sized + crate::XMLWriter>(
        &self,
        element: XMLElementBuilder<'t, 'w, W>,
    ) -> Result<XMLElementBuilder<'t, 'w, W>, W::Error> {
        match self {
            RequireValueEnum::Value(v) => element.with_escaped_attribute("value", v),
            RequireValueEnum::Alias(v) => element.with_escaped_attribute("alias", v),
            RequireValueEnum::Offset {
                extnumber,
                offset,
                direction,
            } => element
                .with_escaped_attribute("offset", offset)?
                .with_escaped_attribute("extnumber", extnumber)?
                .with_escaped_attribute("dir", direction),
            RequireValueEnum::Bitpos(v) => element.with_escaped_attribute("bitpos", v),
        }
    }
}
