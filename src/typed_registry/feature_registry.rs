use core::fmt;

use super::common::{AliasDeprecationKind, CommentendChildren, SemVarVersion};
use crate::{Expression, ParseResult, StdVersion, UnescapedStr, XMLElementBuilder};

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

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[vkxml(tag = "feature")]
pub struct Feature<'a> {
    /// version C preprocessor name
    #[vkxml(attribute)]
    pub name: &'a str,
    /// API tag used internally, not necessarily an actual API name
    #[vkxml(attribute(seperator = crate::CommaSeperator))]
    pub api: enumflags2::BitFlags<VulkanApi>,
    #[vkxml(attribute)]
    pub depends: Option<&'a str>,
    /// version number
    #[vkxml(attribute)]
    pub number: SemVarVersion,
    /// descriptive text with no semantic meaning
    #[vkxml(attribute)]
    pub comment: Option<UnescapedStr<'a>>,
    /// features to require/remove in this version
    #[vkxml(child)]
    pub children: Vec<FeatureChild<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum FeatureChild<'a> {
    /// features to require in this version
    Require(Require<'a>),
    /// features to remove in this version
    Remove(Remove<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[vkxml(tag = "require")]
pub struct Require<'a> {
    /// descriptive text with no semantic meaning
    #[vkxml(attribute)]
    pub comment: Option<UnescapedStr<'a>>,
    /// API feature name
    #[vkxml(attribute)]
    pub feature: Option<StdVersion>,
    #[vkxml(attribute)]
    pub api: Option<VulkanApi>,
    #[vkxml(attribute(rename = "depends"))]
    pub dependencies: Option<VulkanDependencies<'a>>,
    #[vkxml(child)]
    pub values: CommentendChildren<'a, RequireValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[vkxml(tag = "remove")]
pub struct Remove<'a> {
    /// descriptive text with no semantic meaning
    #[vkxml(attribute)]
    pub comment: Option<UnescapedStr<'a>>,
    /// API feature name
    #[vkxml(attribute)]
    pub feature: Option<StdVersion>,
    #[vkxml(attribute)]
    pub api: Option<VulkanApi>,
    #[vkxml(attribute)]
    pub reasonlink: Option<&'a str>,
    #[vkxml(attribute(rename = "depends"))]
    pub dependencies: Option<VulkanDependencies<'a>>,
    #[vkxml(child)]
    pub values: CommentendChildren<'a, RequireValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum RequireValue<'a> {
    #[vkxml(tag = "type")]
    Type {
        #[vkxml(attribute)]
        name: &'a str,
        /// descriptive text with no semantic meaning
        #[vkxml(attribute)]
        comment: Option<UnescapedStr<'a>>,
    },
    #[vkxml(tag = "command")]
    Command {
        #[vkxml(attribute)]
        name: &'a str,
        /// descriptive text with no semantic meaning
        #[vkxml(attribute)]
        comment: Option<UnescapedStr<'a>>,
    },
    #[vkxml(tag = "feature")]
    Feature {
        #[vkxml(attribute)]
        name: &'a str,
        #[vkxml(attribute(rename = "struct"))]
        struct_name: &'a str,
        /// descriptive text with no semantic meaning
        #[vkxml(attribute)]
        comment: Option<UnescapedStr<'a>>,
    },
    Enum(RequireEnum<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[vkxml(tag = "enum")]
pub struct RequireEnum<'a> {
    #[vkxml(attribute)]
    pub name: Option<&'a str>,
    /// name of a separately defined enumerated type to which the extension enumerant is added
    #[vkxml(attribute)]
    pub extends: Option<&'a str>,
    /// If `None` then it's a Refrence Enum, otherwise it's an Extension Enum
    #[vkxml(attribute(flattened))]
    pub value: Option<RequireValueEnum<'a>>,
    /// preprocessor protection symbol for the enum
    #[vkxml(attribute)]
    pub protect: Option<&'a str>,
    #[vkxml(attribute)]
    pub api: Option<VulkanApi>,
    #[vkxml(attribute)]
    pub deprecated: Option<AliasDeprecationKind>,
    /// descriptive text with no semantic meaning
    #[vkxml(attribute)]
    pub comment: Option<UnescapedStr<'a>>,
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
    fn from_attributes(
        attributes: &[crate::Attribute<'xml>],
        location: crate::Location,
    ) -> ParseResult<Result<Self, &'static [&'static str]>> {
        if let Some(value) = crate::try_from_attribute(attributes, "value", location)? {
            Ok(Ok(RequireValueEnum::Value(value)))
        } else if let Some(alias) = crate::try_from_attribute(attributes, "alias", location)? {
            Ok(Ok(RequireValueEnum::Alias(alias)))
        } else if let Some(offset) = crate::try_from_attribute(attributes, "offset", location)? {
            Ok(Ok(RequireValueEnum::Offset {
                // extnumber: attribute(node, "extnumber")?,
                extnumber: crate::try_from_attribute(attributes, "extnumber", location)?,
                offset,
                direction: crate::try_from_attribute(attributes, "dir", location)?,
            }))
        } else if let Some(bitpos) = crate::try_from_attribute(attributes, "bitpos", location)? {
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
