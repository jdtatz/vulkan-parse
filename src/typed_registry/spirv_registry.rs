use std::fmt;

use super::common::StdVersion;
use crate::{Expression, StdVersionParseError};

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[vkxml(tag = "spirvextension")]
pub struct SpirvExtension<'a> {
    #[vkxml(attribute)]
    pub name: &'a str,
    // TODO, usually the only diffrence between `name` & `enable_extension` is the prefix ("SPV_" vs "VK_"), but not always
    #[vkxml(child)]
    pub enable_version: Option<VersionEnable>,
    #[vkxml(child)]
    pub enable_extension: ExtensionEnable<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "spirvcapability")]
pub struct SpirvCapability<'a> {
    #[vkxml(attribute)]
    pub name: &'a str,
    #[vkxml(child)]
    pub enables: Vec<EnableSpirvCapability<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "enable")]
pub enum EnableSpirvCapability<'a> {
    #[vkxml(discriminant = "version")]
    Version(VersionEnable),
    #[vkxml(discriminant = "extension")]
    Extension(ExtensionEnable<'a>),
    #[vkxml(discriminant = "struct")]
    Struct(StructEnable<'a>),
    #[vkxml(discriminant = "property")]
    Property(PropertyEnable<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, TryFromEscapedStr, DisplayEscaped)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum EnableRequires<'a> {
    Core(StdVersion),
    Extension(&'a str),
    Mix(StdVersion, &'a str),
}

impl<'a, 'de: 'a> TryFrom<&'de str> for EnableRequires<'a> {
    type Error = StdVersionParseError;

    fn try_from(s: &'de str) -> Result<Self, Self::Error> {
        Ok(if let Some((v, e)) = s.split_once(',') {
            EnableRequires::Mix(v.parse()?, e)
        } else if let Ok(v) = s.parse() {
            EnableRequires::Core(v)
        } else {
            EnableRequires::Extension(s)
        })
    }
}
impl<'a> fmt::Display for EnableRequires<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EnableRequires::Core(v) => write!(f, "{v}"),
            EnableRequires::Extension(e) => write!(f, "{e}"),
            EnableRequires::Mix(v, e) => write!(f, "{v},{e}"),
        }
    }
}

/// If the API version is supported, the SPIR-V extension or capability is enabled.
#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "enable", discriminant = "version")]
pub struct VersionEnable {
    #[vkxml(attribute)]
    pub version: StdVersion,
}

/// If the API extension is supported and enabled, the SPIR-V extension or capability is enabled.
#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "enable")]
pub struct ExtensionEnable<'a> {
    #[vkxml(attribute)]
    pub extension: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct StructEnable<'a> {
    /// API feature structure name
    #[vkxml(attribute(rename = "struct"))]
    pub name: &'a str,
    /// API feature name, matching the name of a member of the `name` structure
    #[vkxml(attribute)]
    pub feature: &'a str,
    /// list of API feature version numbers and/or extension names.
    #[vkxml(attribute)]
    pub requires: EnableRequires<'a>,
    /// Another API feature name which is an alias of `feature`. Needed when the same feature is provided by two different API versions or extensions.
    #[vkxml(attribute)]
    pub alias: Option<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct PropertyEnable<'a> {
    /// API property structure name
    #[vkxml(attribute(rename = "property"))]
    pub name: &'a str,
    /// API property name, matching the name of a member of the `name` structure
    #[vkxml(attribute)]
    pub member: &'a str,
    /// A value, matching an API enum value. If the property is a bitfield, `value` must be a bitmask value belonging to the `member` bitfield type. Otherwise, `value` must be an enum name defined for the `member` enumeration type.
    #[vkxml(attribute)]
    pub value: Expression<'a>,
    /// list of API feature version numbers and/or extension names.
    #[vkxml(attribute)]
    pub requires: EnableRequires<'a>,
}
