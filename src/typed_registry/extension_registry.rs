use core::str::FromStr;
use std::fmt;

use super::{
    FeatureChild,
    common::StdVersion,
    feature_registry::{Require, VulkanApi},
};
use crate::{UnescapedStr, VulkanDependencies};

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
pub enum ExtensionKind {
    #[strum(serialize = "instance")]
    Instance,
    #[strum(serialize = "device")]
    Device,
    // PhysicalDevice,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromEscapedStr, DisplayEscaped)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum ExtensionSupport {
    Api(enumflags2::BitFlags<VulkanApi>),
    Disabled,
}

impl FromStr for ExtensionSupport {
    type Err = <VulkanApi as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "disabled" {
            Ok(Self::Disabled)
        } else {
            s.split(',')
                .map(VulkanApi::from_str)
                .collect::<Result<_, Self::Err>>()
                .map(Self::Api)
        }
    }
}

impl fmt::Display for ExtensionSupport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtensionSupport::Api(value) => write!(
                f,
                "{}",
                crate::InterspersedDisplay::<crate::CommaSeperator, _>::new(value)
            ),
            ExtensionSupport::Disabled => write!(f, "disabled"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TryFromEscapedStr, DisplayEscaped)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum ExtensionPromotion<'a> {
    Core(StdVersion),
    Extension(&'a str),
}

impl<'a, 'de: 'a> From<&'de str> for ExtensionPromotion<'a> {
    fn from(s: &'de str) -> Self {
        if let Ok(ver) = s.parse() {
            Self::Core(ver)
        } else {
            Self::Extension(s)
        }
    }
}

impl<'a> fmt::Display for ExtensionPromotion<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Core(v) => write!(f, "{v}"),
            Self::Extension(e) => write!(f, "{e}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[vkxml(tag = "extension")]
pub struct Extension<'a> {
    /// extension name string
    #[vkxml(attribute)]
    pub name: &'a str,
    /// extension number (positive integer, should be unique)
    #[vkxml(attribute)]
    pub number: u32,
    #[vkxml(attribute(rename = "type"))]
    pub kind: Option<ExtensionKind>,
    /// profile name(s) supporting this extension, e.g. 'vulkan' or 'disabled' to never generate output.
    #[vkxml(attribute)]
    pub supported: ExtensionSupport,
    /// vulkan versions / extensions required
    #[vkxml(attribute(rename = "depends"))]
    pub dependencies: Option<VulkanDependencies<'a>>,
    /// vulkan apis that the extension has been ratified by Khronos for
    #[vkxml(attribute(seperator = crate::CommaSeperator))]
    pub ratified: Option<enumflags2::BitFlags<VulkanApi>>,

    // Only missing for VK_RESERVED_do_not_use_94 & VK_RESERVED_do_not_use_146
    /// name of the author (usually a company or project name)
    #[vkxml(attribute)]
    pub author: Option<&'a str>,
    // Only missing for VK_KHR_mir_surface, VK_RESERVED_do_not_use_94, & VK_RESERVED_do_not_use_146
    /// contact responsible for the tag (name and contact information)
    #[vkxml(attribute)]
    pub contact: Option<&'a str>,
    /// Vulkan version or a name of an extension that this extension was promoted to
    #[vkxml(attribute(rename = "promotedto"))]
    pub promoted_to: Option<ExtensionPromotion<'a>>,
    /// Vulkan version or a name of an extension that deprecates this extension
    #[vkxml(attribute(rename = "deprecatedby"))]
    pub deprecated_by: Option<ExtensionPromotion<'a>>,
    /// Vulkan version or a name of an extension that obsoletes this extension
    #[vkxml(attribute(rename = "obsoletedby"))]
    pub obsoleted_by: Option<ExtensionPromotion<'a>>,
    /// order relative to other extensions, default 0
    #[vkxml(attribute(rename = "sortorder"))]
    pub sort_order: Option<i32>,
    /// should be one of the platform names defined in the <platform> tag.
    #[vkxml(attribute)]
    pub platform: Option<&'a str>,
    /// is this extension released provisionally
    #[vkxml(attribute)]
    pub provisional: Option<bool>,
    /// List of the extension's special purposes
    #[vkxml(attribute(rename = "specialuse", seperator = crate::CommaSeperator))]
    pub special_use: Option<Vec<&'a str>>,
    /// descriptive text with no semantic meaning
    #[vkxml(attribute)]
    pub comment: Option<UnescapedStr<'a>>,
    #[vkxml(child)]
    pub children: Vec<FeatureChild<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[vkxml(tag = "extension")]
pub struct PseudoExtension<'a> {
    /// extension name string
    #[vkxml(attribute)]
    pub name: &'a str,
    /// profile name(s) supporting this extension, e.g. 'vulkan' or 'disabled' to never generate output.
    #[vkxml(attribute)]
    pub supported: ExtensionSupport,
    /// descriptive text with no semantic meaning
    #[vkxml(attribute)]
    pub comment: Option<UnescapedStr<'a>>,
    #[vkxml(child)]
    pub requires: Vec<Require<'a>>,
}
