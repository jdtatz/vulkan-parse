use std::{borrow::Cow, fmt};

use roxmltree::Node;

use super::{
    common::{SemVarVersion, StdVersion},
    feature_registry::Require,
};
use crate::{attribute, try_attribute, try_attribute_fs, try_attribute_sep, Parse, ParseResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum ExtensionKind {
    #[strum(serialize = "instance")]
    Instance,
    #[strum(serialize = "device")]
    Device,
    // PhysicalDevice,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum ExtensionSupport {
    #[strum(serialize = "vulkan")]
    Vulkan,
    #[strum(serialize = "disabled")]
    Disabled,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum ExtensionPromotion<'a> {
    Core(StdVersion),
    Extension(Cow<'a, str>),
}

impl<'a> From<&'a str> for ExtensionPromotion<'a> {
    fn from(s: &'a str) -> Self {
        if let Ok(ver) = s.parse() {
            Self::Core(ver)
        } else {
            Self::Extension(Cow::Borrowed(s))
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct Extension<'a> {
    /// extension name string
    pub name: Cow<'a, str>,
    /// extension number (positive integer, should be unique)
    pub number: u32,
    pub kind: Option<ExtensionKind>,
    /// profile name(s) supporting this extension, e.g. 'vulkan' or 'disabled' to never generate output.
    pub supported: ExtensionSupport,
    /// core version of Vulkan required by the extension
    pub requires_core: Option<SemVarVersion>,
    /// list of extension names required by this
    pub requires_depencies: Option<Vec<Cow<'a, str>>>,

    // Only missing for VK_RESERVED_do_not_use_94 & VK_RESERVED_do_not_use_146
    /// name of the author (usually a company or project name)
    pub author: Option<Cow<'a, str>>,
    // Only missing for VK_KHR_mir_surface, VK_RESERVED_do_not_use_94, & VK_RESERVED_do_not_use_146
    /// contact responsible for the tag (name and contact information)
    pub contact: Option<Cow<'a, str>>,
    /// Vulkan version or a name of an extension that this extension was promoted to
    pub promoted_to: Option<ExtensionPromotion<'a>>,
    /// Vulkan version or a name of an extension that deprecates this extension
    pub deprecated_by: Option<ExtensionPromotion<'a>>,
    /// Vulkan version or a name of an extension that obsoletes this extension
    pub obsoleted_by: Option<ExtensionPromotion<'a>>,
    /// order relative to other extensions, default 0
    pub sort_order: Option<i32>,
    /// should be one of the platform names defined in the <platform> tag.
    pub platform: Option<Cow<'a, str>>,
    /// is this extension released provisionally
    pub provisional: Option<bool>,
    /// List of the extension's special purposes
    pub special_use: Option<Vec<Cow<'a, str>>>,
    /// descriptive text with no semantic meaning
    pub comment: Option<Cow<'a, str>>,
    pub requires: Vec<Require<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct PseudoExtension<'a> {
    /// extension name string
    pub name: Cow<'a, str>,
    /// profile name(s) supporting this extension, e.g. 'vulkan' or 'disabled' to never generate output.
    pub supported: ExtensionSupport,
    /// descriptive text with no semantic meaning
    pub comment: Option<Cow<'a, str>>,
    pub requires: Vec<Require<'a>>,
}

impl<'a, 'input> Parse<'a, 'input> for Extension<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("extension") {
            if let Some(number) = try_attribute_fs(node, "number")? {
                Ok(Some(Extension {
                    name: attribute(node, "name")?,
                    number,
                    kind: try_attribute(node, "type")?,
                    supported: attribute(node, "supported")?,
                    requires_core: try_attribute_fs(node, "requiresCore")?,
                    requires_depencies: try_attribute_sep::<_, ','>(node, "requires")?,
                    author: try_attribute(node, "author")?,
                    contact: try_attribute(node, "contact")?,
                    promoted_to: try_attribute(node, "promotedto")?,
                    deprecated_by: try_attribute(node, "deprecatedby")?,
                    obsoleted_by: try_attribute(node, "obsoletedby")?,
                    sort_order: try_attribute_fs(node, "sortorder")?,
                    requires: Parse::parse(node)?,
                    platform: try_attribute(node, "platform")?,
                    provisional: try_attribute_fs(node, "provisional")?,
                    special_use: try_attribute_sep::<_, ','>(node, "specialuse")?,
                    comment: try_attribute(node, "comment")?,
                }))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for PseudoExtension<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("extension") && !node.has_attribute("number") {
            Ok(Some(PseudoExtension {
                name: attribute(node, "name")?,
                supported: attribute(node, "supported")?,
                requires: Parse::parse(node)?,
                comment: try_attribute(node, "comment")?,
            }))
        } else {
            Ok(None)
        }
    }
}
