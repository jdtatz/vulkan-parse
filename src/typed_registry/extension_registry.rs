use std::{borrow::Cow, fmt};

use roxmltree::Node;
use serde::Serialize;

use super::{
    common::{SemVarVersion, StdVersion},
    feature_registry::Require,
};
use crate::{attribute, try_attribute, try_attribute_fs, try_attribute_sep, Parse, ParseResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]
pub enum ExtensionKind {
    #[strum(serialize = "instance")]
    Instance,
    #[strum(serialize = "device")]
    Device,
    // PhysicalDevice,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]
pub enum ExtensionSupport {
    #[strum(serialize = "vulkan")]
    Vulkan,
    #[strum(serialize = "disabled")]
    Disabled,
}

// FIXME Unsure of the sortorder attribute's purpose,
//  may be better to be an Option<NonZeroU8> if it's a versioning attr
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]
pub enum SortOrder {
    #[strum(serialize = "1")]
    One,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
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
            Self::Core(v) => write!(f, "VK_VERSION_{}_{}", v.major, v.minor),
            Self::Extension(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Extension<'a> {
    pub name: Cow<'a, str>,
    pub number: u32,
    pub kind: Option<ExtensionKind>,
    pub supported: ExtensionSupport,
    pub requires_core: Option<SemVarVersion>,
    pub requires_depencies: Option<Vec<Cow<'a, str>>>,

    // Only missing for VK_RESERVED_do_not_use_94 & VK_RESERVED_do_not_use_146
    pub author: Option<Cow<'a, str>>,
    // Only missing for VK_KHR_mir_surface, VK_RESERVED_do_not_use_94, & VK_RESERVED_do_not_use_146
    pub contact: Option<Cow<'a, str>>,
    pub promoted_to: Option<ExtensionPromotion<'a>>,
    pub deprecated_by: Option<ExtensionPromotion<'a>>,
    pub obsoleted_by: Option<ExtensionPromotion<'a>>,
    pub sort_order: Option<SortOrder>,
    pub platform: Option<Cow<'a, str>>,
    pub provisional: Option<bool>,
    pub special_use: Option<Cow<'a, str>>,
    pub comment: Option<Cow<'a, str>>,
    pub requires: Box<[Require<'a>]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PseudoExtension<'a> {
    pub name: Cow<'a, str>,
    pub supported: ExtensionSupport,
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
                    sort_order: try_attribute(node, "sortorder")?,
                    requires: Parse::parse(node)?,
                    platform: try_attribute(node, "platform")?,
                    provisional: try_attribute_fs(node, "provisional")?,
                    special_use: try_attribute(node, "specialuse")?,
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
