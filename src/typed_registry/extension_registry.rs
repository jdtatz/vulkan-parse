use std::borrow::Cow;
use std::{fmt, str::FromStr};

use roxmltree::Node;
use serde::Serialize;

use crate::{get_req_attr, Parse, ParseResult};

use super::common::*;
use super::feature_registry::Require;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ExtensionKind {
    Instance,
    Device,
    // PhysicalDevice,
}

impl FromStr for ExtensionKind {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "instance" => Ok(Self::Instance),
            "device" => Ok(Self::Device),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <extension type=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => Err(()),
        }
    }
}

impl fmt::Display for ExtensionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Instance => write!(f, "instance"),
            Self::Device => write!(f, "device"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ExtensionSupport {
    Vulkan,
    Disabled,
}

impl FromStr for ExtensionSupport {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "vulkan" => Ok(Self::Vulkan),
            "disabled" => Ok(Self::Disabled),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <extension supported=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => Err(()),
        }
    }
}

impl fmt::Display for ExtensionSupport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Vulkan => write!(f, "vulkan"),
            Self::Disabled => write!(f, "disabled"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum ExtensionPromotion<'a> {
    Core(SemVarVersion),
    Extension(Cow<'a, str>),
}

impl<'a> ExtensionPromotion<'a> {
    pub fn from_str(s: &'a str) -> Self {
        if let Some(ver) = SemVarVersion::from_std_str(s) {
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
    pub requires_depencies: Option<Box<[Cow<'a, str>]>>,

    // Only missing for VK_RESERVED_do_not_use_94 & VK_RESERVED_do_not_use_146
    pub author: Option<Cow<'a, str>>,
    // Only missing for VK_KHR_mir_surface, VK_RESERVED_do_not_use_94, & VK_RESERVED_do_not_use_146
    pub contact: Option<Cow<'a, str>>,
    pub promoted_to: Option<ExtensionPromotion<'a>>,

    pub comment: Option<Cow<'a, str>>,
    pub requires: Box<[Require<'a>]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PseudoExtension<'a> {
    pub name: Cow<'a, str>,
    pub supported: ExtensionSupport,
    pub comment: Option<Cow<'a, str>>,
    pub requires: Box<[Require<'a>]>,
}

impl<'a, 'input> Parse<'a, 'input> for Extension<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("extension") {
            if let Some(number) = node.attribute("number") {
                Ok(Some(Extension {
                    name: get_req_attr(node, "name").map(Cow::Borrowed)?,
                    number: number.parse().unwrap(),
                    kind: node.attribute("type").and_then(|v| v.parse().ok()),
                    supported: get_req_attr(node, "supported")?.parse().unwrap(),
                    requires_core: node.attribute("requiresCore").and_then(|v| v.parse().ok()),
                    requires_depencies: node
                        .attribute("requires")
                        .map(|v| v.split(',').map(Cow::Borrowed).collect()),
                    author: node.attribute("author").map(Cow::Borrowed),
                    contact: node.attribute("contact").map(Cow::Borrowed),
                    promoted_to: node
                        .attribute("promotedto")
                        .map(|v| ExtensionPromotion::from_str(v)),
                    requires: Parse::parse(node)?,
                    comment: node.attribute("comment").map(Cow::Borrowed),
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
                name: get_req_attr(node, "name").map(Cow::Borrowed)?,
                supported: get_req_attr(node, "supported")?.parse().unwrap(),
                requires: Parse::parse(node)?,
                comment: node.attribute("comment").map(Cow::Borrowed),
            }))
        } else {
            Ok(None)
        }
    }
}
