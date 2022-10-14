use std::borrow::Cow;
use std::str::FromStr;

use roxmltree::Node;

use crate::{get_req_attr, Parse, ParseResult};

use super::common::*;
use super::feature_registry::Require;

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExtensionPromotion<'a> {
    Core(SemVarVersion),
    Extension(Cow<'a, str>),
}

impl<'a> ExtensionPromotion<'a> {
    pub fn from_str(s: &'a str) -> Self {
        if let Some(ver) = s.strip_prefix("VK_VERSION_") {
            let (major, minor) = ver.split_once('_').unwrap();
            Self::Core(SemVarVersion {
                major: major.parse().unwrap(),
                minor: minor.parse().unwrap(),
                patch: None,
            })
        } else {
            Self::Extension(Cow::Borrowed(s))
        }
    }
}

#[derive(Debug)]
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

impl<'a, 'input> Parse<'a, 'input> for Extension<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("extension") {
            Ok(Some(Extension {
                name: get_req_attr(node, "name").map(Cow::Borrowed)?,
                number: get_req_attr(node, "number")?.parse().unwrap(),
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
    }
}
