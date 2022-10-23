use std::{borrow::Cow, fmt};

use roxmltree::Node;
use serde::Serialize;

use super::common::StdVersion;
use crate::{
    attribute, try_attribute, try_attribute_fs, ErrorKind, Expression, Parse, ParseResult,
    StdVersionParseError,
};

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SpirvExtension<'a> {
    pub name: Cow<'a, str>,
    // TODO, usually the only diffrence between `name` & `enable_extension` is the prefix ("SPV_" vs "VK_"), but not always
    pub enable_extension: ExtensionEnable<'a>,
    pub enable_version: Option<VersionEnable>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SpirvCapability<'a> {
    pub name: Cow<'a, str>,
    pub enables: Box<[EnableSpirvCapability<'a>]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum EnableSpirvCapability<'a> {
    Version(VersionEnable),
    Extension(ExtensionEnable<'a>),
    Struct(StructEnable<'a>),
    Property(PropertyEnable<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum EnableRequires<'a> {
    Core(StdVersion),
    Extension(Cow<'a, str>),
    Mix(StdVersion, Cow<'a, str>),
}

impl<'a> TryFrom<&'a str> for EnableRequires<'a> {
    type Error = StdVersionParseError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        Ok(if let Some((v, e)) = s.split_once(',') {
            EnableRequires::Mix(v.parse()?, Cow::Borrowed(e))
        } else if let Ok(v) = s.parse() {
            EnableRequires::Core(v)
        } else {
            EnableRequires::Extension(Cow::Borrowed(s))
        })
    }
}
impl<'a> fmt::Display for EnableRequires<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EnableRequires::Core(v) => write!(f, "{}", v),
            EnableRequires::Extension(e) => write!(f, "{}", e),
            EnableRequires::Mix(v, e) => write!(f, "{},{}", v, e),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct VersionEnable(pub StdVersion);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ExtensionEnable<'a>(pub Cow<'a, str>);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct StructEnable<'a> {
    pub name: Cow<'a, str>,
    pub feature: Cow<'a, str>,
    pub requires: EnableRequires<'a>,
    pub alias: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PropertyEnable<'a> {
    pub name: Cow<'a, str>,
    pub member: Cow<'a, str>,
    pub value: Expression<'a>,
    pub requires: EnableRequires<'a>,
}

impl<'a, 'input> Parse<'a, 'input> for SpirvExtension<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("spirvextension") {
            let mut it = node
                .children()
                .into_iter()
                .filter(|n| n.has_tag_name("enable"));
            Ok(Some(SpirvExtension {
                name: (attribute(node, "name")?),
                enable_extension: it
                    .clone()
                    .find_map(|n| ExtensionEnable::try_parse(n).transpose())
                    .ok_or_else(|| ErrorKind::MissingChildElement("extension", node.id()))??,
                enable_version: it
                    .find_map(|n| VersionEnable::try_parse(n).transpose())
                    .transpose()?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for SpirvCapability<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("spirvcapability") {
            Ok(Some(SpirvCapability {
                name: (attribute(node, "name")?),
                enables: Parse::parse(node)?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for EnableSpirvCapability<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(v) = VersionEnable::try_parse(node)? {
            Ok(Some(EnableSpirvCapability::Version(v)))
        } else if let Some(v) = ExtensionEnable::try_parse(node)? {
            Ok(Some(EnableSpirvCapability::Extension(v)))
        } else if let Some(v) = StructEnable::try_parse(node)? {
            Ok(Some(EnableSpirvCapability::Struct(v)))
        } else if let Some(v) = PropertyEnable::try_parse(node)? {
            Ok(Some(EnableSpirvCapability::Property(v)))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for VersionEnable {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(try_attribute_fs(node, "version")?.map(VersionEnable))
    }
}

impl<'a, 'input> Parse<'a, 'input> for ExtensionEnable<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(try_attribute(node, "extension")?.map(ExtensionEnable))
    }
}

impl<'a, 'input> Parse<'a, 'input> for StructEnable<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(name) = try_attribute(node, "struct")? {
            Ok(Some(StructEnable {
                name,
                feature: (attribute(node, "feature")?),
                requires: (attribute(node, "requires")?),
                alias: try_attribute(node, "alias")?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for PropertyEnable<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(name) = try_attribute(node, "property")? {
            Ok(Some(PropertyEnable {
                name,
                requires: (attribute(node, "requires")?),
                member: (attribute(node, "member")?),
                value: attribute(node, "value")?,
            }))
        } else {
            Ok(None)
        }
    }
}
