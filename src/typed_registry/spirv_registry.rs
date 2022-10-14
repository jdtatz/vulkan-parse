use std::borrow::Cow;

use roxmltree::Node;

use crate::{get_req_attr, parse_cexpr, ErrorKind, Expression, Parse, ParseResult};

use super::common::*;

#[derive(Debug)]
pub struct SpirvExtension<'a> {
    pub name: Cow<'a, str>,
    // TODO, usually the only diffrence between `name` & `enable_extension` is the prefix ("SPV_" vs "VK_"), but not always
    pub enable_extension: Cow<'a, str>,
    pub enable_version: Option<SemVarVersion>,
}

#[derive(Debug)]
pub struct SpirvCapability<'a> {
    pub name: Cow<'a, str>,
    pub enables: Box<[EnableSpirvCapability<'a>]>,
}

#[derive(Debug)]
pub enum EnableSpirvCapability<'a> {
    Version(VersionEnable),
    Extension(ExtensionEnable<'a>),
    Struct(StructEnable<'a>),
    Property(PropertyEnable<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EnableRequires<'a> {
    Core(SemVarVersion),
    Extension(Cow<'a, str>),
    Mix(SemVarVersion, Cow<'a, str>),
}

impl<'a> EnableRequires<'a> {
    pub fn from_str(s: &'a str) -> Self {
        if let Some((v, e)) = s.split_once(',') {
            EnableRequires::Mix(SemVarVersion::from_std_str(v).unwrap(), Cow::Borrowed(e))
        } else if let Some(v) = SemVarVersion::from_std_str(s) {
            EnableRequires::Core(v)
        } else {
            EnableRequires::Extension(Cow::Borrowed(s))
        }
    }
}

#[derive(Debug)]
pub struct VersionEnable(SemVarVersion);

#[derive(Debug)]
pub struct ExtensionEnable<'a>(Cow<'a, str>);

#[derive(Debug)]
pub struct StructEnable<'a> {
    pub name: Cow<'a, str>,
    pub feature: Cow<'a, str>,
    pub requires: EnableRequires<'a>,
}

#[derive(Debug)]
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
                name: Cow::Borrowed(get_req_attr(node, "name")?),
                enable_extension: it
                    .clone()
                    .find_map(|n| n.attribute("extension").map(Cow::Borrowed))
                    .unwrap(),
                enable_version: it.find_map(|n| {
                    n.attribute("version")
                        .map(|v| SemVarVersion::from_std_str(v).unwrap())
                }),
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
                name: Cow::Borrowed(get_req_attr(node, "name")?),
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
        if let Some(v) = node.attribute("version") {
            Ok(SemVarVersion::from_std_str(v).map(VersionEnable))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for ExtensionEnable<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(node
            .attribute("extension")
            .map(Cow::Borrowed)
            .map(ExtensionEnable))
    }
}

impl<'a, 'input> Parse<'a, 'input> for StructEnable<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(s) = node.attribute("struct") {
            Ok(Some(StructEnable {
                name: Cow::Borrowed(s),
                feature: Cow::Borrowed(get_req_attr(node, "feature")?),
                requires: EnableRequires::from_str(get_req_attr(node, "requires")?),
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for PropertyEnable<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(s) = node.attribute("property") {
            Ok(Some(PropertyEnable {
                name: Cow::Borrowed(s),
                requires: EnableRequires::from_str(get_req_attr(node, "requires")?),
                member: Cow::Borrowed(get_req_attr(node, "member")?),
                value: parse_cexpr(get_req_attr(node, "value")?)
                    .map_err(|e| ErrorKind::MixedParseError(e, node.id()))?,
            }))
        } else {
            Ok(None)
        }
    }
}
