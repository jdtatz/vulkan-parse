use std::{borrow::Cow, str::FromStr};

use roxmltree::Node;

use crate::{get_req_attr, parse_cexpr, ErrorKind, Expression, Parse, ParseResult};

use super::common::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FeatureApi {
    Vulkan,
}

impl FromStr for FeatureApi {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "vulkan" => Ok(Self::Vulkan),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <feature api=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct Feature<'a> {
    pub name: Cow<'a, str>,
    pub api: FeatureApi,
    pub number: SemVarVersion,
    pub comment: Option<Cow<'a, str>>,
    pub requires: Box<[Require<'a>]>,
}

#[derive(Debug)]
pub struct Require<'a> {
    pub comment: Option<Cow<'a, str>>,
    pub values: CommentendChildren<'a, RequireValue<'a>>,
}

#[derive(Debug)]
pub enum RequireValue<'a> {
    Type {
        name: Cow<'a, str>,
        comment: Option<Cow<'a, str>>,
    },
    Command {
        name: Cow<'a, str>,
        comment: Option<Cow<'a, str>>,
    },
    Enum {
        extends: Option<Cow<'a, str>>,
        value: Option<RequireValueEnum<'a>>,
        comment: Option<Cow<'a, str>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OffsetDirection {
    Negative,
}

impl FromStr for OffsetDirection {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "-" => Ok(Self::Negative),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <feature><require><enum dir=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum RequireValueEnum<'a> {
    Value(Expression<'a>),
    Alias(Cow<'a, str>),
    Offset {
        // Required for <feature><require>, but optional in <extension><require>
        // extnumber: u32,
        extnumber: Option<u32>,
        offset: u32,
        direction: Option<OffsetDirection>,
    },
    Bitpos(u8),
}

impl<'a, 'input> Parse<'a, 'input> for Feature<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("feature") {
            Ok(Some(Feature {
                name: get_req_attr(node, "name").map(Cow::Borrowed)?,
                api: get_req_attr(node, "api")?.parse().unwrap(),
                number: get_req_attr(node, "number")?.parse().unwrap(),
                comment: node.attribute("comment").map(Cow::Borrowed),
                requires: Parse::parse(node)?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for Require<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("require") {
            Ok(Some(Require {
                comment: node.attribute("comment").map(Cow::Borrowed),
                values: Parse::parse(node)?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for RequireValue<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        match node.tag_name().name() {
            "type" => Ok(Some(RequireValue::Type {
                name: get_req_attr(node, "name").map(Cow::Borrowed)?,
                comment: node.attribute("comment").map(Cow::Borrowed),
            })),
            "command" => Ok(Some(RequireValue::Command {
                name: get_req_attr(node, "name").map(Cow::Borrowed)?,
                comment: node.attribute("comment").map(Cow::Borrowed),
            })),
            "enum" => Ok(Some(RequireValue::Enum {
                extends: node.attribute("extends").map(Cow::Borrowed),
                value: Parse::parse(node)?,
                comment: node.attribute("comment").map(Cow::Borrowed),
            })),
            _ => Ok(None),
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for RequireValueEnum<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(value) = node.attribute("value") {
            Ok(Some(RequireValueEnum::Value(
                parse_cexpr(value).map_err(|e| ErrorKind::MixedParseError(e, node.id()))?,
            )))
        } else if let Some(alias) = node.attribute("alias") {
            Ok(Some(RequireValueEnum::Alias(Cow::Borrowed(alias))))
        } else if let Some(offset) = node.attribute("offset") {
            Ok(Some(RequireValueEnum::Offset {
                // extnumber: get_req_attr(node, "extnumber")?.parse().unwrap(),
                extnumber: node.attribute("extnumber").and_then(|v| v.parse().ok()),
                offset: offset.parse().unwrap(),
                direction: node.attribute("dir").and_then(|v| v.parse().ok()),
            }))
        } else if let Some(bitpos) = node.attribute("bitpos") {
            Ok(Some(RequireValueEnum::Bitpos(bitpos.parse().unwrap())))
        } else {
            Ok(None)
        }
    }
}
