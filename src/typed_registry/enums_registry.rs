use std::borrow::Cow;

use roxmltree::Node;
use serde::Serialize;

use crate::{attribute, attribute_fs, try_attribute, Expression, Parse, ParseResult, Terminated};

use super::common::*;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Enums<'a> {
    pub name: Cow<'a, str>,
    pub comment: Option<Cow<'a, str>>,
    pub values: EnumsValues<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum EnumsValues<'a> {
    /// no type attribute
    Constants(CommentendChildren<'a, DefinitionOrAlias<'a, ConstantEnum<'a>>>),
    /// type="enum"
    Enum(
        CommentendChildren<'a, DefinitionOrAlias<'a, ValueEnum<'a>>>,
        Option<UnusedEnum<'a>>,
    ),
    /// type="bitmask"
    Bitmask(CommentendChildren<'a, DefinitionOrAlias<'a, BitmaskEnum<'a>>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ConstantEnum<'a> {
    pub name: Cow<'a, str>,
    pub type_name: Cow<'a, str>,
    pub value: Expression<'a>,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ValueEnum<'a> {
    pub name: Cow<'a, str>,
    pub value: i64,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BitPosEnum<'a> {
    pub name: Cow<'a, str>,
    pub bitpos: u8,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum BitmaskEnum<'a> {
    Value(ValueEnum<'a>),
    BitPos(BitPosEnum<'a>),
}

/// <enums type="enum"> ... <unused /> </<enums>
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct UnusedEnum<'a> {
    pub start: i64,
    pub comment: Option<Cow<'a, str>>,
}

impl<'a, 'input> Parse<'a, 'input> for Enums<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(Enums {
            name: attribute(node, "name")?,
            comment: try_attribute(node, "comment")?,
            values: Parse::parse(node)?,
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for EnumsValues<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        let ty_attr = try_attribute(node, "type")?;
        match ty_attr {
            None => Ok(Some(Self::Constants(Parse::parse(node)?))),
            Some("enum") => Ok(Some(Terminated::parse(node)?.transform(Self::Enum))),
            Some("bitmask") => Ok(Some(Self::Bitmask(Parse::parse(node)?))),
            _ => Ok(None),
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for ConstantEnum<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("enum") {
            Ok(Some(ConstantEnum {
                name: attribute(node, "name")?,
                type_name: attribute(node, "type")?,
                value: attribute(node, "value")?,
                comment: try_attribute(node, "comment")?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for ValueEnum<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("enum") {
            let value: &str = attribute(node, "value")?;
            let value = if let Some(hex) = value
                .strip_prefix("0x")
                .or_else(|| value.strip_prefix("0X"))
            {
                i64::from_str_radix(hex, 16).unwrap()
            } else {
                value.parse().unwrap()
            };
            Ok(Some(ValueEnum {
                name: attribute(node, "name")?,
                value,
                comment: try_attribute(node, "comment")?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for BitmaskEnum<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("enum") {
            if node.has_attribute("value") {
                Ok(Some(BitmaskEnum::Value(Parse::parse(node)?)))
            } else if node.has_attribute("bitpos") {
                Ok(Some(BitmaskEnum::BitPos(Parse::parse(node)?)))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for BitPosEnum<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(BitPosEnum {
            name: attribute(node, "name")?,
            bitpos: attribute_fs(node, "bitpos")?,
            comment: try_attribute(node, "comment")?,
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for UnusedEnum<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("unused") {
            let start: &str = attribute(node, "start")?;
            let start = if let Some(hex) = start
                .strip_prefix("0x")
                .or_else(|| start.strip_prefix("0X"))
            {
                i64::from_str_radix(hex, 16).unwrap()
            } else {
                start.parse().unwrap()
            };

            Ok(Some(UnusedEnum {
                start,
                comment: try_attribute(node, "comment")?,
            }))
        } else {
            Ok(None)
        }
    }
}
