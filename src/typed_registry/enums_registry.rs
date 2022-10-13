use std::borrow::Cow;

use roxmltree::Node;

use crate::{get_req_attr, parse_cexpr, ErrorKind, Expression, Parse, ParseResult, Terminated};

use super::common::*;

#[derive(Debug, Clone)]
pub struct Enums<'a> {
    pub name: Cow<'a, str>,
    pub comment: Option<Cow<'a, str>>,
    pub values: EnumsValues<'a>,
}

#[derive(Debug, Clone)]
pub enum EnumsValues<'a> {
    /// no type attribute
    Constants(CommentendChildren<'a, DefinitionOrAlias<'a, ConstantEnum<'a>>>),
    /// type="enum"
    Enum(Terminated<CommentendChildren<'a, DefinitionOrAlias<'a, ValueEnum<'a>>>, UnusedEnum<'a>>),
    /// type="bitmask"
    Bitmask(CommentendChildren<'a, DefinitionOrAlias<'a, BitmaskEnum<'a>>>),
}

#[derive(Debug, Clone)]
pub struct ConstantEnum<'a> {
    pub name: Cow<'a, str>,
    pub type_name: Cow<'a, str>,
    pub value: Expression<'a>,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone)]
pub struct ValueEnum<'a> {
    pub name: Cow<'a, str>,
    pub value: i64,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone)]
pub struct BitPosEnum<'a> {
    pub name: Cow<'a, str>,
    pub bitpos: u8,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone)]
pub enum BitmaskEnum<'a> {
    Value(ValueEnum<'a>),
    BitPos(BitPosEnum<'a>),
}

/// <enums type="enum"> ... <unused /> </<enums>
#[derive(Debug, Clone)]
pub struct UnusedEnum<'a> {
    pub start: i64,
    pub comment: Option<Cow<'a, str>>,
}

impl<'a, 'input> Parse<'a, 'input> for Enums<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(Enums {
            name: get_req_attr(node, "name").map(Cow::Borrowed)?,
            comment: node.attribute("comment").map(Cow::Borrowed),
            values: Parse::parse(node)?,
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for EnumsValues<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        let ty_attr = node.attribute("type");
        match ty_attr {
            None => Ok(Some(Self::Constants(Parse::parse(node)?))),
            Some("enum") => Ok(Some(Self::Enum(Parse::parse(node)?))),
            Some("bitmask") => Ok(Some(Self::Bitmask(Parse::parse(node)?))),
            _ => Ok(None),
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for ConstantEnum<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("enum") {
            Ok(Some(ConstantEnum {
                name: get_req_attr(node, "name").map(Cow::Borrowed)?,
                type_name: get_req_attr(node, "type").map(Cow::Borrowed)?,
                value: parse_cexpr(get_req_attr(node, "value")?)
                    .map_err(|e| ErrorKind::MixedParseError(e, node.id()))?,
                comment: node.attribute("comment").map(Cow::Borrowed),
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for ValueEnum<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("enum") {
            let value = get_req_attr(node, "value")?;
            let value = if let Some(hex) = value
                .strip_prefix("0x")
                .or_else(|| value.strip_prefix("0X"))
            {
                i64::from_str_radix(hex, 16).unwrap()
            } else {
                value.parse().unwrap()
            };
            Ok(Some(ValueEnum {
                name: get_req_attr(node, "name").map(Cow::Borrowed)?,
                value,
                comment: node.attribute("comment").map(Cow::Borrowed),
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
            name: get_req_attr(node, "name").map(Cow::Borrowed)?,
            bitpos: get_req_attr(node, "bitpos")?.parse().unwrap(),
            comment: node.attribute("comment").map(Cow::Borrowed),
        }))
    }
}

impl<'a, 'input> Parse<'a, 'input> for UnusedEnum<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("unused") {
            let start = get_req_attr(node, "start")?;
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
                comment: node.attribute("comment").map(Cow::Borrowed),
            }))
        } else {
            Ok(None)
        }
    }
}
