use std::{
    borrow::Cow,
    num::{NonZeroU8, ParseIntError},
};

use roxmltree::Node;

use super::common::{CommentendChildren, DefinitionOrAlias};
use crate::{
    attribute, attribute_fs, try_attribute, try_attribute_fs, Expression, Parse, ParseResult,
    Terminated,
};

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct Enums<'a> {
    pub name: Cow<'a, str>,
    pub bit_width: Option<NonZeroU8>,
    pub comment: Option<Cow<'a, str>>,
    pub values: EnumsValues<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct ConstantEnum<'a> {
    pub name: Cow<'a, str>,
    pub type_name: Cow<'a, str>,
    pub value: Expression<'a>,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct ValueEnum<'a> {
    pub name: Cow<'a, str>,
    pub value: i64,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct BitPosEnum<'a> {
    pub name: Cow<'a, str>,
    pub bitpos: u8,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum BitmaskEnum<'a> {
    Value(ValueEnum<'a>),
    BitPos(BitPosEnum<'a>),
}

impl<'a> BitmaskEnum<'a> {
    #[must_use]
    pub fn name(&self) -> &Cow<'a, str> {
        match self {
            BitmaskEnum::Value(v) => &v.name,
            BitmaskEnum::BitPos(b) => &b.name,
        }
    }
    #[must_use]
    pub fn comment(&self) -> Option<&Cow<'a, str>> {
        match self {
            BitmaskEnum::Value(v) => v.comment.as_ref(),
            BitmaskEnum::BitPos(b) => b.comment.as_ref(),
        }
    }
}

/// <enums type="enum"> ... <unused /> </<enums>
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct UnusedEnum<'a> {
    pub start: i64,
    pub comment: Option<Cow<'a, str>>,
}

impl<'a, 'input> Parse<'a, 'input> for Enums<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        Ok(Some(Enums {
            name: attribute(node, "name")?,
            bit_width: try_attribute_fs(node, "bitwidth")?,
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

#[derive(Debug)]
struct RadixInt(i64);

impl TryFrom<&str> for RadixInt {
    type Error = ParseIntError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Some(hex) = value
            .strip_prefix("0x")
            .or_else(|| value.strip_prefix("0X"))
        {
            i64::from_str_radix(hex, 16)
        } else {
            value.parse()
        }
        .map(RadixInt)
    }
}

impl<'a, 'input> Parse<'a, 'input> for ValueEnum<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("enum") {
            Ok(Some(ValueEnum {
                name: attribute(node, "name")?,
                value: attribute::<RadixInt>(node, "value")?.0,
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
            Ok(Some(UnusedEnum {
                start: attribute::<RadixInt>(node, "start")?.0,
                comment: try_attribute(node, "comment")?,
            }))
        } else {
            Ok(None)
        }
    }
}
