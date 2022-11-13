use std::num::{NonZeroU8, ParseIntError};

use roxmltree::Node;

use super::common::{CommentendChildren, DefinitionOrAlias};
use crate::{
    attribute, attribute_fs, try_attribute, try_attribute_fs, Expression, Parse, ParseResult,
    Terminated,
};

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct Enums<'a> {
    /// name of the corresponding <type> associated with this group
    pub name: &'a str,
    /// bit width of the enum value type. If omitted, a default value of 32 is used.
    pub bit_width: Option<NonZeroU8>,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
    pub values: EnumsValues<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum EnumsValues<'a> {
    // no type attribute
    /// Hardcoded constants, not an enumerated type
    Constants(CommentendChildren<'a, DefinitionOrAlias<'a, ConstantEnum<'a>>>),
    // type="enum"
    /// An enum where each value is distinct
    Enum(
        CommentendChildren<'a, DefinitionOrAlias<'a, ValueEnum<'a>>>,
        Option<UnusedEnum<'a>>,
    ),
    // type="bitmask"
    /// An enum where the values constitute a bitmask
    Bitmask(CommentendChildren<'a, DefinitionOrAlias<'a, BitmaskEnum<'a>>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct ConstantEnum<'a> {
    /// Enumerant name, a legal C preprocessor token name
    pub name: &'a str,
    /// a C scalar type corresponding to the type of `value`, although only uint32_t, uint64_t, and float are currently meaningful
    pub type_name: &'a str,
    /// numeric value in the form of a legal C expression when evaluated at compile time in the generated header files
    pub value: Expression<'a>,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct ValueEnum<'a> {
    /// Enumerant name, a legal C preprocessor token name
    pub name: &'a str,
    /// numeric value in the form of a legal C expression when evaluated at compile time in the generated header files
    pub value: i64,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct BitPosEnum<'a> {
    /// Enumerant name, a legal C preprocessor token name
    pub name: &'a str,
    /// literal integer bit position in a bitmask
    pub bitpos: u8,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum BitmaskEnum<'a> {
    Value(ValueEnum<'a>),
    BitPos(BitPosEnum<'a>),
}

impl<'a> BitmaskEnum<'a> {
    #[must_use]
    pub fn name(&self) -> &&'a str {
        match self {
            BitmaskEnum::Value(v) => &v.name,
            BitmaskEnum::BitPos(b) => &b.name,
        }
    }
    #[must_use]
    pub fn comment(&self) -> Option<&&'a str> {
        match self {
            BitmaskEnum::Value(v) => v.comment.as_ref(),
            BitmaskEnum::BitPos(b) => b.comment.as_ref(),
        }
    }
}

// <enums type="enum"> ... <unused /> </<enums>
/// defines a range of enumerants not currently being used
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct UnusedEnum<'a> {
    /// defines a single unused enumerant
    pub start: i64,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
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
