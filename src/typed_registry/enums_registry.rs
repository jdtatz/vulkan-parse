use core::fmt;
use std::{
    num::{NonZeroU8, ParseIntError},
    str::FromStr,
};

use roxmltree::Node;

use super::common::{CommentendChildren, DefinitionOrAlias};
use crate::{Expression, UnescapedStr};

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "enums")]
pub struct Enums<'a> {
    /// name of the corresponding <type> associated with this group
    #[xml(attribute())]
    pub name: &'a str,
    /// bit width of the enum value type. If omitted, a default value of 32 is used.
    #[xml(attribute(rename = "bitwidth"))]
    pub bit_width: Option<NonZeroU8>,
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<UnescapedStr<'a>>,
    #[xml(flatten)]
    pub values: EnumsValues<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[xml(tag = "enums")]
pub enum EnumsValues<'a> {
    // type="enum"
    /// An enum where each value is distinct
    #[xml(discriminant(attr = "type", value = "enum"))]
    Enum {
        #[xml(child)]
        values: CommentendChildren<'a, DefinitionOrAlias<'a, ValueEnum<'a>>>,
        #[xml(child)]
        unused_values: Option<UnusedEnum<'a>>,
    },
    // type="bitmask"
    /// An enum where the values constitute a bitmask
    #[xml(discriminant(attr = "type", value = "bitmask"))]
    Bitmask {
        #[xml(child)]
        values: CommentendChildren<'a, DefinitionOrAlias<'a, BitmaskEnum<'a>>>,
    },
    // no type attribute
    /// Hardcoded constants, not an enumerated type
    Constants {
        #[xml(child)]
        constants: CommentendChildren<'a, DefinitionOrAlias<'a, ConstantEnum<'a>>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "enum")]
pub struct ConstantEnum<'a> {
    /// Enumerant name, a legal C preprocessor token name
    #[xml(attribute())]
    pub name: &'a str,
    /// a C scalar type corresponding to the type of `value`, although only uint32_t, uint64_t, and float are currently meaningful
    #[xml(attribute(rename = "type"))]
    pub type_name: &'a str,
    /// numeric value in the form of a legal C expression when evaluated at compile time in the generated header files
    #[xml(attribute())]
    pub value: Expression<'a>,
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<UnescapedStr<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "enum")]
pub struct ValueEnum<'a> {
    /// Enumerant name, a legal C preprocessor token name
    #[xml(attribute())]
    pub name: &'a str,
    /// numeric value in the form of a legal C expression when evaluated at compile time in the generated header files
    #[xml(attribute(mapped = "RadixInt"))]
    pub value: i64,
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<UnescapedStr<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "enum")]
pub struct BitPosEnum<'a> {
    /// Enumerant name, a legal C preprocessor token name
    #[xml(attribute())]
    pub name: &'a str,
    /// literal integer bit position in a bitmask
    #[xml(attribute())]
    pub bitpos: u8,
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<UnescapedStr<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[xml(tag = "enum")]
pub enum BitmaskEnum<'a> {
    #[xml(discriminant(attr = "value"))]
    Value(ValueEnum<'a>),
    #[xml(discriminant(attr = "bitpos"))]
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
    pub fn comment(&self) -> Option<UnescapedStr<'a>> {
        match self {
            BitmaskEnum::Value(v) => v.comment.clone(),
            BitmaskEnum::BitPos(b) => b.comment.clone(),
        }
    }
}

// <enums type="enum"> ... <unused /> </<enums>
/// defines a range of enumerants not currently being used
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "unused")]
pub struct UnusedEnum<'a> {
    /// defines a single unused enumerant
    #[xml(attribute(mapped = "RadixInt"))]
    pub start: i64,
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<&'a str>,
}

#[derive(Debug, Clone, Copy, TryFromEscapedStr, DisplayEscaped)]
struct RadixInt(i64);

impl FromStr for RadixInt {
    type Err = ParseIntError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
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

impl fmt::Display for RadixInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<RadixInt> for i64 {
    fn from(value: RadixInt) -> Self {
        value.0
    }
}

impl From<&i64> for RadixInt {
    fn from(value: &i64) -> Self {
        Self(*value)
    }
}
