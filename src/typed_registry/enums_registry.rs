use core::fmt;
use std::{
    num::{NonZeroU8, ParseIntError},
    str::FromStr,
};

use super::common::{CommentendChildren, DefinitionOrAlias};
use crate::{Expression, UnescapedStr};

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[vkxml(tag = "enums")]
pub struct Enums<'a> {
    /// name of the corresponding <type> associated with this group
    #[vkxml(attribute)]
    pub name: &'a str,
    /// bit width of the enum value type. If omitted, a default value of 32 is used.
    #[vkxml(attribute(rename = "bitwidth"))]
    pub bit_width: Option<NonZeroU8>,
    /// descriptive text with no semantic meaning
    #[vkxml(attribute)]
    pub comment: Option<UnescapedStr<'a>>,
    #[vkxml(flatten)]
    pub values: EnumsValues<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "enums")]
pub enum EnumsValues<'a> {
    // type="enum"
    /// An enum where each value is distinct
    #[vkxml(discriminant(r#type = "enum"))]
    Enum {
        #[vkxml(child)]
        values: CommentendChildren<'a, DefinitionOrAlias<'a, ValueEnum<'a>>>,
        #[vkxml(child)]
        unused_values: Option<UnusedEnum<'a>>,
    },
    // type="bitmask"
    /// An enum where the values constitute a bitmask
    #[vkxml(discriminant(r#type = "bitmask"))]
    Bitmask {
        #[vkxml(child)]
        values: CommentendChildren<'a, DefinitionOrAlias<'a, BitmaskEnum<'a>>>,
    },
    // no type attribute
    /// Hardcoded constants, not an enumerated type
    Constants {
        #[vkxml(child)]
        constants: CommentendChildren<'a, DefinitionOrAlias<'a, ConstantEnum<'a>>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[vkxml(tag = "enum")]
pub struct ConstantEnum<'a> {
    /// Enumerant name, a legal C preprocessor token name
    #[vkxml(attribute)]
    pub name: &'a str,
    /// a C scalar type corresponding to the type of `value`, although only uint32_t, uint64_t, and float are currently meaningful
    #[vkxml(attribute(rename = "type"))]
    pub type_name: &'a str,
    /// numeric value in the form of a legal C expression when evaluated at compile time in the generated header files
    #[vkxml(attribute)]
    pub value: Expression<'a>,
    /// descriptive text with no semantic meaning
    #[vkxml(attribute)]
    pub comment: Option<UnescapedStr<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[vkxml(tag = "enum")]
pub struct ValueEnum<'a> {
    /// Enumerant name, a legal C preprocessor token name
    #[vkxml(attribute)]
    pub name: &'a str,
    /// numeric value in the form of a legal C expression when evaluated at compile time in the generated header files
    #[vkxml(attribute(mapped = "RadixInt"))]
    pub value: i64,
    /// descriptive text with no semantic meaning
    #[vkxml(attribute)]
    pub comment: Option<UnescapedStr<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[vkxml(tag = "enum")]
pub struct BitPosEnum<'a> {
    /// Enumerant name, a legal C preprocessor token name
    #[vkxml(attribute)]
    pub name: &'a str,
    /// literal integer bit position in a bitmask
    #[vkxml(attribute)]
    pub bitpos: u8,
    /// descriptive text with no semantic meaning
    #[vkxml(attribute)]
    pub comment: Option<UnescapedStr<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "enum")]
pub enum BitmaskEnum<'a> {
    #[vkxml(discriminant = "value")]
    Value(ValueEnum<'a>),
    #[vkxml(discriminant = "bitpos")]
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
#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[vkxml(tag = "unused")]
pub struct UnusedEnum<'a> {
    /// defines a single unused enumerant
    #[vkxml(attribute(mapped = "RadixInt"))]
    pub start: i64,
    /// descriptive text with no semantic meaning
    #[vkxml(attribute)]
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
