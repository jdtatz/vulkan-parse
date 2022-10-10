use std::borrow::Cow;

use crate::Expression;

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
    Enum(
        CommentendChildren<'a, DefinitionOrAlias<'a, ValueEnum<'a>>>,
        Option<UnusedEnum<'a>>,
    ),
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
