use std::borrow::Cow;

use crate::Expression;

use super::common::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FeatureApi {
    Vulkan,
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
        extends: Cow<'a, str>,
        value: RequireValueEnum<'a>,
        comment: Option<Cow<'a, str>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OffsetDirection {
    Negative,
}

#[derive(Debug)]
pub enum RequireValueEnum<'a> {
    Value(Expression<'a>),
    Alias(Cow<'a, str>),
    Offset {
        extnumber: u32,
        offset: u32,
        direction: Option<OffsetDirection>,
    },
    Bitpos(u8),
}
