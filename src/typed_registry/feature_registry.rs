use std::borrow::Cow;

use roxmltree::Node;

use super::common::{CommentendChildren, SemVarVersion};
use crate::{
    attribute, attribute_fs, try_attribute, try_attribute_fs, Expression, Parse, ParseResult,
    StdVersion,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum FeatureApi {
    #[strum(serialize = "vulkan")]
    Vulkan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct Feature<'a> {
    pub name: Cow<'a, str>,
    pub api: FeatureApi,
    pub number: SemVarVersion,
    pub comment: Option<Cow<'a, str>>,
    pub requires: Vec<Require<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct Require<'a> {
    pub comment: Option<Cow<'a, str>>,
    pub feature: Option<StdVersion>,
    pub extension: Option<Cow<'a, str>>,
    pub values: CommentendChildren<'a, RequireValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum RequireValue<'a> {
    Type {
        name: Cow<'a, str>,
        comment: Option<Cow<'a, str>>,
    },
    Command {
        name: Cow<'a, str>,
        comment: Option<Cow<'a, str>>,
    },
    Enum(RequireEnum<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct RequireEnum<'a> {
    pub name: Option<Cow<'a, str>>,
    pub extends: Option<Cow<'a, str>>,
    pub value: Option<RequireValueEnum<'a>>,
    pub protect: Option<Cow<'a, str>>,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum OffsetDirection {
    #[strum(serialize = "-")]
    Negative,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
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
                name: attribute(node, "name")?,
                api: attribute(node, "api")?,
                number: attribute_fs(node, "number")?,
                comment: try_attribute(node, "comment")?,
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
                feature: try_attribute_fs(node, "feature")?,
                extension: try_attribute(node, "extension")?,
                comment: try_attribute(node, "comment")?,
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
                name: attribute(node, "name")?,
                comment: try_attribute(node, "comment")?,
            })),
            "command" => Ok(Some(RequireValue::Command {
                name: attribute(node, "name")?,
                comment: try_attribute(node, "comment")?,
            })),
            "enum" => Ok(Some(RequireValue::Enum(RequireEnum {
                name: try_attribute(node, "name")?,
                extends: try_attribute(node, "extends")?,
                protect: try_attribute(node, "protect")?,
                value: Parse::parse(node)?,
                comment: try_attribute(node, "comment")?,
            }))),
            _ => Ok(None),
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for RequireValueEnum<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if let Some(value) = try_attribute(node, "value")? {
            Ok(Some(RequireValueEnum::Value(value)))
        } else if let Some(alias) = try_attribute(node, "alias")? {
            Ok(Some(RequireValueEnum::Alias(alias)))
        } else if let Some(offset) = try_attribute_fs(node, "offset")? {
            Ok(Some(RequireValueEnum::Offset {
                // extnumber: attribute(node, "extnumber")?,
                extnumber: try_attribute_fs(node, "extnumber")?,
                offset,
                direction: try_attribute(node, "dir")?,
            }))
        } else if let Some(bitpos) = try_attribute_fs(node, "bitpos")? {
            Ok(Some(RequireValueEnum::Bitpos(bitpos)))
        } else {
            Ok(None)
        }
    }
}
