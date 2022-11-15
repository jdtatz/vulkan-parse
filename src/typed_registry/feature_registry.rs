use roxmltree::Node;

use super::common::{CommentendChildren, SemVarVersion};
use crate::{
    attribute, attribute_fs, parse_children, try_attribute, try_attribute_fs, Expression, Parse,
    ParseResult, StdVersion,
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
    /// version C preprocessor name
    pub name: &'a str,
    /// API tag used internally, not necessarily an actual API name
    pub api: FeatureApi,
    /// version number
    pub number: SemVarVersion,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
    /// features to require in this version
    pub requires: Vec<Require<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct Require<'a> {
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
    /// API feature name
    pub feature: Option<StdVersion>,
    pub extension: Option<&'a str>,
    pub values: CommentendChildren<'a, RequireValue<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum RequireValue<'a> {
    Type {
        name: &'a str,
        /// descriptive text with no semantic meaning
        comment: Option<&'a str>,
    },
    Command {
        name: &'a str,
        /// descriptive text with no semantic meaning
        comment: Option<&'a str>,
    },
    Enum(RequireEnum<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct RequireEnum<'a> {
    pub name: Option<&'a str>,
    /// name of a separately defined enumerated type to which the extension enumerant is added
    pub extends: Option<&'a str>,
    /// If `None` then it's a Refrence Enum, otherwise it's an Exstension Enum
    pub value: Option<RequireValueEnum<'a>>,
    /// preprocessor protection symbol for the enum
    pub protect: Option<&'a str>,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
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
    Alias(&'a str),
    Offset {
        // Required for <feature><require>, but optional in <extension><require>
        // extnumber: u32,
        /// extension number. The extension number in turn specifies the starting value of a block (range) of values reserved for enumerants
        extnumber: Option<u32>,
        /// the offset within an extension block
        offset: u32,
        /// if present, the calculated enumerant value will be negative, instead of positive. Negative enumerant values are normally used only for Vulkan error codes
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
                requires: parse_children(node)?,
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
                values: parse_children(node)?,
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
                value: Parse::try_parse(node)?,
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
