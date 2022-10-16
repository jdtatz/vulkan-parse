use std::{borrow::Cow, fmt, str::FromStr};

use roxmltree::Node;
use serde::Serialize;

use crate::{ErrorKind, Parse, ParseResult};

use super::FieldLike;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Command<'a> {
    pub proto: FieldLike<'a>,
    pub params: Box<[CommandParam<'a>]>,

    pub successcodes: Option<SuccessCodes<'a>>,
    pub errorcodes: Option<Box<[Cow<'a, str>]>>,

    pub queues: Option<enumflags2::BitFlags<Queue>>,
    pub cmdbufferlevel: Option<enumflags2::BitFlags<CommandBufferLevel>>,
    pub description: Option<Cow<'a, str>>,
    pub implicitexternsyncparams: Option<ImplicitExternSyncParams<'a>>,
    pub tasks: Option<enumflags2::BitFlags<Task>>,
    pub videocoding: Option<VideoCoding>,
    pub renderpass: Option<Renderpass>,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CommandParam<'a> {
    pub base: FieldLike<'a>,
    pub validstructs: Option<Box<[Cow<'a, str>]>>,
    pub stride: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ImplicitExternSyncParams<'a> {
    pub params: Box<[Cow<'a, str>]>,
}

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]
pub enum Queue {
    #[strum(serialize = "graphics")]
    GRAPHICS,
    #[strum(serialize = "compute")]
    COMPUTE,
    #[strum(serialize = "transfer")]
    TRANSFER,
    #[strum(serialize = "sparse_binding")]
    SPARSE_BINDING,
    #[strum(serialize = "protected")]
    PROTECTED,
    #[strum(serialize = "decode")]
    VIDEO_DECODE,
    #[strum(serialize = "encode")]
    VIDEO_ENCODE,
    #[strum(serialize = "opticalflow")]
    OPTICAL_FLOW,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum SuccessCodes<'a> {
    /// `successcodes="VK_SUCCESS"`
    DefaultSuccess,
    Codes(Box<[Cow<'a, str>]>),
}

impl<'a> SuccessCodes<'a> {
    pub fn from_str(s: &'a str) -> Self {
        if s == "VK_SUCCESS" {
            Self::DefaultSuccess
        } else {
            Self::Codes(s.split(',').map(Cow::Borrowed).collect())
        }
    }
}

impl<'a> fmt::Display for SuccessCodes<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DefaultSuccess => write!(f, "VK_SUCCESS"),
            Self::Codes(codes) => crate::fmt_write_interspersed(f, codes.iter(), ","),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]
pub enum Renderpass {
    #[strum(serialize = "inside")]
    Inside,
    #[strum(serialize = "outside")]
    Outside,
    #[strum(serialize = "both")]
    Both,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]
pub enum VideoCoding {
    #[strum(serialize = "inside")]
    Inside,
    #[strum(serialize = "outside")]
    Outside,
    #[strum(serialize = "both")]
    Both,
}

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]
pub enum CommandBufferLevel {
    #[strum(serialize = "primary")]
    Primary,
    #[strum(serialize = "secondary")]
    Secondary,
}

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]
pub enum Task {
    #[strum(serialize = "action")]
    ACTION,
    #[strum(serialize = "state")]
    STATE,
    #[strum(serialize = "synchronization")]
    SYNCHRONIZATION,
    #[strum(serialize = "indirection")]
    INDIRECTION,
}

impl<'a, 'input> Parse<'a, 'input> for CommandParam<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("param") {
            Ok(Some(CommandParam {
                base: Parse::parse(node)?,
                validstructs: node
                    .attribute("validstructs")
                    .map(|s| s.split(',').map(Cow::Borrowed).collect()),
                stride: node.attribute("stride").map(Cow::Borrowed),
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for ImplicitExternSyncParams<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("implicitexternsyncparams") {
            Ok(Some(ImplicitExternSyncParams {
                params: node
                    .children()
                    .filter_map(|n| n.text())
                    .map(Cow::Borrowed)
                    .collect(),
            }))
        } else {
            Ok(None)
        }
    }
}

fn comma_sep_bitflags<T: FromStr + enumflags2::BitFlag>(
    s: &str,
) -> Option<enumflags2::BitFlags<T>> {
    s.split(',').map(T::from_str).collect::<Result<_, _>>().ok()
}

impl<'a, 'input> Parse<'a, 'input> for Command<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("command") {
            let mut it = node.children().filter(Node::is_element);
            let proto_node = it
                .next()
                .filter(|n| n.has_tag_name("proto"))
                .ok_or_else(|| ErrorKind::MissingChildElement("proto", node.id()))?;
            let proto = Parse::parse(proto_node)?;
            let last = node.last_element_child().unwrap();
            let (params, iesp) = if let Some(iesp) = ImplicitExternSyncParams::try_parse(last)? {
                it.next_back();
                (
                    it.map(CommandParam::parse).collect::<ParseResult<_>>()?,
                    Some(iesp),
                )
            } else {
                (
                    it.map(CommandParam::parse).collect::<ParseResult<_>>()?,
                    None,
                )
            };

            Ok(Some(Command {
                proto,
                params,
                implicitexternsyncparams: iesp,
                successcodes: node.attribute("returnedonly").map(SuccessCodes::from_str),
                errorcodes: node
                    .attribute("errorcodes")
                    .map(|s| s.split(',').map(Cow::Borrowed).collect()),
                queues: node.attribute("queues").and_then(comma_sep_bitflags),
                cmdbufferlevel: node
                    .attribute("cmdbufferlevel")
                    .and_then(comma_sep_bitflags),
                description: node.attribute("description").map(Cow::Borrowed),
                tasks: node.attribute("tasks").and_then(comma_sep_bitflags),
                videocoding: node.attribute("videocoding").and_then(|v| v.parse().ok()),
                renderpass: node.attribute("renderpass").and_then(|v| v.parse().ok()),
                comment: node.attribute("comment").map(Cow::Borrowed),
            }))
        } else {
            Ok(None)
        }
    }
}
