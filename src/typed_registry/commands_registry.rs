use std::{borrow::Cow, fmt};

use roxmltree::Node;
use serde::Serialize;

use super::FieldLike;
use crate::{try_attribute, try_attribute_sep, ErrorKind, Parse, ParseResult};

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Command<'a> {
    pub proto: FieldLike<'a>,
    pub params: Box<[CommandParam<'a>]>,

    pub success_codes: Option<SuccessCodes<'a>>,
    pub error_codes: Option<Vec<Cow<'a, str>>>,

    pub queues: Option<enumflags2::BitFlags<Queue>>,
    pub cmd_buffer_level: Option<enumflags2::BitFlags<CommandBufferLevel>>,
    pub description: Option<Cow<'a, str>>,
    pub implicit_extern_sync_params: Option<ImplicitExternSyncParams<'a>>,
    pub tasks: Option<enumflags2::BitFlags<Task>>,
    pub video_coding: Option<VideoCoding>,
    pub renderpass: Option<Renderpass>,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CommandParam<'a> {
    pub base: FieldLike<'a>,
    pub valid_structs: Option<Vec<Cow<'a, str>>>,
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
    #[strum(serialize = "transfer")]
    Transfer,
    #[strum(serialize = "graphics")]
    Graphics,
    #[strum(serialize = "compute")]
    Compute,
    #[strum(serialize = "sparse_binding")]
    SparseBinding,
    #[strum(serialize = "protected")]
    Protected,
    #[strum(serialize = "decode")]
    VideoDecode,
    #[strum(serialize = "encode")]
    VideoEncode,
    #[strum(serialize = "opticalflow")]
    OpticalFlow,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum SuccessCodes<'a> {
    /// `successcodes="VK_SUCCESS"`
    DefaultSuccess,
    Codes(Box<[Cow<'a, str>]>),
}

impl<'a> From<&'a str> for SuccessCodes<'a> {
    fn from(s: &'a str) -> Self {
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
    Action,
    #[strum(serialize = "state")]
    State,
    #[strum(serialize = "synchronization")]
    Synchronization,
    #[strum(serialize = "indirection")]
    Indirection,
}

impl<'a, 'input> Parse<'a, 'input> for CommandParam<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("param") {
            Ok(Some(CommandParam {
                base: Parse::parse(node)?,
                valid_structs: try_attribute_sep::<_, ','>(node, "validstructs")?,
                stride: try_attribute(node, "stride")?,
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
                    .filter(|n| n.has_tag_name("param"))
                    .map(|n| n.text().unwrap_or(""))
                    .map(Cow::Borrowed)
                    .collect(),
            }))
        } else {
            Ok(None)
        }
    }
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
            let (params, implicit_extern_sync_params) = crate::parse_terminated(it)?;
            Ok(Some(Command {
                proto,
                params,
                implicit_extern_sync_params,
                success_codes: try_attribute(node, "successcodes")?,
                error_codes: try_attribute_sep::<_, ','>(node, "errorcodes")?,
                queues: try_attribute_sep::<_, ','>(node, "queues")?,
                cmd_buffer_level: try_attribute_sep::<_, ','>(node, "cmdbufferlevel")?,
                description: try_attribute(node, "description")?,
                tasks: try_attribute_sep::<_, ','>(node, "tasks")?,
                video_coding: try_attribute(node, "videocoding")?,
                renderpass: try_attribute(node, "renderpass")?,
                comment: try_attribute(node, "comment")?,
            }))
        } else {
            Ok(None)
        }
    }
}
