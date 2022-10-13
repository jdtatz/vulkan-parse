use std::{borrow::Cow, str::FromStr};

use roxmltree::Node;

use crate::{ErrorKind, Parse, ParseResult};

use super::FieldLike;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct CommandParam<'a> {
    pub base: FieldLike<'a>,
    pub validstructs: Option<Box<[Cow<'a, str>]>>,
    pub stride: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone)]
pub struct ImplicitExternSyncParams<'a> {
    pub params: Box<[Cow<'a, str>]>,
}

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Queue {
    GRAPHICS,
    COMPUTE,
    TRANSFER,
    SPARSE_BINDING,
    PROTECTED,
    VIDEO_DECODE,
    VIDEO_ENCODE,
    OPTICAL_FLOW,
}

impl FromStr for Queue {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "graphics" => Ok(Self::GRAPHICS),
            "compute" => Ok(Self::COMPUTE),
            "transfer" => Ok(Self::TRANSFER),
            "sparse_binding" => Ok(Self::SPARSE_BINDING),
            "protected" => Ok(Self::PROTECTED),
            "decode" => Ok(Self::VIDEO_DECODE),
            "encode" => Ok(Self::VIDEO_ENCODE),
            "opticalflow" => Ok(Self::OPTICAL_FLOW),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <command queues=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => return Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Renderpass {
    Inside,
    Outside,
    Both,
}

impl FromStr for Renderpass {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "inside" => Ok(Self::Inside),
            "outside" => Ok(Self::Outside),
            "both" => Ok(Self::Both),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <command renderpass=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => return Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VideoCoding {
    Inside,
    Outside,
    Both,
}

impl FromStr for VideoCoding {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "inside" => Ok(Self::Inside),
            "outside" => Ok(Self::Outside),
            "both" => Ok(Self::Both),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <command videocoding=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => return Ok(None),
        }
    }
}

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CommandBufferLevel {
    Primary,
    Secondary,
}

impl FromStr for CommandBufferLevel {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "primary" => Ok(Self::Primary),
            "secondary" => Ok(Self::Secondary),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <command cmdbufferlevel=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => return Ok(None),
        }
    }
}

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Task {
    ACTION,
    STATE,
    SYNCHRONIZATION,
    INDIRECTION,
}

impl FromStr for Task {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "action" => Ok(Self::ACTION),
            "state" => Ok(Self::STATE),
            "synchronization" => Ok(Self::SYNCHRONIZATION),
            "indirection" => Ok(Self::INDIRECTION),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <command tasks=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => return Ok(None),
        }
    }
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
