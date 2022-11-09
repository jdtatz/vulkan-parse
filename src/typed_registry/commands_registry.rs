use std::{borrow::Cow, ops};

use roxmltree::Node;

use super::FieldLike;
use crate::{try_attribute, try_attribute_sep, ErrorKind, Parse, ParseResult};

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct Command<'a> {
    pub proto: Proto<'a>,
    pub params: Vec<CommandParam<'a>>,

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

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Proto<'a> {
    pub base: FieldLike<'a>,
}

impl<'a> ops::Deref for Proto<'a> {
    type Target = FieldLike<'a>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl<'a> ops::DerefMut for Proto<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct CommandParam<'a> {
    pub base: FieldLike<'a>,
    pub valid_structs: Option<Vec<Cow<'a, str>>>,
    pub stride: Option<Cow<'a, str>>,
}

impl<'a> ops::Deref for CommandParam<'a> {
    type Target = FieldLike<'a>;

    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

impl<'a> ops::DerefMut for CommandParam<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.base
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct ImplicitExternSyncParam<'a> {
    pub description: Cow<'a, str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct ImplicitExternSyncParams<'a> {
    pub params: Vec<ImplicitExternSyncParam<'a>>,
}

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum SuccessCodes<'a> {
    /// `successcodes="VK_SUCCESS"`
    DefaultSuccess,
    Codes(Vec<Cow<'a, str>>),
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

impl<'s, 'a: 's> IntoIterator for &'s SuccessCodes<'a> {
    type Item = &'s Cow<'a, str>;

    type IntoIter = <&'s [Cow<'a, str>] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            SuccessCodes::DefaultSuccess => [Cow::Borrowed("VK_SUCCESS")].iter(),
            SuccessCodes::Codes(codes) => codes.iter(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum Renderpass {
    #[strum(serialize = "inside")]
    Inside,
    #[strum(serialize = "outside")]
    Outside,
    #[strum(serialize = "both")]
    Both,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum CommandBufferLevel {
    #[strum(serialize = "primary")]
    Primary,
    #[strum(serialize = "secondary")]
    Secondary,
}

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
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

impl<'a, 'input> Parse<'a, 'input> for Proto<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("proto") {
            Ok(Some(Proto {
                base: Parse::parse(node)?,
            }))
        } else {
            Ok(None)
        }
    }
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

impl<'a, 'input> Parse<'a, 'input> for ImplicitExternSyncParam<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("param") {
            Ok(Some(Self {
                description: Cow::Borrowed(node.text().unwrap_or("")),
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
                params: Parse::parse(node)?,
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
