use std::ops;

use roxmltree::Node;

use super::FieldLike;
use crate::{parse_children, try_attribute, Parse, ParseResult};

/// Structured definition of a single API command (function)
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub struct Command<'a> {
    /// C function prototype, including the return type
    pub proto: Proto<'a>,
    /// function parameters
    pub params: Vec<CommandParam<'a>>,
    /// possible successful return codes from the command
    pub success_codes: Option<SuccessCodes<'a>>,
    /// possible error return codes from the command
    pub error_codes: Option<Vec<&'a str>>,
    /// the command queues this command can be placed on
    pub queues: Option<enumflags2::BitFlags<Queue>>,
    /// the command buffer levels that this command can be called by
    pub cmd_buffer_level: Option<enumflags2::BitFlags<CommandBufferLevel>>,
    /// spec-language descriptions of objects that are not parameters of the command, but are related to them and also require external synchronization
    pub implicit_extern_sync_params: Option<ImplicitExternSyncParams<'a>>,
    /// the tasks this command performs, as described in the “Queue Operation” section of the Vulkan Specification
    pub tasks: Option<enumflags2::BitFlags<Task>>,
    /// whether the command can be issued only inside a video coding scope, only outside a video coding scope, or both. The default is outside
    pub video_coding: Option<ScopeValidity>,
    /// whether the command can be issued only inside a render pass, only outside a render pass, or both.
    pub renderpass: Option<ScopeValidity>,
    /// descriptive text with no semantic meaning
    pub comment: Option<&'a str>,
}

/// C function prototype of a command, up to the function name and return type but not including function parameters
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
    /// only applicable for parameters which are pointers to
    /// VkBaseInStructure or VkBaseOutStructure types, used as abstract
    /// placeholders. Specifies a list of structures which
    /// may be passed in place of the parameter, or anywhere in the pNext
    /// chain of the parameter.
    pub valid_structs: Option<Vec<&'a str>>,
    /// name of param containing the byte stride between consecutive elements in this array.
    /// Is assumed tightly packed if omitted.
    pub stride: Option<&'a str>,
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
    pub description: &'a str,
}

/// spec-language descriptions of objects that are not parameters of the command, but are related to them and also require external synchronization
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
    Codes(Vec<&'a str>),
}

impl<'a> From<&'a str> for SuccessCodes<'a> {
    fn from(s: &'a str) -> Self {
        if s == "VK_SUCCESS" {
            Self::DefaultSuccess
        } else {
            Self::Codes(s.split(',').collect())
        }
    }
}

impl<'s, 'a: 's> IntoIterator for &'s SuccessCodes<'a> {
    type Item = &'s &'a str;

    type IntoIter = <&'s [&'a str] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            SuccessCodes::DefaultSuccess => ["VK_SUCCESS"].iter(),
            SuccessCodes::Codes(codes) => codes.iter(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum ScopeValidity {
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

impl<'a> Parse<'a> for Proto<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("proto") {
            Ok(Some(Proto {
                base: Parse::parse(node)?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a> Parse<'a> for CommandParam<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("param") {
            Ok(Some(CommandParam {
                base: Parse::parse(node)?,
                valid_structs: try_attribute(node, "validstructs")?
                    .map(crate::CommaSeperated::into),
                stride: try_attribute(node, "stride")?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a> Parse<'a> for ImplicitExternSyncParam<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("param") {
            Ok(Some(Self {
                description: node.text().unwrap_or(""),
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a> Parse<'a> for ImplicitExternSyncParams<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("implicitexternsyncparams") {
            Ok(Some(ImplicitExternSyncParams {
                params: parse_children(node)?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a> Parse<'a> for Command<'a> {
    fn try_parse<'input: 'a>(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("command") {
            let (proto, params, implicit_extern_sync_params) = parse_children(node)?;
            Ok(Some(Command {
                proto,
                params,
                implicit_extern_sync_params,
                success_codes: try_attribute(node, "successcodes")?,
                error_codes: try_attribute(node, "errorcodes")?.map(crate::CommaSeperated::into),
                queues: try_attribute::<_, true>(node, "queues")?.map(crate::CommaSeperated::into),
                cmd_buffer_level: try_attribute::<_, true>(node, "cmdbufferlevel")?
                    .map(crate::CommaSeperated::into),
                tasks: try_attribute::<_, true>(node, "tasks")?.map(crate::CommaSeperated::into),
                video_coding: try_attribute::<_, false>(node, "videocoding")?,
                renderpass: try_attribute::<_, false>(node, "renderpass")?,
                comment: try_attribute(node, "comment")?,
            }))
        } else {
            Ok(None)
        }
    }
}
