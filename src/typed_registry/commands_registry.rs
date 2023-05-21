use core::{fmt, ops};

use roxmltree::Node;
use vulkan_parse_derive_helper::DisplayEscaped;

use super::{FieldLike, VulkanApi};

/// Structured definition of a single API command (function)
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "command")]
pub struct Command<'a> {
    /// C function prototype, including the return type
    #[xml(child)]
    pub proto: Proto<'a>,
    /// function parameters
    #[xml(child)]
    pub params: Vec<CommandParam<'a>>,
    /// possible successful return codes from the command
    #[xml(attribute(rename = "successcodes"))]
    pub success_codes: Option<SuccessCodes<'a>>,
    /// possible error return codes from the command
    #[xml(attribute(rename = "errorcodes", seperator = "crate::CommaSeperator"))]
    pub error_codes: Option<Vec<&'a str>>,
    /// the command queues this command can be placed on
    #[xml(attribute(seperator = "crate::CommaSeperator"))]
    pub queues: Option<enumflags2::BitFlags<Queue>>,
    /// the command buffer levels that this command can be called by
    #[xml(attribute(rename = "cmdbufferlevel", seperator = "crate::CommaSeperator"))]
    pub cmd_buffer_level: Option<enumflags2::BitFlags<CommandBufferLevel>>,
    /// spec-language descriptions of objects that are not parameters of the command, but are related to them and also require external synchronization
    #[xml(child)]
    pub implicit_extern_sync_params: Option<ImplicitExternSyncParams<'a>>,
    /// the tasks this command performs, as described in the “Queue Operation” section of the Vulkan Specification
    #[xml(attribute(seperator = "crate::CommaSeperator"))]
    pub tasks: Option<enumflags2::BitFlags<Task>>,
    /// whether the command can be issued only inside a video coding scope, only outside a video coding scope, or both. The default is outside
    #[xml(attribute(rename = "videocoding"))]
    pub video_coding: Option<ScopeValidity>,
    /// whether the command can be issued only inside a render pass, only outside a render pass, or both.
    #[xml(attribute())]
    pub renderpass: Option<ScopeValidity>,
    /// which vulkan api the command belongs to
    #[xml(attribute())]
    pub api: Option<VulkanApi>,
    /// descriptive text with no semantic meaning
    #[xml(attribute())]
    pub comment: Option<&'a str>,
}

/// C function prototype of a command, up to the function name and return type but not including function parameters
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[xml(tag = "proto")]
pub struct Proto<'a> {
    #[xml(flatten)]
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

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "param")]
pub struct CommandParam<'a> {
    #[xml(flatten)]
    pub base: FieldLike<'a>,
    /// only applicable for parameters which are pointers to
    /// VkBaseInStructure or VkBaseOutStructure types, used as abstract
    /// placeholders. Specifies a list of structures which
    /// may be passed in place of the parameter, or anywhere in the pNext
    /// chain of the parameter.
    #[xml(attribute(rename = "validstructs", seperator = "crate::CommaSeperator"))]
    pub valid_structs: Option<Vec<&'a str>>,
    /// name of param containing the byte stride between consecutive elements in this array.
    /// Is assumed tightly packed if omitted.
    #[xml(attribute())]
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

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[xml(tag = "param")]
pub struct ImplicitExternSyncParam<'a> {
    #[xml(text)]
    pub description: &'a str,
}

/// spec-language descriptions of objects that are not parameters of the command, but are related to them and also require external synchronization
#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[xml(tag = "implicitexternsyncparams")]
pub struct ImplicitExternSyncParams<'a> {
    #[xml(child)]
    pub params: Vec<ImplicitExternSyncParam<'a>>,
}

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    strum::EnumString,
    strum::Display,
    TryFromEscapedStr,
    DisplayEscaped,
)]
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

#[derive(Debug, Clone, PartialEq, Eq, TryFromEscapedStr, DisplayEscaped)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum SuccessCodes<'a> {
    /// `successcodes="VK_SUCCESS"`
    DefaultSuccess,
    Codes(Vec<&'a str>),
}

impl<'a, 'de: 'a> From<&'de str> for SuccessCodes<'a> {
    fn from(s: &'de str) -> Self {
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

impl<'a> fmt::Display for SuccessCodes<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SuccessCodes::DefaultSuccess => write!(f, "VK_SUCCESS"),
            SuccessCodes::Codes(codes) => {
                crate::InterspersedDisplay::<crate::CommaSeperator, _>::new(codes).fmt(f)
            }
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    strum::EnumString,
    strum::Display,
    TryFromEscapedStr,
    DisplayEscaped,
)]
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
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    strum::EnumString,
    strum::Display,
    TryFromEscapedStr,
    DisplayEscaped,
)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum CommandBufferLevel {
    #[strum(serialize = "primary")]
    Primary,
    #[strum(serialize = "secondary")]
    Secondary,
}

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    strum::EnumString,
    strum::Display,
    TryFromEscapedStr,
    DisplayEscaped,
)]
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
