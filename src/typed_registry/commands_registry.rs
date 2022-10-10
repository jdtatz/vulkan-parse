use std::borrow::Cow;

use super::FieldLike;

#[derive(Debug, Clone)]
pub struct Command<'a> {
    pub proto: FieldLike<'a>,
    pub params: Box<[CommandParam<'a>]>,

    pub successcodes: Option<SuccessCodes>,
    pub errorcodes: Option<Box<[Cow<'a, str>]>>,

    pub queues: Option<Queue>,
    pub cmdbufferlevel: Option<CommandBufferLevel>,
    pub description: Option<Cow<'a, str>>,
    pub implicitexternsyncparams: Box<[Cow<'a, str>]>,
    pub tasks: Option<Task>,
    pub videocoding: Option<VideoCoding>,
    pub renderpass: Option<Renderpass>,
    pub comment: Option<Cow<'a, str>>,
}

#[derive(Debug, Clone)]
pub struct CommandParam<'a> {
    pub base: FieldLike<'a>,
    pub validstructs: Box<[Cow<'a, str>]>,
    pub stride: Option<Cow<'a, str>>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SuccessCodes {
    /// `successcodes="VK_SUCCESS"`
    DefaultSuccess,
    Codes(Vec<String>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Renderpass {
    Inside,
    Outside,
    Both,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VideoCoding {
    Inside,
    Outside,
    Both,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CommandBufferLevel {
    PrimaryOnly,
    Both,
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
