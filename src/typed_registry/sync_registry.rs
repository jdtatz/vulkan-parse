use super::CommentendChildren;

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum SyncChild<'a> {
    Stage(SyncStage<'a>),
    Access(SyncAccess<'a>),
    Pipeline(SyncPipeline<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "syncstage")]
pub struct SyncStage<'a> {
    #[vkxml(attribute)]
    pub name: &'a str,
    #[vkxml(attribute)]
    pub alias: Option<&'a str>,
    #[vkxml(child)]
    pub support: CommentendChildren<'a, SyncStageChild<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "syncaccess")]
pub struct SyncAccess<'a> {
    #[vkxml(attribute)]
    pub name: &'a str,
    #[vkxml(attribute)]
    pub alias: Option<&'a str>,
    #[vkxml(child)]
    pub support: CommentendChildren<'a, SyncAccessChild<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "syncpipeline")]
pub struct SyncPipeline<'a> {
    #[vkxml(attribute)]
    pub name: &'a str,
    #[vkxml(attribute(seperator = crate::CommaSeperator))]
    pub depends: Option<Vec<&'a str>>,
    #[vkxml(child)]
    pub enables: Vec<SyncPipelineStage<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum SyncStageChild<'a> {
    Support(SyncStageSupport<'a>),
    Equivalent(SyncStageEquivalent<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum SyncAccessChild<'a> {
    Support(SyncAccessSupport<'a>),
    Equivalent(SyncAccessEquivalent<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "syncsupport")]
pub struct SyncStageSupport<'a> {
    #[vkxml(attribute(seperator = crate::CommaSeperator))]
    pub queues: Option<Vec<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "syncequivalent")]
pub struct SyncStageEquivalent<'a> {
    #[vkxml(attribute(rename = "stage", seperator = crate::CommaSeperator))]
    pub stages: Option<Vec<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "syncsupport")]
pub struct SyncAccessSupport<'a> {
    #[vkxml(attribute(rename = "stage", seperator = crate::CommaSeperator))]
    pub stages: Option<Vec<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "syncequivalent")]
pub struct SyncAccessEquivalent<'a> {
    #[vkxml(attribute(seperator = crate::CommaSeperator))]
    pub access: Option<Vec<&'a str>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "syncpipelinestage")]
pub struct SyncPipelineStage<'a> {
    #[vkxml(text)]
    pub name: &'a str,
    #[vkxml(attribute)]
    pub order: Option<&'a str>,
    #[vkxml(attribute)]
    pub before: Option<&'a str>,
}
