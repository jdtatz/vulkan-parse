use super::CommentendChildren;

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "videocodec")]
pub struct VideoCodec<'a> {
    #[vkxml(attribute)]
    pub name: &'a str,
    #[vkxml(attribute)]
    pub extend: Option<&'a str>,
    #[vkxml(attribute)]
    pub value: Option<&'a str>,
    #[vkxml(child)]
    pub support: CommentendChildren<'a, VideoCodecChild<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
pub enum VideoCodecChild<'a> {
    Capabilities(VideoCapabilities<'a>),
    Format(VideoFormat<'a>),
    Profiles(VideoProfiles<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "videocapabilities")]
pub struct VideoCapabilities<'a> {
    #[vkxml(attribute(rename = "struct"))]
    pub struct_name: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "videoformat")]
pub struct VideoFormat<'a> {
    #[vkxml(attribute)]
    pub name: &'a str,
    #[vkxml(attribute)]
    pub usage: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "videoprofiles")]
pub struct VideoProfiles<'a> {
    #[vkxml(attribute(rename = "struct"))]
    pub struct_name: &'a str,
    #[vkxml(child)]
    pub members: CommentendChildren<'a, VideoProfileMember<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "videoprofilemember")]
pub struct VideoProfileMember<'a> {
    #[vkxml(attribute)]
    pub name: &'a str,
    #[vkxml(child)]
    pub profiles: CommentendChildren<'a, VideoProfile<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, VkXMLConv)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[vkxml(tag = "videoprofile")]
pub struct VideoProfile<'a> {
    #[vkxml(attribute)]
    pub name: &'a str,
    #[vkxml(attribute)]
    pub value: &'a str,
}
