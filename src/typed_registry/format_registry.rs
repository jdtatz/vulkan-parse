use std::{borrow::Cow, fmt, num::NonZeroU8, str::FromStr};

use roxmltree::Node;
use serde::Serialize;

use crate::{get_req_attr, Parse, ParseResult};

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]

pub struct Format<'a> {
    pub name: Cow<'a, str>,
    pub class: Cow<'a, str>,
    pub block_size: NonZeroU8,
    pub texels_per_block: NonZeroU8,
    pub block_extent: Option<[NonZeroU8; 3]>,
    pub packed: Option<NonZeroU8>,
    pub compressed: Option<FormatCompressionType>,
    pub chroma: Option<FormatChroma>,
    pub children: Box<[FormatChild<'a>]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]

pub enum FormatChild<'a> {
    Component {
        name: ComponentName,
        bits: ComponentBits,
        numeric_format: ComponentNumericFormat,
        plane_index: Option<u8>,
    },

    Plane {
        index: u8,
        width_divisor: NonZeroU8,
        height_divisor: NonZeroU8,
        compatible: Cow<'a, str>,
    },

    SpirvImageFormat {
        name: Cow<'a, str>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]

pub enum FormatChroma {
    #[strum(serialize = "420")]
    Type420,
    #[strum(serialize = "422")]
    Type422,
    #[strum(serialize = "444")]
    Type444,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]

pub enum FormatCompressionType {
    #[strum(serialize = "BC")]
    BC,
    #[strum(serialize = "ETC2")]
    ETC2,
    #[strum(serialize = "EAC")]
    EAC,
    // FIXME I'm really not sure what else to name these
    /// Adaptive Scalable Texture Compression (ASTC) Low Dynamic Range (LDR)
    #[allow(non_camel_case_types)]
    #[strum(serialize = "ASTC LDR")]
    ASTC_LDR,
    /// Adaptive Scalable Texture Compression (ASTC) High Dynamic Range (HDR)
    #[allow(non_camel_case_types)]
    #[strum(serialize = "ASTC HDR")]
    ASTC_HDR,
    #[strum(serialize = "PVRTC")]
    PVRTC,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]

pub enum ComponentBits {
    Compressed,
    Bits(NonZeroU8),
}

impl FromStr for ComponentBits {
    type Err = <NonZeroU8 as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "compressed" {
            Ok(Self::Compressed)
        } else {
            s.parse().map(Self::Bits)
        }
    }
}

impl fmt::Display for ComponentBits {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Compressed => write!(f, "compressed"),
            Self::Bits(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]

pub enum ComponentNumericFormat {
    SRGB,
    UNORM,
    SNORM,
    UINT,
    SINT,
    USCALED,
    SSCALED,
    UFLOAT,
    SFLOAT,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display, Serialize)]

pub enum ComponentName {
    A,
    R,
    G,
    B,
    S,
    D,
}

fn block_extent_from_str(s: &str) -> Option<[NonZeroU8; 3]> {
    if let Some((x, rest)) = s.split_once(',') {
        if let Some((y, z)) = rest.split_once(',') {
            return Some([x.parse().unwrap(), y.parse().unwrap(), z.parse().unwrap()]);
        }
    }
    #[cfg(debug_assertions)]
    todo!("Unexpected <format blockExtent=...> of {:?}", s);
    #[cfg(not(debug_assertions))]
    None
}

impl<'a, 'input> Parse<'a, 'input> for Format<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("format") {
            Ok(Some(Format {
                name: get_req_attr(node, "name").map(Cow::Borrowed)?,
                class: get_req_attr(node, "name").map(Cow::Borrowed)?,
                block_size: get_req_attr(node, "blockSize")?.parse().unwrap(),
                texels_per_block: get_req_attr(node, "texelsPerBlock")?.parse().unwrap(),
                block_extent: node
                    .attribute("blockExtent")
                    .and_then(block_extent_from_str),
                packed: node.attribute("packed").and_then(|v| v.parse().ok()),
                compressed: node.attribute("compressed").and_then(|v| v.parse().ok()),
                chroma: node.attribute("chroma").and_then(|v| v.parse().ok()),
                children: Parse::parse(node)?,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'a, 'input> Parse<'a, 'input> for FormatChild<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        match node.tag_name().name() {
            "component" => Ok(Some(FormatChild::Component {
                name: get_req_attr(node, "name")?.parse().unwrap(),
                bits: get_req_attr(node, "bits")?.parse().unwrap(),
                numeric_format: get_req_attr(node, "numericFormat")?.parse().unwrap(),
                plane_index: node.attribute("planeIndex").and_then(|v| v.parse().ok()),
            })),
            "plane" => Ok(Some(FormatChild::Plane {
                index: get_req_attr(node, "index")?.parse().unwrap(),
                width_divisor: get_req_attr(node, "widthDivisor")?.parse().unwrap(),
                height_divisor: get_req_attr(node, "heightDivisor")?.parse().unwrap(),
                compatible: Cow::Borrowed(get_req_attr(node, "index")?),
            })),
            "spirvimageformat" => Ok(Some(FormatChild::SpirvImageFormat {
                name: get_req_attr(node, "name").map(Cow::Borrowed)?,
            })),
            _ => Ok(None),
        }
    }
}
