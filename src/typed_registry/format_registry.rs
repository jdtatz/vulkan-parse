use std::{borrow::Cow, num::NonZeroU8, str::FromStr};

use roxmltree::Node;

use crate::{get_req_attr, Parse, ParseResult};

#[derive(Debug, Clone, PartialEq, Eq)]

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

#[derive(Debug, Clone, PartialEq, Eq)]

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

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum FormatChroma {
    Type420,
    Type422,
    Type444,
}

impl FromStr for FormatChroma {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "420" => Ok(Self::Type420),
            "422" => Ok(Self::Type422),
            "444" => Ok(Self::Type444),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <format chroma=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum FormatCompressionType {
    BC,
    ETC2,
    EAC,
    // FIXME I'm really not sure what else to name these
    /// Adaptive Scalable Texture Compression (ASTC) Low Dynamic Range (LDR)
    #[allow(non_camel_case_types)]
    ASTC_LDR,
    /// Adaptive Scalable Texture Compression (ASTC) High Dynamic Range (HDR)
    #[allow(non_camel_case_types)]
    ASTC_HDR,
    PVRTC,
}

impl FromStr for FormatCompressionType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "BC" => Ok(Self::BC),
            "ETC2" => Ok(Self::ETC2),
            "EAC" => Ok(Self::EAC),
            "ASTC LDR" => Ok(Self::ASTC_LDR),
            "ASTC HDR" => Ok(Self::ASTC_HDR),
            "PVRTC" => Ok(Self::PVRTC),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <format compressed=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]

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

#[derive(Debug, Clone, PartialEq, Eq)]

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

impl FromStr for ComponentNumericFormat {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "SRGB" => Ok(Self::SRGB),
            "UNORM" => Ok(Self::UNORM),
            "SNORM" => Ok(Self::SNORM),
            "UINT" => Ok(Self::UINT),
            "SINT" => Ok(Self::SINT),
            "USCALED" => Ok(Self::USCALED),
            "SSCALED" => Ok(Self::SSCALED),
            "UFLOAT" => Ok(Self::UFLOAT),
            "SFLOAT" => Ok(Self::SFLOAT),
            #[cfg(debug_assertions)]
            s => todo!(
                "Unexpected <format><component numericFormat=...> of {:?}",
                s
            ),
            #[cfg(not(debug_assertions))]
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum ComponentName {
    A,
    R,
    G,
    B,
    S,
    D,
}

impl FromStr for ComponentName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" => Ok(Self::A),
            "R" => Ok(Self::R),
            "G" => Ok(Self::G),
            "B" => Ok(Self::B),
            "S" => Ok(Self::S),
            "D" => Ok(Self::D),
            #[cfg(debug_assertions)]
            s => todo!("Unexpected <format><component name=...> of {:?}", s),
            #[cfg(not(debug_assertions))]
            _ => Err(()),
        }
    }
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
