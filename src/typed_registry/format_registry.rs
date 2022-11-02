use std::{borrow::Cow, fmt, num::NonZeroU8, str::FromStr};

use roxmltree::Node;

use crate::{attribute, attribute_fs, try_attribute, try_attribute_fs, Parse, ParseResult};

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]

pub struct Format<'a> {
    pub name: Cow<'a, str>,
    pub class: Cow<'a, str>,
    pub block_size: NonZeroU8,
    pub texels_per_block: NonZeroU8,
    pub block_extent: Option<BlockExtent>,
    pub packed: Option<NonZeroU8>,
    pub compressed: Option<FormatCompressionType>,
    pub chroma: Option<FormatChroma>,
    pub children: Box<[FormatChild<'a>]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct BlockExtent(pub NonZeroU8, pub NonZeroU8, pub NonZeroU8);

impl FromStr for BlockExtent {
    type Err = <NonZeroU8 as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some((x, rest)) = s.split_once(',') {
            if let Some((y, z)) = rest.split_once(',') {
                return Ok(BlockExtent(x.parse()?, y.parse()?, z.parse()?));
            }
        }
        todo!("Unexpected <format blockExtent=...> of {:?}", s);
    }
}

impl fmt::Display for BlockExtent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let BlockExtent(x, y, z) = self;
        write!(f, "{x},{y},{z}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]

pub enum FormatChroma {
    #[strum(serialize = "420")]
    Type420,
    #[strum(serialize = "422")]
    Type422,
    #[strum(serialize = "444")]
    Type444,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]

pub enum FormatCompressionType {
    #[strum(serialize = "BC")]
    Bc,
    #[strum(serialize = "ETC2")]
    Etc2,
    #[strum(serialize = "EAC")]
    Eac,
    // FIXME I'm really not sure what else to name these
    /// Adaptive Scalable Texture Compression (ASTC) Low Dynamic Range (LDR)
    #[strum(serialize = "ASTC LDR")]
    AstcLdr,
    /// Adaptive Scalable Texture Compression (ASTC) High Dynamic Range (HDR)
    #[strum(serialize = "ASTC HDR")]
    AstcHdr,
    #[strum(serialize = "PVRTC")]
    Pvrtc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]

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
            Self::Bits(b) => write!(f, "{b}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]

pub enum ComponentName {
    A,
    R,
    G,
    B,
    S,
    D,
}

impl<'a, 'input> Parse<'a, 'input> for Format<'a> {
    fn try_parse(node: Node<'a, 'input>) -> ParseResult<Option<Self>> {
        if node.has_tag_name("format") {
            Ok(Some(Format {
                name: attribute(node, "name")?,
                class: attribute(node, "class")?,
                block_size: attribute_fs(node, "blockSize")?,
                texels_per_block: attribute_fs(node, "texelsPerBlock")?,
                block_extent: try_attribute_fs(node, "blockExtent")?,
                packed: try_attribute_fs(node, "packed")?,
                compressed: try_attribute(node, "compressed")?,
                chroma: try_attribute(node, "chroma")?,
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
                name: attribute(node, "name")?,
                bits: attribute_fs(node, "bits")?,
                numeric_format: attribute(node, "numericFormat")?,
                plane_index: try_attribute_fs(node, "planeIndex")?,
            })),
            "plane" => Ok(Some(FormatChild::Plane {
                index: attribute_fs(node, "index")?,
                width_divisor: attribute_fs(node, "widthDivisor")?,
                height_divisor: attribute_fs(node, "heightDivisor")?,
                compatible: attribute(node, "compatible")?,
            })),
            "spirvimageformat" => Ok(Some(FormatChild::SpirvImageFormat {
                name: attribute(node, "name")?,
            })),
            _ => Ok(None),
        }
    }
}
