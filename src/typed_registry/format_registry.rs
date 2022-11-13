use std::{borrow::Cow, fmt, num::NonZeroU8, str::FromStr};

use roxmltree::Node;

use crate::{attribute, attribute_fs, try_attribute, try_attribute_fs, Parse, ParseResult};

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]

pub struct Format<'a> {
    /// format name
    pub name: Cow<'a, str>,
    /// format class. A string whose value is shared by a group of formats which may be compatible, and is a textual description of something important that group has in common
    pub class: Cow<'a, str>,
    /// texel block size, in bytes, of the format
    pub block_size: NonZeroU8,
    /// number of texels in a texel block of the format
    pub texels_per_block: NonZeroU8,
    /// Three-dimensional extent of a texel block
    pub block_extent: Option<BlockExtent>,
    /// number of bits into which the format is packed
    pub packed: Option<NonZeroU8>,
    /// general texture compression scheme
    pub compressed: Option<Cow<'a, str>>,
    /// The format's {YCbCr} encoding. Marks if {YCbCr} samplers are required by default when using this format
    pub chroma: Option<FormatChroma>,
    pub children: Vec<FormatChild<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]

pub enum FormatChild<'a> {
    Component {
        /// name of this component
        name: ComponentName,
        /// number of bits in this component if it's not compressed
        bits: ComponentBits,
        /// scalar data type of the component
        numeric_format: ComponentNumericFormat,
        /// which plane this component lies in
        plane_index: Option<u8>,
    },

    Plane {
        /// image plane being defined. Image planes are in the range [0,p-1] where p is the number of planes in the format.
        index: u8,
        /// relative width of this plane. A value of k means that this plane is 1/k the width of the overall format.
        width_divisor: NonZeroU8,
        /// relative height of this plane. A value of k means that this plane is 1/k the height of the overall format.
        height_divisor: NonZeroU8,
        /// single-plane format that this plane is compatible with
        compatible: Cow<'a, str>,
    },

    SpirvImageFormat {
        /// name of the SPIR-V image format
        name: Cow<'a, str>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
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
#[cfg_attr(feature = "serialize", derive(Serialize))]

pub enum FormatChroma {
    #[strum(serialize = "420")]
    Type420,
    #[strum(serialize = "422")]
    Type422,
    #[strum(serialize = "444")]
    Type444,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]

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
#[cfg_attr(feature = "serialize", derive(Serialize))]

pub enum ComponentNumericFormat {
    /// R, G, and B components are unsigned normalized values that represent values using sRGB nonlinear encoding, while the A component (if one exists) is a regular unsigned normalized value
    SRGB,
    /// unsigned normalized values in the range [0,1]
    UNORM,
    /// signed normalized values in the range [-1,1]
    SNORM,
    /// unsigned integer values in the range [0,exp2(n)-1]
    UINT,
    /// signed integer values in the range [-exp2(n-1),exp2(n-1)-1]
    SINT,
    /// unsigned integer values that get converted to floating-point in the range [0,exp2(n)-1]
    USCALED,
    /// signed integer values that get converted to floating-point in the range [-exp2(n-1),exp2(n-1)-1]
    SSCALED,
    /// unsigned floating-point numbers (used by packed, shared exponent, and some compressed formats)
    UFLOAT,
    /// signed floating-point numbers
    SFLOAT,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::EnumString, strum::Display)]
#[cfg_attr(feature = "serialize", derive(Serialize))]

pub enum ComponentName {
    #[strum(serialize = "A")]
    Alpha,
    #[strum(serialize = "R")]
    Red,
    #[strum(serialize = "G")]
    Green,
    #[strum(serialize = "B")]
    Blue,
    #[strum(serialize = "S")]
    Stencil,
    #[strum(serialize = "D")]
    Depth,
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
