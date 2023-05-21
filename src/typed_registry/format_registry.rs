use std::{fmt, num::NonZeroU8, str::FromStr};

use roxmltree::Node;

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]
#[xml(tag = "format")]
pub struct Format<'a> {
    /// format name
    #[xml(attribute())]
    pub name: &'a str,
    /// format class. A string whose value is shared by a group of formats which may be compatible, and is a textual description of something important that group has in common
    #[xml(attribute())]
    pub class: &'a str,
    /// texel block size, in bytes, of the format
    #[xml(attribute(rename = "blockSize"))]
    pub block_size: NonZeroU8,
    /// number of texels in a texel block of the format
    #[xml(attribute(rename = "texelsPerBlock"))]
    pub texels_per_block: NonZeroU8,
    /// Three-dimensional extent of a texel block
    #[xml(attribute(rename = "blockExtent"))]
    pub block_extent: Option<BlockExtent>,
    /// number of bits into which the format is packed
    #[xml(attribute())]
    pub packed: Option<NonZeroU8>,
    /// general texture compression scheme
    #[xml(attribute())]
    pub compressed: Option<&'a str>,
    /// The format's {YCbCr} encoding. Marks if {YCbCr} samplers are required by default when using this format
    #[xml(attribute())]
    pub chroma: Option<FormatChroma>,
    #[xml(child)]
    pub children: Vec<FormatChild<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq, XMLSerialization)]
#[cfg_attr(feature = "serialize", skip_serializing_none, derive(Serialize))]

pub enum FormatChild<'a> {
    #[xml(tag = "component")]
    Component {
        /// name of this component
        #[xml(attribute())]
        name: ComponentName,
        /// number of bits in this component if it's not compressed
        #[xml(attribute())]
        bits: ComponentBits,
        /// scalar data type of the component
        #[xml(attribute(rename = "numericFormat"))]
        numeric_format: ComponentNumericFormat,
        /// which plane this component lies in
        #[xml(attribute(rename = "planeIndex"))]
        plane_index: Option<u8>,
    },
    #[xml(tag = "plane")]
    Plane {
        /// image plane being defined. Image planes are in the range [0,p-1] where p is the number of planes in the format.
        #[xml(attribute())]
        index: u8,
        /// relative width of this plane. A value of k means that this plane is 1/k the width of the overall format.
        #[xml(attribute(rename = "widthDivisor"))]
        width_divisor: NonZeroU8,
        /// relative height of this plane. A value of k means that this plane is 1/k the height of the overall format.
        #[xml(attribute(rename = "heightDivisor"))]
        height_divisor: NonZeroU8,
        /// single-plane format that this plane is compatible with
        #[xml(attribute())]
        compatible: &'a str,
    },
    #[xml(tag = "spirvimageformat")]
    SpirvImageFormat {
        /// name of the SPIR-V image format
        #[xml(attribute())]
        name: &'a str,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromEscapedStr, DisplayEscaped)]
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

pub enum FormatChroma {
    #[strum(serialize = "420")]
    Type420,
    #[strum(serialize = "422")]
    Type422,
    #[strum(serialize = "444")]
    Type444,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromEscapedStr, DisplayEscaped)]
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
