use core::{
    fmt::{self, Write},
    marker::PhantomData,
    str::FromStr,
};
use std::borrow::Cow;

pub trait FromEscapedStr<'s>: Sized {
    fn from_escaped_str(s: &'s str) -> Self;
}

pub trait TryFromEscapedStr<'s>: Sized {
    type Error;
    // type Error: 'static + StdError;

    fn try_from_escaped_str(s: &'s str) -> Result<Self, Self::Error>;
}

impl<'s, T: FromEscapedStr<'s>> TryFromEscapedStr<'s> for T {
    type Error = core::convert::Infallible;

    fn try_from_escaped_str(s: &'s str) -> Result<Self, Self::Error> {
        Ok(Self::from_escaped_str(s))
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct UnescapedStr<'s>(pub(crate) Cow<'s, str>);

impl<'s> UnescapedStr<'s> {
    #[must_use]
    pub fn as_str(&self) -> &str {
        match &self.0 {
            Cow::Borrowed(s) => s,
            Cow::Owned(s) => s,
        }
    }
}

impl<'s> Default for UnescapedStr<'s> {
    fn default() -> Self {
        Self(Cow::Borrowed(""))
    }
}

impl<'s> From<&'s str> for UnescapedStr<'s> {
    fn from(value: &'s str) -> Self {
        Self(Cow::Borrowed(value))
    }
}

impl<'s> FromEscapedStr<'s> for UnescapedStr<'s> {
    fn from_escaped_str(s: &'s str) -> Self {
        if s.contains('&') {
            // Self(Cow::Owned(
            //     s.replace("&quot;", "\"")
            //         .replace("&apos;", "'")
            //         .replace("&lt;", "<")
            //         .replace("&gt;", ">")
            //         .replace("&amp;", "&"),
            // ))
            let mut unescaped = String::with_capacity(s.len());
            for v in s.split('&') {
                if let Some(v) = v.strip_prefix("quot;") {
                    unescaped.push('"');
                    unescaped.push_str(v)
                } else if let Some(v) = v.strip_prefix("apos;") {
                    unescaped.push('\'');
                    unescaped.push_str(v)
                } else if let Some(v) = v.strip_prefix("lt;") {
                    unescaped.push('<');
                    unescaped.push_str(v)
                } else if let Some(v) = v.strip_prefix("gt;") {
                    unescaped.push('>');
                    unescaped.push_str(v)
                } else if let Some(v) = v.strip_prefix("amp;") {
                    unescaped.push('&');
                    unescaped.push_str(v)
                } else {
                    unescaped.push_str(v)
                }
            }
            Self(Cow::Owned(unescaped))
        } else {
            Self(Cow::Borrowed(s))
        }
    }
}

impl<'s> fmt::Display for UnescapedStr<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

// FIXME: dangerous
impl<'s> FromEscapedStr<'s> for &'s str {
    fn from_escaped_str(s: &'s str) -> Self {
        s
    }
}

macro_rules! impl_try_from_esc_str {
    ($($t:ty),* $(,)?) => {
        $(
            impl<'s> TryFromEscapedStr<'s> for $t {
                type Error = <Self as FromStr>::Err;

                fn try_from_escaped_str(s: &'s str) -> Result<Self, Self::Error> {
                    Self::from_str(s)
                }
            }

        )*
    }
}

impl_try_from_esc_str! {
    bool,
    u8,
    u32,
    i32,
    usize,
    std::num::NonZeroU8,
    std::num::NonZeroU32,
    std::num::NonZeroUsize,
}

pub trait DisplayEscaped: fmt::Display {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt(f)
    }
}

impl<T: ?Sized + DisplayEscaped> DisplayEscaped for &T {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).escaped_fmt(f)
    }
}
impl<T: ?Sized + DisplayEscaped> DisplayEscaped for &mut T {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).escaped_fmt(f)
    }
}

#[repr(transparent)]
struct UnescapedWriter<'w, W: fmt::Write>(&'w mut W);

impl<'w, W: fmt::Write> fmt::Write for UnescapedWriter<'w, W> {
    //TODO: can this be done more efficiently using `str.split_inclusive` or something else?
    // The offical `EscapeUnicode`, `EscapeDefault`, & `EscapeDebug` use this method though
    fn write_str(&mut self, s: &str) -> fmt::Result {
        s.chars().try_for_each(|c| self.write_char(c))
    }
    fn write_char(&mut self, c: char) -> fmt::Result {
        match c {
            '"' => self.0.write_str("&quot;"),
            '\'' => self.0.write_str("&apos;"),
            '<' => self.0.write_str("&lt;"),
            '>' => self.0.write_str("&gt;"),
            '&' => self.0.write_str("&amp;"),
            c => self.0.write_char(c),
        }
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct Unescaped<V>(pub V);

impl<V: fmt::Display> fmt::Display for Unescaped<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<V: fmt::Display> DisplayEscaped for Unescaped<V> {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(UnescapedWriter(f), "{}", self.0)
    }
}

impl<'s> DisplayEscaped for UnescapedStr<'s> {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Cow::Borrowed(s) => write!(f, "{s}"),
            Cow::Owned(s) => write!(UnescapedWriter(f), "{s}"),
        }
    }
}

impl DisplayEscaped for bool {}
impl DisplayEscaped for u8 {}
impl DisplayEscaped for u32 {}
impl DisplayEscaped for i32 {}
impl DisplayEscaped for usize {}
impl DisplayEscaped for std::num::NonZeroU8 {}
impl DisplayEscaped for std::num::NonZeroU32 {}
impl DisplayEscaped for std::num::NonZeroUsize {}

// FIXME: dangerous
impl DisplayEscaped for str {}
impl<'xml> DisplayEscaped for fmt::Arguments<'xml> {}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub(crate) struct Escaped<'v, V: ?Sized>(pub &'v V);

impl<'v, V: ?Sized + DisplayEscaped> fmt::Display for Escaped<'v, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (&self.0).escaped_fmt(f)
    }
}

pub trait Seperator {
    // TODO: Switch to `Pattern` once it's stabilized
    // type Value: fmt::Display + core::str::pattern::Pattern<'static>;
    // const SEP: Self::Value;
    const SEP: char;
}

pub struct CommaSeperator;
impl Seperator for CommaSeperator {
    const SEP: char = ',';
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub(crate) struct InterspersedDisplay<'v, S, V: ?Sized>(&'v V, PhantomData<S>);

impl<'v, S, V: ?Sized> InterspersedDisplay<'v, S, V> {
    pub fn new(v: &'v V) -> Self {
        InterspersedDisplay(v, PhantomData)
    }
}

impl<'v, S: Seperator, V: ?Sized + crate::Iterable> fmt::Display for InterspersedDisplay<'v, S, V>
where
    <<V as crate::Iterable>::IT<'v> as IntoIterator>::Item: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut is_first = true;
        for v in crate::Iterable::iter(self.0) {
            if is_first {
                is_first = false;
            } else {
                f.write_char(S::SEP)?;
            }
            v.fmt(f)?;
        }
        Ok(())
    }
}

impl<'v, S: Seperator, V: ?Sized + crate::Iterable> DisplayEscaped for InterspersedDisplay<'v, S, V>
where
    <<V as crate::Iterable>::IT<'v> as IntoIterator>::Item: DisplayEscaped,
{
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut is_first = true;
        for v in crate::Iterable::iter(self.0) {
            if is_first {
                is_first = false;
            } else {
                f.write_char(S::SEP)?;
            }
            v.escaped_fmt(f)?;
        }
        Ok(())
    }
}
