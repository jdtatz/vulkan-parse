use core::fmt::Debug;
use std::{fmt, ops::Range, str::FromStr};

use logos::{Lexer, Logos};

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Error {
    pub span: Range<usize>,
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "unexpected token at {:?}", self.span)
    }
}

impl std::error::Error for Error {}

pub struct ResultIter<'source, Token: Logos<'source>> {
    lexer: Lexer<'source, Token>,
}

impl<'source, Token: Logos<'source>> From<Lexer<'source, Token>> for ResultIter<'source, Token> {
    fn from(lexer: Lexer<'source, Token>) -> Self {
        Self { lexer }
    }
}

impl<'source, Token> Iterator for ResultIter<'source, Token>
where
    Token: Logos<'source> + PartialEq,
{
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next().map(|token| {
            if token == Token::ERROR {
                Err(Error {
                    span: self.lexer.span(),
                })
            } else {
                Ok(token)
            }
        })
    }
}

#[must_use]
pub fn tokenize(src: &str, parsing_macros: bool, objc_compat: bool) -> ResultIter<'_, Token> {
    let extras = TokenExtras {
        keep_new_lines: parsing_macros,
        objc_compat,
    };
    ResultIter::from(Lexer::with_extras(src, extras))
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum Constant {
    Char(u8),
    Integer(u64),
    Float(f64),
}

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Char(l), Self::Char(r)) => l == r,
            (Self::Integer(l), Self::Integer(r)) => l == r,
            (Self::Float(l), Self::Float(r)) => l == r,
            (Self::Float(f), Self::Integer(i)) | (Self::Integer(i), Self::Float(f)) => {
                (*i as f64) == *f
            }
            _ => false,
        }
    }
}

// SAFETY: Floating point constants must be finite (NaN is a macro)
impl Eq for Constant {}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Char(c) => write!(f, "'{}'", (*c) as char),
            Constant::Integer(i) => write!(f, "{i}"),
            Constant::Float(n) => write!(f, "{n}"),
        }
    }
}

impl Constant {
    fn from_c_char<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Result<Self, <char as FromStr>::Err> {
        let s = lex.slice();
        let s = s.strip_prefix('L').unwrap_or(s);
        let end = s.len() - 1;
        Ok(Constant::Char(char::from_str(&s[1..end])? as u8))
    }
}

const IS: &[char] = &['u', 'U', 'l', 'L'];
const FS: &[char] = &['f', 'F', 'l', 'L'];

fn fix_literal(lit: &str) -> &str {
    let lit = lit.strip_prefix('L').unwrap_or(lit);
    let lit = lit.strip_prefix('"').unwrap().strip_suffix('"').unwrap();
    lit
}

fn fix_escaped_literal(lit: &str) -> &str {
    let lit = lit.strip_prefix('L').unwrap_or(lit);
    let lit = lit
        .strip_prefix("&quot;")
        .unwrap()
        .strip_suffix("&quot;")
        .unwrap();
    lit
}

#[derive(Debug, Clone, Copy, Default)]
pub struct TokenExtras {
    pub keep_new_lines: bool,
    pub objc_compat: bool,
}

impl TokenExtras {
    fn new_line_filter(self) -> logos::Filter<()> {
        if self.keep_new_lines {
            logos::Filter::Emit(())
        } else {
            logos::Filter::Skip
        }
    }

    fn deprecation_comment(self, comment: &str) -> logos::Filter<&str> {
        if self.keep_new_lines {
            logos::Filter::Emit(comment.strip_prefix("// DEPRECATED:").unwrap())
        } else {
            logos::Filter::Skip
        }
    }

    fn objc_is_err(self) -> logos::FilterResult<()> {
        if self.objc_compat {
            logos::FilterResult::Emit(())
        } else {
            logos::FilterResult::Error
        }
    }
}

#[derive(Logos, Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[logos(extras = TokenExtras)]
#[logos(subpattern decimal = r"[1-9][0-9]*")]
#[logos(subpattern hex = r"[0-9a-fA-F]+")]
#[logos(subpattern octal = r"[0-7]+")]
#[logos(subpattern exp = r"[Ee][+-]?[0-9]+")]
#[logos(subpattern float_suffix = r"[fFlL]")]
#[logos(subpattern int_suffix = r"[uUlL]*")]
pub enum Token<'a> {
    #[error]
    Error,
    #[token("auto")]
    Auto,
    #[token("break")]
    Break,
    #[token("case")]
    Case,
    #[token("char")]
    Char,
    #[token("const")]
    Const,
    #[token("continue")]
    Continue,
    #[token("default")]
    Default,
    #[token("do")]
    Do,
    #[token("double")]
    Double,
    #[token("else")]
    Else,
    #[token("enum")]
    Enum,
    #[token("extern")]
    Extern,
    #[token("float")]
    Float,
    #[token("for")]
    For,
    #[token("goto")]
    Goto,
    #[token("if")]
    If,
    #[token("inline")]
    Inline,
    #[token("int")]
    Int,
    #[token("long")]
    Long,
    #[token("register")]
    Register,
    #[token("restrict")]
    Restrict,
    #[token("return")]
    Return,
    #[token("short")]
    Short,
    #[token("signed")]
    Signed,
    #[token("sizeof")]
    SizeOf,
    #[token("static")]
    Static,
    #[token("struct")]
    Struct,
    #[token("switch")]
    Switch,
    #[token("typedef")]
    TypeDef,
    #[token("union")]
    Union,
    #[token("unsigned")]
    UnSigned,
    #[token("void")]
    Void,
    #[token("volatile")]
    Volatile,
    #[token("while")]
    While,
    #[token("_Bool")]
    Bool,
    #[token("_Complex")]
    Complex,
    #[token("_Imaginary")]
    Imaginary,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(".")]
    Dot,
    #[token("-&gt;")]
    #[token("->")]
    Point,
    #[token("++")]
    Increment,
    #[token("--")]
    Decrement,
    #[token("&amp;")]
    #[token("&")]
    Ampersand,
    #[token("*")]
    MulStar,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("~")]
    Tilde,
    #[token("!")]
    Exclamation,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("&lt;&lt;")]
    #[token("<<")]
    LShift,
    #[token("&gt;&gt;")]
    #[token(">>")]
    RShift,
    #[token("&lt;")]
    #[token("<")]
    LessThan,
    #[token("&gt;")]
    #[token(">")]
    GreaterThan,
    #[token("&lt;=")]
    #[token("<=")]
    LessThanEqual,
    #[token("&gt;=")]
    #[token(">=")]
    GreaterThanEqual,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("^")]
    Caret,
    #[token("|")]
    VerticalBar,
    #[token("&amp;&amp;")]
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("?")]
    Question,
    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token("...")]
    Ellipsis,
    #[token("=")]
    Assign,
    #[token("*=")]
    MulAssign,
    #[token("/=")]
    DivAssign,
    #[token("%=")]
    RemAssign,
    #[token("+=")]
    AddAssign,
    #[token("-=")]
    SubAssign,
    #[token("&lt;&lt;=")]
    #[token("<<=")]
    LShiftAssign,
    #[token("&gt;&gt;=")]
    #[token(">>=")]
    RShiftAssign,
    #[token("&amp;=")]
    #[token("&=")]
    AndAssign,
    #[token("|=")]
    OrAssign,
    #[token("^=")]
    XorAssign,
    #[token(",")]
    Comma,
    #[token("@", |lex| lex.extras.objc_is_err())]
    ObjectiveCAt,

    //
    #[regex(r"\n", |lex| lex.extras.new_line_filter())]
    NewLine,
    #[regex(r"[ \t\r\f]+", logos::skip)]
    Whitespace,

    // pre-processing tokens
    // FIXME
    #[doc(hidden)]
    #[regex("//#define\\s*")]
    _MalformedDefine,
    #[doc(hidden)]
    #[regex(r"// DEPRECATED:[^\n]*", |lex| lex.extras.deprecation_comment(lex.slice()))]
    _DeprecationComment(&'a str),
    #[regex(r"//[^\n]*", logos::skip)]
    Comment,
    #[token("#")]
    Pound,
    #[token("##")]
    DoublePound,
    #[regex(r"\\[ \t\r\f]*\n", logos::skip)]
    BackSlash,

    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| lex.slice())]
    Identifier(&'a str),
    #[regex("0(?&int_suffix)?", |_lex| Constant::Integer(0))]
    #[regex("(?&decimal)(?&int_suffix)?", |lex| lex.slice().trim_end_matches(IS).parse().map(Constant::Integer))]
    #[regex("0(?&octal)(?&int_suffix)?", |lex| u64::from_str_radix(lex.slice()[1..].trim_end_matches(IS), 8).map(Constant::Integer))]
    #[regex("0[xX](?&hex)(?&int_suffix)?", |lex| u64::from_str_radix(lex.slice()[2..].trim_end_matches(IS), 16).map(Constant::Integer))]
    #[regex(r"L?'(\\.|[^\\'])+'", |lex| Constant::from_c_char(lex))]
    #[regex(r#"[0-9]+\.[0-9]*(?&exp)?(?&float_suffix)?"#, |lex| lex.slice().trim_end_matches(FS).parse().map(Constant::Float))]
    #[regex(r#"\.[0-9]+(?&exp)?(?&float_suffix)?"#, |lex| lex.slice().trim_end_matches(FS).parse().map(Constant::Float))]
    #[regex(r#"[0-9]+(?&exp)(?&float_suffix)?"#, |lex| lex.slice().trim_end_matches(FS).parse().map(Constant::Float))]
    #[regex(r#"[0-9]+(?&exp)?[fF]"#, |lex| lex.slice().trim_end_matches(FS).parse().map(Constant::Float))]
    Constant(Constant),
    // FIXME ASAP
    #[regex(r#"L?&quot;([^&\\]|\\t|\\u|\\n)*&quot;"#, |lex| fix_escaped_literal(lex.slice()))]
    #[regex(r#"L?"([^"\\]|\\t|\\u|\\n|\\")*""#, |lex| fix_literal(lex.slice()))]
    Literal(&'a str),
}

impl<'a> From<Constant> for Token<'a> {
    fn from(value: Constant) -> Self {
        Token::Constant(value)
    }
}

impl<'a> Token<'a> {
    #[must_use]
    pub fn from_literal(s: &str) -> Option<Self> {
        match s {
            "auto" => Some(Token::Auto),
            "break" => Some(Token::Break),
            "case" => Some(Token::Case),
            "char" => Some(Token::Char),
            "const" => Some(Token::Const),
            "continue" => Some(Token::Continue),
            "default" => Some(Token::Default),
            "do" => Some(Token::Do),
            "double" => Some(Token::Double),
            "else" => Some(Token::Else),
            "enum" => Some(Token::Enum),
            "extern" => Some(Token::Extern),
            "float" => Some(Token::Float),
            "for" => Some(Token::For),
            "goto" => Some(Token::Goto),
            "if" => Some(Token::If),
            "inline" => Some(Token::Inline),
            "int" => Some(Token::Int),
            "long" => Some(Token::Long),
            "register" => Some(Token::Register),
            "restrict" => Some(Token::Restrict),
            "return" => Some(Token::Return),
            "short" => Some(Token::Short),
            "signed" => Some(Token::Signed),
            "sizeof" => Some(Token::SizeOf),
            "static" => Some(Token::Static),
            "struct" => Some(Token::Struct),
            "switch" => Some(Token::Switch),
            "typedef" => Some(Token::TypeDef),
            "union" => Some(Token::Union),
            "unsigned" => Some(Token::UnSigned),
            "void" => Some(Token::Void),
            "volatile" => Some(Token::Volatile),
            "while" => Some(Token::While),
            "_Bool" => Some(Token::Bool),
            "_Complex" => Some(Token::Complex),
            "_Imaginary" => Some(Token::Imaginary),
            "[" => Some(Token::LBrack),
            "]" => Some(Token::RBrack),
            "(" => Some(Token::LParen),
            ")" => Some(Token::RParen),
            "{" => Some(Token::LBrace),
            "}" => Some(Token::RBrace),
            "." => Some(Token::Dot),
            "->" => Some(Token::Point),
            "++" => Some(Token::Increment),
            "--" => Some(Token::Decrement),
            "&" => Some(Token::Ampersand),
            "*" => Some(Token::MulStar),
            "+" => Some(Token::Plus),
            "-" => Some(Token::Minus),
            "~" => Some(Token::Tilde),
            "!" => Some(Token::Exclamation),
            "/" => Some(Token::Slash),
            "%" => Some(Token::Percent),
            "<<" => Some(Token::LShift),
            ">>" => Some(Token::RShift),
            "<" => Some(Token::LessThan),
            ">" => Some(Token::GreaterThan),
            "<=" => Some(Token::LessThanEqual),
            ">=" => Some(Token::GreaterThanEqual),
            "==" => Some(Token::Equal),
            "!=" => Some(Token::NotEqual),
            "^" => Some(Token::Caret),
            "|" => Some(Token::VerticalBar),
            "&&" => Some(Token::And),
            "||" => Some(Token::Or),
            "?" => Some(Token::Question),
            ":" => Some(Token::Colon),
            ";" => Some(Token::SemiColon),
            "..." => Some(Token::Ellipsis),
            "=" => Some(Token::Assign),
            "*=" => Some(Token::MulAssign),
            "/=" => Some(Token::DivAssign),
            "%=" => Some(Token::RemAssign),
            "+=" => Some(Token::AddAssign),
            "-=" => Some(Token::SubAssign),
            "<<=" => Some(Token::LShiftAssign),
            ">>=" => Some(Token::RShiftAssign),
            "&=" => Some(Token::AndAssign),
            "|=" => Some(Token::OrAssign),
            "^=" => Some(Token::XorAssign),
            "," => Some(Token::Comma),
            "@" => Some(Token::ObjectiveCAt),
            "#" => Some(Token::Pound),
            "##" => Some(Token::DoublePound),
            "\\" => Some(Token::BackSlash),
            "//#define" => Some(Token::_MalformedDefine),
            "\n" => Some(Token::NewLine),
            " " => Some(Token::Whitespace),
            _ => None,
        }
    }

    #[must_use]
    pub fn is_ident_like(&self) -> bool {
        matches!(
            self,
            Token::Auto
                | Token::Break
                | Token::Case
                | Token::Char
                | Token::Const
                | Token::Continue
                | Token::Default
                | Token::Do
                | Token::Double
                | Token::Else
                | Token::Enum
                | Token::Extern
                | Token::Float
                | Token::For
                | Token::Goto
                | Token::If
                | Token::Inline
                | Token::Int
                | Token::Long
                | Token::Register
                | Token::Restrict
                | Token::Return
                | Token::Short
                | Token::Signed
                | Token::SizeOf
                | Token::Static
                | Token::Struct
                | Token::Switch
                | Token::TypeDef
                | Token::Union
                | Token::UnSigned
                | Token::Void
                | Token::Volatile
                | Token::While
                | Token::Bool
                | Token::Complex
                | Token::Imaginary
                | Token::_MalformedDefine
                | Token::Identifier(_)
                | Token::Constant(_)
                | Token::Literal(_)
        )
    }
}

impl<'a> fmt::Display for Token<'a> {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Auto => write!(f, "auto"),
            Token::Break => write!(f, "break"),
            Token::Case => write!(f, "case"),
            Token::Char => write!(f, "char"),
            Token::Const => write!(f, "const"),
            Token::Continue => write!(f, "continue"),
            Token::Default => write!(f, "default"),
            Token::Do => write!(f, "do"),
            Token::Double => write!(f, "double"),
            Token::Else => write!(f, "else"),
            Token::Enum => write!(f, "enum"),
            Token::Extern => write!(f, "extern"),
            Token::Float => write!(f, "float"),
            Token::For => write!(f, "for"),
            Token::Goto => write!(f, "goto"),
            Token::If => write!(f, "if"),
            Token::Inline => write!(f, "inline"),
            Token::Int => write!(f, "int"),
            Token::Long => write!(f, "long"),
            Token::Register => write!(f, "register"),
            Token::Restrict => write!(f, "restrict"),
            Token::Return => write!(f, "return"),
            Token::Short => write!(f, "short"),
            Token::Signed => write!(f, "signed"),
            Token::SizeOf => write!(f, "sizeof"),
            Token::Static => write!(f, "static"),
            Token::Struct => write!(f, "struct"),
            Token::Switch => write!(f, "switch"),
            Token::TypeDef => write!(f, "typedef"),
            Token::Union => write!(f, "union"),
            Token::UnSigned => write!(f, "unsigned"),
            Token::Void => write!(f, "void"),
            Token::Volatile => write!(f, "volatile"),
            Token::While => write!(f, "while"),
            Token::Bool => write!(f, "_Bool"),
            Token::Complex => write!(f, "_Complex"),
            Token::Imaginary => write!(f, "_Imaginary"),
            Token::LBrack => write!(f, "["),
            Token::RBrack => write!(f, "]"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Dot => write!(f, "."),
            Token::Point => write!(f, "->"),
            Token::Increment => write!(f, "++"),
            Token::Decrement => write!(f, "--"),
            Token::Ampersand => write!(f, "&"),
            Token::MulStar => write!(f, "*"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Tilde => write!(f, "~"),
            Token::Exclamation => write!(f, "!"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::LShift => write!(f, "<<"),
            Token::RShift => write!(f, ">>"),
            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),
            Token::LessThanEqual => write!(f, "<="),
            Token::GreaterThanEqual => write!(f, ">="),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::Caret => write!(f, "^"),
            Token::VerticalBar => write!(f, "|"),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::Question => write!(f, "?"),
            Token::Colon => write!(f, ":"),
            Token::SemiColon => write!(f, ";"),
            Token::Ellipsis => write!(f, "..."),
            Token::Assign => write!(f, "="),
            Token::MulAssign => write!(f, "*="),
            Token::DivAssign => write!(f, "/="),
            Token::RemAssign => write!(f, "%="),
            Token::AddAssign => write!(f, "+="),
            Token::SubAssign => write!(f, "-="),
            Token::LShiftAssign => write!(f, "<<="),
            Token::RShiftAssign => write!(f, ">>="),
            Token::AndAssign => write!(f, "&="),
            Token::OrAssign => write!(f, "|="),
            Token::XorAssign => write!(f, "^="),
            Token::Comma => write!(f, ","),
            Token::ObjectiveCAt => write!(f, "@"),
            Token::Pound => write!(f, "#"),
            Token::DoublePound => write!(f, "##"),
            Token::BackSlash => write!(f, "\\"),
            Token::_MalformedDefine => write!(f, "//#define"),
            Token::NewLine => writeln!(f),
            Token::Whitespace => write!(f, " "),
            Token::Comment => writeln!(f, "//"),
            Token::_DeprecationComment(c) => write!(f, "// DEPRECATED:{c}"),
            Token::Identifier(id) => write!(f, "{id}"),
            Token::Constant(c) => write!(f, "{c}"),
            Token::Literal(lit) => write!(f, "\"{lit}\""),
            Token::Error => unreachable!(),
        }?;
        Ok(())
    }
}

impl<'a> crate::DisplayEscaped for Token<'a> {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Point => write!(f, "-&gt;"),
            Token::Ampersand => write!(f, "&amp;"),
            Token::And => write!(f, "&amp;&amp;"),
            Token::LShift => write!(f, "&lt;&lt;"),
            Token::RShift => write!(f, "&gt;&gt;"),
            Token::LessThan => write!(f, "&lt;"),
            Token::GreaterThan => write!(f, "&gt;"),
            Token::LessThanEqual => write!(f, "&lt;="),
            Token::GreaterThanEqual => write!(f, "&gt;="),
            Token::AndAssign => write!(f, "&amp;="),
            Token::Constant(Constant::Char(c)) => write!(f, "&apos;{c}&apos;"),
            Token::Literal(lit) => write!(f, "&quot;{lit}&quot;"),
            t => fmt::Display::fmt(t, f),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_float_constants() {
        let tokens = Token::lexer("1000.0F").into_iter().collect::<Vec<_>>();
        assert_eq!(tokens, &[Token::Constant(Constant::Float(1000.0))]);
    }
    #[test]
    fn test_literal() {
        let tokens = Token::lexer("\"VK_KHR_surface\"")
            .into_iter()
            .collect::<Vec<_>>();
        assert_eq!(tokens, &[Token::Literal("VK_KHR_surface")]);
    }
    #[test]
    fn test_escaped_literal() {
        let tokens = Token::lexer("&quot;VK_KHR_surface&quot;")
            .into_iter()
            .collect::<Vec<_>>();
        assert_eq!(tokens, &[Token::Literal("VK_KHR_surface")]);
    }
}
