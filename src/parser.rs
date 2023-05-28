use std::{
    borrow::Cow,
    fmt, iter,
    num::{NonZeroU32, NonZeroU8},
    ops::Deref,
};

use crate::{
    lexer::tokenize, ArrayLength, BaseTypeType, BitmaskType, Constant, DisplayEscaped, ErrorKind,
    FieldLike, FieldLikeSizing, FnPtrType, HandleKind, HandleType, MacroDefine, MacroDefineValue,
    ParseResult, PointerKind, Token, UnescapedStr,
};
// Using the C grammer from https://web.archive.org/web/20181230041359if_/http://www.open-std.org/jtc1/sc22/wg14/www/abq/c17_updated_proposed_fdis.pdf
// the link is from https://rust-lang.github.io/unsafe-code-guidelines/layout/structs-and-tuples.html#c-compatible-layout-repr-c

// C Type Decleration types
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]

pub enum TypeQualifer {
    Const,
    Restrict,
    Volatile,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]

pub enum TypeSpecifier<'a> {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Struct(&'a str),
    Union(&'a str),
    Enum(&'a str),
    TypedefName(&'a str),
}

impl<'a> TypeSpecifier<'a> {
    fn from_plain(ident: &'a str, is_struct: bool) -> Self {
        match ident {
            "void" => TypeSpecifier::Void,
            "char" => TypeSpecifier::Char,
            "short" => TypeSpecifier::Short,
            "int" => TypeSpecifier::Int,
            "long" => TypeSpecifier::Long,
            "float" => TypeSpecifier::Float,
            "double" => TypeSpecifier::Double,
            _ if is_struct => TypeSpecifier::Struct(ident),
            _ => TypeSpecifier::TypedefName(ident),
        }
    }

    #[must_use]
    pub fn as_identifier(&self) -> &'a str {
        match self {
            TypeSpecifier::Void => "void",
            TypeSpecifier::Char => "char",
            TypeSpecifier::Short => "short",
            TypeSpecifier::Int => "int",
            TypeSpecifier::Long => "long",
            TypeSpecifier::Float => "float",
            TypeSpecifier::Double => "double",
            TypeSpecifier::Struct(id)
            | TypeSpecifier::Union(id)
            | TypeSpecifier::Enum(id)
            | TypeSpecifier::TypedefName(id) => id,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]

pub enum TypeName<'a> {
    Specifier(TypeSpecifier<'a>),
    Pointer {
        pointee_ty: Box<TypeName<'a>>,
    },
    Array(Box<TypeName<'a>>, Option<NonZeroU32>),
    Function {
        return_ty: Box<TypeName<'a>>,
        name: &'a str,
        arg_tys: Vec<TypeName<'a>>,
    },
    Qualified(TypeQualifer, Box<TypeName<'a>>),
}

enum TypeSpecifierOrQual<'a> {
    Specifier(TypeSpecifier<'a>),
    Qualifier(TypeQualifer),
}

// C Expression types

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum FixOrder {
    Prefix,
    Postfix,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]

pub enum ComparisionOp {
    LT,
    LTE,
    Eq,
    NEq,
    GTE,
    GT,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]

pub enum UnaryOp<'a> {
    Address,
    Indirection,
    Positive,
    Negative,
    BitwiseNegation,
    LogicalNegation,
    Increment(FixOrder),
    Decrement(FixOrder),
    Cast(TypeName<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum BinaryOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Remainder,
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum MemberAccess {
    /// '.'
    Direct,
    /// '->'
    Pointer,
}

#[derive(Debug, Clone, PartialEq, Eq, TryFromEscapedStr)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum Expression<'a> {
    Identifier(&'a str),
    Constant(Constant),
    Literal(&'a str),
    SizeOf(TypeName<'a>),
    Unary(UnaryOp<'a>, Box<Expression<'a>>),
    Binary(BinaryOp, Box<Expression<'a>>, Box<Expression<'a>>),
    Comparision(ComparisionOp, Box<Expression<'a>>, Box<Expression<'a>>),
    Assignment(Option<BinaryOp>, Box<Expression<'a>>, Box<Expression<'a>>),
    TernaryIfElse(
        Box<Expression<'a>>,
        Box<Expression<'a>>,
        Box<Expression<'a>>,
    ),
    FunctionCall(Box<Expression<'a>>, Vec<Expression<'a>>),
    Comma(Box<Expression<'a>>, Box<Expression<'a>>),
    Member(MemberAccess, Box<Expression<'a>>, &'a str),
    ArrayElement(Box<Expression<'a>>, Box<Expression<'a>>),
}

fn wrap_unary<'a>(op: UnaryOp<'a>, e: Expression<'a>) -> Expression<'a> {
    Expression::Unary(op, Box::new(e))
}

fn wrap_binary<'a>(el: Expression<'a>, op: BinaryOp, er: Expression<'a>) -> Expression<'a> {
    Expression::Binary(op, Box::new(el), Box::new(er))
}

fn wrap_ternary<'a>(cond: Expression<'a>, t: Expression<'a>, f: Expression<'a>) -> Expression<'a> {
    Expression::TernaryIfElse(Box::new(cond), Box::new(t), Box::new(f))
}

fn wrap_assignment<'a>(
    op: Option<BinaryOp>,
    to: Expression<'a>,
    from: Expression<'a>,
) -> Expression<'a> {
    Expression::Assignment(op, Box::new(to), Box::new(from))
}

fn wrap_comparision<'a>(
    el: Expression<'a>,
    op: ComparisionOp,
    er: Expression<'a>,
) -> Expression<'a> {
    Expression::Comparision(op, Box::new(el), Box::new(er))
}

fn wrap_member<'a>(v: Expression<'a>, access: MemberAccess, member: &'a str) -> Expression<'a> {
    Expression::Member(access, Box::new(v), member)
}

fn wrap_array_access<'a>(v: Expression<'a>, idx: Expression<'a>) -> Expression<'a> {
    Expression::ArrayElement(Box::new(v), Box::new(idx))
}

fn wrap_fn_call<'a>(f: Expression<'a>, args: Vec<Expression<'a>>) -> Expression<'a> {
    Expression::FunctionCall(Box::new(f), args)
}

fn wrap_comma<'a>(head: Expression<'a>, tail: Expression<'a>) -> Expression<'a> {
    Expression::Comma(Box::new(head), Box::new(tail))
}

impl<'a> From<UnaryOp<'a>> for Token<'static> {
    fn from(val: UnaryOp) -> Self {
        match val {
            UnaryOp::Address => Token::Ampersand,
            UnaryOp::Indirection => Token::MulStar,
            UnaryOp::Positive => Token::Plus,
            UnaryOp::Negative => Token::Minus,
            UnaryOp::BitwiseNegation => Token::Tilde,
            UnaryOp::LogicalNegation => Token::Exclamation,
            UnaryOp::Increment(_) => Token::Increment,
            UnaryOp::Decrement(_) => Token::Decrement,
            UnaryOp::Cast(_) => todo!(),
        }
    }
}

impl From<BinaryOp> for Token<'static> {
    fn from(val: BinaryOp) -> Self {
        match val {
            BinaryOp::Addition => Token::Plus,
            BinaryOp::Subtraction => Token::Minus,
            BinaryOp::Multiplication => Token::MulStar,
            BinaryOp::Division => Token::Slash,
            BinaryOp::Remainder => Token::Percent,
            BinaryOp::LeftShift => Token::LShift,
            BinaryOp::RightShift => Token::RShift,
            BinaryOp::BitwiseAnd => Token::Ampersand,
            BinaryOp::BitwiseOr => Token::VerticalBar,
            BinaryOp::BitwiseXor => Token::Caret,
            BinaryOp::LogicalAnd => Token::And,
            BinaryOp::LogicalOr => Token::Or,
        }
    }
}

impl From<ComparisionOp> for Token<'static> {
    fn from(val: ComparisionOp) -> Self {
        match val {
            ComparisionOp::LT => Token::LessThan,
            ComparisionOp::LTE => Token::LessThanEqual,
            ComparisionOp::Eq => Token::Equal,
            ComparisionOp::NEq => Token::NotEqual,
            ComparisionOp::GTE => Token::GreaterThanEqual,
            ComparisionOp::GT => Token::GreaterThan,
        }
    }
}

impl From<MemberAccess> for Token<'static> {
    fn from(val: MemberAccess) -> Self {
        match val {
            MemberAccess::Direct => Token::Dot,
            MemberAccess::Pointer => Token::Point,
        }
    }
}

// Only used for roundtrip
impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.yield_tokens( |token| write!(f, "{token}"), false)
    }
}

impl DisplayEscaped for Expression<'_> {
    fn escaped_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.yield_tokens( |token| token.escaped_fmt(f), false)
    }
}

impl<'a> Expression<'a> {
    pub fn yield_tokens<E, F: FnMut(Token<'a>) -> Result<(), E>>(&self, mut yield_token: F, is_inner: bool) -> Result<(), E> {
        match self {
            Expression::Identifier(id) => yield_token(Token::Identifier(id)),
            Expression::Constant(c) => yield_token(Token::Constant(*c)),
            Expression::Literal(lit) => yield_token(Token::Literal(lit)),
            Expression::SizeOf(_) => todo!(),
            Expression::Unary(UnaryOp::Increment(FixOrder::Postfix), e) => {
                e.yield_tokens(&mut yield_token, is_inner)?;
                yield_token(Token::Increment)
            }
            Expression::Unary(UnaryOp::Decrement(FixOrder::Postfix), e) => {
                e.yield_tokens(&mut yield_token, is_inner)?;
                yield_token(Token::Decrement)
            }
            Expression::Unary(UnaryOp::Cast(ty), e) => todo!("({:?}){:?}", ty, e),
            Expression::Unary(op, e) => {
                yield_token(op.clone().into())?;
                e.yield_tokens(&mut yield_token, is_inner)
            }
            Expression::Binary(op, l, r) => {
                if is_inner {
                    yield_token(Token::LParen)?;
                }
                l.yield_tokens(&mut yield_token, true)?;
                yield_token(op.clone().into())?;
                r.yield_tokens(&mut yield_token, true)?;
                if is_inner {
                    yield_token(Token::RParen)?;
                }
                Ok(())
            }
            Expression::Comparision(op, l, r) => {
                if is_inner {
                    yield_token(Token::LParen)?;
                }
                l.yield_tokens(&mut yield_token, true)?;
                yield_token(op.clone().into())?;
                r.yield_tokens(&mut yield_token, true)?;
                if is_inner {
                    yield_token(Token::RParen)?;
                }
                Ok(())
            }
            Expression::Assignment(_, _, _) => todo!(),
            Expression::TernaryIfElse(cond, et, ef) => {
                if is_inner {
                    yield_token(Token::LParen)?;
                }
                cond.yield_tokens(&mut yield_token, true)?;
                yield_token(Token::Question)?;
                et.yield_tokens(&mut yield_token, true)?;
                yield_token(Token::Colon)?;
                ef.yield_tokens(&mut yield_token, true)?;
                if is_inner {
                    yield_token(Token::RParen)?;
                }
                Ok(())
            }
            Expression::FunctionCall(func, args) => {
                func.yield_tokens(&mut yield_token, true)?;
                yield_token(Token::LParen)?;
                let mut is_first = true;
                for arg in args.iter() {
                    if is_first {
                        is_first = false;
                    } else {
                        yield_token(Token::Comma)?;
                    }
                    arg.yield_tokens(&mut yield_token, false)?;
                }
                yield_token(Token::RParen)
            }
            Expression::Comma(head, tail) => {
                head.yield_tokens(&mut yield_token, true)?;
                yield_token(Token::Comma)?;
                tail.yield_tokens(&mut yield_token, true)
            }
            Expression::Member(access, v, member) => {
                v.yield_tokens(&mut yield_token, true)?;
                yield_token(access.clone().into())?;
                yield_token(Token::Identifier(member))
            }
            Expression::ArrayElement(e, i) => {
                e.yield_tokens(&mut yield_token, true)?;
                yield_token(Token::LBrack)?;
                i.yield_tokens(&mut yield_token, true)?;
                yield_token(Token::RBrack)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VkXMLToken<'a> {
    C(Token<'a>),
    TextTag {
        name: &'a str,
        text: crate::UnescapedStr<'a>,
    },
}

impl<'a> From<Token<'a>> for VkXMLToken<'a> {
    fn from(value: Token<'a>) -> Self {
        VkXMLToken::C(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VkXMLTokens<'a>(pub Vec<VkXMLToken<'a>>);

impl<'a> Deref for VkXMLTokens<'a> {
    type Target = [VkXMLToken<'a>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> FromIterator<VkXMLToken<'a>> for VkXMLTokens<'a> {
    fn from_iter<T: IntoIterator<Item = VkXMLToken<'a>>>(iter: T) -> Self {
        VkXMLTokens(iter.into_iter().collect())
    }
}

impl<'a> FromIterator<Token<'a>> for VkXMLTokens<'a> {
    fn from_iter<T: IntoIterator<Item = Token<'a>>>(iter: T) -> Self {
        VkXMLTokens(iter.into_iter().map(VkXMLToken::C).collect())
    }
}

enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<T, L: Iterator<Item = T>, R: Iterator<Item = T>> Iterator for Either<L, R> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Left(l) => l.next(),
            Self::Right(r) => r.next(),
        }
    }
}

fn vk_token_flat_map<'a, 'input: 'a>(
    n: roxmltree::Node<'a, 'input>,
    parsing_macros: bool,
    objc_compat: bool,
) -> impl IntoIterator<Item = ParseResult<VkXMLToken<'a>>> {
    // Empty elements are disallowed in vulkan's mixed pseudo-c/xml, except in <comment>
    if n.is_element() {
        let text = n
            .text_storage()
            .map(crate::UnescapedStr::from)
            .unwrap_or_default();
        Either::Left(iter::once(Ok(VkXMLToken::TextTag {
            name: n.tag_name().name(),
            text,
        })))
    } else {
        let text = n.text().unwrap_or("");
        Either::Right(tokenize(text, parsing_macros, objc_compat).map(move |r| {
            r.map(VkXMLToken::C)
                .map_err(|e| ErrorKind::LexerError(e).with_location(&n))
        }))
    }
}

impl<'a> peg::Parse for VkXMLTokens<'a> {
    type PositionRepr = usize;
    fn start(&self) -> usize {
        0
    }

    fn is_eof(&self, pos: usize) -> bool {
        pos >= self.len()
    }

    fn position_repr(&self, pos: usize) -> usize {
        pos
    }
}

pub type ParseError = peg::error::ParseError<<VkXMLTokens<'static> as peg::Parse>::PositionRepr>;

pub trait TryFromTokens<'a>: Sized {
    const PARSING_MACROS: bool;
    const OBJC_COMPAT: bool;

    fn try_from_tokens(tokens: &VkXMLTokens<'a>) -> Result<Self, ParseError>;
    fn try_from_elements<'input: 'a, I: IntoIterator<Item = roxmltree::Node<'a, 'input>>>(
        elements: I,
        parent_loc: crate::Location,
    ) -> ParseResult<Self> {
        let tokens = elements
            .into_iter()
            .flat_map(|n| vk_token_flat_map(n, Self::PARSING_MACROS, Self::OBJC_COMPAT))
            .collect::<ParseResult<_>>()?;
        Self::try_from_tokens(&tokens)
            .map_err(|e| crate::ErrorKind::PegParsingError(e).with_location(parent_loc))
    }
    fn try_from_node<'input: 'a>(node: roxmltree::Node<'a, 'input>) -> ParseResult<Self> {
        Self::try_from_elements(
            node.children().filter(|n| n.is_element() || n.is_text()),
            (&node).into(),
        )
    }
}

impl<'peg, 'a: 'peg> peg::ParseElem<'peg> for VkXMLTokens<'a> {
    type Element = &'peg VkXMLToken<'a>;

    fn parse_elem(&'peg self, pos: usize) -> peg::RuleResult<Self::Element> {
        match self[pos..].first() {
            Some(c) => peg::RuleResult::Matched(pos + 1, c),
            None => peg::RuleResult::Failed,
        }
    }
}

impl<'a> peg::ParseLiteral for VkXMLTokens<'a> {
    fn parse_string_literal(&self, pos: usize, literal: &str) -> peg::RuleResult<()> {
        if let Some(VkXMLToken::C(tok)) = self.get(pos) {
            let literal_token = Token::from_literal(literal).unwrap_or_else(|| {
                unreachable!(
                    "This shouldn't even be possible, {literal:?} doesn't have a matching token."
                )
            });
            if &literal_token == tok {
                peg::RuleResult::Matched(pos + 1, ())
            } else {
                peg::RuleResult::Failed
            }
        } else {
            peg::RuleResult::Failed
        }
    }
}

impl<'peg, 'a: 'peg> peg::ParseSlice<'peg> for VkXMLTokens<'a> {
    type Slice = &'peg [VkXMLToken<'a>];
    fn parse_slice(&'peg self, p1: usize, p2: usize) -> Self::Slice {
        &self[p1..p2]
    }
}

/// C is not a context-free languge and requires building a symbol table of typedefs at parse time to resolve ambigutites with cast expressions
pub(crate) fn is_typedef_name(name: &str) -> bool {
    // FIXME: actually build a symbol table as we go that is already filled with the standard typedefs (size_t, uint32_t, ...),
    name.ends_with("_t") || name.starts_with("Vk") || name.starts_with("PFN_vk")
}

peg::parser! {
    pub grammar c_with_vk_ext<'a>() for VkXMLTokens<'a> {
        pub rule identifier() -> &'a str
          = quiet!{[VkXMLToken::C(Token::Identifier(i))] { (*i) }}
          / expected!("Identifier")

        rule typedef_name() -> &'a str
          = quiet!{[VkXMLToken::C(Token::Identifier(i)) if is_typedef_name(i)] { (*i) }}
          / expected!("Typedef Name")

        rule type_specifier() -> TypeSpecifier<'a>
          = "void" { TypeSpecifier::Void }
          / "char" { TypeSpecifier::Char }
          / "short" { TypeSpecifier::Short }
          / "int" { TypeSpecifier::Int }
          / "long" { TypeSpecifier::Long }
          / "float" { TypeSpecifier::Float }
          / "double" { TypeSpecifier::Double }
          / "struct" i:identifier() { TypeSpecifier::Struct(i) }
          / "union" i:identifier() { TypeSpecifier::Union(i) }
          / "enum" i:identifier() { TypeSpecifier::Enum(i) }
          / i:typedef_name() { TypeSpecifier::TypedefName(i) }

        pub rule type_name() -> TypeName<'a>
          = ty:type_specifier() { TypeName::Specifier(ty) }


        // C-expr rules

        pub rule constant() -> Constant
          = quiet!{[VkXMLToken::C(Token::Constant(c))] { *c }}
          / expected!("Constant")

        rule literal() -> &'a str
          = quiet!{[VkXMLToken::C(Token::Literal(l))] { l }}
          / expected!("String Literal")

        pub rule expr() -> Expression<'a> = precedence!{
        //   x:(@) "," y:@ { wrap_comma(x, y) }
        //   --
          x:@ "=" y:(@) { wrap_assignment(None, x, y) }
          x:@ "+=" y:(@) { wrap_assignment(Some(BinaryOp::Addition), x, y) }
          x:@ "-=" y:(@) { wrap_assignment(Some(BinaryOp::Subtraction), x, y) }
          x:@ "*=" y:(@) { wrap_assignment(Some(BinaryOp::Multiplication), x, y) }
          x:@ "/=" y:(@) { wrap_assignment(Some(BinaryOp::Division), x, y) }
          x:@ "%=" y:(@) { wrap_assignment(Some(BinaryOp::Remainder), x, y) }
          x:@ "<<=" y:(@) { wrap_assignment(Some(BinaryOp::LeftShift), x, y) }
          x:@ ">>=" y:(@) { wrap_assignment(Some(BinaryOp::RightShift), x, y) }
          x:@ "&=" y:(@) { wrap_assignment(Some(BinaryOp::BitwiseAnd), x, y) }
          x:@ "|=" y:(@) { wrap_assignment(Some(BinaryOp::BitwiseOr), x, y) }
          x:@ "^=" y:(@) { wrap_assignment(Some(BinaryOp::BitwiseXor), x, y) }
          --
          x:@ "?" y:expr() ":" z:(@) { wrap_ternary(x, y, z) }
          --
          x:(@) "||" y:@ { wrap_binary(x, BinaryOp::LogicalOr, y) }
          --
          x:(@) "&&" y:@ { wrap_binary(x, BinaryOp::LogicalAnd, y) }
          --
          x:(@) "|" y:@ { wrap_binary(x, BinaryOp::BitwiseOr, y) }
          --
          x:(@) "^" y:@ { wrap_binary(x, BinaryOp::BitwiseXor, y) }
          --
          x:(@) "&" y:@ { wrap_binary(x, BinaryOp::BitwiseAnd, y) }
          --
          x:(@) "==" y:@ { wrap_comparision(x, ComparisionOp::Eq, y) }
          x:(@) "!=" y:@ { wrap_comparision(x, ComparisionOp::NEq, y) }
          --
          x:(@) "<=" y:@ { wrap_comparision(x, ComparisionOp::LTE, y) }
          x:(@) ">=" y:@ { wrap_comparision(x, ComparisionOp::GTE, y) }
          x:(@) "<" y:@ { wrap_comparision(x, ComparisionOp::LT, y) }
          x:(@) ">" y:@ { wrap_comparision(x, ComparisionOp::LT, y) }
          --
          x:(@) "<<" y:@ { wrap_binary(x, BinaryOp::LeftShift, y) }
          x:(@) ">>" y:@ { wrap_binary(x, BinaryOp::RightShift, y) }
          --
          x:(@) "+" y:@ { wrap_binary(x, BinaryOp::Addition, y) }
          x:(@) "-" y:@ { wrap_binary(x, BinaryOp::Subtraction, y) }
          --
          x:(@) "*" y:@ { wrap_binary(x, BinaryOp::Multiplication, y) }
          x:(@) "/" y:@ { wrap_binary(x, BinaryOp::Division, y) }
          x:(@) "%" y:@ { wrap_binary(x, BinaryOp::Remainder, y) }
          --
          "(" ty:type_name() ")" x:(@) { wrap_unary(UnaryOp::Cast(ty), x) }
          --
          "&" x:(@) { wrap_unary(UnaryOp::Address, x) }
          "*" x:(@) { wrap_unary(UnaryOp::Indirection, x) }
          "+" x:(@) { wrap_unary(UnaryOp::Positive, x) }
          "-" x:(@) { wrap_unary(UnaryOp::Negative, x) }
          "~" x:(@) { wrap_unary(UnaryOp::BitwiseNegation, x) }
          "!" x:(@) { wrap_unary(UnaryOp::LogicalNegation, x) }
          "++" x:(@) { wrap_unary(UnaryOp::Increment(FixOrder::Prefix), x) }
          "--" x:(@) { wrap_unary(UnaryOp::Decrement(FixOrder::Prefix), x) }
          --
          x:(@) "." y:identifier() { wrap_member(x, MemberAccess::Direct, y) }
          x:(@) "->" y:identifier() { wrap_member(x, MemberAccess::Pointer, y) }
          x:(@) "[" y:expr() "]" { wrap_array_access(x, y) }
          x:(@) "(" y:(expr() ** ",") ","? ")" { wrap_fn_call(x, y) }
          x:(@) "++" { wrap_unary(UnaryOp::Increment(FixOrder::Postfix), x) }
          x:(@) "--" { wrap_unary(UnaryOp::Decrement(FixOrder::Postfix), x) }
          --
          i:identifier() { Expression::Identifier(i) }
          l:literal() { Expression::Literal(l) }
          c:constant() { Expression::Constant(c) }
          "(" e:expr() ")" { e }
        }

        // C vk.xml exts
        rule type_tag() -> &'a str
          = quiet!{[VkXMLToken::TextTag { name: "type", text: UnescapedStr(Cow::Borrowed(text)) }] { text }}
          / expected!("<type>...</type>")

        // handle pointer info, can be '*' or '**' or '* const*'
        rule pointer_kind() -> PointerKind
            = "*" inner:(c:"const"? "*" { c.is_some() })? {
                inner.map_or(PointerKind::Single, |inner_is_const| PointerKind::Double { inner_is_const })
            }

        rule typed_tag(name_text: rule<&'a str>) -> FieldLike<'a>
            = is_const:"const"? is_struct:"struct"? type_name:type_tag() pointer_kind:pointer_kind()? name:name_text() {
                FieldLike {
                    pointer_kind,
                    is_const: is_const.is_some(),
                    ..FieldLike::default_new(name, TypeSpecifier::from_plain(type_name, is_struct.is_some()))
                }
            }

        rule name_tag() -> &'a str
         = quiet!{[VkXMLToken::TextTag { name: "name", text: UnescapedStr(Cow::Borrowed(text)) }] { text }}
         / expected!("<name>...</name>")

        rule enum_tag() -> &'a str
         = quiet!{[VkXMLToken::TextTag { name: "enum", text: UnescapedStr(Cow::Borrowed(text)) }] { text }}
         / expected!("<enum>...</enum>")

        rule comment_tag() -> UnescapedStr<'a>
         = quiet!{[VkXMLToken::TextTag { name: "comment", text }] { text.clone() }}
         / expected!("<comment>...</comment>")

        rule integer() -> u64
         = quiet!{[VkXMLToken::C(Token::Constant(Constant::Integer(v)))] { *v }}
         / expected!("Integral Constant")

        /// parses the content of <member> ... </member> or <proto> ... </proto> or <param> ... </param>
        pub rule field_like() -> FieldLike<'a>
            = typed:typed_tag(<name_tag()>) sizing:(
                ":" bitfield_size:(v:integer() { NonZeroU8::new(v.try_into().unwrap()).unwrap() }) { FieldLikeSizing::BitfieldSize(bitfield_size) }
                / array_shape:("[" n:(
                    v:integer() { ArrayLength::Static(NonZeroU32::new(v.try_into().unwrap()).unwrap()) }
                    / name:enum_tag() { ArrayLength::Constant(name) }
                ) "]" { n })+ { FieldLikeSizing::ArrayShape(array_shape) }
            )? comment:comment_tag()? {
                FieldLike {
                    sizing,
                    comment: comment.map(UnescapedStr::from),
                    ..typed
                }
            }

        /// <type category="basetype"> ... </type>
        pub rule type_basetype() -> BaseTypeType<'a>
            = "struct" name:name_tag() ";" { BaseTypeType::Forward(name) }
            / "typedef" typed:typed_tag(<name_tag()>) ";" { BaseTypeType::TypeDef(typed) }
            // Workaround because it should be `typedef struct <type>__IOSurface</type>* <name>IOSurfaceRef</name>;` not `typedef struct __IOSurface* <name>IOSurfaceRef</name>;`
            / quiet!{"typedef" "struct" [VkXMLToken::C(Token::Identifier("__IOSurface"))] "*" name:name_tag() ";" { BaseTypeType::TypeDef(FieldLike {
                pointer_kind: Some(PointerKind::Single),
                is_const: false,
                ..FieldLike::default_new(name, TypeSpecifier::Struct("__IOSurface"))
            }) }}
            / pre:(([VkXMLToken::C(c)] {c.clone()})+) name:name_tag() post:(([VkXMLToken::C(c)] {c.clone()})+) { BaseTypeType::DefineGuarded { pre, name, post } }


        rule define_macro()
         = quiet!{"#" [VkXMLToken::C(Token::Identifier("define"))]}
         / expected!("#define")

        /// <type category="define"> ... </type>
        pub rule type_define() -> MacroDefine<'a>
            = dc:quiet!{([VkXMLToken::C(Token::_DeprecationComment(c))] {c})?} "\n"* is_disabled:(define_macro() {false} / "//#define" {true}) name:name_tag() value:(
                "(" params:(identifier() ** ",") ")" expression:(([VkXMLToken::C(c)] {c.clone()})+) { MacroDefineValue::FunctionDefine { params, expression } }
                /  e:expr() { MacroDefineValue::Expression(e) }
                / macro_name:type_tag() "(" args:(expr() ** ",") ","? ")" { MacroDefineValue::MacroFunctionCall { name: macro_name, args } }
            ) { MacroDefine {
                name,
                comment: None,
                requires: None,
                api: None,
                deprecated: None,
                deprecation_comment: dc.copied(),
                is_disabled,
                value,
            } }

        rule vkapi_ptr_macro()
            = quiet!{[VkXMLToken::C(Token::Identifier("VKAPI_PTR"))]}
            / expected!("VKAPI_PTR")

        /// <type category="funcptr"> ... </type>
        pub rule type_funcptr() -> FnPtrType<'a>
          = "typedef" return_type_name:type_specifier() return_type_pointer_kind:pointer_kind()? "(" vkapi_ptr_macro() "*" name:name_tag() ")" "(" params:(
            "void" { None }
            / params:(typed_tag(<identifier()>) ** ",") { Some(params) }
          ) ")" ";" { FnPtrType { name, return_type_name, return_type_pointer_kind, params, requires: None } }

        rule dispatchable_handle_kind() -> HandleKind
          = quiet!{[VkXMLToken::TextTag { name: "type", text: UnescapedStr(Cow::Borrowed("VK_DEFINE_HANDLE")) }] { HandleKind::Dispatch }}
          / expected!("<type>VK_DEFINE_HANDLE</type>")

        rule non_dispatchable_handle_kind() -> HandleKind
          = quiet!{[VkXMLToken::TextTag { name: "type", text: UnescapedStr(Cow::Borrowed("VK_DEFINE_NON_DISPATCHABLE_HANDLE")) }] { HandleKind::NoDispatch }}
          / expected!("<type>VK_DEFINE_NON_DISPATCHABLE_HANDLE</type>")

        rule handle_kind() -> HandleKind = dispatchable_handle_kind() / non_dispatchable_handle_kind()

        /// <type category="handle">
        pub rule type_handle() -> HandleType<'a>
          = handle_kind:handle_kind() "(" name:name_tag() ")" { HandleType { name, handle_kind, obj_type_enum: "", parent: None } }

        rule bitmask_flags() -> bool
            = quiet!{[VkXMLToken::TextTag { name: "type", text: UnescapedStr(Cow::Borrowed("VkFlags")) }] { false }}
            / expected!("<type>VkFlags</type>")

        rule bitmask64_flags() -> bool
            = quiet!{[VkXMLToken::TextTag { name: "type", text: UnescapedStr(Cow::Borrowed("VkFlags64")) }] { true }}
            / expected!("<type>VkFlags64</type>")

        /// <type category="bitmask">
        pub rule type_bitmask() -> BitmaskType<'a>
          = "typedef" is_64bits:(bitmask_flags() / bitmask64_flags()) name:name_tag() ";" { BitmaskType {name, is_64bits, has_bitvalues: false, api: None } }
    }
}

#[derive(Debug, Clone)]
pub enum TextError {
    Lexer(crate::lexer::Error),
    Parser(ParseError),
}

impl fmt::Display for TextError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TextError::Lexer(_) => write!(f, "Lexer error"),
            TextError::Parser(_) => write!(f, "Parser error"),
        }
    }
}

impl std::error::Error for TextError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            TextError::Lexer(e) => Some(e),
            TextError::Parser(e) => Some(e),
        }
    }
}

impl<'a, 'de: 'a> TryFrom<&'de str> for Expression<'a> {
    type Error = TextError;

    fn try_from(value: &'de str) -> Result<Self, Self::Error> {
        let c_toks = tokenize(value, false, false)
            .collect::<Result<_, _>>()
            .map_err(TextError::Lexer)?;
        c_with_vk_ext::expr(&c_toks).map_err(TextError::Parser)
    }
}

impl<'s, 'a: 's> TryFrom<&'s [Token<'a>]> for Expression<'a> {
    type Error = peg::error::ParseError<usize>;

    fn try_from(value: &'s [Token<'a>]) -> Result<Self, Self::Error> {
        let c_toks = value.iter().cloned().collect();
        c_with_vk_ext::expr(&c_toks)
    }
}

pub trait IntoVkXMLTokens<'t>: Sized {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>);
    fn to_tokens_vector(self) -> Vec<VkXMLToken<'t>> {
        let mut tokens = Vec::with_capacity(32);
        self.to_tokens(&mut tokens);
        tokens
    }
}

impl<'t, T: IntoVkXMLTokens<'t>, I: IntoIterator<Item = T>> IntoVkXMLTokens<'t> for I {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        self.into_iter().map(|v| v.to_tokens(tokens)).collect()
    }
}

impl<'t, 'a: 't> IntoVkXMLTokens<'t> for Constant {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        tokens.push(VkXMLToken::C(self.into()));
    }
}

impl<'t, 'a: 't> IntoVkXMLTokens<'t> for Token<'a> {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        tokens.push(VkXMLToken::from(self));
    }
}

impl<'t, 'a: 't> IntoVkXMLTokens<'t> for &'_ Token<'a> {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        tokens.push(VkXMLToken::from(self.clone()));
    }
}

impl<'t, 'a: 't> IntoVkXMLTokens<'t> for &'_ Expression<'a> {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        self.yield_tokens::<core::convert::Infallible, _>( move |token| {
            tokens.push(VkXMLToken::from(token));
            Ok(())
        }, false).unwrap()
    }
}

macro_rules! into_vkxml_tokens {
    (@ $tokens:ident) => { () };
    (@ $tokens:ident $l:literal $(, $($tail:tt)*)?) => {
        {
            $tokens.push(VkXMLToken::C(Token::from_literal($l).unwrap()));
            $(into_vkxml_tokens!(@ $tokens $($tail)*))?
        }
    };
    (@ $tokens:ident $l:literal if $e:expr $(, $($tail:tt)*)?) => {
        {
            if $e {
                $tokens.push(VkXMLToken::C(Token::from_literal($l).unwrap()));
            }
            $(into_vkxml_tokens!(@ $tokens $($tail)*))?
        }
    };
    (@ $tokens:ident match $m:expr => { $($p:pat => [$($pt:tt)*]),+ $(,)? } $(, $($tail:tt)*)?) => {
        {
            match $m {
                $(
                    $p => into_vkxml_tokens!(@ $tokens $($pt)* )
                ),+
            }
            $(into_vkxml_tokens!(@ $tokens $($tail)*))?
        }
    };
    (@ $tokens:ident $tag:ident = $e:expr $(, $($tail:tt)*)?) => {
        {
            $tokens.push(VkXMLToken::TextTag {
                name: stringify!($tag),
                text: $e.clone().into(),
            });
            $(into_vkxml_tokens!(@ $tokens $($tail)*))?
        }
    };
    (@ $tokens:ident [$tks:expr] # $sep:literal $(, $($tail:tt)*)?) => {
        {
            {
                let mut is_first = true;
                for v in $tks {
                    if is_first {
                        is_first = false;
                    } else {
                        $tokens.push(VkXMLToken::C(Token::from_literal($sep).unwrap()));
                    }
                    v.to_tokens($tokens);
                }
            }
            $(into_vkxml_tokens!(@ $tokens $($tail)*))?
        }
    };
    (@ $tokens:ident $t:expr $(, $($tail:tt)*)?) => {
        {
            $t.to_tokens($tokens);
            $(into_vkxml_tokens!(@ $tokens $($tail)*))?
        }
    };
}

impl<'t> IntoVkXMLTokens<'t> for &'_ PointerKind {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        into_vkxml_tokens!(@tokens match self => {
            PointerKind::Single => ["*"],
            PointerKind::Double { inner_is_const } => ["*", "const" if *inner_is_const, "*"]
        });
    }
}

struct TypedTag<'s, 'a: 's, const NAME_IS_TAG: bool>(&'s FieldLike<'a>);

impl<'t, 's, 'a: 't + 's, const NAME_IS_TAG: bool> IntoVkXMLTokens<'t>
    for TypedTag<'s, 'a, NAME_IS_TAG>
{
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        let field = self.0;
        into_vkxml_tokens!(@tokens
            "const" if field.is_const,
            "struct" if matches!(field.type_name, TypeSpecifier::Struct(_)),
            type = field.type_name.as_identifier(),
            field.pointer_kind.as_ref(),
            match NAME_IS_TAG => {
                true => [name = field.name],
                false => [Token::Identifier(field.name)]
            }
        );
    }
}

impl<'a, 'xml: 'a> TryFromTokens<'xml> for FieldLike<'a> {
    const PARSING_MACROS: bool = false;
    const OBJC_COMPAT: bool = false;

    fn try_from_tokens(tokens: &VkXMLTokens<'xml>) -> Result<Self, ParseError> {
        c_with_vk_ext::field_like(tokens)
    }
}

impl<'t, 'a: 't> IntoVkXMLTokens<'t> for &'_ ArrayLength<'a> {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        into_vkxml_tokens!(@tokens
            "[",
            match self => {
                ArrayLength::Constant(c) => [enum = *c],
                ArrayLength::Static(n) => [Constant::Integer(n.get().into())],
            },
            "]"
        );
    }
}

impl<'t, 'a: 't> IntoVkXMLTokens<'t> for &'_ FieldLike<'a> {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        into_vkxml_tokens!(@tokens
            TypedTag::<true>(self),
            match &self.sizing => {
                Some(FieldLikeSizing::BitfieldSize(bitfield_size)) => [":", Constant::Integer(bitfield_size.get().into())],
                Some(FieldLikeSizing::ArrayShape(array_shape)) => [array_shape],
                None => [],
            },
            match self.comment.clone() => {
                Some(comment) => [comment = comment],
                None => [],
            }
        );
    }
}

impl<'a, 'xml: 'a> TryFromTokens<'xml> for BaseTypeType<'a> {
    const PARSING_MACROS: bool = true;
    const OBJC_COMPAT: bool = true;

    fn try_from_tokens(tokens: &VkXMLTokens<'xml>) -> Result<Self, ParseError> {
        c_with_vk_ext::type_basetype(tokens)
    }
}

impl<'t, 'a: 't> IntoVkXMLTokens<'t> for &'_ BaseTypeType<'a> {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        into_vkxml_tokens!(@tokens match self => {
            BaseTypeType::Forward(name) => ["struct", name = *name, ";"],
            BaseTypeType::TypeDef(typedef) => ["typedef", TypedTag::<true>(typedef), ";"],
            BaseTypeType::DefineGuarded { pre, name, post } => [pre, name = *name, post]
        });
    }
}

impl<'a, 'xml: 'a> TryFromTokens<'xml> for MacroDefine<'a> {
    const PARSING_MACROS: bool = true;
    const OBJC_COMPAT: bool = false;

    fn try_from_tokens(tokens: &VkXMLTokens<'xml>) -> Result<Self, ParseError> {
        c_with_vk_ext::type_define(tokens)
    }
}

impl<'t, 'a: 't> IntoVkXMLTokens<'t> for &'_ MacroDefine<'a> {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        into_vkxml_tokens!(@tokens
            match self.deprecation_comment => {
                Some(comment) => [Token::_DeprecationComment(comment), "\n"],
                None => [],
            },
            match self.is_disabled => {
                true => [Token::_MalformedDefine],
                false => ["#", Token::Identifier("define")],
            },
            name = self.name,
            match &self.value => {
                MacroDefineValue::Expression(expr) => [expr],
                MacroDefineValue::FunctionDefine { params, expression } => ["(", [params.iter().copied().map(Token::Identifier)] # ",", ")", " ", expression],
                MacroDefineValue::MacroFunctionCall {
                    name: fn_name,
                    args,
                } => [" ", type = *fn_name, "(", [args] # ",", ")"]
            }
        );
    }
}

impl<'a, 'xml: 'a> TryFromTokens<'xml> for FnPtrType<'a> {
    const PARSING_MACROS: bool = false;
    const OBJC_COMPAT: bool = false;

    fn try_from_tokens(tokens: &VkXMLTokens<'xml>) -> Result<Self, ParseError> {
        c_with_vk_ext::type_funcptr(tokens)
    }
}

impl<'t, 'a: 't> IntoVkXMLTokens<'t> for &'_ FnPtrType<'a> {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        into_vkxml_tokens!(@tokens
            "typedef",
            Token::Identifier(self.return_type_name.as_identifier()),
            self.return_type_pointer_kind.as_ref(),
            "(",
            Token::Identifier("VKAPI_PTR"),
            "*",
            name = self.name,
            ")",
            "(",
            match self.params.as_deref() => {
                Some(params) => [[params.iter().map(TypedTag::<false>)] # ","],
                None => ["void"]
            },
            ")",
            ";"
        );
    }
}

impl<'a, 'xml: 'a> TryFromTokens<'xml> for HandleType<'a> {
    const PARSING_MACROS: bool = false;
    const OBJC_COMPAT: bool = false;

    fn try_from_tokens(tokens: &VkXMLTokens<'xml>) -> Result<Self, ParseError> {
        c_with_vk_ext::type_handle(tokens)
    }
}

impl<'t, 'a: 't> IntoVkXMLTokens<'t> for &'_ HandleType<'a> {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        into_vkxml_tokens!(@tokens type = <&'static str>::from(self.handle_kind), "(", name = self.name, ")");
    }
}

impl<'a, 'xml: 'a> TryFromTokens<'xml> for BitmaskType<'a> {
    const PARSING_MACROS: bool = false;
    const OBJC_COMPAT: bool = false;

    fn try_from_tokens(tokens: &VkXMLTokens<'xml>) -> Result<Self, ParseError> {
        c_with_vk_ext::type_bitmask(tokens)
    }
}

impl<'t, 'a: 't> IntoVkXMLTokens<'t> for &'_ BitmaskType<'a> {
    fn to_tokens(self, tokens: &mut Vec<VkXMLToken<'t>>) {
        let ty_name = if self.is_64bits {
            "VkFlags64"
        } else {
            "VkFlags"
        };
        into_vkxml_tokens!(@tokens "typedef", type = ty_name, name = self.name, ";");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vk_version_patch() {
        const S: &str = "((uint32_t)(version) & 0xFFFU)";
        let e = Expression::try_from(S).unwrap();
        assert_eq!(
            e,
            Expression::Binary(
                BinaryOp::BitwiseAnd,
                Box::new(Expression::Unary(
                    UnaryOp::Cast(TypeName::Specifier(TypeSpecifier::TypedefName(
                        "uint32_t".into()
                    ))),
                    Box::new(Expression::Identifier("version".into()))
                )),
                Box::new(Expression::Constant(Constant::Integer(0xFFF)))
            )
        );
    }
}
