use std::{
    borrow::Cow,
    fmt,
    num::{NonZeroU32, NonZeroU8},
    ops::Deref,
};

use crate::{
    lexer::tokenize, ArrayLength, BaseTypeType, Constant, ErrorKind, FieldLike, FieldLikeSizing,
    FnPtrType, MacroDefine, MacroDefineValue, ParseResult, PointerKind, Token,
};
// Using the C grammer from https://web.archive.org/web/20181230041359if_/http://www.open-std.org/jtc1/sc22/wg14/www/abq/c17_updated_proposed_fdis.pdf
// the link is from https://rust-lang.github.io/unsafe-code-guidelines/layout/structs-and-tuples.html#c-compatible-layout-repr-c

// C Type Decleration types
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]

pub enum TypeQualifer {
    Const,
    Restrict,
    Volatile,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]

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
    pub fn as_identifier(&self) -> &str {
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
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]

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
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum FixOrder {
    Prefix,
    Postfix,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]

pub enum ComparisionOp {
    LT,
    LTE,
    Eq,
    NEq,
    GTE,
    GT,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]

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
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
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
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum MemberAccess {
    /// '.'
    Direct,
    /// '->'
    Pointer,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
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

// fn wrap_comma<'a>(head: Expression<'a>, tail: Expression<'a>) -> Expression<'a> {
//     Expression::Comma(Box::new(head), Box::new(tail))
// }

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
        let mut tokens = Vec::new();
        self.to_tokens(&mut tokens, false);
        for token in tokens {
            write!(f, "{token}")?;
        }
        Ok(())
    }
}

impl<'a> Expression<'a> {
    pub fn to_tokens<'s>(&'s self, tokens: &mut Vec<Token<'s>>, is_inner: bool) {
        match self {
            Expression::Identifier(id) => tokens.push(Token::Identifier(id)),
            Expression::Constant(c) => tokens.push(Token::Constant(*c)),
            Expression::Literal(lit) => tokens.push(Token::Literal(lit)),
            Expression::SizeOf(_) => todo!(),
            Expression::Unary(UnaryOp::Increment(FixOrder::Postfix), e) => {
                e.to_tokens(tokens, is_inner);
                tokens.push(Token::Increment);
            }
            Expression::Unary(UnaryOp::Decrement(FixOrder::Postfix), e) => {
                e.to_tokens(tokens, is_inner);
                tokens.push(Token::Decrement);
            }
            Expression::Unary(UnaryOp::Cast(ty), e) => todo!("({:?}){:?}", ty, e),
            Expression::Unary(op, e) => {
                tokens.push(op.clone().into());
                e.to_tokens(tokens, is_inner);
            }
            Expression::Binary(op, l, r) => {
                if is_inner {
                    tokens.push(Token::LParen);
                }
                l.to_tokens(tokens, true);
                tokens.push(op.clone().into());
                r.to_tokens(tokens, true);
                if is_inner {
                    tokens.push(Token::RParen);
                }
            }
            Expression::Comparision(op, l, r) => {
                if is_inner {
                    tokens.push(Token::LParen);
                }
                l.to_tokens(tokens, true);
                tokens.push(op.clone().into());
                r.to_tokens(tokens, true);
                if is_inner {
                    tokens.push(Token::RParen);
                }
            }
            Expression::Assignment(_, _, _) => todo!(),
            Expression::TernaryIfElse(cond, et, ef) => {
                if is_inner {
                    tokens.push(Token::LParen);
                }
                cond.to_tokens(tokens, true);
                tokens.push(Token::Question);
                et.to_tokens(tokens, true);
                tokens.push(Token::Colon);
                ef.to_tokens(tokens, true);
                if is_inner {
                    tokens.push(Token::RParen);
                }
            }
            Expression::FunctionCall(func, args) => {
                func.to_tokens(tokens, true);
                tokens.push(Token::LParen);
                let mut is_first = true;
                for arg in args.iter() {
                    if is_first {
                        is_first = false;
                    } else {
                        tokens.push(Token::Comma);
                    }
                    arg.to_tokens(tokens, false);
                }
                tokens.push(Token::RParen);
            }
            Expression::Comma(head, tail) => {
                head.to_tokens(tokens, true);
                tokens.push(Token::Comma);
                tail.to_tokens(tokens, true);
            }
            Expression::Member(access, v, member) => {
                v.to_tokens(tokens, true);
                tokens.push(access.clone().into());
                tokens.push(Token::Identifier(member));
            }
            Expression::ArrayElement(e, i) => {
                e.to_tokens(tokens, true);
                tokens.push(Token::LBrack);
                i.to_tokens(tokens, true);
                tokens.push(Token::RBrack);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VkXMLToken<'a> {
    C(Token<'a>),
    TextTag { name: &'a str, text: &'a str },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VkXMLTokens<'s, 'a>(pub Cow<'s, [VkXMLToken<'a>]>);

impl<'s, 'a> Deref for VkXMLTokens<'s, 'a> {
    type Target = [VkXMLToken<'a>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'s, 'a: 's> FromIterator<VkXMLToken<'a>> for VkXMLTokens<'s, 'a> {
    fn from_iter<T: IntoIterator<Item = VkXMLToken<'a>>>(iter: T) -> Self {
        VkXMLTokens(Cow::Owned(iter.into_iter().collect()))
    }
}

impl<'s, 'a: 's> FromIterator<Token<'a>> for VkXMLTokens<'s, 'a> {
    fn from_iter<T: IntoIterator<Item = Token<'a>>>(iter: T) -> Self {
        VkXMLTokens(Cow::Owned(iter.into_iter().map(VkXMLToken::C).collect()))
    }
}

fn vk_tokenize<'s, 'a: 's, 'input>(
    node: roxmltree::Node<'a, 'input>,
    parsing_macros: bool,
    objc_compat: bool,
) -> ParseResult<VkXMLTokens<'s, 'a>> {
    node.children()
        .filter(|n| n.is_element() || n.is_text())
        .flat_map(|n| {
            // Empty elements are disallowed in vulkan's mixed pseudo-c/xml, except in <comment>
            let text = n.text().unwrap_or("");
            if n.is_element() {
                vec![Ok(VkXMLToken::TextTag {
                    name: (n.tag_name().name()),
                    text: (text),
                })]
            } else {
                tokenize(text, parsing_macros, objc_compat)
                    .into_iter()
                    .map(|r| {
                        r.map(VkXMLToken::C)
                            .map_err(|e| ErrorKind::LexerError(e, n.id()))
                    })
                    .collect()
            }
        })
        .collect()
}

impl<'s, 'a> peg::Parse for VkXMLTokens<'s, 'a> {
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

pub type ParseError =
    peg::error::ParseError<<VkXMLTokens<'static, 'static> as peg::Parse>::PositionRepr>;

pub trait TryFromTokens<'s, 'a: 's>: 's + Sized {
    const PARSING_MACROS: bool;
    const OBJC_COMPAT: bool;

    fn try_from_tokens(tokens: &VkXMLTokens<'s, 'a>) -> Result<Self, ParseError>;
    fn try_from_node(node: roxmltree::Node<'a, 's>) -> Result<Self, crate::ErrorKind> {
        let tokens = vk_tokenize(node, Self::PARSING_MACROS, Self::OBJC_COMPAT)?;
        Self::try_from_tokens(&tokens).map_err(|e| crate::ErrorKind::PegParsingError(e, node.id()))
    }
}

impl<'input: 's, 's, 'a> peg::ParseElem<'input> for VkXMLTokens<'s, 'a> {
    type Element = &'s VkXMLToken<'a>;

    fn parse_elem(&'input self, pos: usize) -> peg::RuleResult<Self::Element> {
        match self[pos..].first() {
            Some(c) => peg::RuleResult::Matched(pos + 1, c),
            None => peg::RuleResult::Failed,
        }
    }
}

impl<'s, 'a> peg::ParseLiteral for VkXMLTokens<'s, 'a> {
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

impl<'input, 's, 'a: 'input> peg::ParseSlice<'input> for VkXMLTokens<'s, 'a> {
    type Slice = &'input [VkXMLToken<'a>];
    fn parse_slice(&'input self, p1: usize, p2: usize) -> Self::Slice {
        &self[p1..p2]
    }
}

/// C is not a context-free languge and requires building a symbol table of typedefs at parse time to resolve ambigutites with cast expressions
pub(crate) fn is_typedef_name(name: &str) -> bool {
    // FIXME: actually build a symbol table as we go that is already filled with the standard typedefs (size_t, uint32_t, ...),
    name.ends_with("_t") || name.starts_with("Vk")
}

peg::parser! {
    pub grammar c_with_vk_ext<'s, 'a>() for VkXMLTokens<'s, 'a> {
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
          = quiet!{[VkXMLToken::TextTag { name, text } if *name == "type"] { text }}
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
         = quiet!{[VkXMLToken::TextTag { name, text } if *name == "name"] { text }}
         / expected!("<name>...</name>")

        rule enum_tag() -> &'a str
         = quiet!{[VkXMLToken::TextTag { name, text } if *name == "enum"] { text }}
         / expected!("<enum>...</enum>")

        rule comment_tag() -> &'a str
         = quiet!{[VkXMLToken::TextTag { name, text } if *name == "comment"] { text }}
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
                    comment,
                    ..typed
                }
            }

        /// <type category="basetype"> ... </type>
        pub rule type_basetype() -> BaseTypeType<'a>
            = "struct" name:name_tag() ";" { BaseTypeType::Forward(name) }
            / "typedef" typed:typed_tag(<name_tag()>) ";" { BaseTypeType::TypeDef(typed) }
            // Workaround because it should be `typedef struct <type>__IOSurface</type>* <name>IOSurfaceRef</name>;` not `typedef struct __IOSurface* <name>IOSurfaceRef</name>;`
            / quiet!{"typedef" "struct" [VkXMLToken::C(Token::Identifier(i)) if *i == "__IOSurface"] "*" name:name_tag() ";" { BaseTypeType::TypeDef(FieldLike {
                pointer_kind: Some(PointerKind::Single),
                is_const: false,
                ..FieldLike::default_new(name, TypeSpecifier::Struct("__IOSurface"))
            }) }}
            / pre:(([VkXMLToken::C(c)] {c.clone()})+) name:name_tag() post:(([VkXMLToken::C(c)] {c.clone()})+) { BaseTypeType::DefineGuarded { pre, name, post } }


        rule define_macro()
         = quiet!{"#" [VkXMLToken::C(Token::Identifier(i)) if *i == "define"]}
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
                deprecation_comment: dc.copied(),
                is_disabled,
                value,
            } }

        rule vkapi_ptr_macro()
            = quiet!{[VkXMLToken::C(Token::Identifier(id)) if *id == "VKAPI_PTR"]}
            / expected!("VKAPI_PTR")

        /// <type category="funcptr"> ... </type>
        pub rule type_funcptr() -> FnPtrType<'a>
          = "typedef" ty_name:type_specifier() ptr:"*"? "(" vkapi_ptr_macro() "*" name:name_tag() ")" "(" params:(
            "void" ")" ";" { None }
            / params:(typed_tag(<identifier()>) ** ",") ")" ";" { Some(params) }
          ) { FnPtrType { name, return_type_name: ty_name, return_type_pointer_kind: ptr.map(|()| PointerKind::Single), params, requires: None } }
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

impl<'a> TryFrom<&'a str> for Expression<'a> {
    type Error = TextError;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        let c_toks = tokenize(value, false, false)
            .into_iter()
            .collect::<Result<_, _>>()
            .map_err(TextError::Lexer)?;
        c_with_vk_ext::expr(&c_toks).map_err(TextError::Parser)
    }
}

impl<'s, 'a: 's> TryFrom<&'s [Token<'a>]> for Expression<'a> {
    type Error = peg::error::ParseError<usize>;

    fn try_from(value: &'s [Token<'a>]) -> Result<Self, Self::Error> {
        let c_toks = VkXMLTokens(value.iter().cloned().map(VkXMLToken::C).collect());
        c_with_vk_ext::expr(&c_toks)
    }
}

pub trait IntoVkXMLTokens<'t> {
    fn to_tokens(&'t self, tokens: &mut Vec<VkXMLToken<'t>>);
    fn to_tokens_vector(&'t self) -> Vec<VkXMLToken<'t>> {
        let mut tokens = Vec::with_capacity(32);
        self.to_tokens(&mut tokens);
        tokens
    }
}

fn pointer_kind_tokens<'s>(pointer_kind: &Option<PointerKind>, tokens: &mut Vec<VkXMLToken<'s>>) {
    match pointer_kind {
        Some(PointerKind::Single) => tokens.push(VkXMLToken::C(Token::MulStar)),
        Some(PointerKind::Double {
            inner_is_const: true,
        }) => {
            tokens.push(VkXMLToken::C(Token::MulStar));
            tokens.push(VkXMLToken::C(Token::Const));
            tokens.push(VkXMLToken::C(Token::MulStar));
        }
        Some(PointerKind::Double {
            inner_is_const: false,
        }) => {
            tokens.push(VkXMLToken::C(Token::MulStar));
            tokens.push(VkXMLToken::C(Token::MulStar));
        }
        None => {}
    };
}

fn typed_tag_tokens<'s, 'a: 's>(
    field: &'s FieldLike<'a>,
    name_is_tag: bool,
    tokens: &mut Vec<VkXMLToken<'s>>,
) {
    if field.is_const {
        tokens.push(VkXMLToken::C(Token::Const));
    }
    if matches!(field.type_name, TypeSpecifier::Struct(_)) {
        tokens.push(VkXMLToken::C(Token::Struct));
    }
    tokens.push(VkXMLToken::TextTag {
        name: ("type"),
        text: (field.type_name.as_identifier()),
    });
    pointer_kind_tokens(&field.pointer_kind, tokens);
    if name_is_tag {
        tokens.push(VkXMLToken::TextTag {
            name: "name",
            text: field.name,
        });
    } else {
        tokens.push(VkXMLToken::C(Token::Identifier(field.name)));
    }
}

impl<'s, 'a: 's> TryFromTokens<'s, 'a> for FieldLike<'a> {
    const PARSING_MACROS: bool = false;
    const OBJC_COMPAT: bool = false;

    fn try_from_tokens(tokens: &VkXMLTokens<'s, 'a>) -> Result<Self, ParseError> {
        c_with_vk_ext::field_like(tokens)
    }
}

impl<'s, 'a: 's> IntoVkXMLTokens<'s> for &'s FieldLike<'a> {
    fn to_tokens(&'s self, tokens: &mut Vec<VkXMLToken<'s>>) {
        typed_tag_tokens(self, true, tokens);
        match &self.sizing {
            Some(FieldLikeSizing::BitfieldSize(bitfield_size)) => {
                tokens.push(VkXMLToken::C(Token::Colon));
                tokens.push(VkXMLToken::C(Token::Constant(Constant::Integer(
                    bitfield_size.get().into(),
                ))));
            }
            Some(FieldLikeSizing::ArrayShape(array_shape)) => {
                for shape in array_shape.iter() {
                    tokens.push(VkXMLToken::C(Token::LBrack));

                    match shape {
                        ArrayLength::Static(n) => tokens.push(VkXMLToken::C(Token::Constant(
                            Constant::Integer(n.get().into()),
                        ))),
                        ArrayLength::Constant(c) => {
                            tokens.push(VkXMLToken::TextTag {
                                name: ("enum"),
                                text: c,
                            });
                        }
                    }
                    tokens.push(VkXMLToken::C(Token::RBrack));
                }
            }
            None => {}
        }
        if let Some(comment) = self.comment.as_ref() {
            tokens.push(VkXMLToken::TextTag {
                name: ("comment"),
                text: comment,
            });
        }
    }
}

impl<'s, 'a: 's> TryFromTokens<'s, 'a> for BaseTypeType<'a> {
    const PARSING_MACROS: bool = true;
    const OBJC_COMPAT: bool = true;

    fn try_from_tokens(tokens: &VkXMLTokens<'s, 'a>) -> Result<Self, ParseError> {
        c_with_vk_ext::type_basetype(tokens)
    }
}

impl<'s, 'a: 's> IntoVkXMLTokens<'s> for &'s BaseTypeType<'a> {
    fn to_tokens(&'s self, tokens: &mut Vec<VkXMLToken<'s>>) {
        match self {
            BaseTypeType::Forward(name) => tokens.extend([
                VkXMLToken::C(Token::Struct),
                VkXMLToken::TextTag {
                    name: "name",
                    text: name,
                },
                VkXMLToken::C(Token::SemiColon),
            ]),
            BaseTypeType::TypeDef(typedef) => {
                tokens.push(VkXMLToken::C(Token::TypeDef));
                typed_tag_tokens(typedef, true, tokens);
                tokens.push(VkXMLToken::C(Token::SemiColon));
            }
            BaseTypeType::DefineGuarded { pre, name, post } => {
                tokens.extend(pre.iter().cloned().map(VkXMLToken::C));
                tokens.push(VkXMLToken::TextTag {
                    name: "name",
                    text: name,
                });
                tokens.extend(post.iter().cloned().map(VkXMLToken::C));
            }
        }
    }
}

impl<'s, 'a: 's> TryFromTokens<'s, 'a> for MacroDefine<'a> {
    const PARSING_MACROS: bool = true;
    const OBJC_COMPAT: bool = false;

    fn try_from_tokens(tokens: &VkXMLTokens<'s, 'a>) -> Result<Self, ParseError> {
        c_with_vk_ext::type_define(tokens)
    }
}

impl<'s, 'a: 's> IntoVkXMLTokens<'s> for &'s MacroDefine<'a> {
    fn to_tokens(&'s self, tokens: &mut Vec<VkXMLToken<'s>>) {
        if let Some(comment) = self.deprecation_comment {
            tokens.push(VkXMLToken::C(Token::_DeprecationComment(comment)));
            tokens.push(VkXMLToken::C(Token::NewLine));
        }
        if self.is_disabled {
            tokens.push(VkXMLToken::C(Token::_MalformedDefine));
        } else {
            tokens.push(VkXMLToken::C(Token::Pound));
            tokens.push(VkXMLToken::C(Token::Identifier("define")));
        }
        tokens.push(VkXMLToken::TextTag {
            name: "name",
            text: self.name,
        });
        match &self.value {
            MacroDefineValue::Expression(expr) => {
                let mut expr_tokens = Vec::new();
                expr.to_tokens(&mut expr_tokens, false);
                tokens.extend(expr_tokens.into_iter().map(VkXMLToken::C));
            }
            MacroDefineValue::FunctionDefine { params, expression } => {
                tokens.push(VkXMLToken::C(Token::LParen));
                let mut is_first = true;
                for param in params.iter() {
                    if is_first {
                        is_first = false;
                    } else {
                        tokens.push(VkXMLToken::C(Token::Comma));
                    }
                    tokens.push(VkXMLToken::C(Token::Identifier(param)));
                }
                tokens.push(VkXMLToken::C(Token::RParen));
                tokens.push(VkXMLToken::C(Token::Whitespace));
                tokens.extend(expression.iter().cloned().map(VkXMLToken::C));
            }
            MacroDefineValue::MacroFunctionCall {
                name: fn_name,
                args,
            } => {
                tokens.push(VkXMLToken::C(Token::Whitespace));
                tokens.push(VkXMLToken::TextTag {
                    name: "type",
                    text: fn_name,
                });
                tokens.push(VkXMLToken::C(Token::LParen));
                let mut expr_tokens = Vec::with_capacity(32);
                let mut is_first = true;
                for arg in args.iter() {
                    if is_first {
                        is_first = false;
                    } else {
                        tokens.push(VkXMLToken::C(Token::Comma));
                    }
                    arg.to_tokens(&mut expr_tokens, false);
                    tokens.extend(expr_tokens.drain(..).map(VkXMLToken::C));
                }
                tokens.push(VkXMLToken::C(Token::RParen));
            }
        }
    }
}

impl<'s, 'a: 's> TryFromTokens<'s, 'a> for FnPtrType<'a> {
    const PARSING_MACROS: bool = false;
    const OBJC_COMPAT: bool = false;

    fn try_from_tokens(tokens: &VkXMLTokens<'s, 'a>) -> Result<Self, ParseError> {
        c_with_vk_ext::type_funcptr(tokens)
    }
}

impl<'s, 'a: 's> IntoVkXMLTokens<'s> for &'s FnPtrType<'a> {
    fn to_tokens(&'s self, tokens: &mut Vec<VkXMLToken<'s>>) {
        tokens.push(VkXMLToken::C(Token::TypeDef));
        tokens.push(VkXMLToken::C(Token::Identifier(
            self.return_type_name.as_identifier(),
        )));
        pointer_kind_tokens(&self.return_type_pointer_kind, tokens);
        tokens.push(VkXMLToken::C(Token::LParen));
        tokens.push(VkXMLToken::C(Token::Identifier("VKAPI_PTR")));
        tokens.push(VkXMLToken::C(Token::MulStar));
        tokens.push(VkXMLToken::TextTag {
            name: "name",
            text: self.name,
        });
        tokens.push(VkXMLToken::C(Token::RParen));
        tokens.push(VkXMLToken::C(Token::LParen));
        if let Some(params) = self.params.as_deref() {
            let mut is_first = true;
            for param in params {
                if is_first {
                    is_first = false;
                } else {
                    tokens.push(VkXMLToken::C(Token::Comma));
                }
                typed_tag_tokens(param, false, tokens);
            }
        } else {
            tokens.push(VkXMLToken::C(Token::Void));
        }
        tokens.push(VkXMLToken::C(Token::RParen));
        tokens.push(VkXMLToken::C(Token::SemiColon));
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
