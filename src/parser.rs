use std::{
    borrow::Cow,
    fmt,
    num::{NonZeroU32, NonZeroU8},
    ops::Deref,
};

use logos::{Lexer, Logos};
use serde::Serialize;

use crate::{
    lexer::ResultIter as LexerResultIter, ArrayLength, BaseTypeType, Constant, DefineType,
    DefineTypeValue, ErrorKind, FieldLike, FnPtrType, ParseResult, PointerKind, Token, TokenExtras,
};
// Using the C grammer from https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf

// C Type Decleration types
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]

pub enum TypeQualifer {
    Const,
    Restrict,
    Volatile,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]

pub enum TypeSpecifier<'a> {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Struct(Cow<'a, str>),
    Union(Cow<'a, str>),
    Enum(Cow<'a, str>),
    TypedefName(Cow<'a, str>),
}

impl<'a> TypeSpecifier<'a> {
    fn from_plain(ident: Cow<'a, str>, is_struct: bool) -> Self {
        match &*ident {
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

    pub fn as_identifier(&self) -> &str {
        match self {
            TypeSpecifier::Void => "void",
            TypeSpecifier::Char => "char",
            TypeSpecifier::Short => "short",
            TypeSpecifier::Int => "int",
            TypeSpecifier::Long => "long",
            TypeSpecifier::Float => "float",
            TypeSpecifier::Double => "double",
            TypeSpecifier::Struct(id) => id,
            TypeSpecifier::Union(id) => id,
            TypeSpecifier::Enum(id) => id,
            TypeSpecifier::TypedefName(id) => id,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]

pub enum TypeName<'a> {
    Specifier(TypeSpecifier<'a>),
    Pointer {
        pointee_ty: Box<TypeName<'a>>,
    },
    Array(Box<TypeName<'a>>, Option<NonZeroU32>),
    Function {
        return_ty: Box<TypeName<'a>>,
        name: Cow<'a, str>,
        arg_tys: Box<[TypeName<'a>]>,
    },
    Qualified(TypeQualifer, Box<TypeName<'a>>),
}

enum TypeSpecifierOrQual<'a> {
    Specifier(TypeSpecifier<'a>),
    Qualifier(TypeQualifer),
}

// C Expression types

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum FixOrder {
    Prefix,
    Postfix,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]

pub enum ComparisionOp {
    LT,
    LTE,
    Eq,
    NEq,
    GTE,
    GT,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]

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

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum MemberAccess {
    /// '.'
    Direct,
    /// '->'
    Pointer,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Expression<'a> {
    Identifier(Cow<'a, str>),
    Constant(Constant),
    Literal(Cow<'a, str>),
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
    FunctionCall(Box<Expression<'a>>, Box<[Expression<'a>]>),
    Comma(Box<Expression<'a>>, Box<Expression<'a>>),
    Member(MemberAccess, Box<Expression<'a>>, Cow<'a, str>),
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

fn wrap_member<'a>(
    v: Expression<'a>,
    access: MemberAccess,
    member: Cow<'a, str>,
) -> Expression<'a> {
    Expression::Member(access, Box::new(v), member)
}

fn wrap_array_access<'a>(v: Expression<'a>, idx: Expression<'a>) -> Expression<'a> {
    Expression::ArrayElement(Box::new(v), Box::new(idx))
}

fn wrap_fn_call<'a>(f: Expression<'a>, args: Vec<Expression<'a>>) -> Expression<'a> {
    Expression::FunctionCall(Box::new(f), args.into_boxed_slice())
}

// fn wrap_comma<'a>(head: Expression<'a>, tail: Expression<'a>) -> Expression<'a> {
//     Expression::Comma(Box::new(head), Box::new(tail))
// }

impl fmt::Display for TypeSpecifier<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeSpecifier::Void => write!(f, "void"),
            TypeSpecifier::Char => write!(f, "char"),
            TypeSpecifier::Short => write!(f, "short"),
            TypeSpecifier::Int => write!(f, "int"),
            TypeSpecifier::Long => write!(f, "long"),
            TypeSpecifier::Float => write!(f, "float"),
            TypeSpecifier::Double => write!(f, "double"),
            TypeSpecifier::Struct(ident) => write!(f, "struct {}", ident),
            TypeSpecifier::Union(ident) => write!(f, "union {}", ident),
            TypeSpecifier::Enum(ident) => write!(f, "enum {}", ident),
            TypeSpecifier::TypedefName(ident) => write!(f, "{}", ident),
        }
    }
}

impl fmt::Display for TypeName<'_> {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl fmt::Display for UnaryOp<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Address => write!(f, "&"),
            UnaryOp::Indirection => write!(f, "*"),
            UnaryOp::Positive => write!(f, "+"),
            UnaryOp::Negative => write!(f, "-"),
            UnaryOp::BitwiseNegation => write!(f, "~"),
            UnaryOp::LogicalNegation => write!(f, "!"),
            UnaryOp::Increment(_) => write!(f, "++"),
            UnaryOp::Decrement(_) => write!(f, "--"),
            UnaryOp::Cast(ty) => write!(f, "({})", ty),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Addition => write!(f, "+"),
            BinaryOp::Subtraction => write!(f, "-"),
            BinaryOp::Multiplication => write!(f, "*"),
            BinaryOp::Division => write!(f, "/"),
            BinaryOp::Remainder => write!(f, "%"),
            BinaryOp::LeftShift => write!(f, "<<"),
            BinaryOp::RightShift => write!(f, ">>"),
            BinaryOp::BitwiseAnd => write!(f, "&"),
            BinaryOp::BitwiseOr => write!(f, "|"),
            BinaryOp::BitwiseXor => write!(f, "^"),
            BinaryOp::LogicalAnd => write!(f, "&&"),
            BinaryOp::LogicalOr => write!(f, "||"),
        }
    }
}

impl fmt::Display for ComparisionOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ComparisionOp::LT => write!(f, "<"),
            ComparisionOp::LTE => write!(f, "<="),
            ComparisionOp::Eq => write!(f, "=="),
            ComparisionOp::NEq => write!(f, "!="),
            ComparisionOp::GTE => write!(f, ">="),
            ComparisionOp::GT => write!(f, ">"),
        }
    }
}

impl fmt::Display for MemberAccess {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemberAccess::Direct => write!(f, "."),
            MemberAccess::Pointer => write!(f, "->"),
        }
    }
}

struct MaybeParenWrap<'b, 'a>(&'b Expression<'a>);

impl fmt::Display for MaybeParenWrap<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if matches!(
            self.0,
            Expression::Identifier(_) | Expression::Constant(_) | Expression::Literal(_)
        ) {
            write!(f, "{}", self.0)
        } else {
            write!(f, "({})", self.0)
        }
    }
}

// Only used for roundtrip
impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(id) => write!(f, "{}", id),
            Expression::Constant(c) => write!(f, "{}", c),
            Expression::Literal(lit) => write!(f, "{:?}", lit),
            Expression::SizeOf(_) => todo!(),
            Expression::Unary(UnaryOp::Increment(FixOrder::Postfix), e) => {
                write!(f, "{}++", MaybeParenWrap(e))
            }
            Expression::Unary(UnaryOp::Decrement(FixOrder::Postfix), e) => {
                write!(f, "{}--", MaybeParenWrap(e))
            }
            Expression::Unary(op, e) => write!(f, "{}{}", op, MaybeParenWrap(e)),
            Expression::Binary(op, l, r) => {
                write!(f, "{} {} {}", MaybeParenWrap(l), op, MaybeParenWrap(r))
            }
            Expression::Comparision(op, l, r) => {
                write!(f, "{} {} {}", MaybeParenWrap(l), op, MaybeParenWrap(r))
            }
            Expression::Assignment(_, _, _) => todo!(),
            Expression::TernaryIfElse(cond, et, ef) => write!(f, "{} ? {} : {}", cond, et, ef),
            Expression::FunctionCall(func, args) => {
                write!(f, "{}(", func)?;
                for arg in args.iter() {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ")")
            }
            Expression::Comma(head, tail) => {
                write!(f, "{} , {}", MaybeParenWrap(head), MaybeParenWrap(tail))
            }
            Expression::Member(access, v, member) => write!(f, "{}{}{}", v, access, member),
            Expression::ArrayElement(e, i) => write!(f, "{}[{}]", e, i),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VkXMLToken<'a> {
    C(Token<'a>),
    TextTag {
        name: Cow<'a, str>,
        text: Cow<'a, str>,
    },
}

impl<'a> fmt::Display for VkXMLToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VkXMLToken::C(token) => write!(f, "{}", token),
            VkXMLToken::TextTag { name, text } => write!(f, "<{0}>{1}</{0}>", name, text),
        }
    }
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

impl<'s, 'a: 's> From<Lexer<'a, Token<'a>>> for VkXMLTokens<'s, 'a> {
    fn from(lex: Lexer<'a, Token<'a>>) -> Self {
        lex.into_iter().map(VkXMLToken::C).collect()
    }
}

pub(crate) fn vk_tokenize<'s, 'a: 's, 'input>(
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
                    name: Cow::Borrowed(n.tag_name().name()),
                    text: Cow::Borrowed(text),
                })]
            } else {
                let extras = TokenExtras {
                    keep_new_lines: parsing_macros,
                    objc_compat,
                };
                LexerResultIter::from(Lexer::with_extras(text, extras))
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
            let literal_token =
                Token::from_literal(literal).unwrap_or_else(|| panic!("I'm dum {:?}", literal));
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

#[derive(Debug)]
enum BitFieldSizeOrArrayshape<'a> {
    BitfieldSize(NonZeroU8),
    ArrayShape(Box<[ArrayLength<'a>]>),
}

/// C is not a context-free languge and requires building a symbol table of typedefs at parse time to resolve ambigutites with cast expressions
pub(crate) fn is_typedef_name(name: &str) -> bool {
    // FIXME: actually build a symbol table as we go that is already filled with the standard typedefs (size_t, uint32_t, ...),
    name.ends_with("_t") || name.starts_with("Vk")
}

peg::parser! {
    pub grammar c_with_vk_ext<'s, 'a>() for VkXMLTokens<'s, 'a> {
        pub rule identifier() -> Cow<'a, str>
          = quiet!{[VkXMLToken::C(Token::Identifier(i))] { i.clone() }}
          / expected!("Identifier")

        rule typedef_name() -> Cow<'a, str>
          = quiet!{[VkXMLToken::C(Token::Identifier(i)) if is_typedef_name(i)] { i.clone() }}
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

        rule literal() -> Cow<'a, str>
          = quiet!{[VkXMLToken::C(Token::Literal(l))] { l.clone() }}
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
        rule type_tag() -> Cow<'a, str>
          = quiet!{[VkXMLToken::TextTag { name, text } if name == "type"] { text.clone() }}
          / expected!("<type>...</type>")

        // handle pointer info, can be '*' or '**' or '* const*'
        rule pointer_kind() -> PointerKind
            = "*" inner:(c:"const"? "*" { c.is_some() })? {
                inner.map_or(PointerKind::Single, |inner_is_const| PointerKind::Double { inner_is_const })
            }

        rule typed_tag(name_text: rule<Cow<'a, str>>) -> FieldLike<'a>
            = is_const:"const"? is_struct:"struct"? type_name:type_tag() pointer_kind:pointer_kind()? name:name_text() {
                FieldLike {
                    pointer_kind,
                    is_const: is_const.is_some(),
                    ..FieldLike::default_new(name, TypeSpecifier::from_plain(type_name, is_struct.is_some()))
                }
            }

        rule name_tag() -> Cow<'a, str>
         = quiet!{[VkXMLToken::TextTag { name, text } if name == "name"] { text.clone() }}
         / expected!("<name>...</name>")

        rule enum_tag() -> Cow<'a, str>
         = quiet!{[VkXMLToken::TextTag { name, text } if name == "enum"] { text.clone() }}
         / expected!("<enum>...</enum>")

        rule comment_tag() -> Cow<'a, str>
         = quiet!{[VkXMLToken::TextTag { name, text } if name == "comment"] { text.clone() }}
         / expected!("<comment>...</comment>")

        rule integer() -> u64
         = quiet!{[VkXMLToken::C(Token::Constant(Constant::Integer(v)))] { *v }}
         / expected!("Integral Constant")

        /// parses the content of <member> ... </member> or <proto> ... </proto> or <param> ... </param>
        pub rule field_like() -> FieldLike<'a>
            = typed:typed_tag(<name_tag()>) bitOrArr:(
                ":" bitfield_size:(v:integer() { NonZeroU8::new(v.try_into().unwrap()).unwrap() }) { BitFieldSizeOrArrayshape::BitfieldSize(bitfield_size) }
                / array_shape:("[" n:(
                    v:integer() { ArrayLength::Static(NonZeroU32::new(v.try_into().unwrap()).unwrap()) }
                    / name:enum_tag() { ArrayLength::Constant(name) }
                ) "]" { n })+ { BitFieldSizeOrArrayshape::ArrayShape(array_shape.into_boxed_slice()) }
            )? comment:comment_tag()? {
                let (bitfield_size, array_shape) = match bitOrArr {
                    Some(BitFieldSizeOrArrayshape::BitfieldSize(size)) => (Some(size), None),
                    Some(BitFieldSizeOrArrayshape::ArrayShape(shape)) => (None, Some(shape)),
                    None => (None, None),
                };
                FieldLike {
                    bitfield_size,
                    array_shape,
                    comment,
                    ..typed
                }
            }

        /// <type category="basetype"> ... </type>
        pub rule type_basetype() -> BaseTypeType<'a>
            = "struct" name:name_tag() ";" { BaseTypeType::Forward(name) }
            / "typedef" typed:typed_tag(<name_tag()>) ";" { BaseTypeType::TypeDef(typed) }
            // Workaround because it should be `typedef struct <type>__IOSurface</type>* <name>IOSurfaceRef</name>;` not `typedef struct __IOSurface* <name>IOSurfaceRef</name>;`
            / quiet!{"typedef" "struct" [VkXMLToken::C(Token::Identifier(i)) if i == "__IOSurface"] "*" name:name_tag() ";" { BaseTypeType::TypeDef(FieldLike {
                pointer_kind: Some(PointerKind::Single),
                is_const: false,
                ..FieldLike::default_new(name, TypeSpecifier::Struct(Cow::Borrowed("__IOSurface")))
            }) }}
            / pre:(([VkXMLToken::C(c)] {c.clone()})+) name:name_tag() post:(([VkXMLToken::C(c)] {c.clone()})+) { BaseTypeType::DefineGuarded { pre, name, post } }

        /// <type category="define"> ... </type>
        pub rule type_define(name_attr: Option<&'a str>, requires_attr: Option<&'a str>) -> DefineType<'a>
            = dc:quiet!{([VkXMLToken::C(Token::_DeprecationComment(c))] {c})?} "\n"* is_disabled:("#define" {false} / "//#define" {true}) name:name_tag() value:(
                "(" params:(identifier() ** ",") ")" expression:(([VkXMLToken::C(c)] {c.clone()})+) {
                    DefineTypeValue::FunctionDefine {
                        params: params.into_boxed_slice(),
                        expression: expression.into_boxed_slice(),
                    }
                }
                /  e:expr() { DefineTypeValue::Expression(e) }
                / macro_name:type_tag() "(" args:(expr() ** ",") ","? ")" { DefineTypeValue::MacroFunctionCall {
                    name: macro_name,
                    args: args.into_boxed_slice(),
                } }
            ) { DefineType {
                name,
                comment: None,
                requires: requires_attr.map(Cow::Borrowed),
                deprecation_comment: dc.cloned(),
                is_disabled,
                value,
            } }
            / l:(([VkXMLToken::C(c)] {c.clone()})+) {
                name_attr.unwrap_or_else(|| panic!("{:?}", l));
                DefineType {
                name: name_attr.map(Cow::Borrowed).expect("If no name is found inside the tag <type category=\"define\"> then it must be an attribute"),
                comment: None, requires: requires_attr.map(Cow::Borrowed), deprecation_comment: None, is_disabled: false, value: DefineTypeValue::Code(l.into_boxed_slice())
            } }


        rule vkapi_ptr_macro()
            = quiet!{[VkXMLToken::C(Token::Identifier(id)) if id == "VKAPI_PTR"]}
            / expected!("VKAPI_PTR")

        /// <type category="funcptr"> ... </type>
        pub rule type_funcptr(requires_attr: Option<&'a str>) -> FnPtrType<'a>
          = "typedef" ty_name:type_specifier() ptr:"*"? "(" vkapi_ptr_macro() "*" name:name_tag() ")" "(" params:(
            "void" ")" ";" { None }
            / params:(typed_tag(<identifier()>) ** ",") ")" ";" { Some(params) }
          ) { FnPtrType { name_and_return: FieldLike { pointer_kind: ptr.map(|()| PointerKind::Single), ..FieldLike::default_new(name, ty_name) }, params, requires: requires_attr.map(Cow::Borrowed) } }
  }
}

impl<'a> TryFrom<&'a str> for Expression<'a> {
    type Error = peg::error::ParseError<usize>;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        let c_toks = Token::lexer(value).into();
        c_with_vk_ext::expr(&c_toks)
    }
}

impl<'s, 'a: 's> TryFrom<&'s [Token<'a>]> for Expression<'a> {
    type Error = peg::error::ParseError<usize>;

    fn try_from(value: &'s [Token<'a>]) -> Result<Self, Self::Error> {
        let c_toks = VkXMLTokens(value.iter().cloned().map(VkXMLToken::C).collect());
        c_with_vk_ext::expr(&c_toks)
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
        )
    }
}
