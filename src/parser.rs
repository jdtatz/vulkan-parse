use std::{
    borrow::Cow,
    fmt,
    num::{NonZeroU32, NonZeroU8},
    ops::Deref,
};

use logos::Lexer;

use crate::{
    ArrayLength, Constant, DefineType, DefineTypeValue, FieldLike, FnPtrType, PointerKind, Token,
};
// Using the C grammer from https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf

// C Type Decleration types
#[derive(Debug, Clone, PartialEq, Eq)]

pub enum TypeQualifer {
    Const,
    Restrict,
    Volatile,
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum TypeIdentifier<'a> {
    Plain(Cow<'a, str>),
    Struct(Cow<'a, str>),
    Union(Cow<'a, str>),
    Enum(Cow<'a, str>),
}
#[derive(Debug, Clone, PartialEq, Eq)]

pub enum TypeSpecifier<'a> {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Identifier(TypeIdentifier<'a>),
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
            _ if is_struct => TypeSpecifier::Identifier(TypeIdentifier::Struct(ident)),
            _ => TypeSpecifier::Identifier(TypeIdentifier::Plain(ident)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum Type<'a> {
    Specifier(TypeSpecifier<'a>),
    Pointer {
        pointee_ty: Box<Type<'a>>,
    },
    Array(Box<Type<'a>>, Option<NonZeroU32>),
    Function {
        return_ty: Box<Type<'a>>,
        name: Cow<'a, str>,
        arg_tys: Box<[Type<'a>]>,
    },
    Qualified(TypeQualifer, Box<Type<'a>>),
}

enum TypeSpecifierOrQual<'a> {
    Specifier(TypeSpecifier<'a>),
    Qualifier(TypeQualifer),
}

// C Expression types

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FixOrder {
    Prefix,
    Postfix,
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum ComparisionOp {
    LT,
    LTE,
    Eq,
    NEq,
    GTE,
    GT,
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub enum UnaryOp<'a> {
    Address,
    Indirection,
    Positive,
    Negative,
    BitwiseNegation,
    LogicalNegation,
    Increment(FixOrder),
    Decrement(FixOrder),
    Cast(Type<'a>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
pub enum MemberAccess {
    /// '.'
    Direct,
    /// '->'
    Pointer,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression<'a> {
    Identifier(Cow<'a, str>),
    Constant(Constant),
    Literal(Cow<'a, str>),
    SizeOf(Type<'a>),
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
    Comma(Box<[Expression<'a>]>),
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

fn comma_expr_or_single(mut v: Vec<Expression>) -> Expression {
    if v.len() == 1 {
        v.pop().unwrap()
    } else {
        Expression::Comma(v.into_boxed_slice())
    }
}

impl fmt::Display for TypeIdentifier<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeIdentifier::Plain(ident) => write!(f, "{}", ident),
            TypeIdentifier::Struct(ident) => write!(f, "struct {}", ident),
            TypeIdentifier::Union(ident) => write!(f, "union {}", ident),
            TypeIdentifier::Enum(ident) => write!(f, "enum {}", ident),
        }
    }
}

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
            TypeSpecifier::Identifier(ident) => write!(f, "{}", ident),
        }
    }
}

impl fmt::Display for Type<'_> {
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

impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(id) => write!(f, "{}", id),
            Expression::Constant(c) => write!(f, "{}", c),
            Expression::Literal(lit) => write!(f, "{:?}", lit),
            Expression::SizeOf(_) => todo!(),
            Expression::Unary(UnaryOp::Increment(FixOrder::Postfix), e) => write!(f, "{}++", e),
            Expression::Unary(UnaryOp::Decrement(FixOrder::Postfix), e) => write!(f, "{}--", e),
            Expression::Unary(op, e) => write!(f, "{}{}", op, e),
            Expression::Binary(op, l, r) => write!(f, "{} {} {}", l, op, r),
            Expression::Comparision(op, l, r) => write!(f, "{} {} {}", l, op, r),
            Expression::Assignment(_, _, _) => todo!(),
            Expression::TernaryIfElse(cond, et, ef) => write!(f, "{} ? {} : {}", cond, et, ef),
            Expression::FunctionCall(func, args) => {
                write!(f, "{}(", func)?;
                for arg in args.iter() {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ")")
            }
            Expression::Comma(v) => {
                debug_assert!(v.len() > 1);
                write!(f, "{}", &v[0])?;
                for e in &v[1..] {
                    write!(f, ", {}", e)?;
                }
                Ok(())
            }
            Expression::Member(access, v, member) => write!(f, "{}{}{}", v, access, member),
            Expression::ArrayElement(e, i) => write!(f, "{}[{}]", e, i),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone)]
pub struct VkXMLTokens<'s, 'a>(pub Cow<'s, [VkXMLToken<'a>]>);

impl<'s, 'a> Deref for VkXMLTokens<'s, 'a> {
    type Target = [VkXMLToken<'a>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> FromIterator<VkXMLToken<'a>> for VkXMLTokens<'static, 'a> {
    fn from_iter<T: IntoIterator<Item = VkXMLToken<'a>>>(iter: T) -> Self {
        VkXMLTokens(Cow::Owned(iter.into_iter().collect()))
    }
}

impl<'a> From<Lexer<'a, Token<'a>>> for VkXMLTokens<'static, 'a> {
    fn from(lex: Lexer<'a, Token<'a>>) -> Self {
        lex.into_iter().map(VkXMLToken::C).collect()
    }
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

peg::parser! {
    pub grammar c_with_vk_ext<'s, 'a>() for VkXMLTokens<'s, 'a> {
        pub rule identifier() -> Cow<'a, str>
          = quiet!{[VkXMLToken::C(Token::Identifier(i))] { i.clone() }}
          / expected!("Identifier")

        rule type_identifier() -> TypeIdentifier<'a>
          = "struct" i:identifier() { TypeIdentifier::Struct(i) }
          / "union" i:identifier() { TypeIdentifier::Union(i) }
          / "enum" i:identifier() { TypeIdentifier::Enum(i) }
          / i:identifier() { TypeIdentifier::Plain(i) }

        rule type_specifier() -> TypeSpecifier<'a>
          = i:type_identifier() { TypeSpecifier::Identifier(i) }
          / "void" { TypeSpecifier::Void }
          / "char" { TypeSpecifier::Char }
          / "short" { TypeSpecifier::Short }
          / "int" { TypeSpecifier::Int }
          / "long" { TypeSpecifier::Long }
          / "float" { TypeSpecifier::Float }
          / "double" { TypeSpecifier::Double }

        pub rule type_name() -> Type<'a>
          = ty:type_specifier() { Type::Specifier(ty) }


        // C-expr rules

        pub rule constant() -> Constant
          = quiet!{[VkXMLToken::C(Token::Constant(c))] { *c }}
          / expected!("Constant")

        rule literal() -> Cow<'a, str>
          = quiet!{[VkXMLToken::C(Token::Literal(l))] { l.clone() }}
          / expected!("String Literal")

        rule primary_expr() -> Expression<'a>
          = i:identifier() { Expression::Identifier(i) }
          / l:literal() { Expression::Literal(l) }
          / c:constant() { Expression::Constant(c) }
          / "(" e:expr() ")" { e }

        rule expression() -> Expression<'a> = precedence!{
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
          x:(@) "(" y:(expression() ** ",") ","? ")" { wrap_fn_call(x, y) }
          x:(@) "++" { wrap_unary(UnaryOp::Increment(FixOrder::Postfix), x) }
          x:(@) "--" { wrap_unary(UnaryOp::Decrement(FixOrder::Postfix), x) }
          --
          p:primary_expr() { p }
        }

        pub rule expr() -> Expression<'a>
          = v:(expression() ++ ",") { comma_expr_or_single(v) }


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

        /// <type category="define"> ... </type>
        pub rule type_define(name_attr: Option<&'a str>, requires_attr: Option<&'a str>) -> DefineType<'a>
            = "\n"* is_disabled:("#define" {false} / "//#define" {true}) name:name_tag() value:(
                "(" params:(identifier() ** ",") ")" expression:(([VkXMLToken::C(c)] {c.clone()})+) {
                    DefineTypeValue::FunctionDefine {
                        params: params.into_boxed_slice(),
                        expression: expression.into_boxed_slice(),
                    }
                }
                /  e:expr() { DefineTypeValue::Expression(e) }
                / macro_name:type_tag() "(" args:(expression() ** ",") ","? ")" { DefineTypeValue::MacroFunctionCall {
                    name: macro_name,
                    args: args.into_boxed_slice(),
                } }
            ) { DefineType {
                name,
                comment: None,
                requires: requires_attr.map(Cow::Borrowed),
                is_disabled,
                value,
            } }
            / l:(([VkXMLToken::C(c)] {c.clone()})+) {
                name_attr.unwrap_or_else(|| panic!("{:?}", l));
                DefineType {
                name: name_attr.map(Cow::Borrowed).expect("If no name is found inside the tag <type category=\"define\"> then it must be an attribute"),
                comment: None, requires: requires_attr.map(Cow::Borrowed), is_disabled: false, value: DefineTypeValue::Code(l.into_boxed_slice())
            } }


        rule vkapi_ptr_macro()
            = quiet!{[VkXMLToken::C(Token::Identifier(id)) if id == "VKAPI_PTR"]}
            / expected!("VKAPI_PTR")

        /// <type category="funcptr"> ... </type>
        pub rule type_funcptr(requires_attr: Option<&'a str>) -> FnPtrType<'a>
          = "typedef" ty_name:type_specifier() ptr:"*"? "(" vkapi_ptr_macro() "*" name:name_tag() ")" "(" params:(
            "void" ")" ";" { Vec::new() }
            / params:(typed_tag(<identifier()>) ** ",") ")" ";" { params }
          ) { FnPtrType { name_and_return: FieldLike { pointer_kind: ptr.map(|()| PointerKind::Single), ..FieldLike::default_new(name, ty_name) }, params: params.into_boxed_slice(), requires: requires_attr.map(Cow::Borrowed) } }
  }
}
