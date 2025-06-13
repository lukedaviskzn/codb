use std::{borrow::Borrow, collections::BTreeMap, fmt::{Debug, Display}, hash::Hash, ops::Bound, sync::{Arc, Mutex}};

use codb_core::{Ident, IdentForest, IdentPath, IdentTree, NestedIdent};
use indexmap::IndexMap;
use itertools::Itertools;

use crate::{db::{pager::Pager, registry::{CompositeTTypeId, Registry, TTypeId}, DbRelationSet}, expression::{ArithmeticOp, ArrayLiteral, Branch, CompositeLiteral, ControlFlow, EnumLiteral, Expression, FunctionInvocation, IfControlFlow, InterpreterAction, Literal, LogicalOp, MatchControlFlow, Op, StructLiteral}, query::{lexer::TokenKind, schema_query::{RelationSchemaQuery, SchemaQuery, TypeSchemaQuery}}, typesystem::{function::{Function, FunctionArg}, ttype::{ArrayType, CompositeType, EnumType, ScalarType, StructType, TType}, value::ScalarValue, TypeError, TypeSet}};

use super::{lexer::{Keyword, LexContext, LexError, LexErrorKind, Lexer, Symbol, Token, TokenTag}, Span};

#[derive(Debug, Clone, thiserror::Error)]
#[error("[{span}] error while{}: {kind}", .context.as_ref().map(|ctx| format!(" {ctx}")).unwrap_or(" parsing".into()))]
pub struct ParseError {
    pub span: Span,
    pub context: Option<ParseContext>,
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub fn with_context(mut self, context: ParseContext) -> ParseError {
        self.context = Some(context);
        self
    }
}

#[derive(Debug, Clone)]
pub enum ParseContext {
    LexContext(Option<LexContext>),
    ParsingDataQuery,
    ParsingSchemaQuery,
    ParsingExpression,
    ParsingIf,
    ParsingMatch,
    ParsingBranch,
    ParsingAction,
    ParsingFunction,
    ParsingFunctionArg,
    ParsingFunctionInvocation,
    ParsingLiteral,
    ParsingCompositeLiteral,
    ParsingScalarLiteral,
    ParsingStructLiteral,
    ParsingEnumLiteral,
    ParsingArrayLiteral,
    ParsingStructType,
    ParsingEnumType,
    ParsingArrayType,
    ParsingAnonymousType,
    ParsingTTypeId,
    ParsingCompositeTTypeId,
    ParsingNestedIdent,
    ParsingIdentPath,
    ParsingIdentTree,
    ParsingIdentForest,
}

impl Display for ParseContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseContext::LexContext(ctx) => match ctx {
                        Some(ctx) => Display::fmt(ctx, f),
                        None => write!(f, "tokenising"),
                    },
            ParseContext::ParsingDataQuery => write!(f, "parsing data query"),
            ParseContext::ParsingSchemaQuery => write!(f, "parsing schema query"),
            ParseContext::ParsingExpression => write!(f, "parsing expression"),
            ParseContext::ParsingIf => write!(f, "parsing if control flow"),
            ParseContext::ParsingMatch => write!(f, "parsing match control flow"),
            ParseContext::ParsingBranch => write!(f, "parsing branch"),
            ParseContext::ParsingAction => write!(f, "parsing action"),
            ParseContext::ParsingFunction => write!(f, "parsing function"),
            ParseContext::ParsingFunctionArg => write!(f, "parsing function argument"),
            ParseContext::ParsingFunctionInvocation => write!(f, "parsing function invocation"),
            ParseContext::ParsingLiteral => write!(f, "parsing value"),
            ParseContext::ParsingCompositeLiteral => write!(f, "parsing composite value"),
            ParseContext::ParsingScalarLiteral => write!(f, "parsing scalar value"),
            ParseContext::ParsingStructLiteral => write!(f, "parsing struct value"),
            ParseContext::ParsingEnumLiteral => write!(f, "parsing enum value"),
            ParseContext::ParsingArrayLiteral => write!(f, "parsing array value"),
            ParseContext::ParsingStructType => write!(f, "parsing struct type"),
            ParseContext::ParsingEnumType => write!(f, "parsing enum type"),
            ParseContext::ParsingArrayType => write!(f, "parsing array type"),
            ParseContext::ParsingAnonymousType => write!(f, "parsing anonymous type"),
            ParseContext::ParsingTTypeId => write!(f, "parsing type id"),
            ParseContext::ParsingCompositeTTypeId => write!(f, "parsing composite type id"),
            ParseContext::ParsingNestedIdent => write!(f, "parsing nested ident"),
            ParseContext::ParsingIdentPath => write!(f, "parsing ident path"),
            ParseContext::ParsingIdentTree => write!(f, "parsing ident tree"),
            ParseContext::ParsingIdentForest => write!(f, "parsing ident forest"),
        }
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum ParseErrorKind {
    #[error("{0}")]
    LexError(#[from] LexErrorKind),
    #[error("{0}")]
    TypeError(#[from] TypeError),
    #[error("unexpected end of input")]
    UnexpectedEnd,
    #[error("unexpected `{0}`")]
    UnexpectedTokenKind(TokenKind),
    #[error("unexpected `{0}`")]
    UnexpectedTokenTag(TokenTag),
    #[error("unexpected `{got}` expected one of `{expected:?}`")]
    ExpectedTokenKindOneOf {
        expected: Box<[TokenKind]>,
        got: TokenKind,
    },
    #[error("expected `{expected:?}` got `{got}`")]
    ExpectedTokenKind {
        expected: TokenKind,
        got: TokenKind,
    },
    #[error("expected `{expected:?}` got `{got}`")]
    ExpectedTokenTagGotKind {
        expected: TokenTag,
        got: TokenKind,
    },
    #[error("expected `{expected:?}` got `{got}`")]
    ExpectedTokenTag {
        expected: TokenTag,
        got: TokenTag,
    },
    #[error("expected `{expected:?}` got `{got}`")]
    ExpectedScalarLiteralType {
        expected: ScalarType,
        got: TokenKind,
    },
    #[error("unexpected `{got}` expected one of `{expected:?}`")]
    ExpectedScalarLiteralTypeOneOf {
        expected: Box<[ScalarType]>,
        got: TokenKind,
    },
    #[error("match branch has duplicate tag `{0}`")]
    DuplicateBranch(Ident),
    #[error("duplicate key `{0}`")]
    DuplicateKey(String),
    #[error("cannot create value of type `never`")]
    NeverValue,
}

impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        Self {
            span: value.span,
            context: Some(ParseContext::LexContext(value.context)),
            kind: value.kind.into(),
        }
    }
}

pub(crate) trait Parse {
    type Args<'a>;

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized;
    fn is_first(token: &TokenKind) -> bool;
}

pub(crate) struct ExpressionArgs<'a> {
    pager: Arc<Mutex<Pager>>,
    registry: &'a Registry,
    relations: &'a DbRelationSet,
}

impl Parse for SchemaQuery {
    type Args<'a> = ();

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let keyword_token = lexer.expect_token_tag(TokenTag::Keyword)?;
        let TokenKind::Keyword(keyword) = keyword_token.kind else { unreachable!() };
        
        match keyword {
            Keyword::Type => {
                let (name, name_span) = IdentPath::parse(lexer, ()).map_err(|err| err.with_context(ParseContext::ParsingSchemaQuery))?;
                lexer.expect_symbol(Symbol::Equal)?;
                let (ttype, ttype_span) = CompositeType::parse(lexer, ()).map_err(|err| err.with_context(ParseContext::ParsingSchemaQuery))?;
                Ok((SchemaQuery::Type(TypeSchemaQuery::Create {
                    name,
                    ttype,
                }), keyword_token.span.merge(ttype_span)))
            },
            Keyword::Relation => {
                let (name, _) = Ident::parse(lexer, ()).map_err(|err| err.with_context(ParseContext::ParsingSchemaQuery))?;
                lexer.expect_symbol(Symbol::Equal).map_err(|err| err.with_context(ParseContext::ParsingSchemaQuery))?;
                let (ttype, _) = StructType::parse(lexer, ())?;
                
                lexer.expect_symbol(Symbol::LessThan).map_err(|err| err.with_context(ParseContext::ParsingSchemaQuery))?;
                let (pkey, pkey_span) = IdentForest::parse(lexer, ())?;
                lexer.expect_symbol(Symbol::GreaterThan).map_err(|err| err.with_context(ParseContext::ParsingSchemaQuery))?;

                Ok((SchemaQuery::Relation(RelationSchemaQuery::Create {
                    name,
                    ttype,
                    pkey,
                }), keyword_token.span.merge(pkey_span)))
            },
            keyword => {
                Err(ParseError {
                    span: keyword_token.span,
                    context: Some(ParseContext::ParsingSchemaQuery),
                    kind: ParseErrorKind::ExpectedTokenKindOneOf {
                        expected: [TokenKind::Keyword(Keyword::Type), TokenKind::Keyword(Keyword::Relation)].into(),
                        got: TokenKind::Keyword(keyword),
                    },
                })
            }
        }
    }

    fn is_first(token: &TokenKind) -> bool {
        match token {
            TokenKind::Keyword(Keyword::Type) => true,
            TokenKind::Keyword(Keyword::Relation) => true,
            _ => false,
        }
    }
}

impl Parse for Ident {
    type Args<'a> = ();

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let token = lexer.expect_token_tag(TokenTag::Ident)?;
        let TokenKind::Ident(ident) = token.kind else { unreachable!() };
        Ok((ident, token.span))
    }

    fn is_first(token: &TokenKind) -> bool {
        matches!(token, TokenKind::Ident(_))
    }
}

impl Parse for IdentPath {
    type Args<'a> = ();

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (idents, list_span) = NonEmpty::parse(lexer, ListArgs { args: (), separator: Symbol::PathSep })
            .map_err(|err| err.with_context(ParseContext::ParsingIdentPath))?;
        
        let mut ident_list = vec![idents.head];
        ident_list.extend(idents.tail);

        let ident_path = IdentPath::try_from(ident_list).expect("invalid ident path");

        Ok((ident_path, list_span))
    }

    fn is_first(token: &TokenKind) -> bool {
        Ident::is_first(token)
    }
}

impl Parse for NestedIdent {
    type Args<'a> = ();

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (ident_list, list_span) = NonEmpty::parse(lexer, ListArgs {
            args: (),
            separator: Symbol::Dot,
        }).map_err(|err| err.with_context(ParseContext::ParsingNestedIdent))?;

        let mut idents = vec![ident_list.head];
        idents.extend(ident_list.tail);
        
        let nested_ident = NestedIdent::try_from(idents).expect("invalid nested ident");

        Ok((nested_ident, list_span))
    }

    fn is_first(token: &TokenKind) -> bool {
        Ident::is_first(token)
    }
}

struct ListArgs<'a, P: Parse> {
    args: P::Args<'a>,
    separator: Symbol,
}

impl<P: Parse> Parse for Vec<P> where for<'a> P::Args<'a>: Clone {
    type Args<'a> = ListArgs<'a, P>;

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let mut out = Vec::new();
        let mut span: Option<Span> = None;
        
        while let Some(token) = lexer.peek() {
            let token = token.as_ref().map_err(|err| err.clone())?;
            
            if !P::is_first(&token.kind) { break }

            let (item, item_span) = P::parse(lexer, args.args.clone())?;
            span = Some(span
                .map(|span| span.merge(item_span))
                .unwrap_or(item_span)
            );

            out.push(item);

            if let Some(Ok(Token { kind: TokenKind::Symbol(symbol), .. })) = lexer.peek() {
                if symbol != &args.separator {
                    break;
                }
            } else {
                break;
            }
        }

        Ok((out, span.unwrap_or(Span::ALL)))
    }

    fn is_first(token: &TokenKind) -> bool {
        P::is_first(token)
    }
}

struct NonEmpty<P: Parse> {
    head: P,
    tail: Vec<P>,
}

impl<P: Parse> Parse for NonEmpty<P> where for<'a> P::Args<'a>: Clone {
    type Args<'a> = ListArgs<'a, P>;

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (head, head_span) = P::parse(lexer, args.args.clone())?;
        let (tail, tail_span) = Vec::<P>::parse(lexer, args)?;

        Ok((Self {
            head,
            tail,
        }, head_span.merge(tail_span)))
    }

    fn is_first(token: &TokenKind) -> bool {
        P::is_first(token)
    }
}

impl Parse for CompositeType {
    type Args<'a> = ();

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let token = lexer.expect_peek().map_err(|err| err.with_context(ParseContext::ParsingCompositeTTypeId))?;
        match &token.kind {
            TokenKind::Keyword(Keyword::Struct) => {
                let (struct_type, struct_type_span) = StructType::parse(lexer, ())?;
                Ok((CompositeType::Struct(struct_type), struct_type_span))
            },
            TokenKind::Keyword(Keyword::Enum) => {
                let (enum_type, enum_type_span) = EnumType::parse(lexer, ())?;
                Ok((CompositeType::Enum(enum_type), enum_type_span))
            },
            kind => return Err(ParseError {
                span: token.span,
                context: Some(ParseContext::ParsingCompositeTTypeId),
                kind: ParseErrorKind::ExpectedTokenKindOneOf {
                    expected: [
                        TokenKind::Keyword(Keyword::Struct),
                        TokenKind::Keyword(Keyword::Enum),
                    ].into(),
                    got: kind.clone(),
                },
            })
        }
    }

    fn is_first(token: &TokenKind) -> bool {
        StructType::is_first(token) || EnumType::is_first(token)
    }
}

impl Parse for StructType {
    type Args<'a> = ();

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let struct_token = lexer.expect_keyword(Keyword::Struct).map_err(|err| err.with_context(ParseContext::ParsingStructType))?;
        lexer.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingStructType))?;

        let (fields, fields_span) = IndexMap::parse(lexer, MapArgs {
            key_args: (),
            value_args: (),
            map_seperator: Symbol::Colon,
            entry_separator: Symbol::Comma,
        })?;
        
        let bracket_token = lexer.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingStructType))?;

        let span = struct_token.span.merge(fields_span).merge(bracket_token.span);

        Ok((StructType::new(fields), span))
    }

    fn is_first(token: &TokenKind) -> bool {
        token == &TokenKind::Keyword(Keyword::Struct)
    }
}

impl Parse for EnumType {
    type Args<'a> = ();

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let enum_token = lexer.expect_keyword(Keyword::Enum).map_err(|err| err.with_context(ParseContext::ParsingEnumType))?;
        lexer.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingEnumType))?;

        let (tags, tags_span) = IndexMap::parse(lexer, MapArgs {
            key_args: (),
            value_args: (),
            map_seperator: Symbol::Comma,
            entry_separator: Symbol::Comma,
        })?;

        let bracket_token = lexer.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingEnumType))?;

        let span = enum_token.span.merge(tags_span).merge(bracket_token.span);

        Ok((EnumType::new(tags), span))
    }

    fn is_first(token: &TokenKind) -> bool {
        token == &TokenKind::Keyword(Keyword::Enum)
    }
}

struct MapArgs<'a, K: Parse, V: Parse> {
    key_args: K::Args<'a>,
    value_args: V::Args<'a>,
    map_seperator: Symbol,
    entry_separator: Symbol,
}

impl<K: Parse + Hash + PartialEq + Eq + Debug, V: Parse> Parse for IndexMap<K, V>
where for<'a> K::Args<'a>: Clone, for<'a> V::Args<'a>: Clone {
    type Args<'a> = MapArgs<'a, K, V>;

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let mut out = IndexMap::new();
        let mut span: Option<Span> = None;
        
        while let Some(token) = lexer.peek() {
            let token = token.as_ref().map_err(|err| err.clone())?;

            span = Some(token.span);
            
            if !K::is_first(&token.kind) { break }

            let (key, mut entry_span) = K::parse(lexer, args.key_args.clone())?;

            if out.contains_key(&key) {
                return Err(ParseError {
                    span: span.unwrap_or(Span::ALL),
                    context: Some(ParseContext::ParsingStructType),
                    kind: ParseErrorKind::DuplicateKey(format!("{key:?}")),
                })
            }

            lexer.expect_symbol(args.map_seperator)?;

            let (value, value_span) = V::parse(lexer, args.value_args.clone())?;
            entry_span = entry_span.merge(value_span);

            out.insert(key, value);
            
            span = Some(span.expect("span not set").merge(entry_span));

            if let Some(Ok(Token { kind: TokenKind::Symbol(symbol), .. })) = lexer.peek() {
                if symbol != &args.entry_separator {
                    break;
                }
            } else {
                break;
            }
        }

        Ok((out, span.unwrap_or(Span::ALL)))
    }

    fn is_first(token: &TokenKind) -> bool {
        K::is_first(token)
    }
}

impl<K: Parse + Hash + Ord + Debug, V: Parse> Parse for BTreeMap<K, V>
where for<'a> K::Args<'a>: Clone, for<'a> V::Args<'a>: Clone {
    type Args<'a> = MapArgs<'a, K, V>;

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let mut out = BTreeMap::new();
        let mut span: Option<Span> = None;
        
        while let Some(token) = lexer.peek() {
            let token = token.as_ref().map_err(|err| err.clone())?;

            span = Some(token.span);
            
            if !K::is_first(&token.kind) { break }

            let (key, mut entry_span) = K::parse(lexer, args.key_args.clone())?;

            if out.contains_key(&key) {
                return Err(ParseError {
                    span: span.unwrap_or(Span::ALL),
                    context: Some(ParseContext::ParsingStructType),
                    kind: ParseErrorKind::DuplicateKey(format!("{key:?}")),
                })
            }

            lexer.expect_symbol(args.map_seperator)?;

            let (value, value_span) = V::parse(lexer, args.value_args.clone())?;
            entry_span = entry_span.merge(value_span);

            out.insert(key, value);
            
            span = Some(span.expect("span not set").merge(entry_span));

            if let Some(Ok(Token { kind: TokenKind::Symbol(symbol), .. })) = lexer.peek() {
                if symbol != &args.entry_separator {
                    break;
                }
            } else {
                break;
            }
        }

        Ok((out, span.unwrap_or(Span::ALL)))
    }

    fn is_first(token: &TokenKind) -> bool {
        K::is_first(token)
    }
}

impl Parse for TTypeId {
    type Args<'a> = ();

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let token = lexer.expect_peek()
            .map_err(|err| err.with_context(ParseContext::ParsingTTypeId))?
            .clone();

        let spanned_ttype_id = match token.kind {
            TokenKind::ScalarType(scalar_type) => {
                lexer.expect_token()?;
                (TTypeId::Scalar(scalar_type), token.span)
            },
            TokenKind::Keyword(Keyword::Struct | Keyword::Enum) => {
                let (composite_type, type_span) = CompositeType::parse(lexer, ())?;

                let span = token.span
                    .merge(type_span)
                    .extend(Some(1))
                    .prepend(Some(1));

                (TTypeId::Composite(CompositeTTypeId::Anonymous(Box::new(composite_type))), span)
            },
            TokenKind::Ident(ident) => {
                lexer.expect_token()?;
                let mut idents = vec![ident];

                let mut span = token.span;

                if let Some(_) = lexer.next_if_symbol(Symbol::PathSep)? {
                    let (ident_path, path_span) = IdentPath::parse(lexer, ())?;
                    span = span.merge(path_span);

                    for ident in ident_path {
                        idents.push(ident);
                    }
                }

                let path = IdentPath::try_from(idents).expect("invalid ident path");
                
                (TTypeId::Composite(CompositeTTypeId::Path(path)), span)
            },
            TokenKind::Symbol(Symbol::SquareBracketOpen) => {
                lexer.expect_token()?;
                let length = if let Some(Token { kind: TokenKind::ScalarLiteral(ScalarValue::Int64(length)), .. }) = lexer.next_if_token_tag(TokenTag::ScalarLiteral)? {
                    Some(length as u64)
                } else {
                    None
                };
                lexer.expect_symbol(Symbol::SquareBracketClose).map_err(|err| err.with_context(ParseContext::ParsingArrayType))?;

                let (inner_ttype_id, inner_span) = TTypeId::parse(lexer, ())?;

                let span = token.span.merge(inner_span);

                let array_type = ArrayType::new(inner_ttype_id, length);

                (array_type.into(), span)
            },
            kind => return Err(ParseError {
                span: token.span,
                context: Some(ParseContext::ParsingTTypeId),
                kind: ParseErrorKind::UnexpectedTokenKind(kind.clone()),
            }),
        };

        Ok(spanned_ttype_id)
    }

    fn is_first(token: &TokenKind) -> bool {
        match token {
            TokenKind::ScalarType(_) => true,
            TokenKind::Keyword(Keyword::Struct | Keyword::Enum) => true,
            TokenKind::Ident(_) => true,
            TokenKind::Symbol(Symbol::SquareBracketOpen) => true,
            _ => false,
        }
    }
}

impl Parse for IdentForest {
    type Args<'a> = ();

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (trees, list_span) = Vec::parse(lexer, ListArgs {
            args: (),
            separator: Symbol::Comma,
        }).map_err(|err| err.with_context(ParseContext::ParsingIdentForest))?;

        Ok((IdentForest::new(trees), list_span))
    }

    fn is_first(token: &TokenKind) -> bool {
        IdentTree::is_first(token)
    }
}

impl Parse for IdentTree {
    type Args<'a> = ();

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (ident, ident_span) = Ident::parse(lexer, ())
            .map_err(|err| err.with_context(ParseContext::ParsingIdentTree))?;
        
        if let None = lexer.next_if_token_kind(&TokenKind::Symbol(Symbol::Dot)).map_err(|err| err.with_context(ParseContext::ParsingIdentTree))? {
            return Ok((IdentTree::new(ident, IdentForest::empty()), ident_span));
        }

        let spanned_forest;
        if let Some(_) = lexer.next_if_token_kind(&TokenKind::Symbol(Symbol::BracketOpen)).map_err(|err| err.with_context(ParseContext::ParsingIdentTree))? {
            spanned_forest = IdentForest::parse(lexer, ()).map_err(|err| err.with_context(ParseContext::ParsingIdentTree))?;
            lexer.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingIdentTree))?;
        } else {
            let (ident_tree, ident_tree_span) = IdentTree::parse(lexer, ())?;
            let span = ident_span.merge(ident_tree_span);
            spanned_forest = (IdentForest::new([ident_tree]), span);
        }

        let (forest, forest_span) = spanned_forest;

        Ok((IdentTree::new(ident, forest), ident_span.merge(forest_span)))
    }

    fn is_first(token: &TokenKind) -> bool {
        Ident::is_first(token)
    }
}

impl Parse for StructLiteral {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (ttype_id, type_span) = TTypeId::parse(lexer, ()).map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;
        
        lexer.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;

        let (fields, fields_span) = BTreeMap::parse(lexer, MapArgs {
            key_args: (),
            value_args: args,
            map_seperator: Symbol::Colon,
            entry_separator: Symbol::Comma,
        }).map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;
        
        lexer.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;

        let span = type_span.merge(fields_span).extend(Some(1));

        Ok((StructLiteral::new(ttype_id, fields), span))
    }

    fn is_first(token: &TokenKind) -> bool {
        Ident::is_first(token) || token == &TokenKind::Keyword(Keyword::Struct)
    }
}

impl Parse for EnumLiteral {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (ttype_id, type_span) = TTypeId::parse(lexer, ()).map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;

        lexer.expect_symbol(Symbol::Hash).map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;

        let (tag, ident_span) = Ident::parse(lexer, ()).map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;

        lexer.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;
        
        let (expr, expr_span) = Expression::parse(lexer, args)?;
        
        lexer.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;

        let span = type_span.merge(expr_span).extend(Some(1));

        Ok((EnumLiteral::new(ttype_id, tag, expr), span))
    }

    fn is_first(token: &TokenKind) -> bool {
        Ident::is_first(token) || token == &TokenKind::Keyword(Keyword::Enum)
    }
}

impl Parse for CompositeLiteral {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<T: Iterator<Item = char>>(lexer: &mut Lexer<T>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        todo!()
    }

    fn is_first(token: &TokenKind) -> bool {
        todo!()
    }
}

pub(crate) struct Parser<T: Iterator<Item = char>> {
    lexer: Lexer<T>,
}

impl<T: Iterator<Item = char>> Parser<T> {
    #[allow(unused)]
    pub fn new(lexer: Lexer<T>) -> Self {
        Self {
            lexer,
        }
    }

    #[allow(unused)]
    pub fn parse_data_query(&mut self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet) -> Result<Expression, ParseError> {
        let (_, expr) = self.parse_expression(pager, registry, relations)?;
        
        if let Some(token) = self.lexer.next() {
            let token = token?;
            return Err(ParseError {
                span: token.span,
                context: Some(ParseContext::ParsingDataQuery),
                kind: ParseErrorKind::UnexpectedTokenKind(token.kind),
            });
        }

        Ok(expr)
    }

    pub fn parse_expression(&mut self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet) -> Result<(Span, Expression), ParseError> {
        let token = self.expect_peek()?;

        let spanned_expression = match token.kind.clone() {
            // op (arithmetic)
            TokenKind::Symbol(Symbol::Plus) => {
                let token = self.expect_token()?;
                let (left_span, left_expr) = self.parse_expression(pager.clone(), registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(pager.clone(), registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Arithmetic(ArithmeticOp::Add(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::Minus) => {
                let token = self.expect_token()?;
                let (left_span, left_expr) = self.parse_expression(pager.clone(), registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(pager.clone(), registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Arithmetic(ArithmeticOp::Sub(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::Asterisk) => {
                let token = self.expect_token()?;
                let (left_span, left_expr) = self.parse_expression(pager.clone(), registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(pager.clone(), registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Arithmetic(ArithmeticOp::Mul(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::ForwardSlash) => {
                let token = self.expect_token()?;
                let (left_span, left_expr) = self.parse_expression(pager.clone(), registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(pager.clone(), registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Arithmetic(ArithmeticOp::Div(
                    left_expr,
                    right_expr,
                )))))
            },
            // op (logical)
            TokenKind::Symbol(Symbol::And) => {
                let token = self.expect_token()?;
                let (left_span, left_expr) = self.parse_expression(pager.clone(), registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(pager.clone(), registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::And(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::Or) => {
                let token = self.expect_token()?;
                let (left_span, left_expr) = self.parse_expression(pager.clone(), registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(pager.clone(), registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Or(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::Not) => {
                let token = self.expect_token()?;
                let (expr_span, expr) = self.parse_expression(pager.clone(), registry, relations)?;

                let span = token.span.merge(expr_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Not(expr)))))
            },
            TokenKind::Symbol(Symbol::DoubleEqual) => {
                let token = self.expect_token()?;
                let (left_span, left_expr) = self.parse_expression(pager.clone(), registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(pager.clone(), registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Eq(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::LessThan) => {
                let token = self.expect_token()?;
                let (left_span, left_expr) = self.parse_expression(pager.clone(), registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(pager.clone(), registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Lt(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::GreaterThan) => {
                let token = self.expect_token()?;
                let (left_span, left_expr) = self.parse_expression(pager.clone(), registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(pager.clone(), registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Gt(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::LessThanOrEqual) => {
                let token = self.expect_token()?;
                let (left_span, left_expr) = self.parse_expression(pager.clone(), registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(pager.clone(), registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Lte(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::GreaterThanOrEqual) => {
                let token = self.expect_token()?;
                let (left_span, left_expr) = self.parse_expression(pager.clone(), registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(pager.clone(), registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Gte(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Keyword(Keyword::If) => {
                let token = self.expect_token()?;
                // if condition { then } else { otherwise } -> ret_type
                let (condition_span, condition) = self.parse_expression(pager.clone(), registry, relations)?;

                self.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
                let (then_span, then) = self.parse_expression(pager.clone(), registry, relations)?;
                self.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
                
                self.expect_keyword(Keyword::Else).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
                
                self.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
                let (otherwise_span, otherwise) = self.parse_expression(pager.clone(), registry, relations)?;
                self.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingIf))?;

                self.expect_symbol(Symbol::RightArrow).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
                let (ret_type_span, ret_type) = self.parse_ttype_id()?;

                let span = token.span
                    .merge(condition_span)
                    .merge(then_span)
                    .merge(otherwise_span)
                    .merge(ret_type_span);

                (span, Expression::ControlFlow(Box::new(ControlFlow::If(IfControlFlow {
                    condition,
                    ret_type,
                    then,
                    otherwise,
                }))))
            },
            TokenKind::Keyword(Keyword::Match) => {
                let token = self.expect_token()?;
                // match param { Tag(tag) => expr, ... } -> ret_type
                let (param_span, param) = self.parse_expression(pager.clone(), registry, relations)?;

                self.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingMatch))?;

                let (all_branches_span, branch_list) = self.parse_non_empty_list_with_rel_args(pager.clone(), registry, relations, Self::parse_branch, &TokenKind::Symbol(Symbol::Comma))?;

                let mut branches = btreemap! {};

                for (branch_span, (tag, branch)) in branch_list {
                    if branches.contains_key(&tag) {
                        return Err(ParseError {
                            span: branch_span,
                            context: Some(ParseContext::ParsingExpression),
                            kind: ParseErrorKind::DuplicateBranch(tag),
                        });
                    }
                    branches.insert(tag, branch);
                }

                self.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingMatch))?;

                self.expect_symbol(Symbol::RightArrow).map_err(|err| err.with_context(ParseContext::ParsingMatch))?;
                let (ret_span, ret_type) = self.parse_ttype_id()?;

                let span = token.span
                    .merge(param_span)
                    .merge(all_branches_span)
                    .merge(ret_span);

                (span, Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
                    param,
                    ret_type,
                    branches,
                }))))
            },
            TokenKind::Symbol(Symbol::Hash) => {
                let token = self.expect_token()?;
                let (_, relation) = self.expect_ident().map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                self.expect_symbol(Symbol::Dot).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                let (action_span, action) = self.expect_ident().map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                let action_str: &str = action.borrow();
                
                let (span, action) = match action_str {
                    "range" => {
                        self.expect_symbol(Symbol::LessThan).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (forest_span, ident_forest) = self.parse_ident_forest()?;
                        self.expect_symbol(Symbol::GreaterThan).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                        self.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (start_span, start) = self.parse_expression(pager.clone(), registry, relations)?;
                        self.expect_symbol(Symbol::Comma).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (end_span, end) = self.parse_expression(pager.clone(), registry, relations)?;
                        self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                        let mut span = token.span
                            .merge(forest_span)
                            .merge(start_span)
                            .merge(end_span)
                            .extend(Some(1));

                        (span, InterpreterAction::Range {
                            relation,
                            ident_forest,
                            start: Box::new(Bound::Included(start)), // todo: proper bounds
                            end: Box::new(Bound::Included(end)), // todo: proper bounds
                        })
                    },
                    "insert" => {
                        self.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (row_span, new_row) = self.parse_expression(pager.clone(), registry, relations)?;
                        self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                        let span = token.span
                            .merge(row_span)
                            .extend(Some(1));

                        (span, InterpreterAction::Insert {
                            relation,
                            new_row: Box::new(new_row),
                        })
                    },
                    "extend" => {
                        self.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (row_span, new_rows) = self.parse_expression(pager.clone(), registry, relations)?;
                        self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                        let span = token.span
                            .merge(row_span)
                            .extend(Some(1));

                        (span, InterpreterAction::Extend {
                            relation,
                            new_rows: Box::new(new_rows),
                        })
                    },
                    "remove" => {
                        self.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (row_span, pkey) = self.parse_expression(pager.clone(), registry, relations)?;
                        self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                        let span = token.span
                            .merge(row_span)
                            .extend(Some(1));

                        (span, InterpreterAction::Remove {
                            relation,
                            pkey: Box::new(pkey),
                        })
                    },
                    "retain" => {
                        self.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (function_span, function) = self.parse_function(pager.clone(), registry, relations)?;
                        self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                        let span = token.span
                            .merge(function_span)
                            .extend(Some(1));

                        (span, InterpreterAction::Retain {
                            relation,
                            condition: Box::new(function),
                        })
                    },
                    _ => return Err(ParseError {
                        span: action_span,
                        context: Some(ParseContext::ParsingAction),
                        kind: ParseErrorKind::ExpectedTokenKindOneOf {
                            expected: [
                                TokenKind::Ident(id!("range")),
                                TokenKind::Ident(id!("insert")),
                                TokenKind::Ident(id!("extend")),
                                TokenKind::Ident(id!("remove")),
                                TokenKind::Ident(id!("retain")),
                            ].into(),
                            got: TokenKind::Ident(action),
                        },
                    }),
                };

                (span, Expression::Action(action))
            }
            // function invocation or nested ident
            TokenKind::Ident(ident) => {
                let token = self.expect_token()?;
                let mut idents = vec![ident];

                let next_token = self.lexer.peek();

                match next_token {
                    Some(Ok(Token { kind: TokenKind::Symbol(Symbol::PathSep), .. })) => {
                        self.lexer.next();
                        let (ident_path_span, ident_path) = self.parse_ident_path()?;
                        for ident in ident_path {
                            idents.push(ident);
                        }
                        let path = IdentPath::try_from(idents).expect("invalid ident path");

                        self.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingFunctionInvocation))?;
                        let (arg_list_span, args) = self.parse_non_empty_list_with_rel_args(
                            pager, registry, relations,
                            Self::parse_expression,
                            &TokenKind::Symbol(Symbol::Comma)
                        )?;
                        self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingFunctionInvocation))?;

                        let args = args.into_iter().map(|(_, arg)| arg).collect_vec();

                        let span = token.span
                            .merge(ident_path_span)
                            .merge(arg_list_span)
                            .extend(Some(1));
                        
                        (span, Expression::FunctionInvocation(FunctionInvocation::new(path, args)))
                    },
                    Some(Ok(Token { kind: TokenKind::Symbol(Symbol::BracketOpen), .. })) => {
                        let (arg_list_span, args) = self.parse_non_empty_list_with_rel_args(
                            pager, registry, relations,
                            Self::parse_expression,
                            &TokenKind::Symbol(Symbol::Comma)
                        )?;
                        self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingFunctionInvocation))?;

                        let args = args.into_iter().map(|(_, span)| span).collect_vec();
                        
                        let path = IdentPath::try_from(idents).expect("invalid ident path");

                        let span = token.span
                            .merge(arg_list_span)
                            .extend(Some(1));

                        (span, Expression::FunctionInvocation(FunctionInvocation::new(path, args)))
                    }
                    Some(Ok(Token { kind: TokenKind::Symbol(Symbol::Dot), .. })) => {
                        self.lexer.next();

                        let (nested_ident_span, nested_ident) = self.parse_nested_ident()?;

                        for ident in nested_ident {
                            idents.push(ident);
                        }
                        let nested_ident = NestedIdent::try_from(idents).expect("invalid nested ident");

                        let span = token.span
                            .merge(nested_ident_span);

                        (span, Expression::NestedIdent(nested_ident))
                    },
                    _ => {
                        let nested_ident = NestedIdent::try_from(idents).expect("invalid nested ident");
                        (token.span, Expression::NestedIdent(nested_ident))
                    },
                }
            }
            _ => {
                let (span, value) = self.parse_literal(pager, registry, relations).map_err(|err| err.with_context(ParseContext::ParsingExpression))?;
                (span, Expression::Literal(value))
            },
        };

        Ok(spanned_expression)
    }

    pub fn parse_function(&mut self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet) -> Result<(Span, Function), ParseError> {
        // (args, ...)
        self.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingFunction))?;
        let (arg_span, args) = self.parse_non_empty_list(Self::parse_function_arg, &TokenKind::Symbol(Symbol::Comma))?;
        let args = args.into_iter().map(|(_, arg)| arg).collect_vec();
        self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingFunction))?;

        // -> result_type
        self.expect_symbol(Symbol::RightArrow).map_err(|err| err.with_context(ParseContext::ParsingFunction))?;
        let (result_span, result_ttype_id) = self.parse_ttype_id()?;
        
        // { expression }
        self.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingFunction))?;
        let (expr_span, expression) = self.parse_expression(pager.clone(), registry, relations)?;
        self.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingFunction))?;

        let span = arg_span
            .merge(result_span)
            .merge(expr_span)
            .extend(Some(1))
            .prepend(Some(1));

        let function = Function::new(pager, registry, relations, args, result_ttype_id, expression).map_err(|err| ParseError {
            span,
            context: Some(ParseContext::ParsingFunction),
            kind: err.into(),
        })?;

        Ok((span, function))
    }

    pub fn parse_function_arg(&mut self) -> Result<(Span, FunctionArg), ParseError> {
        let (ident_span, ident) = self.expect_ident().map_err(|err| err.with_context(ParseContext::ParsingFunctionArg))?;
        self.expect_symbol(Symbol::Colon).map_err(|err| err.with_context(ParseContext::ParsingFunctionArg))?;
        let (type_span, ttype_id) = self.parse_ttype_id()?;

        let span = ident_span.merge(type_span);

        Ok((span, FunctionArg::new(ident, ttype_id)))
    }

    pub fn parse_branch(&mut self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet) -> Result<(Span, (Ident, Branch)), ParseError> {
        let (tag_span, tag) = self.expect_ident().map_err(|err| err.with_context(ParseContext::ParsingBranch))?;
        self.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingBranch))?;
        let (ident_span, ident) = self.expect_ident().map_err(|err| err.with_context(ParseContext::ParsingBranch))?;
        self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingBranch))?;
        self.expect_symbol(Symbol::RightFatArrow).map_err(|err| err.with_context(ParseContext::ParsingBranch))?;
        let (expr_span, expression) = self.parse_expression(pager, registry, relations)?;

        let span = tag_span.merge(ident_span).merge(expr_span);

        Ok((span, (tag, Branch::new(ident, expression))))
    }

    pub fn parse_literal(&mut self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet) -> Result<(Span, Literal), ParseError> {
        let next_peek = self.expect_peek()
            .map_err(|err| err.with_context(ParseContext::ParsingLiteral))?
            .clone();

        let spanned_value = match next_peek.kind {
            TokenKind::Symbol(Symbol::Minus) | TokenKind::Symbol(Symbol::Plus) => {
                let sign_token = self.expect_token().map_err(|err| err.with_context(ParseContext::ParsingScalarLiteral))?;

                let token = self.expect_token().map_err(|err| err.with_context(ParseContext::ParsingScalarLiteral))?;

                let sign = (sign_token.kind == TokenKind::Symbol(Symbol::Plus)) as i8 * 2 - 1;

                let value = match token.kind {
                    TokenKind::ScalarLiteral(ScalarValue::Int32(number)) => ScalarValue::Int32(number * sign as i32),
                    TokenKind::ScalarLiteral(ScalarValue::Int64(number)) => ScalarValue::Int64(number * sign as i64),
                    kind => return Err(ParseError {
                        span: token.span,
                        context: Some(ParseContext::ParsingScalarLiteral),
                        kind: ParseErrorKind::ExpectedScalarLiteralTypeOneOf {
                            expected: [
                                ScalarType::Int32,
                                ScalarType::Int64,
                            ].into(),
                            got: kind,
                        },
                    }),
                };

                (token.span, Literal::Scalar(value))
            },
            
            TokenKind::ScalarLiteral(_) => {
                let token = self.expect_token().map_err(|err| err.with_context(ParseContext::ParsingLiteral))?;
                let TokenKind::ScalarLiteral(value) = token.kind else { unreachable!() };

                (token.span, Literal::Scalar(value))
            },
            _ => { // not a scalar, either composite or array, which both start with type id
                let (type_span, ttype_id) = self.parse_ttype_id()?;

                if let TTypeId::Scalar(scalar_type) = &ttype_id {
                    return Err(ParseError {
                        span: type_span,
                        context: Some(ParseContext::ParsingCompositeLiteral),
                        kind: ParseErrorKind::ExpectedTokenTagGotKind {
                            expected: TokenTag::Ident,
                            got: TokenKind::ScalarType(*scalar_type),
                        },
                    });
                }

                let ttype = registry.ttype(&ttype_id).ok_or_else(|| ParseError {
                    span: type_span,
                    context: Some(ParseContext::ParsingLiteral),
                    kind: TypeError::TypeNotFound(ttype_id.clone()).into(),
                })?;

                match ttype {
                    TType::Scalar(scalar_type) => return Err(ParseError {
                        span: type_span,
                        context: Some(ParseContext::ParsingLiteral),
                        kind: TypeError::TypeSetInvalid {
                            expected: TypeSet::Composite, // todo: make composite or array once proper errors done
                            got: scalar_type.into(),
                        }.into(),
                    }),
                    TType::Composite(CompositeType::Struct(_)) => {
                        let (struct_span, struct_value) = self.parse_struct_contents(pager, registry, relations, ttype_id)?;
                        (type_span.merge(struct_span), struct_value.into())
                    },
                    TType::Composite(CompositeType::Enum(_)) => {
                        self.expect_symbol(Symbol::Hash).map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;
                        let (struct_span, struct_value) = self.parse_enum_contents(pager, registry, relations, ttype_id)?;
                        (type_span.merge(struct_span), struct_value.into())
                    },
                    TType::Array(_) => {
                        self.expect_symbol(Symbol::SquareBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingArrayLiteral))?;

                        let (list_span, entries) = self.parse_non_empty_list_with_rel_args(pager, registry, relations, Self::parse_expression, &TokenKind::Symbol(Symbol::Comma))?;
                        let entries = entries.into_iter().map(|(_, value)| value).collect_vec();

                        self.expect_symbol(Symbol::SquareBracketClose).map_err(|err| err.with_context(ParseContext::ParsingArrayLiteral))?;

                        let span = type_span.merge(list_span).extend(Some(1));

                        let array_value = ArrayLiteral::new(ttype_id, entries);

                        (span, array_value.into())
                    },
                }
            }
        };

        Ok(spanned_value)
    }

    pub fn parse_struct_contents(&mut self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, ttype_id: TTypeId) -> Result<(Span, StructLiteral), ParseError> {
        self.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;
        
        let (list_span, field_list) = self.parse_non_empty_list_with_rel_args(
            pager, registry, relations,
            Self::parse_field_value,
            &TokenKind::Symbol(Symbol::Comma)
        )?;
        let field_list = field_list.into_iter().map(|(_, field)| field);
        
        self.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;

        let span = list_span.extend(Some(1)).prepend(Some(1));

        let mut fields = btreemap! {};
        for (ident, value) in field_list {
            if fields.contains_key(&ident) {
                return Err(ParseError {
                    span,
                    context: Some(ParseContext::ParsingStructLiteral),
                    kind: ParseErrorKind::DuplicateKey(ident),
                });
            }
            fields.insert(ident, value);
        }

        let struct_value = StructLiteral::new(ttype_id, fields);

        Ok((span, struct_value))
    }

    pub fn parse_enum_contents(&mut self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet, ttype_id: TTypeId) -> Result<(Span, EnumLiteral), ParseError> {
        let (span, (tag, value)) = self.parse_tag_value(pager, registry, relations)?;
        
        let enum_value = EnumLiteral::new(ttype_id, tag, value);
        
        Ok((span, enum_value))
    }

    pub fn parse_field_value(&mut self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet) -> Result<(Span, (Ident, Expression)), ParseError> {
        let (ident_span, ident) = self.expect_ident().map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;
        self.expect_symbol(Symbol::Colon).map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;
        let (value_span, value) = self.parse_expression(pager, registry, relations)?;

        let span = ident_span.merge(value_span);

        Ok((span, (ident, value)))
    }

    pub fn parse_tag_value(&mut self, pager: Arc<Mutex<Pager>>, registry: &Registry, relations: &DbRelationSet) -> Result<(Span, (Ident, Expression)), ParseError> {
        let (ident_span, ident) = self.expect_ident().map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;
        
        self.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;
        let (value_span, value) = self.parse_expression(pager, registry, relations)?;
        self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;

        let span = ident_span.merge(value_span).extend(Some(1));

        Ok((span, (ident, value)))
    }

    pub fn expect_int32(&mut self) -> Result<(Span, i32), ParseError> {
        let token = self.expect_token()?;
        let TokenKind::ScalarLiteral(ScalarValue::Int32(number)) = token.kind else {
            return Err(ParseError {
                span: token.span,
                context: None,
                kind: ParseErrorKind::ExpectedScalarLiteralType {
                    expected: ScalarType::Int32,
                    got: token.kind,
                },
            });
        };
        Ok((token.span, number))
    }

    pub fn expect_int64(&mut self) -> Result<(Span, i64), ParseError> {
        let token = self.expect_token()?;
        let TokenKind::ScalarLiteral(ScalarValue::Int64(number)) = token.kind else {
            return Err(ParseError {
                span: token.span,
                context: None,
                kind: ParseErrorKind::ExpectedScalarLiteralType {
                    expected: ScalarType::Int64,
                    got: token.kind,
                },
            });
        };
        Ok((token.span, number))
    }

    pub fn expect_string(&mut self) -> Result<(Span, String), ParseError> {
        let token = self.expect_token()?;
        let TokenKind::ScalarLiteral(ScalarValue::String(string)) = token.kind else {
            return Err(ParseError {
                span: token.span,
                context: None,
                kind: ParseErrorKind::ExpectedScalarLiteralType {
                    expected: ScalarType::String,
                    got: token.kind,
                },
            });
        };
        Ok((token.span, string))
    }

    pub fn next_if_ident(&mut self) -> Result<Option<(Span, Ident)>, ParseError> {
        let Some(token) = self.next_if_token_tag(TokenTag::Ident)? else { return Ok(None) };
        let TokenKind::Ident(ident) = token.kind else { unreachable!() };
        Ok(Some((token.span, ident)))
    }
}

#[cfg(test)]
mod tests {
    use std::{ops::Bound, sync::{Arc, Mutex}};

    use crate::{db::{pager::Pager, registry::Registry, DbRelationSet}, query::lexer::Lexer};

    use super::Parser;

    #[test]
    fn test() {
        let string = "
        []struct { active: bool, id: int32, name: string } [
            struct { active: bool, id: int32, name: string } {
                active: false,
                id: - 0i32 1_i32, // 0 - 1 instead of just -1 as negation operator isn't finished
                name: \"El Jones, Jim\"
            },
            struct { active: bool, id: int32, name: string } {
                active: true,
                id: 1_i32,
                name: \"Jim Jones\"
            },
            struct { active: bool, id: int32, name: string } {
                active: true,
                id: 2_i32,
                name: \"Jimboni Jonesi\"
            }
        ]";

        let pager = Arc::new(Mutex::new(Pager::new_memory()));
        let relations = DbRelationSet::new();
        let registry = Registry::new(pager.clone(), &relations);

        let lexer = Lexer::new(string.chars());
        let mut parser = Parser::new(lexer);
        match parser.parse_expression(pager, &registry, &relations) {
            Ok((_, expr)) => {
                // i aint writing a test for this
                dbg!(expr);
            },
            Err(err) => {
                let rest = match err.span.end_bound() {
                    Bound::Included(i) => i + 1,
                    Bound::Excluded(i) => i,
                    Bound::Unbounded => string.len(),
                };
                let rest = &string[rest..];
                println!("{}\x1b[31;1m{}\x1b[0m{}", &string[..err.span.start], &string[err.span.into_range()], rest);
                panic!("{err}")
            },
        }
    }
}
