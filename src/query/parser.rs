use std::{borrow::Borrow, collections::BTreeMap, fmt::{Debug, Display}, hash::Hash, ops::Bound, sync::{Arc, Mutex}};

use codb_core::{Ident, IdentForest, IdentPath, IdentTree, NestedIdent};
use indexmap::IndexMap;

use crate::{db::{pager::Pager, registry::{CompositeTTypeId, Registry, TTypeId}, DbRelationSet}, expression::{ArithmeticOp, ArrayLiteral, Branch, CompositeLiteral, CompositeLiteralInner, ControlFlow, EnumLiteral, Expression, FunctionInvocation, IfControlFlow, InterpreterAction, Literal, LogicalOp, MatchControlFlow, Op, StructLiteral}, query::{lex::{TokenKind, TokenSlice}, schema_query::{ModuleSchemaQuery, RelationSchemaQuery, SchemaQuery, TypeSchemaQuery}, DataQuery}, typesystem::{function::Function, ttype::{ArrayType, CompositeType, EnumType, ScalarType, StructType}, value::ScalarValue, TypeError}};

use super::{lex::{Keyword, LexErrorKind, Symbol, Token, TokenTag}, Span};

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
    ParsingDataQuery,
    ParsingSchemaQuery,
    ParsingExpression,
    ParsingOperation,
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
            ParseContext::ParsingDataQuery => write!(f, "parsing data query"),
            ParseContext::ParsingSchemaQuery => write!(f, "parsing schema query"),
            ParseContext::ParsingExpression => write!(f, "parsing expression"),
            ParseContext::ParsingOperation => write!(f, "parsing operation"),
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

pub(crate) trait Parse {
    type Args<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized;
}

#[derive(Clone)]
pub(crate) struct ExpressionArgs<'a> {
    pub pager: Arc<Mutex<Pager>>,
    pub registry: &'a Registry,
    pub relations: &'a DbRelationSet,
}

impl Parse for DataQuery {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (expr, span) = Expression::parse(tokens, args)?;
        
        if let Some(token) = tokens.next() {
            return Err(ParseError {
                span: token.span,
                context: Some(ParseContext::ParsingDataQuery),
                kind: ParseErrorKind::UnexpectedTokenKind(token.kind),
            });
        }

        Ok((DataQuery(expr), span))
    }
}

impl Parse for SchemaQuery {
    type Args<'a> = ();

    fn parse<'a>(tokens: &mut TokenSlice<'a>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let keyword_token = tokens.expect_token_tag(TokenTag::Keyword)?;
        let TokenKind::Keyword(keyword) = keyword_token.kind else { unreachable!() };
        
        match keyword {
            Keyword::Module => {
                let (name, name_span) = IdentPath::parse(tokens, ()).map_err(|err| err.with_context(ParseContext::ParsingSchemaQuery))?;
                Ok((SchemaQuery::Module(ModuleSchemaQuery::Create {
                    name,
                }), keyword_token.span.merge(name_span)))
            },
            Keyword::Type => {
                let (name, _) = IdentPath::parse(tokens, ()).map_err(|err| err.with_context(ParseContext::ParsingSchemaQuery))?;
                tokens.expect_symbol(Symbol::Equal)?;
                let (ttype, ttype_span) = CompositeType::parse(tokens, ()).map_err(|err| err.with_context(ParseContext::ParsingSchemaQuery))?;
                Ok((SchemaQuery::Type(TypeSchemaQuery::Create {
                    name,
                    ttype,
                }), keyword_token.span.merge(ttype_span)))
            },
            Keyword::Relation => {
                let (name, _) = Ident::parse(tokens, ()).map_err(|err| err.with_context(ParseContext::ParsingSchemaQuery))?;
                tokens.expect_symbol(Symbol::Equal).map_err(|err| err.with_context(ParseContext::ParsingSchemaQuery))?;
                let (ttype, _) = StructType::parse(tokens, ())?;
                
                tokens.expect_symbol(Symbol::LessThan).map_err(|err| err.with_context(ParseContext::ParsingSchemaQuery))?;
                let (pkey, _) = IdentForest::parse(tokens, ())?;
                let final_token = tokens.expect_symbol(Symbol::GreaterThan).map_err(|err| err.with_context(ParseContext::ParsingSchemaQuery))?;

                Ok((SchemaQuery::Relation(RelationSchemaQuery::Create {
                    name,
                    ttype,
                    pkey,
                }), keyword_token.span.merge(final_token.span)))
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
}

impl Parse for Ident {
    type Args<'a> = ();

    fn parse<'a>(tokens: &mut TokenSlice<'a>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let token = tokens.expect_token_tag(TokenTag::Ident)?;
        let TokenKind::Ident(ident) = token.kind else { unreachable!() };
        Ok((ident, token.span))
    }
}

pub(crate) struct WrappedArgs<'a, P: Parse> {
    args: P::Args<'a>,
    open: Symbol,
    close: Symbol,
}

impl<'a, P: Parse> Clone for WrappedArgs<'a, P> where P::Args<'a>: Clone {
    fn clone(&self) -> Self {
        Self {
            args: self.args.clone(),
            open: self.open,
            close: self.close,
        }
    }
}

pub(crate) struct Wrapped<P: Parse>(P);

impl<P: Parse> Wrapped<P> {
    pub fn unwrap(self) -> P {
        self.0
    }
}

impl<P: Parse> Parse for Wrapped<P> {
    type Args<'a> = WrappedArgs<'a, P>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        tokens.expect_symbol(args.open)?;
        let (item, span) = P::parse(tokens, args.args)?;
        tokens.expect_symbol(args.close)?;
        Ok((Wrapped(item), span))
    }
}

impl<P: Parse> Parse for Option<P> where for<'a> P::Args<'a>: Clone {
    type Args<'a> = P::Args<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        match P::parse(&mut tokens.clone(), args.clone()) {
            Ok(_) => {
                let (item, span) = P::parse(tokens, args)?;
                Ok((Some(item), span))
            },
            Err(err) => Ok((None, Span::new(err.span.start, err.span.start))),
        }
    }
}

pub(crate) struct ListArgs<'a, P: Parse> {
    args: P::Args<'a>,
    separator: Symbol,
    allow_trailing: bool,
}

impl<P: Parse> Parse for Vec<P> where for<'a> P::Args<'a>: Clone {
    type Args<'a> = ListArgs<'a, P>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let mut out = Vec::new();
        let mut span: Option<Span> = None;
        
        while let Some(_) = tokens.peek() {
            if let Err(_) = P::parse(&mut tokens.clone(), args.args.clone()) {
                // if not first then must succeed if trailing separator not allowed
                if out.is_empty() || args.allow_trailing {
                    break;
                }
            }

            let (item, item_span) = P::parse(tokens, args.args.clone())?;
            span = Some(span
                .map(|span| span.merge(item_span))
                .unwrap_or(item_span)
            );

            out.push(item);

            // only continue if there is a separator
            if let Some(Token { kind: TokenKind::Symbol(symbol), .. }) = tokens.peek() {
                if symbol != &args.separator {
                    break;
                }
            } else {
                break;
            }
            tokens.next();
        }

        Ok((out, span.unwrap_or(Span::ALL)))
    }
}

struct NonEmpty<P: Parse> {
    head: P,
    tail: Vec<P>,
}

impl<P: Parse> Parse for NonEmpty<P> where for<'a> P::Args<'a>: Clone {
    type Args<'a> = ListArgs<'a, P>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (head, head_span) = P::parse(tokens, args.args.clone())?;
        if let Some(_) = tokens.next_if_symbol(args.separator) {
            let (tail, tail_span) = Vec::<P>::parse(tokens, args)?;
            
            Ok((Self {
                head,
                tail,
            }, head_span.merge(tail_span)))
        } else {
            Ok((Self {
                head,
                tail: vec![],
            }, head_span))
        }
    }
}

pub(crate) struct MapArgs<'a, K: Parse, V: Parse> {
    key_args: K::Args<'a>,
    value_args: V::Args<'a>,
    map_separator: Option<Symbol>,
    entry_separator: Option<Symbol>,
}

impl<K: Parse + Hash + PartialEq + Eq + Debug, V: Parse> Parse for IndexMap<K, V>
where for<'a> K::Args<'a>: Clone, for<'a> V::Args<'a>: Clone {
    type Args<'a> = MapArgs<'a, K, V>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let mut out = IndexMap::new();
        let mut span: Option<Span> = None;
        
        while let Some(token) = tokens.peek() {
            span = Some(token.span);

            if let Err(_) = K::parse(&mut tokens.clone(), args.key_args.clone()) {
                break;
            }

            let (key, mut entry_span) = K::parse(tokens, args.key_args.clone())?;

            if out.contains_key(&key) {
                return Err(ParseError {
                    span: span.unwrap_or(Span::ALL),
                    context: Some(ParseContext::ParsingStructType),
                    kind: ParseErrorKind::DuplicateKey(format!("{key:?}")),
                })
            }

            if let Some(map_separator) = args.map_separator {
                tokens.expect_symbol(map_separator)?;
            }

            let (value, value_span) = V::parse(tokens, args.value_args.clone())?;
            entry_span = entry_span.merge(value_span);

            out.insert(key, value);
            
            span = Some(span.expect("span not set").merge(entry_span));

            if let Some(entry_separator) = args.entry_separator {
                if let Some(Token { kind: TokenKind::Symbol(symbol), .. }) = tokens.peek() {
                    if symbol != &entry_separator {
                        break;
                    }
                } else {
                    break;
                }
                tokens.next();
            }
        }

        Ok((out, span.unwrap_or(Span::ALL)))
    }
}

impl<K: Parse + Hash + Ord + Debug, V: Parse> Parse for BTreeMap<K, V>
where for<'a> K::Args<'a>: Clone, for<'a> V::Args<'a>: Clone {
    type Args<'a> = MapArgs<'a, K, V>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let mut out = BTreeMap::new();
        let mut span: Option<Span> = None;
        
        while let Some(token) = tokens.peek() {
            span = Some(token.span);

            if let Err(_) = K::parse(&mut tokens.clone(), args.key_args.clone()) {
                break;
            }

            let (key, mut entry_span) = K::parse(tokens, args.key_args.clone())?;

            if out.contains_key(&key) {
                return Err(ParseError {
                    span: span.unwrap_or(Span::ALL),
                    context: Some(ParseContext::ParsingStructType),
                    kind: ParseErrorKind::DuplicateKey(format!("{key:?}")),
                })
            }

            if let Some(map_separator) = args.map_separator {
                tokens.expect_symbol(map_separator)?;
            }

            let (value, value_span) = V::parse(tokens, args.value_args.clone())?;
            entry_span = entry_span.merge(value_span);

            out.insert(key, value);
            
            span = Some(span.expect("span not set").merge(entry_span));

            if let Some(entry_separator) = args.entry_separator {
                if let Some(Token { kind: TokenKind::Symbol(symbol), .. }) = tokens.peek() {
                    if symbol != &entry_separator {
                        break;
                    }
                } else {
                    break;
                }
                tokens.next();
            }
        }

        Ok((out, span.unwrap_or(Span::ALL)))
    }
}

impl Parse for IdentPath {
    type Args<'a> = ();

    fn parse<'a>(tokens: &mut TokenSlice<'a>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (idents, list_span) = NonEmpty::parse(tokens, ListArgs {
            args: (),
            separator: Symbol::PathSep,
            allow_trailing: false,
        })
            .map_err(|err| err.with_context(ParseContext::ParsingIdentPath))?;
        
        let mut ident_list = vec![idents.head];
        ident_list.extend(idents.tail);

        let ident_path = IdentPath::try_from(ident_list).expect("invalid ident path");

        Ok((ident_path, list_span))
    }
}

impl Parse for NestedIdent {
    type Args<'a> = ();

    fn parse<'a>(tokens: &mut TokenSlice<'a>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (ident_list, list_span) = NonEmpty::parse(tokens, ListArgs {
            args: (),
            separator: Symbol::Dot,
            allow_trailing: false,
        }).map_err(|err| err.with_context(ParseContext::ParsingNestedIdent))?;

        let mut idents = vec![ident_list.head];
        idents.extend(ident_list.tail);
        
        let nested_ident = NestedIdent::try_from(idents).expect("invalid nested ident");

        Ok((nested_ident, list_span))
    }
}

impl Parse for CompositeType {
    type Args<'a> = ();

    fn parse<'a>(tokens: &mut TokenSlice<'a>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let token = tokens.expect_peek().map_err(|err| err.with_context(ParseContext::ParsingCompositeTTypeId))?;
        match &token.kind {
            TokenKind::Keyword(Keyword::Struct) => {
                let (struct_type, struct_type_span) = StructType::parse(tokens, ())?;
                Ok((CompositeType::Struct(struct_type), struct_type_span))
            },
            TokenKind::Keyword(Keyword::Enum) => {
                let (enum_type, enum_type_span) = EnumType::parse(tokens, ())?;
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
}

impl Parse for StructType {
    type Args<'a> = ();

    fn parse<'a>(tokens: &mut TokenSlice<'a>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let struct_token = tokens.expect_keyword(Keyword::Struct).map_err(|err| err.with_context(ParseContext::ParsingStructType))?;
        tokens.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingStructType))?;

        let (fields, fields_span) = IndexMap::parse(tokens, MapArgs {
            key_args: (),
            value_args: (),
            map_separator: Some(Symbol::Colon),
            entry_separator: Some(Symbol::Comma),
        })?;
        
        let bracket_token = tokens.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingStructType))?;

        let span = struct_token.span.merge(fields_span).merge(bracket_token.span);

        Ok((StructType::new(fields), span))
    }
}

impl Parse for EnumType {
    type Args<'a> = ();

    fn parse<'a>(tokens: &mut TokenSlice<'a>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let enum_token = tokens.expect_keyword(Keyword::Enum).map_err(|err| err.with_context(ParseContext::ParsingEnumType))?;
        tokens.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingEnumType))?;

        let (wrapped_tags, tags_span) = IndexMap::<Ident, Option<Wrapped<TTypeId>>>::parse(tokens, MapArgs {
            key_args: (),
            value_args: WrappedArgs {
                args: (),
                open: Symbol::BracketOpen,
                close: Symbol::BracketClose,
            },
            map_separator: None,
            entry_separator: Some(Symbol::Comma),
        })?;

        let tags = IndexMap::from_iter(wrapped_tags.into_iter().map(|(tag, ttype_id)|
            (tag, match ttype_id {
                Some(Wrapped(ttype_id)) => ttype_id,
                None => TTypeId::UNIT,
            })
        ));

        let bracket_token = tokens.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingEnumType))?;

        let span = enum_token.span.merge(tags_span).merge(bracket_token.span);

        Ok((EnumType::new(tags), span))
    }
}

impl Parse for TTypeId {
    type Args<'a> = ();

    fn parse<'a>(tokens: &mut TokenSlice<'a>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let token = tokens.expect_peek()
            .map_err(|err| err.with_context(ParseContext::ParsingTTypeId))?
            .clone();

        let spanned_ttype_id = match token.kind {
            TokenKind::ScalarLiteral(ScalarValue::Unit) => {
                tokens.expect_token()?;
                (TTypeId::Scalar(ScalarType::Unit), token.span)
            },
            TokenKind::ScalarType(scalar_type) => {
                tokens.expect_token()?;
                (TTypeId::Scalar(scalar_type), token.span)
            },
            TokenKind::Keyword(Keyword::Struct | Keyword::Enum) => {
                let (composite_type, span) = CompositeType::parse(tokens, ())?;

                (TTypeId::Composite(CompositeTTypeId::Anonymous(Box::new(composite_type))), span)
            },
            TokenKind::Ident(_) => {
                let (path, span) = IdentPath::parse(tokens, ()).map_err(|err| err.with_context(ParseContext::ParsingTTypeId))?;
                
                (TTypeId::Composite(CompositeTTypeId::Path(path)), span)
            },
            TokenKind::Symbol(Symbol::SquareBracketOpen) => {
                tokens.expect_token()?;
                let length = if let Some(Token { kind: TokenKind::ScalarLiteral(ScalarValue::Int64(length)), .. }) = tokens.next_if_token_tag(TokenTag::ScalarLiteral) {
                    Some(length as u64)
                } else {
                    None
                };
                tokens.expect_symbol(Symbol::SquareBracketClose).map_err(|err| err.with_context(ParseContext::ParsingArrayType))?;

                let (inner_ttype_id, inner_span) = TTypeId::parse(tokens, ())?;

                let span = token.span.merge(inner_span);

                let array_type = ArrayType::new(inner_ttype_id, length);

                (TTypeId::new_anonymous(array_type.into()), span)
            },
            kind => return Err(ParseError {
                span: token.span,
                context: Some(ParseContext::ParsingTTypeId),
                kind: ParseErrorKind::UnexpectedTokenKind(kind.clone()),
            }),
        };

        Ok(spanned_ttype_id)
    }
}

impl Parse for IdentForest {
    type Args<'a> = ();

    fn parse<'a>(tokens: &mut TokenSlice<'a>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (trees, list_span) = Vec::parse(tokens, ListArgs {
            args: (),
            separator: Symbol::Comma,
            allow_trailing: true,
        }).map_err(|err| err.with_context(ParseContext::ParsingIdentForest))?;

        Ok((IdentForest::new(trees), list_span))
    }
}

impl Parse for IdentTree {
    type Args<'a> = ();

    fn parse<'a>(tokens: &mut TokenSlice<'a>, _args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (ident, ident_span) = Ident::parse(tokens, ())
            .map_err(|err| err.with_context(ParseContext::ParsingIdentTree))?;
        
        if let None = tokens.next_if_token_kind(&TokenKind::Symbol(Symbol::Dot)) {
            return Ok((IdentTree::new(ident, IdentForest::empty()), ident_span));
        }

        let spanned_forest;
        if let Some(_) = tokens.next_if_token_kind(&TokenKind::Symbol(Symbol::BracketOpen)) {
            spanned_forest = IdentForest::parse(tokens, ()).map_err(|err| err.with_context(ParseContext::ParsingIdentTree))?;
            tokens.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingIdentTree))?;
        } else {
            let (ident_tree, ident_tree_span) = IdentTree::parse(tokens, ())?;
            let span = ident_span.merge(ident_tree_span);
            spanned_forest = (IdentForest::new([ident_tree]), span);
        }

        let (forest, forest_span) = spanned_forest;

        Ok((IdentTree::new(ident, forest), ident_span.merge(forest_span)))
    }
}

impl Parse for Expression {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let token = tokens.expect_peek()?.clone();

        let spanned_expression = match token.kind {
            TokenKind::Symbol(Symbol::Plus) | TokenKind::Symbol(Symbol::Minus) |
            TokenKind::Symbol(Symbol::Asterisk) | TokenKind::Symbol(Symbol::ForwardSlash) |
            TokenKind::Symbol(Symbol::And) | TokenKind::Symbol(Symbol::Or) |
            TokenKind::Symbol(Symbol::Not) | TokenKind::Symbol(Symbol::DoubleEqual) |
            TokenKind::Symbol(Symbol::LessThan) | TokenKind::Symbol(Symbol::GreaterThan) |
            TokenKind::Symbol(Symbol::LessThanOrEqual) | TokenKind::Symbol(Symbol::GreaterThanOrEqual) => {
                let (op, span) = Op::parse(tokens, args)?;
                (Expression::Op(Box::new(op)), span)
            },
            TokenKind::Keyword(Keyword::If) => {
                let (cf, span) = IfControlFlow::parse(tokens, args)?;
                (Expression::ControlFlow(Box::new(ControlFlow::If(cf))), span)
            },
            TokenKind::Keyword(Keyword::Match) => {
                let (cf, span) = MatchControlFlow::parse(tokens, args)?;
                (Expression::ControlFlow(Box::new(ControlFlow::Match(cf))), span)
            },
            TokenKind::Symbol(Symbol::Hash) => {
                let token = tokens.expect_token()?;
                let (relation, _) = Ident::parse(tokens, ()).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                tokens.expect_symbol(Symbol::Dot).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                let (action, action_span) = Ident::parse(tokens, ()).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                let action_str: &str = action.borrow();
                
                let (action, span) = match action_str {
                    "range" => {
                        tokens.expect_symbol(Symbol::LessThan).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (ident_forest, _) = IdentForest::parse(tokens, ())?;
                        tokens.expect_symbol(Symbol::GreaterThan).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                        tokens.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (start, _) = Expression::parse(tokens, args.clone())?;
                        tokens.expect_symbol(Symbol::Comma).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (end, _) = Expression::parse(tokens, args.clone())?;
                        let bracket = tokens.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                        let span = token.span.merge(bracket.span);

                        (InterpreterAction::Range {
                            relation,
                            ident_forest,
                            start: Box::new(Bound::Included(start)), // todo: proper bounds
                            end: Box::new(Bound::Included(end)), // todo: proper bounds
                        }, span)
                    },
                    "insert" => {
                        tokens.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (new_row, _) = Expression::parse(tokens, args.clone())?;
                        let bracket = tokens.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                        let span = token.span.merge(bracket.span);

                        (InterpreterAction::Insert {
                            relation,
                            new_row: Box::new(new_row),
                        }, span)
                    },
                    "extend" => {
                        tokens.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (new_rows, _) = Expression::parse(tokens, args.clone())?;
                        let bracket = tokens.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                        let span = token.span.merge(bracket.span);

                        (InterpreterAction::Extend {
                            relation,
                            new_rows: Box::new(new_rows),
                        }, span)
                    },
                    "remove" => {
                        tokens.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (pkey, _) = Expression::parse(tokens, args.clone())?;
                        let bracket = tokens.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                        let span = token.span.merge(bracket.span);

                        (InterpreterAction::Remove {
                            relation,
                            pkey: Box::new(pkey),
                        }, span)
                    },
                    "retain" => {
                        tokens.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (function, _) = Function::parse(tokens, args.clone())?;
                        let bracket = tokens.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingAction))?;

                        let span = token.span.merge(bracket.span);

                        (InterpreterAction::Retain {
                            relation,
                            condition: Box::new(function),
                        }, span)
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

                (Expression::Action(action), span)
            }
            // literal, function invocation, or nested ident
            kind => {
                if let Ok(_) = Literal::parse(&mut tokens.clone(), args.clone()) {
                    let (literal, span) = Literal::parse(tokens, args.clone()).map_err(|err| err.with_context(ParseContext::ParsingExpression))?;
                    return Ok((Expression::Literal(literal), span));
                }

                if let Ok(_) = FunctionInvocation::parse(tokens, args.clone()) {
                    let (invocation, span) = FunctionInvocation::parse(&mut tokens.clone(), args.clone()).map_err(|err| err.with_context(ParseContext::ParsingExpression))?;
                    return Ok((Expression::FunctionInvocation(invocation), span));
                }

                if let Ok(_) = NestedIdent::parse(tokens, ()) {
                    let (nested_ident, span) = NestedIdent::parse(&mut tokens.clone(), ()).map_err(|err| err.with_context(ParseContext::ParsingExpression))?;
                    return Ok((Expression::NestedIdent(nested_ident), span));
                }

                return Err(ParseError {
                    span: token.span,
                    context: Some(ParseContext::ParsingExpression),
                    kind: ParseErrorKind::UnexpectedTokenKind(kind),
                });
            },
        };

        Ok(spanned_expression)
    }
}

impl Parse for Op {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let token = tokens.expect_peek().map_err(|err| err.with_context(ParseContext::ParsingOperation))?;
        use TokenKind as K;
        use Symbol as S;
        match &token.kind {
            K::Symbol(S::Plus) | K::Symbol(S::Minus) | K::Symbol(S::Asterisk) | K::Symbol(S::ForwardSlash) => {
                let (op, span) = ArithmeticOp::parse(tokens, args)?;
                Ok((Op::Arithmetic(op), span))
            },
            K::Symbol(S::And) | K::Symbol(S::Or) | K::Symbol(S::Not) | K::Symbol(S::DoubleEqual) |
            K::Symbol(S::LessThan) | K::Symbol(S::GreaterThan) |
            K::Symbol(S::LessThanOrEqual) | K::Symbol(S::GreaterThanOrEqual) => {
                let (op, span) = LogicalOp::parse(tokens, args)?;
                Ok((Op::Logical(op), span))
            },
            _ => Err(ParseError {
                span: token.span,
                context: Some(ParseContext::ParsingOperation),
                kind: ParseErrorKind::ExpectedTokenKindOneOf {
                    expected: [
                        K::Symbol(S::Plus),
                        K::Symbol(S::Minus),
                        K::Symbol(S::Asterisk),
                        K::Symbol(S::ForwardSlash),
                        K::Symbol(S::And),
                        K::Symbol(S::Or),
                        K::Symbol(S::Not),
                        K::Symbol(S::DoubleEqual),
                        K::Symbol(S::LessThan),
                        K::Symbol(S::GreaterThan),
                        K::Symbol(S::LessThanOrEqual),
                        K::Symbol(S::GreaterThanOrEqual),
                    ].into(),
                    got: token.kind.clone(),
                },
            })
        }
    }
}

impl Parse for ArithmeticOp {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let token = tokens.expect_token().map_err(|err| err.with_context(ParseContext::ParsingOperation))?;
        use TokenKind as K;
        use Symbol as S;
        match token.kind {
            K::Symbol(S::Plus) => {
                let (left, _) = Expression::parse(tokens, args.clone())?;
                let (right, right_span) = Expression::parse(tokens, args)?;
                let span = token.span.merge(right_span);
                Ok((ArithmeticOp::Add(left, right), span))
            },
            K::Symbol(S::Minus) => {
                let (left, _) = Expression::parse(tokens, args.clone())?;
                let (right, right_span) = Expression::parse(tokens, args)?;
                let span = token.span.merge(right_span);
                Ok((ArithmeticOp::Sub(left, right), span))
            },
            K::Symbol(S::Asterisk) => {
                let (left, _) = Expression::parse(tokens, args.clone())?;
                let (right, right_span) = Expression::parse(tokens, args)?;
                let span = token.span.merge(right_span);
                Ok((ArithmeticOp::Mul(left, right), span))
            },
            K::Symbol(S::ForwardSlash) => {
                let (left, _) = Expression::parse(tokens, args.clone())?;
                let (right, right_span) = Expression::parse(tokens, args)?;
                let span = token.span.merge(right_span);
                Ok((ArithmeticOp::Div(left, right), span))
            },
            kind => Err(ParseError {
                span: token.span,
                context: Some(ParseContext::ParsingOperation),
                kind: ParseErrorKind::ExpectedTokenKindOneOf {
                    expected: [
                        K::Symbol(S::Plus),
                        K::Symbol(S::Minus),
                        K::Symbol(S::Asterisk),
                        K::Symbol(S::ForwardSlash),
                    ].into(),
                    got: kind,
                },
            }),
        }
    }
}

impl Parse for LogicalOp {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let token = tokens.expect_token().map_err(|err| err.with_context(ParseContext::ParsingOperation))?;
        use TokenKind as K;
        use Symbol as S;
        
        match token.kind {
            K::Symbol(S::And) => {
                let (left, _) = Expression::parse(tokens, args.clone())?;
                let (right, right_span) = Expression::parse(tokens, args)?;
                let span = token.span.merge(right_span);
                Ok((LogicalOp::And(left, right), span))
            },
            K::Symbol(S::Or) => {
                let (left, _) = Expression::parse(tokens, args.clone())?;
                let (right, right_span) = Expression::parse(tokens, args)?;
                let span = token.span.merge(right_span);
                Ok((LogicalOp::Or(left, right), span))
            },
            K::Symbol(S::Not) => {
                let (expr, expr_span) = Expression::parse(tokens, args.clone())?;
                let span = token.span.merge(expr_span);
                Ok((LogicalOp::Not(expr), span))
            },
            K::Symbol(S::DoubleEqual) => {
                let (left, _) = Expression::parse(tokens, args.clone())?;
                let (right, right_span) = Expression::parse(tokens, args)?;
                let span = token.span.merge(right_span);
                Ok((LogicalOp::Eq(left, right), span))
            },
            K::Symbol(S::LessThan) => {
                let (left, _) = Expression::parse(tokens, args.clone())?;
                let (right, right_span) = Expression::parse(tokens, args)?;
                let span = token.span.merge(right_span);
                Ok((LogicalOp::Lt(left, right), span))
            },
            K::Symbol(S::GreaterThan) => {
                let (left, _) = Expression::parse(tokens, args.clone())?;
                let (right, right_span) = Expression::parse(tokens, args)?;
                let span = token.span.merge(right_span);
                Ok((LogicalOp::Gt(left, right), span))
            },
            K::Symbol(S::LessThanOrEqual) => {
                let (left, _) = Expression::parse(tokens, args.clone())?;
                let (right, right_span) = Expression::parse(tokens, args)?;
                let span = token.span.merge(right_span);
                Ok((LogicalOp::Lte(left, right), span))
            },
            K::Symbol(S::GreaterThanOrEqual) => {
                let (left, _) = Expression::parse(tokens, args.clone())?;
                let (right, right_span) = Expression::parse(tokens, args)?;
                let span = token.span.merge(right_span);
                Ok((LogicalOp::Gte(left, right), span))
            },
            kind => Err(ParseError {
                span: token.span,
                context: Some(ParseContext::ParsingOperation),
                kind: ParseErrorKind::ExpectedTokenKindOneOf {
                    expected: [
                        K::Symbol(S::And),
                        K::Symbol(S::Or),
                        K::Symbol(S::Not),
                        K::Symbol(S::DoubleEqual),
                        K::Symbol(S::LessThan),
                        K::Symbol(S::GreaterThan),
                        K::Symbol(S::LessThanOrEqual),
                        K::Symbol(S::GreaterThanOrEqual),
                    ].into(),
                    got: kind,
                },
            }),
        }
    }
}

impl Parse for Function {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        // (args, ...)
        let first_token = tokens.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingFunction))?;

        let (args_map, _) = IndexMap::<Ident, TTypeId>::parse(tokens, MapArgs {
            key_args: (),
            value_args: (),
            map_separator: Some(Symbol::Colon),
            entry_separator: Some(Symbol::Comma),
        })?;

        tokens.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingFunction))?;

        // -> result_type
        tokens.expect_symbol(Symbol::RightArrow).map_err(|err| err.with_context(ParseContext::ParsingFunction))?;
        let (ret_type, _) = TTypeId::parse(tokens, ())?;
        
        // { expression }
        tokens.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingFunction))?;
        let (expression, _) = Expression::parse(tokens, args.clone())?;
        let last_token = tokens.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingFunction))?;

        let span = first_token.span.merge(last_token.span);

        let function = Function::new(args.pager, args.registry, args.relations, args_map, ret_type, expression).map_err(|err| ParseError {
            span,
            context: Some(ParseContext::ParsingFunction),
            kind: err.into(),
        })?;

        Ok((function, span))
    }
}

impl Parse for FunctionInvocation {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (path, path_span) = IdentPath::parse(tokens, ()).map_err(|err| err.with_context(ParseContext::ParsingFunctionInvocation))?;
        
        tokens.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingFunctionInvocation))?;
        
        let (args_list, _) = Vec::<Expression>::parse(tokens, ListArgs {
            args,
            separator: Symbol::Comma,
            allow_trailing: true,
        })?;
        
        let bracket = tokens.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingFunctionInvocation))?;

        let span = path_span.merge(bracket.span);
        
        Ok((FunctionInvocation::new(path, args_list), span))
    }
}

impl Parse for MatchControlFlow {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let match_token = tokens.expect_keyword(Keyword::Match).map_err(|err| err.with_context(ParseContext::ParsingMatch))?;

        let (param, _) = Expression::parse(tokens, args.clone()).map_err(|err| err.with_context(ParseContext::ParsingMatch))?;

        tokens.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingMatch))?;

        let (branches, _) = BTreeMap::<Ident, Branch>::parse(tokens, MapArgs {
            key_args: (),
            value_args: args.clone(),
            map_separator: None,
            entry_separator: Some(Symbol::Comma),
        })?;

        tokens.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingMatch))?;
        tokens.expect_symbol(Symbol::RightArrow).map_err(|err| err.with_context(ParseContext::ParsingMatch))?;
        let (ret_type, type_span) = TTypeId::parse(tokens, ()).map_err(|err| err.with_context(ParseContext::ParsingMatch))?;

        let span = match_token.span.merge(type_span);

        Ok((MatchControlFlow {
            param,
            ret_type,
            branches,
        }, span))
    }
}

impl Parse for IfControlFlow {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        // if condition { then } else { otherwise } -> ret_type

        let token = tokens.expect_keyword(Keyword::If)?;
        let (condition, _) = Expression::parse(tokens, args.clone())?;

        tokens.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
        let (then, _) = Expression::parse(tokens, args.clone())?;
        tokens.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
        
        tokens.expect_keyword(Keyword::Else).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
        
        tokens.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
        let (otherwise, _) = Expression::parse(tokens, args.clone())?;
        tokens.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingIf))?;

        tokens.expect_symbol(Symbol::RightArrow).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
        let (ret_type, ret_type_span) = TTypeId::parse(tokens, ())?;

        let span = token.span
            .merge(ret_type_span);

        Ok((IfControlFlow {
            condition,
            ret_type,
            then,
            otherwise,
        }, span))
    }
}

impl Parse for Branch {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (ident, ident_span) = Option::<Wrapped::<Ident>>::parse(tokens, WrappedArgs {
            args: (),
            open: Symbol::BracketOpen,
            close: Symbol::BracketClose,
        })?;
        let ident = ident.map(Wrapped::unwrap).unwrap_or(id!("_"));
           
        tokens.expect_symbol(Symbol::RightFatArrow).map_err(|err| err.with_context(ParseContext::ParsingBranch))?;
        let start_span = ident_span;
        
        let (expression, expr_span) = Expression::parse(tokens, args)?;

        let span = start_span.merge(expr_span);

        Ok((Branch::new(ident, expression), span))
    }
}

impl Parse for Literal {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let next_peek = tokens.expect_peek()
            .map_err(|err| err.with_context(ParseContext::ParsingLiteral))?
            .clone();

        let spanned_value = match next_peek.kind {
            TokenKind::ScalarLiteral(_) => {
                let token = tokens.expect_token().map_err(|err| err.with_context(ParseContext::ParsingLiteral))?;
                let TokenKind::ScalarLiteral(value) = token.kind else { unreachable!() };
                (Literal::Scalar(value), token.span)
            },
            _ => { // not a scalar, must be composite
                let (literal, span) = CompositeLiteral::parse(tokens, args)?;
                (Literal::Composite(literal), span)
            }
        };

        Ok(spanned_value)
    }
}

impl Parse for CompositeLiteral {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let (ttype_id, type_span) = TTypeId::parse(tokens, ())?;
        let (inner, inner_span) = CompositeLiteralInner::parse(tokens, args)?;

        Ok((CompositeLiteral {
            ttype_id,
            inner,
        }, type_span.merge(inner_span)))
    }
}

impl Parse for CompositeLiteralInner {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let token = tokens.expect_peek().map_err(|err| err.with_context(ParseContext::ParsingCompositeLiteral))?;
        match &token.kind {
            TokenKind::Symbol(Symbol::CurlyBracketOpen) => {
                let (literal, span) = StructLiteral::parse(tokens, args)?;
                Ok((CompositeLiteralInner::Struct(literal), span))
            },
            TokenKind::Symbol(Symbol::Dot) => {
                let (literal, span) = EnumLiteral::parse(tokens, args)?;
                Ok((CompositeLiteralInner::Enum(literal), span))
            },
            TokenKind::Symbol(Symbol::SquareBracketOpen) => {
                let (literal, span) = ArrayLiteral::parse(tokens, args)?;
                Ok((CompositeLiteralInner::Array(literal), span))
            },
            kind => {
                return Err(ParseError {
                    span: token.span,
                    context: Some(ParseContext::ParsingCompositeLiteral),
                    kind: ParseErrorKind::ExpectedTokenKindOneOf {
                        expected: [
                            TokenKind::Symbol(Symbol::CurlyBracketOpen),
                            TokenKind::Symbol(Symbol::Dot),
                            TokenKind::Symbol(Symbol::SquareBracketOpen),
                        ].into(),
                        got: kind.clone(),
                    },
                });
            }
        }
    }
}

impl Parse for StructLiteral {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        tokens.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;
        
        let (fields, span) = BTreeMap::<Ident, Expression>::parse(tokens, MapArgs {
            key_args: (),
            value_args: args,
            map_separator: Some(Symbol::Colon),
            entry_separator: Some(Symbol::Comma),
        }).map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;

        tokens.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;

        Ok((StructLiteral::new(fields), span))
    }
}

impl Parse for EnumLiteral {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let dot = tokens.expect_symbol(Symbol::Dot).map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;
        
        let (tag, _) = Ident::parse(tokens, ()).map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;

        let (expr, expr_span) = Option::<Wrapped<Expression>>::parse(tokens, WrappedArgs {
            args,
            open: Symbol::BracketOpen,
            close: Symbol::BracketClose,
        })?;
        let expr = expr.map(Wrapped::unwrap).unwrap_or(Expression::Literal(Literal::UNIT));

        let span = dot.span.merge(expr_span);

        Ok((EnumLiteral::new(tag, expr), span))
    }
}

impl Parse for ArrayLiteral {
    type Args<'a> = ExpressionArgs<'a>;

    fn parse<'a>(tokens: &mut TokenSlice<'a>, args: Self::Args<'_>) -> Result<(Self, Span), ParseError> where Self: Sized {
        let open = tokens.expect_symbol(Symbol::SquareBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingArrayLiteral))?;

        let (entries, _) = Vec::<Expression>::parse(tokens, ListArgs {
            args,
            separator: Symbol::Comma,
            allow_trailing: true,
        })?;

        let close = tokens.expect_symbol(Symbol::SquareBracketClose).map_err(|err| err.with_context(ParseContext::ParsingArrayLiteral))?;

        Ok((ArrayLiteral::new(entries), open.span.merge(close.span)))
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use crate::{db::{pager::Pager, registry::Registry, DbRelationSet}, expression::Expression, lex, ExpressionArgs, Parse, TokenSlice};

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

        let tokens = lex(string).expect("failed to lex");
        let expr = Expression::parse(&mut TokenSlice::from(&*tokens), ExpressionArgs {
            pager,
            registry: &registry,
            relations: &relations,
        }).map(|(expr, _)| expr);
        
        match expr {
            Ok(expr) => {
                dbg!(expr);
            },
            Err(err) => {
                let rest = match err.span.end {
                    Some(i) => i,
                    None => string.len(),
                };
                let rest = &string[rest..];
                println!("{}\x1b[31;1m{}\x1b[0m{}", &string[..err.span.start], &string[err.span.into_range()], rest);
                panic!("{err}")
            },
        }
    }
}
