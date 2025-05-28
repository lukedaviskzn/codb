use std::{borrow::Borrow, fmt::Display, iter::Peekable, ops::Bound};

use codb_core::{Ident, IdentForest, IdentPath, IdentTree, NestedIdent};
use itertools::Itertools;

use crate::{db::{registry::{CompositeTTypeId, Registry, TTypeId}, relation::Relation, DbRelations}, expression::{ArithmeticOp, ArrayLiteral, Branch, ControlFlow, EnumLiteral, Expression, FunctionInvocation, IfControlFlow, InterpreterAction, Literal, LogicalOp, MatchControlFlow, Op, StructLiteral}, query::lexer::TokenKind, typesystem::{function::{Function, FunctionArg}, ttype::{ArrayType, CompositeType, EnumType, ScalarType, StructType, TType}, value::ScalarValue, TypeError, TypeSet}};

use super::{lexer::{Keyword, LexContext, LexError, LexErrorKind, Lexer, Symbol, Token, TokenTag}, Span};

#[derive(Debug, Clone, thiserror::Error)]
#[error("[{span}] error while{}: {kind}", .context.as_ref().map(|ctx| format!(" {ctx}")).unwrap_or_default())]
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
    #[error("unexpected {0}")]
    UnexpectedTokenKind(TokenKind),
    #[error("unexpected {0}")]
    UnexpectedTokenTag(TokenTag),
    #[error("unexpected {got} expected one of {expected:?}")]
    ExpectedTokenKindOneOf {
        expected: Box<[TokenKind]>,
        got: TokenKind,
    },
    #[error("expected {expected:?} got {got}")]
    ExpectedTokenKind {
        expected: TokenKind,
        got: TokenKind,
    },
    #[error("expected {expected:?} got {got}")]
    ExpectedTokenTagGotKind {
        expected: TokenTag,
        got: TokenKind,
    },
    #[error("expected {expected:?} got {got}")]
    ExpectedTokenTag {
        expected: TokenTag,
        got: TokenTag,
    },
    #[error("expected {expected:?} got {got}")]
    ExpectedScalarLiteralType {
        expected: ScalarType,
        got: TokenKind,
    },
    #[error("unexpected {got} expected one of {expected:?}")]
    ExpectedScalarLiteralTypeOneOf {
        expected: Box<[ScalarType]>,
        got: TokenKind,
    },
    #[error("match branch has duplicate tag {0}")]
    DuplicateBranch(Ident),
    #[error("struct type has duplicate field {0}")]
    DuplicateField(Ident),
    #[error("enum type has duplicate tag {0}")]
    DuplicateTag(Ident),
    #[error("cannot create value of type 'never'")]
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

pub struct Parser<T: Iterator<Item = char>> {
    lexer: Peekable<Lexer<T>>,
}

impl<T: Iterator<Item = char>> Parser<T> {
    #[allow(unused)]
    pub fn new(lexer: Lexer<T>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }

    #[allow(unused)]
    pub fn parse<R: Relation>(&mut self, registry: &Registry, relations: &DbRelations<R>) -> Result<Expression, ParseError> {
        self.parse_expression(registry, relations).map(|(_, expr)| expr)
    }

    pub fn parse_expression<R: Relation>(&mut self, registry: &Registry, relations: &DbRelations<R>) -> Result<(Span, Expression), ParseError> {
        let token = self.expect_token()?;

        let spanned_expression = match token.kind {
            // op (arithmetic)
            TokenKind::Symbol(Symbol::Plus) => {
                let (left_span, left_expr) = self.parse_expression(registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Arithmetic(ArithmeticOp::Add(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::Minus) => {
                let (left_span, left_expr) = self.parse_expression(registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Arithmetic(ArithmeticOp::Sub(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::Asterisk) => {
                let (left_span, left_expr) = self.parse_expression(registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Arithmetic(ArithmeticOp::Mul(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::ForwardSlash) => {
                let (left_span, left_expr) = self.parse_expression(registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Arithmetic(ArithmeticOp::Div(
                    left_expr,
                    right_expr,
                )))))
            },
            // op (logical)
            TokenKind::Symbol(Symbol::And) => {
                let (left_span, left_expr) = self.parse_expression(registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::And(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::Or) => {
                let (left_span, left_expr) = self.parse_expression(registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Or(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::Not) => {
                let (expr_span, expr) = self.parse_expression(registry, relations)?;

                let span = token.span.merge(expr_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Not(expr)))))
            },
            TokenKind::Symbol(Symbol::DoubleEqual) => {
                let (left_span, left_expr) = self.parse_expression(registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Eq(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::LessThan) => {
                let (left_span, left_expr) = self.parse_expression(registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Lt(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::GreaterThan) => {
                let (left_span, left_expr) = self.parse_expression(registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Gt(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::LessThanOrEqual) => {
                let (left_span, left_expr) = self.parse_expression(registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Lte(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Symbol(Symbol::GreaterThanOrEqual) => {
                let (left_span, left_expr) = self.parse_expression(registry, relations)?;
                let (right_span, right_expr) = self.parse_expression(registry, relations)?;

                let span = token.span.merge(left_span).merge(right_span);
                
                (span, Expression::Op(Box::new(Op::Logical(LogicalOp::Gte(
                    left_expr,
                    right_expr,
                )))))
            },
            TokenKind::Keyword(Keyword::If) => {
                // if condition { then } else { otherwise } -> ret_type
                let (condition_span, condition) = self.parse_expression(registry, relations)?;

                self.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
                let (then_span, then) = self.parse_expression(registry, relations)?;
                self.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
                
                self.expect_keyword(Keyword::Else).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
                
                self.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingIf))?;
                let (otherwise_span, otherwise) = self.parse_expression(registry, relations)?;
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
                // match param { Tag(tag) => expr, ... } -> ret_type
                let (param_span, param) = self.parse_expression(registry, relations)?;

                self.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingMatch))?;

                let (all_branches_span, branch_list) = self.parse_non_empty_list_with_rel_args(registry, relations, Self::parse_branch, &TokenKind::Symbol(Symbol::Comma))?;

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
                        let (start_span, start) = self.parse_expression(registry, relations)?;
                        self.expect_symbol(Symbol::Comma).map_err(|err| err.with_context(ParseContext::ParsingAction))?;
                        let (end_span, end) = self.parse_expression(registry, relations)?;
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
                        let (row_span, new_row) = self.parse_expression(registry, relations)?;
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
                        let (row_span, new_rows) = self.parse_expression(registry, relations)?;
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
                        let (row_span, pkey) = self.parse_expression(registry, relations)?;
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
                        let (function_span, function) = self.parse_function(registry, relations)?;
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
                            registry, relations,
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
                            registry, relations,
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
            TokenKind::Symbol(Symbol::PathSep) => { // todo: fix this symbol
                let (span, value) = self.parse_literal(registry, relations)?;
                (span, Expression::Literal(value))
            },
            kind => return Err(ParseError {
                span: token.span,
                context: Some(ParseContext::ParsingExpression),
                kind: ParseErrorKind::UnexpectedTokenKind(kind),
            }),
        };

        Ok(spanned_expression)
    }

    pub fn parse_nested_ident(&mut self) -> Result<(Span, NestedIdent), ParseError> {
        let (list_span, idents) = self.parse_non_empty_list(Self::expect_ident, &TokenKind::Symbol(Symbol::Dot))
            .map_err(|err| err.with_context(ParseContext::ParsingNestedIdent))?;
        
        let idents = idents.into_iter().map(|(_, ident)| ident).collect_vec();

        let nested_ident = NestedIdent::try_from(idents).expect("invalid nested ident");

        Ok((list_span, nested_ident))
    }

    pub fn parse_ident_path(&mut self) -> Result<(Span, IdentPath), ParseError> {
        let (list_span, idents) = self.parse_non_empty_list(Self::expect_ident, &TokenKind::Symbol(Symbol::PathSep))
            .map_err(|err| err.with_context(ParseContext::ParsingIdentPath))?;
        let idents = idents.into_iter().map(|(_, ident)| ident).collect_vec();

        let ident_path = IdentPath::try_from(idents).expect("invalid ident path");

        Ok((list_span, ident_path))
    }

    pub fn parse_ident_forest(&mut self) -> Result<(Span, IdentForest), ParseError> {
        let (list_span, trees) = self.parse_non_empty_list(Self::parse_ident_tree, &TokenKind::Symbol(Symbol::Comma))
            .map_err(|err| err.with_context(ParseContext::ParsingIdentForest))?;
        let trees = trees.into_iter().map(|(_, tree)| tree).collect_vec();

        Ok((list_span, IdentForest::new(trees)))
    }

    pub fn parse_ident_tree(&mut self) -> Result<(Span, IdentTree), ParseError> {
        let (ident_span, ident) = self.expect_ident()
            .map_err(|err| err.with_context(ParseContext::ParsingIdentTree))?;
        
        if let None = self.next_if_token_kind(&TokenKind::Symbol(Symbol::Dot)).map_err(|err| err.with_context(ParseContext::ParsingIdentTree))? {
            return Ok((ident_span, IdentTree::new(ident, IdentForest::empty())));
        }

        let spanned_forest;
        if let Some(_) = self.next_if_token_kind(&TokenKind::Symbol(Symbol::BracketOpen)).map_err(|err| err.with_context(ParseContext::ParsingIdentTree))? {
            spanned_forest = self.parse_ident_forest().map_err(|err| err.with_context(ParseContext::ParsingIdentTree))?;
            self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingIdentTree))?;
        } else {
            let (ident_tree_span, ident_tree) = self.parse_ident_tree()?;
            let span = ident_span.merge(ident_tree_span);
            spanned_forest = (span, IdentForest::new([ident_tree]));
        }

        let (forest_span, forest) = spanned_forest;

        Ok((forest_span, IdentTree::new(ident, forest)))
    }

    pub fn parse_function<R: Relation>(&mut self, registry: &Registry, relations: &DbRelations<R>) -> Result<(Span, Function), ParseError> {
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
        let (expr_span, expression) = self.parse_expression(registry, relations)?;
        self.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingFunction))?;

        let span = arg_span
            .merge(result_span)
            .merge(expr_span)
            .extend(Some(1))
            .prepend(Some(1));

        let function = Function::new(registry, relations, args, result_ttype_id, expression).map_err(|err| ParseError {
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

    pub fn parse_branch<R: Relation>(&mut self, registry: &Registry, relations: &DbRelations<R>) -> Result<(Span, (Ident, Branch)), ParseError> {
        let (tag_span, tag) = self.expect_ident().map_err(|err| err.with_context(ParseContext::ParsingBranch))?;
        self.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingBranch))?;
        let (ident_span, ident) = self.expect_ident().map_err(|err| err.with_context(ParseContext::ParsingBranch))?;
        self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingBranch))?;
        self.expect_symbol(Symbol::RightFatArrow).map_err(|err| err.with_context(ParseContext::ParsingBranch))?;
        let (expr_span, expression) = self.parse_expression(registry, relations)?;

        let span = tag_span.merge(ident_span).merge(expr_span);

        Ok((span, (tag, Branch::new(ident, expression))))
    }

    pub fn parse_literal<R: Relation>(&mut self, registry: &Registry, relations: &DbRelations<R>) -> Result<(Span, Literal), ParseError> {
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
                        let (struct_span, struct_value) = self.parse_struct_contents(registry, relations, ttype_id)?;
                        (type_span.merge(struct_span), struct_value.into())
                    },
                    TType::Composite(CompositeType::Enum(_)) => {
                        self.expect_symbol(Symbol::Hash).map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;
                        let (struct_span, struct_value) = self.parse_enum_contents(registry, relations, ttype_id)?;
                        (type_span.merge(struct_span), struct_value.into())
                    },
                    TType::Array(_) => {
                        self.expect_symbol(Symbol::SquareBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingArrayLiteral))?;

                        let (list_span, entries) = self.parse_non_empty_list_with_rel_args(registry, relations, Self::parse_expression, &TokenKind::Symbol(Symbol::Comma))?;
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

    pub fn parse_struct_contents<R: Relation>(&mut self, registry: &Registry, relations: &DbRelations<R>, ttype_id: TTypeId) -> Result<(Span, StructLiteral), ParseError> {
        self.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;
        
        let (list_span, field_list) = self.parse_non_empty_list_with_rel_args(
            registry, relations,
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
                    kind: ParseErrorKind::DuplicateField(ident),
                });
            }
            fields.insert(ident, value);
        }

        let struct_value = StructLiteral::new(ttype_id, fields);

        Ok((span, struct_value))
    }

    pub fn parse_enum_contents<R: Relation>(&mut self, registry: &Registry, relations: &DbRelations<R>, ttype_id: TTypeId) -> Result<(Span, EnumLiteral), ParseError> {
        let (span, (tag, value)) = self.parse_tag_value(registry, relations)?;
        
        let enum_value = EnumLiteral::new(ttype_id, tag, value);
        
        Ok((span, enum_value))
    }

    pub fn parse_field_value<R: Relation>(&mut self, registry: &Registry, relations: &DbRelations<R>) -> Result<(Span, (Ident, Expression)), ParseError> {
        let (ident_span, ident) = self.expect_ident().map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;
        self.expect_symbol(Symbol::Colon).map_err(|err| err.with_context(ParseContext::ParsingStructLiteral))?;
        let (value_span, value) = self.parse_expression(registry, relations)?;

        let span = ident_span.merge(value_span);

        Ok((span, (ident, value)))
    }

    pub fn parse_tag_value<R: Relation>(&mut self, registry: &Registry, relations: &DbRelations<R>) -> Result<(Span, (Ident, Expression)), ParseError> {
        let (ident_span, ident) = self.expect_ident().map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;
        
        self.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;
        let (value_span, value) = self.parse_expression(registry, relations)?;
        self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingEnumLiteral))?;

        let span = ident_span.merge(value_span).extend(Some(1));

        Ok((span, (ident, value)))
    }

    pub fn parse_composite_ttype_id(&mut self) -> Result<(Span, CompositeType), ParseError> {
        let token = self.expect_token().map_err(|err| err.with_context(ParseContext::ParsingCompositeTTypeId))?;
        match token.kind {
            TokenKind::Keyword(Keyword::Struct) => {
                let (struct_type_span, struct_type) = self.parse_struct_type()?;
                Ok((struct_type_span, CompositeType::Struct(struct_type)))
            },
            TokenKind::Keyword(Keyword::Enum) => {
                let (enum_type_span, enum_type) = self.parse_enum_type()?;
                Ok((enum_type_span, CompositeType::Enum(enum_type)))
            },
            kind => return Err(ParseError {
                span: token.span,
                context: Some(ParseContext::ParsingCompositeTTypeId),
                kind: ParseErrorKind::ExpectedTokenKindOneOf {
                    expected: [
                        TokenKind::Keyword(Keyword::Struct),
                        TokenKind::Keyword(Keyword::Enum),
                    ].into(),
                    got: kind,
                },
            })
        }
    }

    pub fn parse_struct_type(&mut self) -> Result<(Span, StructType), ParseError> {
        self.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingStructType))?;
        
        let (field_list_span, field_list) = self.parse_non_empty_list(Self::parse_field, &TokenKind::Symbol(Symbol::Comma))?;
        let field_list = field_list.into_iter()
            .map(|(_, (ident, ttype_id))| (ident, ttype_id))
            .collect_vec();
        self.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingStructType))?;

        let span = field_list_span.extend(Some(1)).prepend(Some(1));

        let mut fields = indexmap! {};
        for (ident, ttype_id) in field_list {
            if fields.contains_key(&ident) {
                return Err(ParseError {
                    span,
                    context: Some(ParseContext::ParsingStructType),
                    kind: ParseErrorKind::DuplicateField(ident),
                })
            }
            fields.insert(ident, ttype_id);
        }

        Ok((span, StructType::new(fields)))
    }

    pub fn parse_enum_type(&mut self) -> Result<(Span, EnumType), ParseError> {
        self.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingEnumType))?;
        let (tag_list_span, tag_list) = self.parse_non_empty_list(Self::parse_tag, &TokenKind::Symbol(Symbol::Comma))?;
        let tag_list = tag_list.into_iter()
            .map(|(_, (ident, ttype_id))| (ident, ttype_id))
            .collect_vec();
        self.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingEnumType))?;

        let span = tag_list_span.extend(Some(1)).prepend(Some(1));

        let mut tags = indexmap! {};
        for (ident, ttype_id) in tag_list {
            if tags.contains_key(&ident) {
                return Err(ParseError {
                    span,
                    context: Some(ParseContext::ParsingEnumType),
                    kind: ParseErrorKind::DuplicateTag(ident),
                })
            }
            tags.insert(ident, ttype_id);
        }

        Ok((span, EnumType::new(tags)))
    }

    pub fn parse_field(&mut self) -> Result<(Span, (Ident, TTypeId)), ParseError> {
        let (ident_span, ident) = self.expect_ident().map_err(|err| err.with_context(ParseContext::ParsingStructType))?;
        self.expect_symbol(Symbol::Colon).map_err(|err| err.with_context(ParseContext::ParsingStructType))?;
        let (type_span, ttype_id) = self.parse_ttype_id()?;

        let span = ident_span.merge(type_span);

        Ok((span, (ident, ttype_id)))
    }

    pub fn parse_tag(&mut self) -> Result<(Span, (Ident, TTypeId)), ParseError> {
        let (ident_span, ident) = self.expect_ident().map_err(|err| err.with_context(ParseContext::ParsingEnumType))?;
        self.expect_symbol(Symbol::BracketOpen).map_err(|err| err.with_context(ParseContext::ParsingEnumType))?;
        let (type_span, ttype_id) = self.parse_ttype_id()?;
        self.expect_symbol(Symbol::BracketClose).map_err(|err| err.with_context(ParseContext::ParsingEnumType))?;

        let span = ident_span
            .merge(type_span)
            .extend(Some(1));

        Ok((span, (ident, ttype_id)))
    }

    pub fn parse_ttype_id(&mut self) -> Result<(Span, TTypeId), ParseError> {
        let token = self.expect_token().map_err(|err| err.with_context(ParseContext::ParsingTTypeId))?;

        let spanned_ttype_id = match token.kind {
            TokenKind::ScalarType(scalar_type) => {
                (token.span, TTypeId::Scalar(scalar_type))
            },
            TokenKind::Keyword(Keyword::Anon) => {
                self.expect_symbol(Symbol::CurlyBracketOpen).map_err(|err| err.with_context(ParseContext::ParsingAnonymousType))?;
                let (type_span, composite_type) = self.parse_composite_ttype_id()?;
                self.expect_symbol(Symbol::CurlyBracketClose).map_err(|err| err.with_context(ParseContext::ParsingAnonymousType))?;

                let span = token.span
                    .merge(type_span)
                    .extend(Some(1))
                    .prepend(Some(1));

                (span, TTypeId::Composite(CompositeTTypeId::Anonymous(Box::new(composite_type))))
            },
            TokenKind::Ident(ident) => {
                let mut idents = vec![ident];

                let mut span = token.span;

                if let Some(_) = self.next_if_symbol(Symbol::PathSep)? {
                    let (path_span, ident_path) = self.parse_ident_path()?;
                    span = span.merge(path_span);

                    for ident in ident_path {
                        idents.push(ident);
                    }
                }

                let path = IdentPath::try_from(idents).expect("invalid ident path");
                
                (span, TTypeId::Composite(CompositeTTypeId::Path(path)))
            },
            TokenKind::Symbol(Symbol::SquareBracketOpen) => {
                let length = if let Some(Token { kind: TokenKind::ScalarLiteral(ScalarValue::Int64(length)), .. }) = self.next_if_token_tag(TokenTag::ScalarLiteral)? {
                    Some(length as u64)
                } else {
                    None
                };
                self.expect_symbol(Symbol::SquareBracketClose).map_err(|err| err.with_context(ParseContext::ParsingArrayType))?;

                let (inner_span, inner_ttype_id) = self.parse_ttype_id()?;

                let span = token.span.merge(inner_span);

                let array_type = ArrayType::new(inner_ttype_id, length);

                (span, array_type.into())
            },
            kind => return Err(ParseError {
                span: token.span,
                context: Some(ParseContext::ParsingTTypeId),
                kind: ParseErrorKind::UnexpectedTokenKind(kind.clone()),
            }),
        };

        Ok(spanned_ttype_id)
    }

    pub fn parse_non_empty_list<I>(&mut self, parse_item: impl Fn(&mut Parser<T>) -> Result<(Span, I), ParseError>, separator: &TokenKind) -> Result<(Span, Box<[(Span, I)]>), ParseError> {
        let (mut span, item) = parse_item(self)?;
        let mut items = vec![(span, item)];

        while let Some(_) = self.next_if_token_kind(&separator)? {
            let (item_span, item) = parse_item(self)?;
            items.push((item_span, item));

            span = span.merge(item_span);
        }

        Ok((span, items.into()))
    }
    
    pub fn parse_non_empty_list_with_rel_args<I, R: Relation>(&mut self, registry: &Registry, relations: &DbRelations<R>, parse_item: impl Fn(&mut Parser<T>, &Registry, &DbRelations<R>) -> Result<(Span, I), ParseError>, separator: &TokenKind) -> Result<(Span, Box<[(Span, I)]>), ParseError> {
        Self::parse_non_empty_list(self, |parser| parse_item(parser, registry, relations), separator)
    }
    
    pub fn expect_token(&mut self) -> Result<Token, ParseError> {
        let token = self.lexer.next().ok_or_else(|| ParseError {
            span: Span::ALL,
            context: None,
            kind: ParseErrorKind::UnexpectedEnd,
        })??;

        Ok(token)
    }
    
    pub fn expect_peek(&mut self) -> Result<&Token, ParseError> {
        let token = self.lexer.peek().ok_or_else(|| ParseError {
            span: Span::ALL,
            context: None,
            kind: ParseErrorKind::UnexpectedEnd,
        })?.as_ref().map_err(|err| err.clone())?;

        Ok(token)
    }
    
    pub fn expect_token_tag(&mut self, tag: TokenTag) -> Result<Token, ParseError> {
        let token = self.expect_token()?;

        let token_tag = token.kind.tag();

        if token_tag != tag {
            return Err(ParseError {
                span: token.span,
                context: None,
                kind: ParseErrorKind::ExpectedTokenTag {
                    expected: tag,
                    got: token_tag,
                },
            });
        }

        Ok(token)
    }

    pub fn expect_token_kind(&mut self, kind: &TokenKind) -> Result<Span, ParseError> {
        let token = self.expect_token()?;

        if token.kind != *kind {
            return Err(ParseError {
                span: token.span,
                context: None,
                kind: ParseErrorKind::ExpectedTokenKind {
                    expected: kind.clone(),
                    got: token.kind,
                },
            });
        }

        Ok(token.span)
    }

    pub fn expect_ident(&mut self) -> Result<(Span, Ident), ParseError> {
        let token = self.expect_token_tag(TokenTag::Ident)?;
        let TokenKind::Ident(ident) = token.kind else { unreachable!() };
        Ok((token.span, ident))
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

    pub fn expect_symbol(&mut self, symbol: Symbol) -> Result<Span, ParseError> {
        let span = self.expect_token_kind(&TokenKind::Symbol(symbol))?;
        Ok(span)
    }
    
    pub fn expect_keyword(&mut self, keyword: Keyword) -> Result<Span, ParseError> {
        let span = self.expect_token_kind(&TokenKind::Keyword(keyword))?;
        Ok(span)
    }
    
    pub fn next_if_token_tag(&mut self, tag: TokenTag) -> Result<Option<Token>, ParseError> {
        let token = self.lexer.next_if(|token| match token {
            Ok(token) if token.kind.tag() == tag => true,
            _ => false,
        });

        match token {
            Some(Ok(ok)) => Ok(Some(ok)),
            Some(Err(err)) => Err(err.into()),
            None => Ok(None),
        }
    }

    pub fn next_if_token_kind(&mut self, kind: &TokenKind) -> Result<Option<Token>, ParseError> {
        let token = self.lexer.next_if(|token| match token {
            Ok(token) if token.kind == *kind => true,
            _ => false,
        });

        match token {
            Some(Ok(ok)) => Ok(Some(ok)),
            Some(Err(err)) => Err(err.into()),
            None => Ok(None),
        }
    }

    pub fn next_if_ident(&mut self) -> Result<Option<(Span, Ident)>, ParseError> {
        let Some(token) = self.next_if_token_tag(TokenTag::Ident)? else { return Ok(None) };
        let TokenKind::Ident(ident) = token.kind else { unreachable!() };
        Ok(Some((token.span, ident)))
    }

    pub fn next_if_symbol(&mut self, symbol: Symbol) -> Result<Option<Span>, ParseError> {
        let Some(token) = self.next_if_token_kind(&TokenKind::Symbol(symbol))? else { return Ok(None) };
        Ok(Some(token.span))
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Bound;

    use crate::{db::{registry::Registry, relation::memory::MemoryRelation, DbRelations}, expression::{ArrayLiteral, Expression, Literal}, query::lexer::Lexer};

    use super::Parser;

    #[test]
    fn test() {
        let string = "
        ::[]anon{struct { active: bool, id: int32, name: string }} [
            ::anon{struct { active: bool, id: int32, name: string }} {
                active: ::false,
                id: ::-1_i32,
                name: ::\"El Jones, Jim\"
            },
            ::anon{struct { active: bool, id: int32, name: string }} {
                active: ::true,
                id: ::1_i32,
                name: ::\"Jim Jones\"
            },
            ::anon{struct { active: bool, id: int32, name: string }} {
                active: ::true,
                id: ::2_i32,
                name: ::\"Jimboni Jonesi\"
            }
        ]";

        let relations = DbRelations::<MemoryRelation>::new();
        let registry = Registry::new(&relations);

        let lexer = Lexer::new(string.chars());
        let mut parser = Parser::new(lexer);
        match parser.parse_expression(&registry, &relations) {
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
