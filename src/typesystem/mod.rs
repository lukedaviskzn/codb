use std::fmt::Debug;

use registry::TypeRegistryError;
use ttype::TType;
use value::{Value, Literal};

use crate::{expr::ExprError, idents::Ident};

pub mod registry;
pub mod ttype;
pub mod value;

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum TypeError {
    #[error("unknown field '{0}'")]
    UnknownField(Ident),
    #[error("missing field '{0}'")]
    MissingField(Ident),
    #[error("unknown tag '{0}'")]
    UnknownTag(Ident),
    #[error("missing tag '{0}'")]
    MissingTag(Ident),
    #[error("cannot access enum tag '{0}' using '.' operator, use match")]
    DotTag(Ident),
    #[error("cannot access field '{0}' on scalar")]
    ScalarField(Ident),
    #[error("value type invalid, expected {expected:?} got {got:?}")]
    ValueTypeInvalid {
        expected: TType,
        got: Value,
    },
    #[error("value literal type invalid, expected {expected:?} got {got:?}")]
    LiteralTypeInvalid {
        expected: TType,
        got: Literal,
    },
    #[error("type invalid, expected {expected:?} got {got:?}")]
    TypeInvalid {
        expected: TType,
        got: TType,
    },
    #[error("refinement failed: {0}")]
    RefinementFailed(#[from] RefinementFailedError),
    #[error("{0}")]
    TypeRegistryError(#[from] TypeRegistryError),
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum RefinementFailedError {
    #[error("{0}")]
    Expr(#[from] Box<ExprError>),
    #[error("{0:?}")]
    Refinement(String),
}
