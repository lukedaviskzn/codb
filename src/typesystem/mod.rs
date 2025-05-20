use std::fmt::Debug;

use codb_core::{Ident, IdentPath};
use ttype::TType;
use value::Value;

use crate::db::registry::TTypeId;

pub mod ttype;
pub mod value;
pub mod function;

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
    #[error("value type invalid, expected {expected:?} got {got:?}")]
    ValueTypeIdInvalid {
        expected: TTypeId,
        got: Value,
    },
    #[error("type invalid, expected {expected:?} got {got:?}")]
    TypeInvalid {
        expected: TType,
        got: TType,
    },
    #[error("type invalid, expected {expected:?} got {got:?}")]
    TypeIdInvalid {
        expected: TTypeId,
        got: TTypeId,
    },
    #[error("function expects {expected} arguments, got {got}")]
    FunctionArgLen {
        expected: usize,
        got: usize,
    },
    #[error("function has two arguments named {arg:?}")]
    FunctionDuplicateArg {
        arg: Ident,
    },
    #[error("type `{0:?}` not found")]
    TypeNotFound(TTypeId),
    #[error("function `{0:?}` not found")]
    FunctionNotFound(IdentPath),
}
