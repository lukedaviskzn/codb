use std::fmt::{Debug, Display};

use codb_core::{Ident, IdentPath};
use ttype::TType;
use value::Value;

use crate::{db::registry::TTypeId};

pub mod ttype;
pub mod value;
pub mod function;
pub mod scope;

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum TypeError {
    #[error("unknown relation '{0}'")]
    UnknownRelation(Ident),
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
    #[error("cannot access field '{0}' on array")]
    ArrayField(Ident),
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
    TypeSetInvalid {
        expected: TypeSet,
        got: TType,
    },
    #[error("type invalid, expected {expected:?} got {got:?}")]
    ValueTypeSetInvalid {
        expected: TypeSet,
        got: Value,
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
    #[error("array expects {expected} entries, got {got}")]
    ArrayLen {
        expected: u64,
        got: u64,
    },
    // #[error("{0}")]
    // FieldNotFound(#[from] FieldNotFoundOn),
    #[error("type `{0:?}` not found")]
    TypeNotFound(TTypeId),
    #[error("function `{0:?}` not found")]
    FunctionNotFound(IdentPath),
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeSet {
    Scalar,
    Composite,
    Struct,
    Enum,
    Array,
}

impl Display for TypeSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSet::Scalar => write!(f, "scalar"),
            TypeSet::Composite => write!(f, "composite"),
            TypeSet::Struct => write!(f, "struct"),
            TypeSet::Enum => write!(f, "enum"),
            TypeSet::Array => write!(f, "array"),
        }
    }
}
