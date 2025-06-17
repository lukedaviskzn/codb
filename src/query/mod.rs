use std::{fmt::{Debug, Display}, ops::Bound};

use crate::{expression::{EvalError, Expression}, query::schema_query::{SchemaError, SchemaQuery}, typesystem::TypeError};

pub mod lex;
pub mod parser;
pub mod schema_query;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: Option<usize>,
}

impl Span {
    pub const ALL: Span = Span {
        start: 0,
        end: None,
    };

    pub fn new(start: usize, end: usize) -> Span {
        Span {
            start,
            end: Some(end),
        }
    }

    pub fn from(start: usize) -> Span {
        Span {
            start,
            end: None,
        }
    }

    pub fn merge(&self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.map(|self_end|
                other.end.map(|other_end|
                    self_end.max(other_end)
                )
            ).flatten(),
        }
    }

    pub fn into_range(self) -> (Bound<usize>, Bound<usize>) {
        (
            Bound::Included(self.start),
            match self.end {
                Some(end) => Bound::Excluded(end),
                None => Bound::Unbounded,
            },
        )
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.start)?;
        if let Some(end) = self.end {
            if end > 1 {
                write!(f, "..{end}")?;
            }
        } else {
            write!(f, "..")?;
        }
        Ok(())
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum QueryExecutionError {
    #[error("Type Error: {0}")]
    TypeError(#[from] TypeError),
    #[error("Eval Error: {0}")]
    EvalError(#[from] EvalError),
    #[error("Schema Error: {0}")]
    SchemaError(#[from] SchemaError),
}

#[derive(Debug, Clone)]
pub enum Query {
    Data(DataQuery),
    Schema(SchemaQuery),
}

#[derive(Debug, Clone)]
pub struct DataQuery(pub Expression);
