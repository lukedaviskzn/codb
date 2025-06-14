use std::{fmt::{Debug, Display}, ops::{Bound, RangeBounds}};

use crate::{expression::{EvalError, Expression}, query::schema_query::{SchemaError, SchemaQuery}, typesystem::TypeError};

pub mod lex;
pub mod parser;
pub mod schema_query;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub length: Option<usize>,
}

impl Span {
    pub const ALL: Span = Span {
        start: 0,
        length: None,
    };

    pub fn at(start: usize) -> Span {
        Span {
            start,
            length: Some(1),
        }
    }

    pub fn beyond(start: usize) -> Span {
        Span {
            start,
            length: None,
        }
    }

    pub fn with_len(start: usize, length: usize) -> Span {
        Span {
            start,
            length: Some(length),
        }
    }
    
    pub fn new(start: usize, length: Option<usize>) -> Span {
        Span {
            start,
            length,
        }
    }

    pub fn merge(self, other: Span) -> Span {
        let start = self.start.min(other.start);
        let length = if let Some(self_len) = self.length {
            if let Some(other_len) = other.length {
                Some((self.start + self_len).max(other.start + other_len) - start)
            } else {
                None
            }
        } else {
            None
        };
        Span {
            start,
            length,
        }
    }

    pub fn extend(self, new_length: Option<usize>) -> Span {
        Self {
            start: self.start,
            length: new_length.map(|nl| self.length.map(|l| l + nl)).flatten(),
        }
    }

    pub fn prepend(self, move_left: Option<usize>) -> Span {
        Self {
            start: move_left.map(|l| self.start.checked_sub(l)).flatten().unwrap_or_default(),
            length: self.length,
        }
    }

    pub fn into_range(self) -> (Bound<usize>, Bound<usize>) {
        (
            Bound::Included(self.start),
            self.end_bound(),
        )
    }

    pub fn end_bound(&self) -> Bound<usize> {
        self.length.map(|l| Bound::Excluded(self.start + l)).unwrap_or(Bound::Unbounded)
    }
}

impl<T: RangeBounds<usize>> From<T> for Span {
    fn from(value: T) -> Self {
        let start = match value.start_bound() {
            Bound::Included(index) => *index,
            Bound::Excluded(index) => index - 1,
            Bound::Unbounded => 0,
        };
        let length = match value.end_bound() {
            Bound::Included(end) => Some(end + 1 - start),
            Bound::Excluded(end) => Some(end - start),
            Bound::Unbounded => None,
        };
        Self {
            start,
            length,
        }
    }
}

impl Into<(Bound<usize>, Bound<usize>)> for Span {
    fn into(self) -> (Bound<usize>, Bound<usize>) {
        self.into_range()
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.start)?;
        if let Some(length) = self.length {
            if length > 1 {
                write!(f, "..")?;
                write!(f, "{}", self.start + length)?;
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
