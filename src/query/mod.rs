use std::ops::Bound;

use crate::{expr::Expression, idents::{Ident, IdentTree}, relation::{PKey, Row}, typesystem::value::Value};

pub enum Query {
    Data(DataQuery),
}

pub enum DataQuery {
    Range {
        relation: Ident,
        ident_trees: Box<[IdentTree]>,
        range_start: Bound<Value>,
        range_end: Bound<Value>,
    },
    Insert {
        relation: Ident,
        row: Row,
    },
    Extend {
        relation: Ident,
        rows: Box<[Row]>,
    },
    Remove {
        relation: Ident,
        pkey: PKey,
    },
    Retain {
        relation: Ident,
        condition: Expression,
    },
}
