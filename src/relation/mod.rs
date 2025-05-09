use std::{io, ops::RangeBounds};

use crate::typesystem::{ttype::CompositeType, value::CompositeValue, NestedIdents, TypeError};

mod memory;
mod file;

pub type PKey = CompositeValue;
pub type Row = CompositeValue;
pub type RowSize = u64;

pub trait RelationRef {
    fn schema(&self) -> &Schema;
    fn range(&self, nested_idents: impl Into<Box<[NestedIdents]>>, range: impl RangeBounds<CompositeValue>) -> Result<impl Iterator<Item = io::Result<Row>>, TypeError>;
    
    #[cfg(test)]
    fn eq(&self, other: &impl RelationRef) -> bool {
        use itertools::Itertools;

        if self.schema() != other.schema() {
            return false;
        }

        let zipped = self.range([], ..).unwrap().zip_longest(other.range([], ..).unwrap());
        
        for rows in zipped {
            match rows {
                itertools::EitherOrBoth::Both(left, right) => if left.unwrap() != right.unwrap() {
                    return false;
                },
                itertools::EitherOrBoth::Left(_) => return false,
                itertools::EitherOrBoth::Right(_) => return false,
            }
        }
        
        true
    }
    
    #[cfg(test)]
    fn draw(&self) -> String {
        let mut out_string = String::new();

        out_string += &format!("{:?}\n", self.schema().ttype);

        for row in self.range([], ..).unwrap() {
            out_string += &format!("{:?}\n", row.unwrap());
        }

        // remove final \n
        out_string.pop();

        out_string
    }
}

pub trait Relation: RelationRef {
    fn insert(&mut self, new_row: Row) -> Result<io::Result<bool>, TypeError>;
    
    fn extend(&mut self, new_rows: impl IntoIterator<Item = Row>) -> Result<io::Result<RowSize>, TypeError> {
        let mut count = 0;
        for new_row in new_rows {
            self.insert(new_row)?;
            count += 1;
        }
        Ok(Ok(count))
    }
    
    fn remove(&mut self, pkey: &PKey) -> Result<io::Result<Option<Row>>, TypeError>;
    fn retain(&mut self, predicate: impl Fn(&Row) -> bool) -> io::Result<RowSize>;
}

#[derive(Debug, thiserror::Error)]
pub enum SchemaError {
    #[error("cannot make primary key: {0}")]
    PrimaryKeyInvalid(TypeError),
}

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Schema {
    ttype: CompositeType,
    pkey: Box<[NestedIdents]>,
}

impl Schema {
    pub fn new(ttype: CompositeType, pkey: Box<[NestedIdents]>) -> Result<Schema, SchemaError> {
        ttype.select(&pkey).map_err(|err| SchemaError::PrimaryKeyInvalid(err))?;

        Ok(Schema {
            ttype,
            pkey,
        })
    }

    pub fn ttype(&self) -> &CompositeType {
        &self.ttype
    }

    pub fn pkey_ttype(&self) -> CompositeType {
        self.ttype.select(&self.pkey).expect("invalid primary key idents")
    }

    pub fn pkey(&self) -> &[NestedIdents] {
        &self.pkey
    }
}
