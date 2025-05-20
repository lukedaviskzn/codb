use std::{io, ops::RangeBounds};

use codb_core::IdentTree;

use crate::{db::registry::{Registry, TTypeId}, typesystem::{ttype::{StructType}, value::Value, TypeError}};

pub mod memory;
pub mod file;

pub type PKey = Value;
pub type Row = Value;
pub type RowSize = u64;

pub trait RelationRef {
    fn schema(&self) -> &Schema;
    fn range(&self, registry: &Registry, ident_trees: impl Into<Box<[IdentTree]>>, range: impl RangeBounds<Value>) -> Result<impl Iterator<Item = io::Result<Row>>, TypeError>;
    
    #[cfg(test)]
    fn eq(&self, registry: &Registry, other: &impl RelationRef) -> bool {
        use itertools::Itertools;

        if self.schema() != other.schema() {
            return false;
        }

        let zipped = self.range(registry, [], ..).unwrap().zip_longest(other.range(registry, [], ..).unwrap());
        
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
    fn draw(&self, registry: &Registry) -> String {
        let mut out_string = String::new();

        out_string += &format!("{:?}\n", self.schema().ttype());

        for row in self.range(registry, [], ..).unwrap() {
            out_string += &format!("{:?}\n", row.unwrap());
        }

        // remove final \n
        out_string.pop();

        out_string
    }
}

pub trait Relation: RelationRef {
    fn insert(&mut self, registry: &Registry, new_row: Row) -> Result<io::Result<bool>, TypeError>;
    
    fn extend(&mut self, registry: &Registry, new_rows: impl IntoIterator<Item = Row>) -> Result<io::Result<RowSize>, TypeError> {
        let mut count = 0;
        for new_row in new_rows {
            if let Err(err) = self.insert(registry, new_row)? {
                return Ok(Err(err));
            }
            count += 1;
        }
        Ok(Ok(count))
    }
    
    fn remove(&mut self, registry: &Registry, pkey: &PKey) -> Result<io::Result<Option<Row>>, TypeError>;
    fn retain(&mut self, predicate: impl Fn(&Row) -> bool) -> io::Result<RowSize>;
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum SchemaError {
    #[error("cannot make primary key: {0}")]
    PrimaryKeyInvalid(TypeError),
}

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Schema {
    ttype: StructType,
    pkey: Box<[IdentTree]>,
}

impl Schema {
    pub fn new(registry: &Registry, ttype: StructType, pkey: impl Into<Box<[IdentTree]>>) -> Result<Schema, SchemaError> {
        let pkey = pkey.into();
        
        if let Err(err) = ttype.select(registry, &pkey) {
            return Err(SchemaError::PrimaryKeyInvalid(err));
        }

        Ok(Schema {
            ttype,
            pkey,
        })
    }

    pub fn ttype(&self) -> &StructType {
        &self.ttype
    }

    pub fn ttype_id(&self) -> TTypeId {
        TTypeId::new_anonymous(self.ttype.clone().into())
    }

    pub fn pkey_ttype(&self, registry: &Registry) -> Result<StructType, TypeError> {
        self.ttype.select(registry, &self.pkey)
    }

    pub fn pkey(&self) -> &[IdentTree] {
        &self.pkey
    }
}
