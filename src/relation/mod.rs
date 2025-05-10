use std::{io, ops::RangeBounds};

use crate::{idents::IdentTree, typesystem::{registry::{TTypeId, TypeRegistry, TypeRegistryError}, ttype::TType, value::Value, TypeError}};

mod memory;
mod file;

pub type PKey = Value;
pub type Row = Value;
pub type RowSize = u64;

pub trait RelationRef {
    fn schema(&self) -> &Schema;
    fn range(&self, registry: &TypeRegistry, ident_trees: impl Into<Box<[IdentTree]>>, range: impl RangeBounds<Value>) -> Result<impl Iterator<Item = io::Result<Row>>, TypeError>;
    
    #[cfg(test)]
    fn eq(&self, registry: &TypeRegistry, other: &impl RelationRef) -> bool {
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
    fn draw(&self, registry: &TypeRegistry) -> String {
        let mut out_string = String::new();

        out_string += &format!("{:?}\n", registry.get_by_id(self.schema().ttype_id()).unwrap());

        for row in self.range(registry, [], ..).unwrap() {
            out_string += &format!("{:?}\n", row.unwrap());
        }

        // remove final \n
        out_string.pop();

        out_string
    }
}

pub trait Relation: RelationRef {
    fn insert(&mut self, registry: &TypeRegistry, new_row: Row) -> Result<io::Result<bool>, TypeError>;
    
    fn extend(&mut self, registry: &TypeRegistry, new_rows: impl IntoIterator<Item = Row>) -> Result<io::Result<RowSize>, TypeError> {
        let mut count = 0;
        for new_row in new_rows {
            if let Err(err) = self.insert(registry, new_row)? {
                return Ok(Err(err));
            }
            count += 1;
        }
        Ok(Ok(count))
    }
    
    fn remove(&mut self, registry: &TypeRegistry, pkey: &PKey) -> Result<io::Result<Option<Row>>, TypeError>;
    fn retain(&mut self, predicate: impl Fn(&Row) -> bool) -> io::Result<RowSize>;
}

#[derive(Debug, thiserror::Error)]
pub enum SchemaError {
    #[error("cannot make primary key: {0}")]
    PrimaryKeyInvalid(TypeError),
    #[error("{0}")]
    TypeRegistryError(#[from] TypeRegistryError),
}

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Schema {
    ttype_id: TTypeId,
    pkey: Box<[IdentTree]>,
}

impl Schema {
    pub fn new(registry: &TypeRegistry, ttype_id: TTypeId, pkey: Box<[IdentTree]>) -> Result<Schema, SchemaError> {
        registry.get_by_id(&ttype_id)?.select(registry, &pkey).map_err(|err| SchemaError::PrimaryKeyInvalid(err))?;

        Ok(Schema {
            ttype_id,
            pkey,
        })
    }

    pub fn ttype_id(&self) -> &TTypeId {
        &self.ttype_id
    }

    pub fn pkey_ttype(&self, registry: &TypeRegistry) -> Result<TType, TypeRegistryError> {
        Ok(registry.get_by_id(&self.ttype_id)?.select(registry, &self.pkey).expect("invalid primary key idents"))
    }

    pub fn pkey(&self) -> &[IdentTree] {
        &self.pkey
    }
}
