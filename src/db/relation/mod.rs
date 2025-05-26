use std::ops::RangeBounds;

use codb_core::IdentForest;

use crate::{db::registry::{Registry, TTypeId}, typesystem::{ttype::StructType, value::StructValue, TypeError}};

pub mod memory;
pub mod file;

pub type Key = StructValue;
pub type Row = StructValue;
pub type RowSize = u64;

pub trait RelationRef {
    fn schema(&self) -> &Schema;
    fn range(&self, registry: &Registry, ident_forest: &IdentForest, range: impl RangeBounds<Key>) -> Result<impl Iterator<Item = Row>, TypeError>;
    
    #[cfg(test)]
    fn eq(&self, registry: &Registry, other: &impl RelationRef) -> bool {
        use itertools::Itertools;

        if self.schema() != other.schema() {
            return false;
        }

        let empty_forest = IdentForest::empty();

        let zipped = self.range(registry, &empty_forest, ..).unwrap().zip_longest(other.range(registry, &empty_forest, ..).unwrap());
        
        for rows in zipped {
            match rows {
                itertools::EitherOrBoth::Both(left, right) => if left != right {
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

        let empty_forest = IdentForest::empty();

        for row in self.range(registry, &empty_forest, ..).unwrap() {
            out_string += &format!("{:?}\n", row);
        }

        // remove final \n
        out_string.pop();

        out_string
    }
}

pub trait Relation: RelationRef {
    fn insert(&mut self, registry: &Registry, new_row: Row) -> Result<bool, TypeError>;
    
    #[allow(unused)]
    fn extend(&mut self, registry: &Registry, new_rows: impl IntoIterator<Item = Row>) -> Result<RowSize, TypeError> {
        let mut count = 0;
        for new_row in new_rows {
            if self.insert(registry, new_row)? {
                count += 1;
            }
        }
        Ok(count)
    }
    
    fn remove(&mut self, registry: &Registry, pkey: &Key) -> Result<Option<Row>, TypeError>;
    fn retain(&mut self, predicate: impl Fn(&Row) -> bool) -> RowSize;
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum SchemaError {
    #[error("cannot make primary key: {0}")]
    PrimaryKeyInvalid(TypeError),
}

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct Schema {
    ttype: StructType,
    pkey: IdentForest,
}

impl Schema {
    #[allow(unused)]
    pub fn new(registry: &Registry, ttype: StructType, pkey: IdentForest) -> Result<Schema, SchemaError> {
        if let Err(err) = ttype.select(registry, &pkey) {
            return Err(SchemaError::PrimaryKeyInvalid(err));
        }

        Ok(Schema {
            ttype,
            pkey,
        })
    }

    #[allow(unused)]
    pub fn ttype(&self) -> &StructType {
        &self.ttype
    }

    pub fn ttype_id(&self) -> TTypeId {
        TTypeId::new_anonymous(self.ttype.clone().into())
    }

    pub fn pkey_ttype(&self, registry: &Registry) -> Result<StructType, TypeError> {
        self.ttype.select(registry, &self.pkey)
    }

    #[allow(unused)]
    pub fn pkey(&self) -> &IdentForest {
        &self.pkey
    }
}
