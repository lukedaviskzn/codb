use std::ops::RangeBounds;

use codb_core::IdentForest;

use crate::{db::registry::{Registry, TTypeId}, query::schema_query::SchemaError, typesystem::{ttype::StructType, value::StructValue, TypeError}};

pub mod memory;

pub type Key = StructValue;
pub type Row = StructValue;
pub type RowSize = u64;

pub trait Relation {
    fn schema(&self) -> Schema;
    fn range(&self, registry: &Registry, ident_forest: &IdentForest, range: impl RangeBounds<Key>) -> impl Iterator<Item = Row>;
    fn insert(&mut self, registry: &Registry, new_row: Row) -> bool;
    
    #[allow(unused)]
    fn extend(&mut self, registry: &Registry, new_rows: impl IntoIterator<Item = Row>) -> RowSize {
        let mut count = 0;
        for new_row in new_rows {
            if self.insert(registry, new_row) {
                count += 1;
            }
        }
        count
    }
    
    fn remove(&mut self, registry: &Registry, pkey: &Key) -> Option<Row>;
    fn retain(&mut self, predicate: impl Fn(&Row) -> bool) -> RowSize;
    
    #[cfg(test)]
    fn eq(&self, registry: &Registry, other: &impl Relation) -> bool {
        use itertools::Itertools;

        if self.schema() != other.schema() {
            return false;
        }

        let empty_forest = IdentForest::empty();

        let zipped = self.range(registry, &empty_forest, ..).zip_longest(other.range(registry, &empty_forest, ..));
        
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

        for row in self.range(registry, &empty_forest, ..) {
            out_string += &format!("{:?}\n", row);
        }

        // remove final \n
        out_string.pop();

        out_string
    }
}

#[binrw]
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
