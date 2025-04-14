use std::{collections::BTreeSet, io};

use super::{Relation, RelationRef, Row, RowSize, Schema};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MemoryRelation {
    schema: Schema,
    rows: BTreeSet<Row>,
}

impl MemoryRelation {
    pub fn new(schema: Schema) -> MemoryRelation {
        MemoryRelation {
            schema,
            rows: BTreeSet::new(),
        }
    }
}

impl RelationRef for MemoryRelation {
    fn schema(&self) -> &Schema {
        &self.schema
    }

    fn iter(&self) -> impl Iterator<Item = io::Result<Row>> {
        self.rows.clone().into_iter().map(|r| Ok(r))
    }
    
    fn contains(&self, row: &Row) -> io::Result<bool> {
        if !self.schema.type_check(row) {
            return Err(io::ErrorKind::InvalidInput.into());
        }
        Ok(self.rows.contains(row))
    }
}

impl Relation for MemoryRelation {
    fn insert(&mut self, new_row: Row) -> io::Result<bool> {
        if !self.schema.type_check(&new_row) {
            return Err(io::ErrorKind::InvalidInput.into());
        }
        Ok(self.rows.insert(new_row))
    }

    fn remove(&mut self, row: &Row) -> io::Result<bool> {
        if !self.schema.type_check(row) {
            return Err(io::ErrorKind::InvalidInput.into());
        }
        Ok(self.rows.remove(row))
    }

    fn retain(&mut self, predicate: impl Fn(&Row) -> bool) -> io::Result<RowSize> {
        let old_len = self.rows.len();
        self.rows.retain(predicate);
        let delta_len = old_len - self.rows.len();
        Ok(delta_len as RowSize)
    }
}

#[cfg(test)]
mod tests {
    use crate::{relation::Column, ttype::{ScalarType, ScalarValue}};

    use super::*;

    #[test]
    fn memory_relation() {
        let mut users = MemoryRelation::new(Schema {
            columns: vec![
                Column::new("id", ScalarType::Int32),
                Column::new("active", ScalarType::Bool),
            ],
        });
        let mut inactive_users = MemoryRelation::new(Schema {
            columns: vec![
                Column::new("id", ScalarType::Int32),
                Column::new("active", ScalarType::Bool),
            ],
        });

        users.insert(Row::new(vec![
            ScalarValue::Int32(0),
            ScalarValue::Bool(false),
        ])).unwrap();
        users.insert(Row::new(vec![
            ScalarValue::Int32(1),
            ScalarValue::Bool(true),
        ])).unwrap();
        users.insert(Row::new(vec![
            ScalarValue::Int32(2),
            ScalarValue::Bool(true),
        ])).unwrap();
        users.insert(Row::new(vec![
            ScalarValue::Int32(3),
            ScalarValue::Bool(false),
        ])).unwrap();
        users.insert(Row::new(vec![
            ScalarValue::Int32(4),
            ScalarValue::Bool(true),
        ])).unwrap();

        let new_rows = users.iter()
            .map(|r| r.unwrap())
            .filter_map(|r| (r.columns[1] == ScalarValue::Bool(false)).then(|| r.clone()));
        inactive_users.extend(new_rows).unwrap();

        let mut inactive_users_expected = MemoryRelation::new(Schema {
            columns: vec![
                Column::new("id", ScalarType::Int32),
                Column::new("active", ScalarType::Bool),
            ],
        });

        let user_3 = Row::new(vec![
            ScalarValue::Int32(3),
            ScalarValue::Bool(false),
        ]);

        inactive_users_expected.insert(Row::new(vec![
            ScalarValue::Int32(0),
            ScalarValue::Bool(false),
        ])).unwrap();
        inactive_users_expected.insert(user_3.clone()).unwrap();

        assert_eq!(inactive_users_expected, inactive_users);
        
        // cannot insert again
        assert_eq!(false, inactive_users.insert(user_3).unwrap());
        inactive_users.insert(Row::new(vec![])).unwrap_err();
        inactive_users.insert(Row::new(vec![ScalarValue::Int32(2)])).unwrap_err();
    }
}
