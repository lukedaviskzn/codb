use std::{collections::BTreeMap, io, ops::{Bound, RangeBounds}};

use crate::typesystem::{value::CompositeValue, NestedIdents, TypeError};

use super::{PKey, Relation, RelationRef, Row, RowSize, Schema};

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct MemoryRelation {
    schema: Schema,
    rows: BTreeMap<PKey, Row>,
}

impl MemoryRelation {
    pub fn new(schema: Schema) -> MemoryRelation {
        MemoryRelation {
            schema,
            rows: BTreeMap::new(),
        }
    }
}

enum MemRelationIter<T: Iterator<Item = io::Result<Row>>, U: Iterator<Item = io::Result<Row>>> {
    Index(T),
    Scan(U),
}

impl<T: Iterator<Item = io::Result<Row>>, U: Iterator<Item = io::Result<Row>>> Iterator for MemRelationIter<T, U> {
    type Item = io::Result<Row>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            MemRelationIter::Index(iter) => iter.next(),
            MemRelationIter::Scan(iter) => iter.next(),
        }
    }
}

impl RelationRef for MemoryRelation {
    fn schema(&self) -> &Schema {
        &self.schema
    }

    fn range(&self, nested_idents: impl Into<Box<[NestedIdents]>>, range: impl RangeBounds<CompositeValue>) -> Result<impl Iterator<Item = io::Result<Row>>, TypeError> {
        let idents = Into::<Box<[NestedIdents]>>::into(nested_idents);
        let selected_type = self.schema.ttype.select(&idents)?;

        if let Bound::Included(bound) | Bound::Excluded(bound) = range.start_bound() {
            selected_type.check(bound)?;
        }
        if let Bound::Included(bound) | Bound::Excluded(bound) = range.end_bound() {
            selected_type.check(bound)?;
        }

        if selected_type.eq(&self.schema.pkey_ttype()) {
            Ok(MemRelationIter::Index(self.rows.range(range).map(|(_, row)| Ok(row.clone()))))
        } else {
            let owned_range = (range.start_bound().cloned(), range.end_bound().cloned());
            Ok(MemRelationIter::Scan(self.rows.iter().filter(move |(pkey, _)| owned_range.contains(pkey)).map(|(_, row)| Ok(row.clone()))))
        }
    }
}

impl Relation for MemoryRelation {
    fn insert(&mut self, new_row: CompositeValue) -> Result<io::Result<bool>, TypeError> {
        self.schema.ttype.check(&new_row)?;

        let pkey = new_row.select(&self.schema.pkey)?;
        
        if self.rows.contains_key(&pkey) {
            Ok(Ok(false))
        } else {
            self.rows.insert(pkey, new_row);
            Ok(Ok(true))
        }
    }

    fn remove(&mut self, pkey: &CompositeValue) -> Result<io::Result<Option<CompositeValue>>, TypeError> {
        self.schema.pkey_ttype().check(pkey)?;
        Ok(Ok(self.rows.remove(pkey)))
    }

    fn retain(&mut self, predicate: impl Fn(&CompositeValue) -> bool) -> io::Result<RowSize> {
        let old_len = self.rows.len();
        self.rows.retain(|_, row| predicate(row));
        let delta_len = old_len - self.rows.len();
        Ok(delta_len as RowSize)
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::typesystem::{ttype::{CompositeType, FieldType, TType}, value::{FieldValue, ScalarValue, Value}};

    use super::*;

    #[test]
    fn memory_relation() {
        fn new_user(id: i32, active: bool) -> CompositeValue {
            CompositeValue::new_struct(vec![
                FieldValue::new("id", Value::Scalar(ScalarValue::Int32(id))),
                FieldValue::new("z_active", Value::Scalar(ScalarValue::Bool(active))),
            ]).unwrap()
        }

        fn new_id(id: i32) -> CompositeValue {
            CompositeValue::new_struct(vec![
                FieldValue::new("id", Value::Scalar(ScalarValue::Int32(id))),
            ]).unwrap()
        }

        let user_ttype = CompositeType::new_struct(vec![
            FieldType::new("id", TType::INT32),
            FieldType::new("z_active", TType::BOOL),
        ]).unwrap();
        let user_pkey = NestedIdents::from_strings(vec!["id"]);
        let user_schema = Schema::new(user_ttype, user_pkey).unwrap();

        let mut users = MemoryRelation::new(user_schema.clone());
        let mut inactive_users = MemoryRelation::new(user_schema.clone());

        users.insert(new_user(0, false)).unwrap();
        users.insert(new_user(1, true)).unwrap();
        users.insert(new_user(2, true)).unwrap();
        users.insert(new_user(3, false)).unwrap();
        users.insert(new_user(4, true)).unwrap();

        let new_rows = users.range([], ..).unwrap()
            .map(|r| r.unwrap())
            .filter_map(|row| {
                let CompositeValue::Struct(value) = row.clone() else { unreachable!() };
                value.fields().iter().find(|f| f.name() == "z_active" && *f.value() == Value::Scalar(ScalarValue::Bool(false)))
                    .map(|_| row)
            });
        inactive_users.extend(new_rows).unwrap();

        let mut inactive_users_expected = MemoryRelation::new(user_schema.clone());

        let user_3 = new_user(3, false);

        inactive_users_expected.insert(new_user(0, false)).unwrap();
        inactive_users_expected.insert(user_3.clone()).unwrap();

        assert_eq!(inactive_users_expected, inactive_users);
        
        // cannot insert again
        assert_eq!(false, inactive_users.insert(user_3).unwrap().unwrap());
        inactive_users.insert(CompositeValue::new_struct(vec![]).unwrap()).unwrap_err();
        inactive_users.insert(CompositeValue::new_struct(vec![FieldValue::new("id", Value::Scalar(ScalarValue::Int32(2)))]).unwrap()).unwrap_err();

        // check range
        let users_range = users.range(users.schema.pkey(), new_id(1)..new_id(3)).unwrap().enumerate().collect_vec();

        assert_eq!(2, users_range.len());

        for (i, user) in users_range {
            assert_eq!(user.unwrap(), new_user(i as i32 + 1, true));
        }
    }
}
