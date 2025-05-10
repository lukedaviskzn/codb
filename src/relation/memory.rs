use std::{collections::BTreeMap, io, ops::{Bound, RangeBounds}};

use crate::{idents::IdentTree, typesystem::{registry::TypeRegistry, value::Value, TypeError}};

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

    fn range(&self, registry: &TypeRegistry, nested_idents: impl Into<Box<[IdentTree]>>, range: impl RangeBounds<Value>) -> Result<impl Iterator<Item = io::Result<Row>>, TypeError> {
        let idents = Into::<Box<[IdentTree]>>::into(nested_idents);
        let selected_type = registry.get_by_id(&self.schema.ttype_id)?.select(registry, &idents)?;

        if let Bound::Included(bound) | Bound::Excluded(bound) = range.start_bound() {
            selected_type.check(registry, bound)?;
        }
        if let Bound::Included(bound) | Bound::Excluded(bound) = range.end_bound() {
            selected_type.check(registry, bound)?;
        }

        if selected_type.eq(&self.schema.pkey_ttype(registry)?) {
            Ok(MemRelationIter::Index(self.rows.range(range).map(|(_, row)| Ok(row.clone()))))
        } else {
            let owned_range = (range.start_bound().cloned(), range.end_bound().cloned());
            Ok(MemRelationIter::Scan(self.rows.iter().filter(move |(pkey, _)| owned_range.contains(pkey)).map(|(_, row)| Ok(row.clone()))))
        }
    }
}

impl Relation for MemoryRelation {
    fn insert(&mut self, registry: &TypeRegistry, new_row: Row) -> Result<io::Result<bool>, TypeError> {
        registry.get_by_id(&self.schema.ttype_id)?.check(registry, &new_row)?;

        let pkey = new_row.select(registry, &self.schema.pkey)?;
        
        if self.rows.contains_key(&pkey) {
            Ok(Ok(false))
        } else {
            self.rows.insert(pkey, new_row);
            Ok(Ok(true))
        }
    }

    fn remove(&mut self, registry: &TypeRegistry, pkey: &PKey) -> Result<io::Result<Option<Row>>, TypeError> {
        self.schema.pkey_ttype(registry)?.check(registry, pkey)?;
        Ok(Ok(self.rows.remove(pkey)))
    }

    fn retain(&mut self, predicate: impl Fn(&Row) -> bool) -> io::Result<RowSize> {
        let old_len = self.rows.len();
        self.rows.retain(|_, row| predicate(row));
        let delta_len = old_len - self.rows.len();
        Ok(delta_len as RowSize)
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::typesystem::{registry::TTypeId, ttype::{CompositeType, FieldType, TType}, value::{CompositeValue, FieldValue, ScalarValue, Value}};

    use super::*;

    #[test]
    fn memory_relation() {
        fn new_user(registry: &TypeRegistry, user_ttype_id: &TTypeId, id: i32, active: bool) -> Value {
            Value::Composite(CompositeValue::new_struct(registry, user_ttype_id, vec![
                FieldValue::new("id".parse().unwrap(), Value::Scalar(ScalarValue::Int32(id))),
                FieldValue::new("active".parse().unwrap(), Value::Scalar(ScalarValue::Bool(active))),
            ]).unwrap())
        }

        fn new_id(registry: &TypeRegistry, user_id_ttype_id: &TTypeId, id: i32) -> Value {
            Value::Composite(CompositeValue::new_struct(registry, user_id_ttype_id, vec![
                FieldValue::new("id".parse().unwrap(), Value::Scalar(ScalarValue::Int32(id))),
            ]).unwrap())
        }

        let mut registry = TypeRegistry::new();

        let user_ttype = TType::Composite(CompositeType::new_struct(vec![
            FieldType::new("id".parse().unwrap(), TTypeId::INT32),
            FieldType::new("active".parse().unwrap(), TTypeId::BOOL),
        ]).unwrap());

        let user_id_ttype_id = TTypeId::Anonymous(Box::new(user_ttype.select(&registry, &IdentTree::from_nested_idents(vec!["id".parse().unwrap()])).unwrap()));

        let user_ttype_id = registry.add("User", user_ttype).unwrap();
        
        let user_pkey = IdentTree::from_nested_idents(vec!["id".parse().unwrap()]);
        let user_schema = Schema::new(&registry, user_ttype_id.clone(), user_pkey).unwrap();

        let mut users = MemoryRelation::new(user_schema.clone());
        let mut inactive_users = MemoryRelation::new(user_schema.clone());

        users.insert(&registry, new_user(&registry, &user_ttype_id, 0, false)).unwrap().unwrap();
        users.insert(&registry, new_user(&registry, &user_ttype_id, 1, true)).unwrap().unwrap();
        users.insert(&registry, new_user(&registry, &user_ttype_id, 2, true)).unwrap().unwrap();
        users.insert(&registry, new_user(&registry, &user_ttype_id, 3, false)).unwrap().unwrap();
        users.insert(&registry, new_user(&registry, &user_ttype_id, 4, true)).unwrap().unwrap();

        let new_rows = users.range(&registry, [], ..).unwrap()
            .map(|r| r.unwrap())
            .filter_map(|row| {
                let Value::Composite(CompositeValue::Struct(value)) = row.clone() else { unreachable!() };
                value.fields().iter().find(|f| f.name() == "active" && *f.value() == Value::Scalar(ScalarValue::Bool(false)))
                    .map(|_| row)
            });
        inactive_users.extend(&registry, new_rows).unwrap().unwrap();

        let mut inactive_users_expected = MemoryRelation::new(user_schema.clone());

        let user_3 = new_user(&registry, &user_ttype_id, 3, false);

        inactive_users_expected.insert(&registry, new_user(&registry, &user_ttype_id, 0, false)).unwrap().unwrap();
        inactive_users_expected.insert(&registry, user_3.clone()).unwrap().unwrap();

        assert_eq!(inactive_users_expected, inactive_users);
        
        // cannot insert again
        assert_eq!(false, inactive_users.insert(&registry, user_3).unwrap().unwrap());
        // inactive_users.insert(&registry, Value::Composite(CompositeValue::new_struct(&registry, &TTypeId::Scalar(ScalarType::Bool), vec![]).unwrap())).unwrap_err();
        // inactive_users.insert(&registry, Value::Composite(CompositeValue::new_struct(&registry, &TTypeId::Scalar(ScalarType::Bool), vec![FieldValue::new("id", Value::Scalar(ScalarValue::Int32(2)))]).unwrap())).unwrap_err();

        // check range
        let users_range = users.range(&registry, users.schema.pkey(), new_id(&registry, &user_id_ttype_id, 1)..new_id(&registry, &user_id_ttype_id, 3)).unwrap().enumerate().collect_vec();

        assert_eq!(2, users_range.len());

        for (i, user) in users_range {
            assert_eq!(user.unwrap(), new_user(&registry, &user_ttype_id, i as i32 + 1, true));
        }
    }
}
