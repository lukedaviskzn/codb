use std::{collections::BTreeMap, ops::{Bound, RangeBounds}};

use codb_core::IdentForest;

use crate::db::registry::{Registry, TTypeId};

use super::{Key, Relation, RelationRef, Row, RowSize, Schema};

#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct MemoryRelation {
    schema: Schema,
    rows: BTreeMap<Key, Row>,
}

impl MemoryRelation {
    pub fn new(schema: Schema) -> MemoryRelation {
        MemoryRelation {
            schema,
            rows: BTreeMap::new(),
        }
    }
}

enum MemRelationIter<T: Iterator<Item = Row>, U: Iterator<Item = Row>> {
    Index(T),
    Scan(U),
}

impl<T: Iterator<Item = Row>, U: Iterator<Item = Row>> Iterator for MemRelationIter<T, U> {
    type Item = Row;

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

    fn range(&self, registry: &Registry, ident_forest: &IdentForest, range: impl RangeBounds<Key>) -> impl Iterator<Item = Row> {
        let selected_type = self.schema.ttype.select(registry, ident_forest).expect("invalid ident forest");

        let selected_type_id = TTypeId::new_anonymous(selected_type.clone().into());

        if let Bound::Included(bound) | Bound::Excluded(bound) = range.start_bound() {
            bound.ttype_id().must_eq(&selected_type_id).expect("invalid bound");
        }
        
        if let Bound::Included(bound) | Bound::Excluded(bound) = range.end_bound() {
            bound.ttype_id().must_eq(&selected_type_id).expect("invalid bound");
        }

        if selected_type.eq(&self.schema.pkey_ttype(registry).expect("invalid pkey")) {
            MemRelationIter::Index(self.rows.range(range).map(|(_, row)| row.clone()))
        } else {
            let owned_range = (range.start_bound().cloned(), range.end_bound().cloned());
            MemRelationIter::Scan(self.rows.iter().filter(move |(pkey, _)| owned_range.contains(pkey)).map(|(_, row)| row.clone()))
        }
    }
}

impl Relation for MemoryRelation {
    fn insert(&mut self, registry: &Registry, new_row: Row) -> bool {
        let schema_ttype_id = self.schema.ttype_id();

        new_row.ttype_id().must_eq(&schema_ttype_id).expect("incorrect row type");

        let pkey = new_row.select(registry, &self.schema.pkey).expect("invalid pkey");
        
        if self.rows.contains_key(&pkey) {
            false
        } else {
            self.rows.insert(pkey, new_row);
            true
        }
    }

    fn remove(&mut self, registry: &Registry, pkey: &Key) -> Option<Row> {
        let pkey_ttype = self.schema.pkey_ttype(registry).expect("invalid pkey forest");
        let pkey_ttype_id = TTypeId::new_anonymous(pkey_ttype.clone().into());

        pkey_ttype_id.must_eq(&pkey.ttype_id()).expect("invalid pkey");

        self.rows.remove(pkey)
    }

    fn retain(&mut self, predicate: impl Fn(&Row) -> bool) -> RowSize {
        let old_len = self.rows.len();
        self.rows.retain(|_, row| predicate(row));
        let delta_len = old_len - self.rows.len();
        delta_len as RowSize
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{db::DbRelations, typesystem::{ttype::StructType, value::{ScalarValue, StructValue}}};

    use super::*;

    #[test]
    fn memory_relation() {
        fn new_user(user_ttype_id: &TTypeId, id: i32, active: bool) -> StructValue {
            // SAFETY: test case
            unsafe { StructValue::new_unchecked(user_ttype_id.clone(), btreemap! {
                id!("id") => ScalarValue::Int32(id).into(),
                id!("active") => ScalarValue::Bool(active).into(),
            }) }
        }

        fn new_id(user_id_ttype_id: &TTypeId, id: i32) -> StructValue {
            // SAFETY: test case
            unsafe { StructValue::new_unchecked(user_id_ttype_id.clone(), btreemap! {
                id!("id") => ScalarValue::Int32(id).into(),
            }) }
        }

        let registry = Registry::new(&DbRelations::<MemoryRelation>::new());

        let user_struct = StructType::new(indexmap! {
            id!("id") => TTypeId::INT32,
            id!("active") => TTypeId::BOOL,
        });

        let user_ttype_id = TTypeId::new_anonymous(user_struct.clone().into());

        let user_id_struct = user_struct.select(
            &registry,
            &IdentForest::from_nested_idents([id!("id").into()])
        ).unwrap();

        let user_id_ttype_id = TTypeId::new_anonymous(user_id_struct.clone().into());

        let user_pkey = IdentForest::from_nested_idents([id!("id").into()]);
        let user_schema = Schema::new(&registry, user_struct, user_pkey).unwrap();

        let mut users = MemoryRelation::new(user_schema.clone());
        let mut inactive_users = MemoryRelation::new(user_schema.clone());

        users.insert(&registry, new_user(&user_ttype_id, 0, false));
        users.insert(&registry, new_user(&user_ttype_id, 1, true));
        users.insert(&registry, new_user(&user_ttype_id, 2, true));
        users.insert(&registry, new_user(&user_ttype_id, 3, false));
        users.insert(&registry, new_user(&user_ttype_id, 4, true));

        let empty_forest = IdentForest::empty();

        let new_rows = users.range(&registry, &empty_forest, ..)
            .filter_map(|row| {
                row.fields().get("active")
                    .filter(|value| **value == ScalarValue::Bool(false).into())
                    .map(|_| row.clone())
            });
        inactive_users.extend(&registry, new_rows);

        let mut inactive_users_expected = MemoryRelation::new(user_schema.clone());

        let user_3 = new_user(&user_ttype_id, 3, false);

        inactive_users_expected.insert(&registry, new_user(&user_ttype_id, 0, false));
        inactive_users_expected.insert(&registry, user_3.clone());

        assert_eq!(inactive_users_expected, inactive_users);
        
        // cannot insert again
        assert_eq!(false, inactive_users.insert(&registry, user_3));
        // inactive_users.insert(&registry, Value::Composite(CompositeValue::new_struct(&registry, &TTypeId::Scalar(ScalarType::Bool), vec![]).unwrap())).unwrap_err();
        // inactive_users.insert(&registry, Value::Composite(CompositeValue::new_struct(&registry, &TTypeId::Scalar(ScalarType::Bool), vec![FieldValue::new("id", Value::Scalar(ScalarValueInner::Int32(2)))]).unwrap())).unwrap_err();

        // check range
        let users_range = users.range(&registry, users.schema.pkey(), new_id(&user_id_ttype_id, 1)..new_id(&user_id_ttype_id, 3)).enumerate().collect_vec();

        assert_eq!(2, users_range.len());

        for (i, user) in users_range {
            assert_eq!(user, new_user(&user_ttype_id, i as i32 + 1, true));
        }
    }
}
