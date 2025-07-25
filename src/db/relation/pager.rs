use std::{collections::BTreeMap, ops::{Bound, RangeBounds}};

use codb_core::IdentForest;

use crate::db::registry::Registry;

use super::{Key, Relation, Row, RowSize, Schema};

#[binrw]
#[derive(Debug, PartialEq, Eq, Clone, serde::Serialize, serde::Deserialize)]
pub struct PagerRelation {
    schema: Schema,
    #[bw(calc = self.rows.len() as u64)]
    len: u64,
    #[br(count = len, map = |rows: Vec<(Key, Row)>| BTreeMap::from_iter(rows.into_iter()))]
    #[bw(map = |rows| Vec::<(Key, Row)>::from_iter(rows.clone().into_iter()))]
    rows: BTreeMap<Key, Row>,
}

impl PagerRelation {
    pub fn new(schema: Schema) -> PagerRelation {
        PagerRelation {
            schema,
            rows: BTreeMap::new(),
        }
    }
}

enum PagerRelationIter<T: Iterator<Item = Row>, U: Iterator<Item = Row>> {
    Index(T),
    Scan(U),
}

impl<T: Iterator<Item = Row>, U: Iterator<Item = Row>> Iterator for PagerRelationIter<T, U> {
    type Item = Row;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            PagerRelationIter::Index(iter) => iter.next(),
            PagerRelationIter::Scan(iter) => iter.next(),
        }
    }
}

impl Relation for PagerRelation {
    fn schema(&self) -> Schema {
        self.schema.clone()
    }

    fn range(&self, registry: &Registry, ident_forest: &IdentForest, range: impl RangeBounds<Key>) -> impl Iterator<Item = Row> {
        let selected_type = self.schema.ttype.select(registry, ident_forest).expect("invalid ident forest");

        if let Bound::Included(bound) | Bound::Excluded(bound) = range.start_bound() {
            if selected_type != bound.ttype() {
                panic!("invalid bound");
            }
        }
        
        if let Bound::Included(bound) | Bound::Excluded(bound) = range.end_bound() {
            if selected_type != bound.ttype() {
                panic!("invalid bound");
            }
        }

        if selected_type.eq(&self.schema.pkey_ttype(registry).expect("invalid pkey")) {
            PagerRelationIter::Index(self.rows.range(range).map(|(_, row)| row.clone()))
        } else {
            let owned_range = (range.start_bound().cloned(), range.end_bound().cloned());
            PagerRelationIter::Scan(self.rows.iter().filter(move |(pkey, _)| owned_range.contains(pkey)).map(|(_, row)| row.clone()))
        }
    }

    fn insert(&mut self, registry: &Registry, new_row: Row) -> bool {
        if self.schema.ttype != new_row.ttype() {
            panic!("invalid row");
        }

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

        if pkey_ttype != pkey.ttype() {
            panic!("invalid pkey");
        }

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
    use std::sync::{Arc, Mutex};

    use itertools::Itertools;

    use crate::{db::{pager::Pager, registry::TTypeId, DbRelationSet}, typesystem::{ttype::StructType, value::{ScalarValue, StructValue}}};

    use super::*;

    #[test]
    fn pager_relation() {
        fn new_user(id: i32, active: bool) -> StructValue {
            // SAFETY: test case
            unsafe { StructValue::new_unchecked(indexmap! {
                id!("id") => ScalarValue::Int32(id).into(),
                id!("active") => ScalarValue::Bool(active).into(),
            }) }
        }

        fn new_id(id: i32) -> StructValue {
            // SAFETY: test case
            unsafe { StructValue::new_unchecked(indexmap! {
                id!("id") => ScalarValue::Int32(id).into(),
            }) }
        }

        let pager = Arc::new(Mutex::new(Pager::new_memory()));
        let registry = Registry::new(pager, &DbRelationSet::new());

        let user_struct = StructType::new(indexmap! {
            id!("id") => TTypeId::INT32,
            id!("active") => TTypeId::BOOL,
        });

        let user_pkey = IdentForest::from_nested_idents([id!("id").into()]);
        let user_schema = Schema::new(&registry, user_struct, user_pkey).unwrap();

        let mut users = PagerRelation::new(user_schema.clone());
        let mut inactive_users = PagerRelation::new(user_schema.clone());

        users.insert(&registry, new_user(0, false));
        users.insert(&registry, new_user(1, true));
        users.insert(&registry, new_user(2, true));
        users.insert(&registry, new_user(3, false));
        users.insert(&registry, new_user(4, true));

        let empty_forest = IdentForest::empty();

        let new_rows = users.range(&registry, &empty_forest, ..)
            .filter_map(|row| {
                row.fields().get("active")
                    .filter(|value| **value == ScalarValue::Bool(false).into())
                    .map(|_| row.clone())
            });
        inactive_users.extend(&registry, new_rows);

        let mut inactive_users_expected = PagerRelation::new(user_schema.clone());

        let user_3 = new_user(3, false);

        inactive_users_expected.insert(&registry, new_user(0, false));
        inactive_users_expected.insert(&registry, user_3.clone());

        assert_eq!(inactive_users_expected, inactive_users);
        
        // cannot insert again
        assert_eq!(false, inactive_users.insert(&registry, user_3));
        // inactive_users.insert(&registry, Value::Composite(CompositeValue::new_struct(&registry, &TTypeId::Scalar(ScalarType::Bool), vec![]).unwrap())).unwrap_err();
        // inactive_users.insert(&registry, Value::Composite(CompositeValue::new_struct(&registry, &TTypeId::Scalar(ScalarType::Bool), vec![FieldValue::new("id", Value::Scalar(ScalarValueInner::Int32(2)))]).unwrap())).unwrap_err();

        // check range
        let users_range = users.range(&registry, users.schema.pkey(), new_id(1)..new_id(3)).enumerate().collect_vec();

        assert_eq!(2, users_range.len());

        for (i, user) in users_range {
            assert_eq!(user, new_user(i as i32 + 1, true));
        }
    }
}
