use std::{collections::BTreeMap, panic::catch_unwind, sync::RwLock};

use codb_core::Ident;
use registry::Registry;
use relation::Relation;

use crate::{query::{Query, QueryExecutionError}, typesystem::{scope::ScopeValues, value::Value}};

pub mod registry;
pub mod relation;

// DB
// |
// v
// Lock(Inner)
// |        \
// v         '----> Type Registry
// Lock(Relations)-----.
// |                    \
// v                     v
// Lock(Relation A)   Lock(Relation B)
    #[allow(unused)]
pub struct Db<R: Relation> {
    inner: RwLock<DbInner<R>>,
}

impl<R: Relation> Db<R> {
    #[allow(unused)]
    pub fn new() -> Self {
        Self {
            inner: RwLock::new(DbInner::new()),
        }
    }

    #[allow(unused)]
    pub fn execute(&self, query: Query) -> Result<Value, QueryExecutionError> {
        let out = catch_unwind(|| {
            match query {
                Query::Data(expression) => {
                    // todo: fix unwrap
                    let db = self.inner.read().unwrap();
                    let registry = db.registry();
                    let relations = db.relations().read().unwrap();
                    expression.eval(registry, &relations, &ScopeValues::EMPTY)
                },
            }
        });

        match out {
            Ok(Ok(out)) => Ok(out),
            Ok(Err(err)) => Err(err.into()),
            Err(err) => Err(QueryExecutionError::UnexpectedPanic(err)),
        }
    }
}

pub struct DbInner<R: Relation> {
    registry: Registry,
    relations: RwLock<DbRelations<R>>,
}

impl<R: Relation> DbInner<R> {
    fn new() -> Self {
        let relations = DbRelations::new();
        
        Self {
            registry: Registry::new(&relations),
            relations: RwLock::new(relations),
        }
    }

    pub fn registry(&self) -> &Registry {
        &self.registry
    }

    #[allow(unused)]
    pub fn registry_mut(&mut self) -> &mut Registry {
        &mut self.registry
    }

    #[allow(unused)]
    pub fn relations(&self) -> &RwLock<DbRelations<R>> {
        &self.relations
    }
}

pub struct DbRelations<R: Relation>(BTreeMap<Ident, RwLock<R>>);

impl<R: Relation> DbRelations<R> {
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub fn get(&self, relation: &str) -> Option<&RwLock<R>> {
        self.0.get(relation)
    }

    pub fn insert(&mut self, name: Ident, relation: R) -> bool {
        if self.0.contains_key(&name) {
            return false;
        }
        self.0.insert(name, RwLock::new(relation));
        true
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Bound;

    use codb_core::IdentForest;
    use itertools::Itertools;

    use crate::{expression::{Expression, InterpreterAction, Literal, StructLiteral}, typesystem::{ttype::{EnumType, StructType}, value::{ArrayValue, EnumValue, ScalarValue, StructValue}}};

    use super::{registry::TTypeId, relation::{memory::MemoryRelation, RelationRef, Schema}, *};

    fn db_initial_values(ttype: &StructType) -> [StructValue; 3] {
        let mut rows = [
            // SAFETY: test case
            unsafe { StructValue::new_unchecked(TTypeId::new_anonymous(ttype.clone().into()), btreemap! {
                id!("id") => Value::Scalar(ScalarValue::Int32(1).into()),
                id!("name") => Value::Scalar(ScalarValue::String("Jim Jones".into()).into()),
                id!("active") => Value::Scalar(ScalarValue::Bool(true).into()),
            }) }.into(),
            // SAFETY: test case
            unsafe { StructValue::new_unchecked(TTypeId::new_anonymous(ttype.clone().into()), btreemap! {
                id!("id") => Value::Scalar(ScalarValue::Int32(2).into()),
                id!("name") => Value::Scalar(ScalarValue::String("Jimboni Jonesi".into()).into()),
                id!("active") => Value::Scalar(ScalarValue::Bool(true).into()),
            }) }.into(),
            // SAFETY: test case
            unsafe { StructValue::new_unchecked(TTypeId::new_anonymous(ttype.clone().into()), btreemap! {
                id!("id") => Value::Scalar(ScalarValue::Int32(-1).into()),
                id!("name") => Value::Scalar(ScalarValue::String("El Jones, Jim".into()).into()),
                id!("active") => Value::Scalar(ScalarValue::Bool(false).into()),
            }) }.into(),
        ];
        rows.sort();
        rows
    }

    fn create_db() -> Db<MemoryRelation> {
        let db = Db::<MemoryRelation>::new();
        
        {
            let db_inner = db.inner.read().unwrap();
            
            let my_type = StructType::new(indexmap! {
                id!("id") => TTypeId::INT32,
                id!("name") => TTypeId::STRING,
                id!("active") => TTypeId::BOOL,
            });

            let mut relations = db_inner.relations().write().unwrap();

            let registry = db_inner.registry();
            
            relations.insert(id!("Rel"), MemoryRelation::new(
                Schema::new(registry, my_type.clone(), IdentForest::from_nested_idents([id!("id").into()])).unwrap()
            ));

            relations.get("Rel").unwrap().write().unwrap().extend(&registry, db_initial_values(&my_type));
        }

        db
    }

    #[test]
    fn test() {
        let db = Db::<MemoryRelation>::new();
        let db = db.inner.read().unwrap();
        
        let my_type = StructType::new(indexmap! {
            id!("id") => TTypeId::INT32,
            id!("name") => TTypeId::STRING,
            id!("active") => TTypeId::BOOL,
        });

        let mut relations = db.relations().write().unwrap();

        let registry = db.registry();
        
        relations.insert(id!("Rel"), MemoryRelation::new(
            Schema::new(registry, my_type, IdentForest::from_nested_idents([id!("id").into()])).unwrap()
        ));

        let rel = relations.get("Rel").unwrap().read().unwrap();
        rel.draw(registry);
    }

    #[test]
    fn execute_range_remove() {
        let db = create_db();

        let value = db.execute(Query::Data(Expression::Action(InterpreterAction::Range {
            relation: id!("Rel"),
            ident_forest: IdentForest::empty(),
            start: Box::new(Bound::Unbounded),
            end: Box::new(Bound::Unbounded),
        }))).unwrap();

        let array: ArrayValue = value.try_into().unwrap();
        let mut entries: Vec<StructValue> = array.entries().iter().map(|entry| entry.clone().try_into().unwrap()).collect_vec();

        let expected_deleted_row: Value;
        let relation_type_option;
        let pkey_value: Literal;
        {
            let db_inner = db.inner.read().unwrap();
            let registry = db_inner.registry();
            let relations = db_inner.relations().read().unwrap();
            let relation = relations.get(&id!("Rel")).unwrap();
            let relation = relation.read().unwrap();

            assert_eq!(entries, db_initial_values(relation.schema().ttype()));

            expected_deleted_row = entries.remove(0).into();

            let pkey_type = TTypeId::new_anonymous(relation.schema().pkey_ttype(registry).unwrap().into());
            
            pkey_value = StructLiteral::new(
                pkey_type,
                btreemap! {
                    id!("id") => Expression::Literal(ScalarValue::Int32(-1).into()),
                },
            ).into();

            relation_type_option = EnumType::new_option(relation.schema().ttype_id());
        }

        let out = db.execute(Query::Data(Expression::Action(InterpreterAction::Remove {
            relation: id!("Rel"),
            pkey: Box::new(Expression::Literal(pkey_value.clone())),
        }))).unwrap();

        let out: EnumValue = out.try_into().unwrap();
        let out = out.into_inner_value();

        assert_eq!(expected_deleted_row, out);

        let out = db.execute(Query::Data(Expression::Action(InterpreterAction::Remove {
            relation: id!("Rel"),
            pkey: Box::new(Expression::Literal(pkey_value.clone())),
        }))).unwrap();

        let out: EnumValue = out.try_into().unwrap();

        let expected_none = EnumValue::new_option_none(TTypeId::new_anonymous(relation_type_option.into()));

        assert_eq!(expected_none, out);
    }
}
