use std::{borrow::Cow, collections::HashMap, sync::RwLock};

use codb_core::Ident;
use registry::{Registry, TTypeId};
use relation::Relation;

use crate::{query::{DataQuery, Query}, scope::{ScopeTypes, ScopeValues}, typesystem::{ttype::StructType, value::{StructValue, Value}}};

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
pub struct Db<R: Relation> {
    inner: RwLock<DbInner<R>>,
}

impl<R: Relation> Db<R> {
    pub fn new() -> Self {
        Self {
            inner: RwLock::new(DbInner::new()),
        }
    }

    pub fn execute(&self, query: Query) {
        match query {
            Query::Data(query) => match query {
                DataQuery::Range {
                    relation,
                    ident_trees,
                    range_start,
                    range_end,
                } => {
                    let db = self.inner.read().unwrap();
                    let registry = &db.registry;

                    let relations = db.relations.read().unwrap();
                    let relation = relations.get(&relation).unwrap().read().unwrap();
                    
                    let rows = relation.range(registry, ident_trees, (range_start, range_end)).unwrap();

                    for row in rows {
                        let row = row.unwrap();
                        dbg!(row);
                    }
                },
                DataQuery::Insert {
                    relation,
                    row,
                } => {
                    let db = self.inner.read().unwrap();
                    let registry = &db.registry;

                    let relations = db.relations.read().unwrap();
                    let mut relation = relations.get(&relation).unwrap().write().unwrap();
                    
                    let success = relation.insert(registry, row).unwrap().unwrap();

                    dbg!(success);
                },
                DataQuery::Extend {
                    relation,
                    rows,
                } => {
                    let db = self.inner.read().unwrap();
                    let registry = &db.registry;

                    let relations = db.relations.read().unwrap();
                    let mut relation = relations.get(&relation).unwrap().write().unwrap();
                    
                    let count = relation.extend(registry, rows).unwrap().unwrap();

                    dbg!(count);
                },
                DataQuery::Remove {
                    relation,
                    pkey,
                } => {
                    let db = self.inner.read().unwrap();
                    let registry = &db.registry;

                    let relations = db.relations.read().unwrap();
                    let mut relation = relations.get(&relation).unwrap().write().unwrap();
                    
                    let value = relation.remove(registry, &pkey).unwrap().unwrap();

                    dbg!(value);
                },
                DataQuery::Retain {
                    relation,
                    condition,
                } => {
                    let db = self.inner.read().unwrap();
                    let registry = db.registry();

                    let relations = db.relations.read().unwrap();
                    let mut relation = relations.get(&relation).unwrap().write().unwrap();

                    let scope = StructType::new(btreemap! {
                        id!("row") => relation.schema().ttype_id(),
                    });

                    let scope_type_id = TTypeId::new_anonymous(scope.clone().into());

                    let scopes = ScopeTypes::one(Cow::Owned(scope));

                    let ttype_id = condition.eval_types(&registry, &scopes).unwrap();
                    ttype_id.must_eq(&TTypeId::BOOL).expect("unreachable");
                    
                    let rows_removed = relation.retain(|row| {
                        let scope = StructValue::new(&registry, scope_type_id.clone(), btreemap! {
                            id!("row") => row.clone(),
                        }).unwrap();

                        let scopes = ScopeValues::one(Cow::Owned(scope));

                        let result = condition.eval(&registry, &scopes).unwrap();
                        result == Value::TRUE
                    }).unwrap();

                    dbg!(rows_removed);
                },
            },
        }
    }
}

pub struct DbInner<R: Relation> {
    registry: Registry,
    relations: RwLock<HashMap<Ident, RwLock<R>>>,
}

impl<R: Relation> DbInner<R> {
    fn new() -> Self {
        Self {
            registry: Registry::new(),
            relations: RwLock::new(HashMap::new()),
        }
    }

    pub fn registry(&self) -> &Registry {
        &self.registry
    }

    pub fn registry_mut(&mut self) -> &mut Registry {
        &mut self.registry
    }

    pub fn relations(&self) -> &RwLock<HashMap<Ident, RwLock<R>>> {
        &self.relations
    }
}

#[cfg(test)]
mod tests {
    use core::panic;
    use std::ops::Bound;

    use codb_core::IdentTree;

    use crate::typesystem::value::ScalarValue;

    use super::{relation::{memory::MemoryRelation, RelationRef, Schema}, *};

    fn create_db() -> Db<MemoryRelation> {
        let db = Db::<MemoryRelation>::new();
        
        {
            let mut db_inner = db.inner.write().unwrap();
        
            let registry = db_inner.registry_mut();
            
            let my_type = StructType::new(btreemap! {
                id!("id") => TTypeId::INT32,
                id!("name") => TTypeId::STRING,
                id!("active") => TTypeId::BOOL,
            });

            let mut relations = db_inner.relations().write().unwrap();

            let registry = db_inner.registry();
            
            relations.insert(id!("Rel"), RwLock::new(MemoryRelation::new(
                Schema::new(registry, my_type.clone(), IdentTree::from_nested_idents([id!("id").into()])).unwrap()
            )));

            relations.get("Rel").unwrap().write().unwrap().extend(&registry, [
                StructValue::new(registry, TTypeId::new_anonymous(my_type.clone().into()), btreemap! {
                    id!("id") => Value::Scalar(ScalarValue::Int32(1).into()),
                    id!("name") => Value::Scalar(ScalarValue::String("Jim Jones".into()).into()),
                    id!("active") => Value::Scalar(ScalarValue::Bool(true).into()),
                }).unwrap().into(),
                StructValue::new(registry, TTypeId::new_anonymous(my_type.clone().into()), btreemap! {
                    id!("id") => Value::Scalar(ScalarValue::Int32(2).into()),
                    id!("name") => Value::Scalar(ScalarValue::String("Jimboni Jonesi".into()).into()),
                    id!("active") => Value::Scalar(ScalarValue::Bool(true).into()),
                }).unwrap().into(),
                StructValue::new(registry, TTypeId::new_anonymous(my_type.clone().into()), btreemap! {
                    id!("id") => Value::Scalar(ScalarValue::Int32(-1).into()),
                    id!("name") => Value::Scalar(ScalarValue::String("El Jones, Jim".into()).into()),
                    id!("active") => Value::Scalar(ScalarValue::Bool(false).into()),
                }).unwrap().into(),
            ]).unwrap().unwrap();
        }

        db
    }

    #[test]
    fn test() {
        let db = Db::<MemoryRelation>::new();
        let mut db = db.inner.write().unwrap();
        
        let registry = db.registry_mut();
        
        let my_type = StructType::new(btreemap! {
            id!("id") => TTypeId::INT32,
            id!("name") => TTypeId::STRING,
            id!("active") => TTypeId::BOOL,
        });

        let mut relations = db.relations().write().unwrap();

        let registry = db.registry();
        
        relations.insert(id!("Rel"), RwLock::new(MemoryRelation::new(
            Schema::new(registry, my_type, IdentTree::from_nested_idents([id!("id").into()])).unwrap()
        )));

        let rel = relations.get("Rel").unwrap().read().unwrap();
        rel.draw(registry);
    }

    #[test]
    fn execute() {
        let db = create_db();

        db.execute(Query::Data(DataQuery::Range {
            relation: id!("Rel"),
            ident_trees: [].into(),
            range_start: Bound::Unbounded,
            range_end: Bound::Unbounded,
        }));
        
        panic!();
    }
}
