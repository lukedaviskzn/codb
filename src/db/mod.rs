use std::{collections::BTreeMap, io, sync::{Arc, Mutex}};

use codb_core::Ident;
use pager::{DbHeader, FreeListPage, HeaderReadGuard, LinkedPageReadGuard, LinkedPageWriteGuard, Page, PagePtr, Pager};
use registry::Registry;
use relation::memory::PagerRelation;

use crate::{db::{registry::module::Module, relation::Schema}, error::NormaliseToIo, query::{schema_query::{ModuleSchemaQuery, RelationSchemaQuery, SchemaError, SchemaQuery, TypeSchemaQuery}, DataQuery, Query, QueryExecutionError}, typesystem::{scope::{ScopeTypes, ScopeValues}, value::{ScalarValue, Value}}};

pub mod registry;
pub mod relation;
pub mod pager;

#[allow(unused)]
pub struct Db {
    pager: Arc<Mutex<Pager>>,
}

impl Db {
    pub fn create_new(pager: Pager) -> io::Result<Db> {
        let pager = Arc::new(Mutex::new(pager));

        let relation_set_page = {
            let mut lock = pager.lock().into_io()?;

            // write dummy header to allocate space
            lock.write_header(&DbHeader::new(PagePtr::MIN, PagePtr::MIN))?;

            let relation_set = DbRelationSet::new();
            lock.new_linked_pages(&relation_set)?
        };
        {
            let manifest = DbManifest::new(pager.clone(), relation_set_page)?;
            
            let mut lock = pager.lock().into_io()?;
            
            let manifest_page = lock.new_linked_pages(&manifest)?;
            
            let freelist_page = lock.write_new_page(&Page::FreeList(FreeListPage {
                next: None,
            }))?;
            
            let header = DbHeader::new(manifest_page, freelist_page);
            lock.write_header(&header)?;
        }
        
        Ok(Db {
            pager,
        })
    }
    
    pub fn open(pager: Pager) -> Db {
        Db {
            pager: Arc::new(Mutex::new(pager)),
        }
    }

    pub fn header(&self) -> HeaderReadGuard {
        HeaderReadGuard::new(self.pager.clone()).expect("failed to read header")
    }

    pub fn manifest(&self) -> LinkedPageReadGuard<DbManifest> {
        let manifest_page = self.header().manifest;
        LinkedPageReadGuard::new(self.pager.clone(), manifest_page).expect("failed to read manifest")
    }

    pub fn manifest_mut(&self) -> LinkedPageWriteGuard<DbManifest> {
        let manifest_page = self.header().manifest;
        LinkedPageWriteGuard::new(self.pager.clone(), manifest_page).expect("failed to write manifest")
    }

    pub fn execute(&self, query: Query) -> Result<Value, QueryExecutionError> {
        match query {
            Query::Data(DataQuery(expression)) => {
                let manifest = self.manifest();
                let registry = &manifest.registry;

                let relations = manifest.relations(self.pager.clone());

                expression.eval_types(self.pager.clone(), registry, &relations, &ScopeTypes::EMPTY)?;

                let result = expression.eval(self.pager.clone(), registry, &relations, &ScopeValues::EMPTY)?;

                Ok(result)
            },
            Query::Schema(schema_query) => {
                match schema_query {
                    SchemaQuery::Module(query) => {
                        let mut manifest = self.manifest_mut();
                        match query {
                            ModuleSchemaQuery::Create { name } => {
                                let (mod_name, path) = name.split_last();
                                let Some(module) = manifest.registry_mut().module_mut(path) else {
                                    return Err(SchemaError::InvalidPath(name.clone()).into());
                                };

                                let added = module.insert(mod_name.clone(), Module::new());
                                Ok(Value::Scalar(ScalarValue::Bool(added)))
                            },
                        }
                    },
                    SchemaQuery::Type(query) => {
                        let mut manifest = self.manifest_mut();
                        match query {
                            TypeSchemaQuery::Create {
                                name,
                                ttype,
                            } => {
                                let (type_name, module_path) = name.split_last();
                                let Some(module) = manifest.registry_mut().module_mut(module_path) else {
                                    return Err(SchemaError::InvalidPath(name).into());
                                };

                                let added = module.insert(type_name.clone(), ttype);

                                Ok(Value::Scalar(ScalarValue::Bool(added)))
                            },
                        }
                    },
                    SchemaQuery::Relation(query) => {
                        let manifest = self.manifest();
                        let mut relations = manifest.relations_mut(self.pager.clone());
                        match query {
                            RelationSchemaQuery::Create {
                                name,
                                ttype,
                                pkey,
                            } => {
                                let schema = Schema::new(&manifest.registry, ttype, pkey)?;
                                let relation = PagerRelation::new(schema);
                                
                                let added = relations.insert(name, relation, self.pager.clone());

                                Ok(Value::Scalar(ScalarValue::Bool(added)))
                            },
                        }
                    },
                }
            },
        }
    }

    pub fn pager(&self) -> &Arc<Mutex<Pager>> {
        &self.pager
    }
}

#[binrw]
pub struct DbManifest {
    registry: Registry,
    relation_set_page: PagePtr,
}

impl DbManifest {
    pub(super) fn new(pager: Arc<Mutex<Pager>>, relation_set_page: PagePtr) -> io::Result<DbManifest> {
        let registry = {
            let relation_set = LinkedPageReadGuard::new(pager.clone(), relation_set_page)?;
            Registry::new(pager, &relation_set)
        };
        
        Ok(Self {
            registry,
            relation_set_page,
        })
    }

    pub fn registry(&self) -> &Registry {
        &self.registry
    }

    pub fn registry_mut(&mut self) -> &mut Registry {
        &mut self.registry
    }

    pub fn relations(&self, pager: Arc<Mutex<Pager>>) -> LinkedPageReadGuard<DbRelationSet> {
        LinkedPageReadGuard::new(pager, self.relation_set_page).expect("failed to read relation set")
    }

    pub fn relations_mut(&self, pager: Arc<Mutex<Pager>>) -> LinkedPageWriteGuard<DbRelationSet> {
        LinkedPageWriteGuard::new(pager, self.relation_set_page).expect("failed to read relation set")
    }
}

#[binrw]
pub struct DbRelationSet {
    #[bw(calc = self.relations.len() as u64)]
    len: u64,
    #[br(count = len, map = |relations: Vec<(Ident, PagePtr)>| BTreeMap::from_iter(relations.into_iter()))]
    #[bw(map = |relations| Vec::<(Ident, PagePtr)>::from_iter(relations.clone().into_iter()))]
    relations: BTreeMap<Ident, PagePtr>,
}

impl DbRelationSet {
    pub fn new() -> Self {
        Self {
            relations: BTreeMap::new(),
        }
    }

    pub fn get(&self, relation: &str, pager: Arc<Mutex<Pager>>) -> Option<LinkedPageReadGuard<PagerRelation>> {
        let page_ptr = self.relations.get(relation)?;
        Some(LinkedPageReadGuard::new(pager, *page_ptr).expect("failed to read page"))
    }

    pub fn get_mut(&self, relation: &str, pager: Arc<Mutex<Pager>>) -> Option<LinkedPageWriteGuard<PagerRelation>> {
        let page_ptr = self.relations.get(relation)?;
        Some(LinkedPageWriteGuard::new(pager, *page_ptr).expect("failed to read page"))
    }

    pub fn insert(&mut self, name: Ident, relation: PagerRelation, pager: Arc<Mutex<Pager>>) -> bool {
        if self.relations.contains_key(&name) {
            return false;
        }

        let page_ptr = pager.lock().expect("failed to lock pager")
            .new_linked_pages(&relation).expect("failed to write new pages");

        self.relations.insert(name, page_ptr);
        true
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Bound;

    use codb_core::{IdentForest, IdentTree};
    use itertools::Itertools;

    use crate::{db::{registry::TTypeId, relation::{Relation, Schema}, Db}, expression::{CompositeLiteral, Expression, InterpreterAction, Literal, StructLiteral}, query::{lex::{lex, TokenSlice}, parser::{ExpressionArgs, Parse}, schema_query::{RelationSchemaQuery, SchemaQuery, TypeSchemaQuery}, DataQuery, Query}, typesystem::{ttype::{CompositeType, StructType}, value::{ArrayValue, CompositeValue, EnumValue, ScalarValue, StructValue, Value}}};

    use super::{pager::Pager, relation::memory::PagerRelation};

    fn db_initial_values() -> [StructValue; 3] {
        let mut rows = [
            // SAFETY: test case
            unsafe { StructValue::new_unchecked(indexmap! {
                id!("id") => Value::Scalar(ScalarValue::Int32(1).into()),
                id!("name") => Value::Scalar(ScalarValue::String("Jim Jones".into()).into()),
                id!("active") => Value::Scalar(ScalarValue::Bool(true).into()),
            }) }.into(),
            // SAFETY: test case
            unsafe { StructValue::new_unchecked(indexmap! {
                id!("id") => Value::Scalar(ScalarValue::Int32(2).into()),
                id!("name") => Value::Scalar(ScalarValue::String("Jimboni Jonesi".into()).into()),
                id!("active") => Value::Scalar(ScalarValue::Bool(true).into()),
            }) }.into(),
            // SAFETY: test case
            unsafe { StructValue::new_unchecked(indexmap! {
                id!("id") => Value::Scalar(ScalarValue::Int32(-1).into()),
                id!("name") => Value::Scalar(ScalarValue::String("El Jones, Jim".into()).into()),
                id!("active") => Value::Scalar(ScalarValue::Bool(false).into()),
            }) }.into(),
        ];
        rows.sort();
        rows
    }

    fn create_db() -> Db {
        let db = Db::create_new(Pager::new_memory()).expect("failed to create db");
        
        {
            let db_manifest = db.manifest();
            let registry = db_manifest.registry();
            let mut relation_set = db_manifest.relations_mut(db.pager().clone());

            let my_type = StructType::new(indexmap! {
                id!("id") => TTypeId::INT32,
                id!("name") => TTypeId::STRING,
                id!("active") => TTypeId::BOOL,
            });

            let mut relation = PagerRelation::new(
                Schema::new(registry, my_type.clone(), IdentForest::from_nested_idents([id!("id").into()])).unwrap()
            );
            
            relation.extend(&registry, db_initial_values());

            relation_set.insert(id!("Rel"), relation, db.pager.clone());
        }

        db
    }

    #[test]
    fn test() {
        let db = Db::create_new(Pager::new_memory()).expect("failed to create db");
        let manifest = db.manifest();
        
        let my_type = StructType::new(indexmap! {
            id!("id") => TTypeId::INT32,
            id!("name") => TTypeId::STRING,
            id!("active") => TTypeId::BOOL,
        });

        let mut relations = manifest.relations_mut(db.pager().clone());

        let registry = manifest.registry();

        let relation = PagerRelation::new(
            Schema::new(registry, my_type, IdentForest::from_nested_idents([id!("id").into()])).unwrap()
        );

        relations.insert(id!("Rel"), relation, db.pager.clone());

        let rel = relations.get("Rel", db.pager.clone()).expect("relation not found");
        rel.draw(registry);
    }

    #[test]
    fn execute_range_remove() {
        let db = create_db();

        let value = db.execute(Query::Data(DataQuery(Expression::Action(InterpreterAction::Range {
            relation: id!("Rel"),
            ident_forest: IdentForest::empty(),
            start: Box::new(Bound::Unbounded),
            end: Box::new(Bound::Unbounded),
        })))).unwrap();

        let array: ArrayValue = value.try_into().unwrap();
        let mut entries: Vec<StructValue> = array.entries().iter().map(|entry| entry.clone().try_into().unwrap()).collect_vec();

        let expected_deleted_row: Value;
        let pkey_value: Literal;
        {
            let db_manifest = db.manifest();
            let registry = db_manifest.registry();
            let relations = db_manifest.relations(db.pager.clone());
            let relation = relations.get("Rel", db.pager.clone()).unwrap();

            assert_eq!(entries, db_initial_values());

            expected_deleted_row = CompositeValue {
                ttype_id: relation.schema().ttype_id(),
                inner: entries.remove(0).into(),
            }.into();

            let pkey_type = TTypeId::new_anonymous(relation.schema().pkey_ttype(registry).unwrap().into());
            
            pkey_value = CompositeLiteral {
                ttype_id: pkey_type,
                inner: StructLiteral::new(
                    btreemap! {
                        id!("id") => Expression::Literal(ScalarValue::Int32(-1).into()),
                    },
                ).into()
            }.into();
        }

        let out = db.execute(Query::Data(DataQuery(Expression::Action(InterpreterAction::Remove {
            relation: id!("Rel"),
            pkey: Box::new(Expression::Literal(pkey_value.clone())),
        })))).unwrap();

        let out: EnumValue = out.try_into().unwrap();
        let out = out.into_inner_value();

        assert_eq!(expected_deleted_row, out);

        let out = db.execute(Query::Data(DataQuery(Expression::Action(InterpreterAction::Remove {
            relation: id!("Rel"),
            pkey: Box::new(Expression::Literal(pkey_value.clone())),
        })))).unwrap();

        let out: EnumValue = out.try_into().unwrap();

        let expected_none = EnumValue::new_option_none();

        assert_eq!(expected_none, out);
    }

    #[test]
    fn data_query() {
        let db = create_db();

        let tokens = lex("
            #Rel.range<id>(
                /* 0-1 instead of just -1 since negation operator isn't finished */
                struct { id: int32 } { id: - 0i32 10i32 },
                struct { id: int32 } { id: 10i32 }
            )
        ").expect("failed to lex");

        let query = {
            let manifest = db.manifest();

            Query::Data(
                DataQuery::parse(&mut TokenSlice::from(&*tokens), ExpressionArgs {
                    pager: db.pager.clone(),
                    registry: &manifest.registry,
                    relations: &manifest.relations(db.pager.clone())
                }).expect("failed to parse").0
            )
        };

        let value = db.execute(query).unwrap();

        let array: ArrayValue = value.try_into().unwrap();
        let mut entries: Vec<StructValue> = array.entries().iter().map(|entry| entry.clone().try_into().unwrap()).collect_vec();

        let expected_deleted_row: Value;
        {
            let db_manifest = db.manifest();
            let relations = db_manifest.relations(db.pager.clone());
            let relation = relations.get("Rel", db.pager.clone()).unwrap();

            assert_eq!(entries, db_initial_values());

            expected_deleted_row = CompositeValue {
                ttype_id: relation.schema().ttype_id(),
                inner: entries.remove(0).into(),
            }.into();
        }

        let tokens = lex("
            #Rel.remove(
                // 0-1 instead of just -1 since negation operator isn't finished
                struct { id: int32 } { id: - 0i32 1i32 }
            )
        ").expect("failed to lex");

        let query = {
            let manifest = db.manifest();

            Query::Data(
                DataQuery::parse(&mut TokenSlice::from(&*tokens), ExpressionArgs {
                    pager: db.pager.clone(),
                    registry: &manifest.registry,
                    relations: &manifest.relations(db.pager.clone())
                }).expect("failed to parse").0
            )
        };

        let out = db.execute(query.clone()).unwrap();

        let out: EnumValue = out.try_into().unwrap();
        let out = out.into_inner_value();

        assert_eq!(expected_deleted_row, out);

        let out = db.execute(query).unwrap();

        let out: EnumValue = out.try_into().unwrap();

        let expected_none = EnumValue::new_option_none();

        assert_eq!(expected_none, out);
    }

    #[test]
    fn schema_query_insert_type() {
        let db = create_db();

        let query = {
            Query::Schema(
                SchemaQuery::Type(TypeSchemaQuery::Create {
                    name: id_path!("MyType"),
                    ttype: CompositeType::Struct(StructType::new(indexmap! {
                        id!("id") => TTypeId::INT32,
                        id!("name") => TTypeId::STRING,
                    })),
                }),
            )
        };

        let out = db.execute(query.clone()).unwrap();

        assert_eq!(Value::TRUE, out);

        let out = db.execute(query.clone()).unwrap();

        assert_eq!(Value::FALSE, out);
    }

    #[test]
    fn schema_query_insert_relation() {
        let db = create_db();

        let query = {
            Query::Schema(
                SchemaQuery::Relation(RelationSchemaQuery::Create {
                    name: id!("MyRelation"),
                    ttype: StructType::new(indexmap! {
                        id!("id") => TTypeId::INT32,
                        id!("name") => TTypeId::STRING,
                    }),
                    pkey: IdentForest::new([IdentTree::new(id!("id"), IdentForest::empty())]),
                }),
            )
        };

        let out = db.execute(query.clone()).unwrap();

        assert_eq!(Value::TRUE, out);

        let out = db.execute(query.clone()).unwrap();

        assert_eq!(Value::FALSE, out);
    }
}
