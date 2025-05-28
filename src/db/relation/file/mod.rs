use std::{fs::File, io::{self, Write}, ops::RangeBounds, path::{Path, PathBuf}};

use codb_core::IdentForest;
use itertools::Itertools;
use ron::ser::PrettyConfig;

use crate::db::registry::Registry;

use super::{memory::MemoryRelation, Key, Relation, RelationRef, Row, RowSize, Schema};

#[derive(Debug)]
pub struct FileRelation {
    schema: Schema,
    filepath: PathBuf,
}

impl FileRelation {
    #[allow(unused)]
    pub fn new(schema: Schema, filepath: impl Into<PathBuf>) -> io::Result<FileRelation> {
        let filepath = Into::<PathBuf>::into(filepath);

        let relation = MemoryRelation::new(schema.clone());
        write_mem_relation(&filepath, &relation)?;
        
        Ok(FileRelation {
            schema,
            filepath,
        })
    }

    #[allow(unused)]
    pub fn open(filepath: impl Into<PathBuf>) -> io::Result<FileRelation> {
        let filepath: PathBuf = filepath.into();
        let relation = read_mem_relation(&filepath)?;
        
        return Ok(FileRelation {
            schema: relation.schema().clone(),
            filepath,
        });
    }
}

fn read_mem_relation(filepath: impl AsRef<Path>) -> io::Result<MemoryRelation> {
    let file = File::open(filepath)?;
    
    ron::de::from_reader::<_, MemoryRelation>(file).map_err(|_| io::ErrorKind::InvalidData.into())
}

fn write_mem_relation(filepath: impl AsRef<Path>, relation: &MemoryRelation) -> io::Result<()> {
    let string = ron::ser::to_string_pretty::<MemoryRelation>(relation, PrettyConfig::default())
        .map_err(|_| io::ErrorKind::InvalidData)?;
    
    let mut file = File::create(filepath)?;
    file.write(string.as_bytes()).map(|_| ())
}

impl RelationRef for FileRelation {
    fn schema(&self) -> &Schema {
        &self.schema
    }

    fn range(&self, registry: &Registry, ident_forest: &IdentForest, range: impl RangeBounds<Key>) -> impl Iterator<Item = Row> {
        let relation = read_mem_relation(&self.filepath).expect("failed to read from file");

        let rows = relation.range(registry, ident_forest, range).collect_vec();

        rows.into_iter()
    }
}

impl Relation for FileRelation {
    fn insert(&mut self, registry: &Registry, new_row: Row) -> bool {
        let mut relation = read_mem_relation(&self.filepath).expect("failed to read from file");

        let inserted = relation.insert(registry, new_row);

        write_mem_relation(&self.filepath, &relation).expect("failed to write to file");

        inserted
    }

    fn remove(&mut self, registry: &Registry, pkey: &super::Key) -> Option<Row> {
        let mut relation = read_mem_relation(&self.filepath).expect("failed to read from file");

        let old_row = relation.remove(registry, pkey);

        write_mem_relation(&self.filepath, &relation).expect("failed to write to file");

        old_row
    }

    fn retain(&mut self, predicate: impl Fn(&Row) -> bool) -> RowSize {
        let mut relation = read_mem_relation(&self.filepath).expect("failed to read from file");

        let count = relation.retain(predicate);

        write_mem_relation(&self.filepath, &relation).expect("failed to write to file");

        count
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{db::{registry::TTypeId, DbRelations}, typesystem::{ttype::StructType, value::{ScalarValue, StructValue, Value}}};

    use super::*;

    #[test]
    fn file_relation() {
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

        let registry = Registry::new(&DbRelations::<FileRelation>::new());

        // let result_ttype = registry.get_id_by_name(RefinedType::RESULT_TYPE_NAME).unwrap();

        // let expression = Expression::ControlFlow(Box::new(ControlFlow::If(IfControlFlow {
        //     condition: Expression::Op(Box::new(Op::Logical(LogicalOp::Lt(
        //         Expression::NestedIdent(id!("this")),
        //         Expression::Value(Value::Scalar(ScalarValueInner::Int32(500).into())),
        //     )))),
        //     then: Expression::Value(EnumValue::new(
        //         &registry, result_ttype.clone(),
        //         id!("Ok"),
        //         Value::UNIT,
        //     ).unwrap().into()),
        //     otherwise: Expression::Value(Value::Composite(CompositeValue::Enum(EnumValue::new(
        //         &registry, result_ttype.clone(),
        //         id!("Err"),
        //         Value::Scalar(ScalarValueInner::String("lt_500".into()).into()),
        //     ).unwrap()))),
        // })));

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

        const USERS_PATH: &str = "target/test_file_relation_users_relation.ron";
        const INACTIVE_USERS_PATH: &str = "target/test_file_relation_inactive_users_relation.ron";

        let mut users = FileRelation::new(user_schema.clone(), USERS_PATH).unwrap();
        let mut inactive_users = FileRelation::new(user_schema.clone(), INACTIVE_USERS_PATH).unwrap();

        users.insert(&registry, new_user(&user_ttype_id, 0, false));
        users.insert(&registry, new_user(&user_ttype_id, 1, true));
        users.insert(&registry, new_user(&user_ttype_id, 2, true));
        users.insert(&registry, new_user(&user_ttype_id, 3, false));
        users.insert(&registry, new_user(&user_ttype_id, 4, true));

        let empty_forest = IdentForest::empty();

        let new_rows = users.range(&registry, &empty_forest, ..)
            .filter_map(|row| {
                row.fields().get("active")
                    .filter(|value| **value == Value::FALSE)
                    .map(|_| row.clone())
            });
        
        inactive_users.extend(&registry, new_rows);

        let mut inactive_users_expected = MemoryRelation::new(user_schema.clone());

        let user_3 = new_user(&user_ttype_id, 3, false);

        inactive_users_expected.insert(&registry, new_user(&user_ttype_id, 0, false));
        inactive_users_expected.insert(&registry, user_3.clone());

        assert!(RelationRef::eq(&inactive_users_expected, &registry, &inactive_users));
        
        // cannot insert again
        assert_eq!(false, inactive_users.insert(&registry, user_3));
        // inactive_users.insert(&registry, Value::Composite(CompositeValue::new_struct(&registry, &user_ttype_id, []).unwrap())).unwrap_err();
        // inactive_users.insert(&registry, Value::Composite(CompositeValue::new_struct(&registry, &user_ttype_id, [FieldValue::new("id", Value::Scalar(ScalarValueInner::Int32(2)))]).unwrap())).unwrap_err();

        // check range
        let users_range = users.range(&registry, users.schema.pkey(), new_id(&user_id_ttype_id, 1)..new_id(&user_id_ttype_id, 3)).enumerate().collect_vec();

        assert_eq!(2, users_range.len());

        for (i, user) in users_range {
            assert_eq!(user, new_user(&user_ttype_id, i as i32 + 1, true));
        }

        users.insert(&registry, new_user(&user_ttype_id, 499, true));
        // users.insert(&registry, new_user(&registry, &user_ttype_id, 500, true)).unwrap_err();
        // users.insert(&registry, new_user(&registry, &user_ttype_id, 501, true)).unwrap_err();

        println!("{}", users.draw(&registry));
        // panic!();
    }
}
