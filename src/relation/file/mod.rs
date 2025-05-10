use std::{fs::File, io::{self, Write}, ops::RangeBounds, path::{Path, PathBuf}};

use either::Either;
use itertools::Itertools;
use ron::ser::PrettyConfig;

use crate::{idents::IdentTree, relation::memory::MemoryRelation, typesystem::{registry::TypeRegistry, value::Value, TypeError}};

use super::{Relation, RelationRef, Row, Schema};

#[derive(Debug)]
pub struct FileRelation {
    schema: Schema,
    filepath: PathBuf,
}

impl FileRelation {
    pub fn new(schema: Schema, filepath: impl Into<PathBuf>) -> io::Result<FileRelation> {
        let filepath = Into::<PathBuf>::into(filepath);

        let relation = MemoryRelation::new(schema.clone());
        write_mem_relation(&filepath, &relation)?;
        
        Ok(FileRelation {
            schema,
            filepath,
        })
    }

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

    fn range(&self, registry: &TypeRegistry, ident_trees: impl Into<Box<[IdentTree]>>, range: impl RangeBounds<Value>) -> Result<impl Iterator<Item = io::Result<Row>>, TypeError> {
        let relation = match read_mem_relation(&self.filepath) {
            Ok(relation) => relation,
            Err(err) => return Ok(Either::Left(std::iter::once(Err(err)))),
        };

        let rows = relation.range(registry, ident_trees, range)?.collect_vec();

        Ok(Either::Right(rows.into_iter()))
    }
}

impl Relation for FileRelation {
    fn insert(&mut self, registry: &TypeRegistry, new_row: Row) -> Result<io::Result<bool>, TypeError> {
        let mut relation = match read_mem_relation(&self.filepath) {
            Ok(relation) => relation,
            Err(err) => return Ok(Err(err)),
        };

        let inserted = match relation.insert(registry, new_row)? {
            Ok(inserted) => inserted,
            Err(err) => return Ok(Err(err)),
        };

        Ok(write_mem_relation(&self.filepath, &relation).map(|_| inserted))
    }

    fn remove(&mut self, registry: &TypeRegistry, pkey: &super::PKey) -> Result<io::Result<Option<Row>>, TypeError> {
        let mut relation = match read_mem_relation(&self.filepath) {
            Ok(relation) => relation,
            Err(err) => return Ok(Err(err)),
        };

        let old_row = match relation.remove(registry, pkey)? {
            Ok(old_row) => old_row,
            Err(err) => return Ok(Err(err)),
        };

        Ok(write_mem_relation(&self.filepath, &relation).map(|_| old_row))
    }

    fn retain(&mut self, predicate: impl Fn(&Row) -> bool) -> io::Result<super::RowSize> {
        let mut relation = match read_mem_relation(&self.filepath) {
            Ok(relation) => relation,
            Err(err) => return Err(err),
        };

        let count = relation.retain(predicate)?;

        write_mem_relation(&self.filepath, &relation).map(|_| count)
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{expr::{ControlFlow, Expression, IfControlFlow, LogicalOp, Op}, typesystem::{registry::TTypeId, ttype::{CompositeType, FieldType, RefinedType, TType}, value::{CompositeValue, EnumValue, FieldValue, ScalarValue, Value}}};

    use super::*;

    #[test]
    fn file_relation() {
        fn new_user(registry: &TypeRegistry, user_ttype_id: &TTypeId, id: i32, active: bool) -> Value {
            Value::Composite(CompositeValue::new_struct(registry, user_ttype_id, [
                FieldValue::new("id".parse().unwrap(), Value::Scalar(ScalarValue::Int32(id))),
                FieldValue::new("active".parse().unwrap(), Value::Scalar(ScalarValue::Bool(active))),
            ]).unwrap())
        }

        fn new_id(registry: &TypeRegistry, user_id_ttype_id: &TTypeId, id: i32) -> Value {
            Value::Composite(CompositeValue::new_struct(registry, user_id_ttype_id, [
                FieldValue::new("id".parse().unwrap(), Value::Scalar(ScalarValue::Int32(id))),
            ]).unwrap())
        }

        let mut registry = TypeRegistry::new();

        let result_ttype = registry.get_id_by_name(RefinedType::RESULT_TYPE_NAME).unwrap();

        let refinement = Expression::ControlFlow(Box::new(ControlFlow::If(IfControlFlow {
            condition: Expression::Op(Box::new(Op::Logical(LogicalOp::Lt(
                Expression::NestedIdent("this".parse().unwrap()),
                Expression::Value(Value::Scalar(ScalarValue::Int32(500))),
            )))),
            then: Expression::Value(Value::Composite(CompositeValue::Enum(EnumValue::new(&registry, &result_ttype,
                FieldValue::new("Ok".parse().unwrap(), Value::UNIT)
            ).unwrap()))),
            otherwise: Expression::Value(Value::Composite(CompositeValue::Enum(EnumValue::new(&registry, &result_ttype,
                FieldValue::new("Err".parse().unwrap(), Value::Scalar(ScalarValue::String("lt_500".into())))
            ).unwrap()))),
        })));

        let refined_int32 = TTypeId::Anonymous(Box::new(TType::Refined(RefinedType::new(&registry, TTypeId::INT32, refinement).unwrap())));

        let user_ttype = TType::Composite(CompositeType::new_struct([
            FieldType::new("id".parse().unwrap(), refined_int32),
            FieldType::new("active".parse().unwrap(), TTypeId::BOOL),
        ]).unwrap());

        let user_id_ttype_id = TTypeId::Anonymous(Box::new(user_ttype.select(&registry, &IdentTree::from_nested_idents(["id".parse().unwrap()])).unwrap()));

        let user_ttype_id = registry.add("User", user_ttype).unwrap();
        
        let user_pkey = IdentTree::from_nested_idents(["id".parse().unwrap()]);
        let user_schema = Schema::new(&registry, user_ttype_id.clone(), user_pkey).unwrap();

        const USERS_PATH: &str = "target/test_file_relation_users_relation.ron";
        const INACTIVE_USERS_PATH: &str = "target/test_file_relation_inactive_users_relation.ron";

        let mut users = FileRelation::new(user_schema.clone(), USERS_PATH).unwrap();
        let mut inactive_users = FileRelation::new(user_schema.clone(), INACTIVE_USERS_PATH).unwrap();

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

        assert!(RelationRef::eq(&inactive_users_expected, &registry, &inactive_users));
        
        // cannot insert again
        assert_eq!(false, inactive_users.insert(&registry, user_3).unwrap().unwrap());
        // inactive_users.insert(&registry, Value::Composite(CompositeValue::new_struct(&registry, &user_ttype_id, []).unwrap())).unwrap_err();
        // inactive_users.insert(&registry, Value::Composite(CompositeValue::new_struct(&registry, &user_ttype_id, [FieldValue::new("id", Value::Scalar(ScalarValue::Int32(2)))]).unwrap())).unwrap_err();

        // check range
        let users_range = users.range(&registry, users.schema.pkey(), new_id(&registry, &user_id_ttype_id, 1)..new_id(&registry, &user_id_ttype_id, 3)).unwrap().enumerate().collect_vec();

        assert_eq!(2, users_range.len());

        for (i, user) in users_range {
            assert_eq!(user.unwrap(), new_user(&registry, &user_ttype_id, i as i32 + 1, true));
        }

        users.insert(&registry, new_user(&registry, &user_ttype_id, 499, true)).unwrap().unwrap();
        // users.insert(&registry, new_user(&registry, &user_ttype_id, 500, true)).unwrap_err();
        // users.insert(&registry, new_user(&registry, &user_ttype_id, 501, true)).unwrap_err();

        println!("{}", users.draw(&registry));
        panic!();
    }
}
