use std::{fs::File, io::{self, Write}, ops::RangeBounds, path::{Path, PathBuf}};

use either::Either;
use itertools::Itertools;
use ron::ser::PrettyConfig;

use crate::{relation::memory::MemoryRelation, typesystem::{value::CompositeValue, NestedIdents, TypeError}};

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

    fn range(&self, nested_idents: impl Into<Box<[NestedIdents]>>, range: impl RangeBounds<CompositeValue>) -> Result<impl Iterator<Item = io::Result<Row>>, TypeError> {
        let relation = match read_mem_relation(&self.filepath) {
            Ok(relation) => relation,
            Err(err) => return Ok(Either::Left(std::iter::once(Err(err)))),
        };

        let rows = relation.range(nested_idents, range)?.collect_vec();

        Ok(Either::Right(rows.into_iter()))
    }
}

impl Relation for FileRelation {
    fn insert(&mut self, new_row: Row) -> Result<io::Result<bool>, TypeError> {
        let mut relation = match read_mem_relation(&self.filepath) {
            Ok(relation) => relation,
            Err(err) => return Ok(Err(err)),
        };

        let inserted = match relation.insert(new_row)? {
            Ok(inserted) => inserted,
            Err(err) => return Ok(Err(err)),
        };

        Ok(write_mem_relation(&self.filepath, &relation).map(|_| inserted))
    }

    fn remove(&mut self, pkey: &super::PKey) -> Result<io::Result<Option<Row>>, TypeError> {
        let mut relation = match read_mem_relation(&self.filepath) {
            Ok(relation) => relation,
            Err(err) => return Ok(Err(err)),
        };

        let old_row = match relation.remove(pkey)? {
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

    use crate::{expr::{ControlFlow, Expression, IfControlFlow, LogicalOp, Op}, typesystem::{ttype::{CompositeType, FieldType, RefinedTType, TType}, value::{EnumValue, FieldValue, ScalarValue, Value}}};

    use super::*;

    #[test]
    fn file_relation() {
        fn new_user(id: i32, active: bool) -> CompositeValue {
            CompositeValue::new_struct(vec![
                FieldValue::new("id", Value::Scalar(ScalarValue::Int32(id))),
                FieldValue::new("active", Value::Scalar(ScalarValue::Bool(active))),
            ]).unwrap()
        }

        fn new_id(id: i32) -> CompositeValue {
            CompositeValue::new_struct(vec![
                FieldValue::new("id", Value::Scalar(ScalarValue::Int32(id))),
            ]).unwrap()
        }

        let user_ttype = CompositeType::new_struct(vec![
            FieldType::new("id", TType::Refined(RefinedTType::new(TType::INT32, {
                Expression::ControlFlow(Box::new(ControlFlow::If(IfControlFlow {
                    ret_ttype: RefinedTType::result_ttype(),
                    condition: Expression::Op(Box::new(Op::Logical(LogicalOp::Lt(
                        Expression::Ident(["this".into()].into()),
                        Expression::Value(TType::INT32, Value::Scalar(ScalarValue::Int32(500))),
                    )))),
                    then: Expression::Value(RefinedTType::result_ttype(), Value::Composite(CompositeValue::Enum(EnumValue::new(FieldValue::new("Ok", Value::UNIT))))),
                    // then: Expression::Value(TType::BOOL, Value::TRUE),
                    otherwise: Expression::Value(RefinedTType::result_ttype(), Value::Composite(CompositeValue::Enum(EnumValue::new(FieldValue::new("Err", Value::Scalar(ScalarValue::String("lt_500".into()))))))),
                })))
            }).unwrap())),
            FieldType::new("active", TType::BOOL),
        ]).unwrap();
        let user_pkey = NestedIdents::from_strings(vec!["id"]);
        let user_schema = Schema::new(user_ttype, user_pkey).unwrap();

        const USERS_PATH: &str = "target/test_file_relation_users_relation.ron";
        const INACTIVE_USERS_PATH: &str = "target/test_file_relation_inactive_users_relation.ron";

        let mut users = FileRelation::new(user_schema.clone(), USERS_PATH).unwrap();
        let mut inactive_users = FileRelation::new(user_schema.clone(), INACTIVE_USERS_PATH).unwrap();

        users.insert(new_user(0, false)).unwrap().unwrap();
        users.insert(new_user(1, true)).unwrap().unwrap();
        users.insert(new_user(2, true)).unwrap().unwrap();
        users.insert(new_user(3, false)).unwrap().unwrap();
        users.insert(new_user(4, true)).unwrap().unwrap();

        let new_rows = users.range([], ..).unwrap()
            .map(|r| r.unwrap())
            .filter_map(|row| {
                let CompositeValue::Struct(value) = row.clone() else { unreachable!() };
                value.fields().iter().find(|f| f.name() == "active" && *f.value() == Value::Scalar(ScalarValue::Bool(false)))
                    .map(|_| row)
            });
        
        inactive_users.extend(new_rows).unwrap().unwrap();

        let mut inactive_users_expected = MemoryRelation::new(user_schema.clone());

        let user_3 = new_user(3, false);

        inactive_users_expected.insert(new_user(0, false)).unwrap().unwrap();
        inactive_users_expected.insert(user_3.clone()).unwrap().unwrap();

        assert!(RelationRef::eq(&inactive_users_expected, &inactive_users));
        
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

        users.insert(new_user(499, true)).unwrap().unwrap();
        users.insert(new_user(500, true)).unwrap_err();
        users.insert(new_user(501, true)).unwrap_err();

        println!("{}", users.draw());
        panic!();
    }
}
