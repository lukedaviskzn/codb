use std::{fs::File, io::{self, Write}, iter::Once, path::{Path, PathBuf}, sync::atomic::AtomicU64};

use binrw::BinRead;
use itertools::Itertools;
use page::Pager;

use crate::error;

use super::{Relation, RelationRef, Row, RowSize, Schema};

mod page;
mod btree;

#[derive(Debug)]
pub struct FileRelation {
    schema: Schema,
    filepath: PathBuf,
}

impl FileRelation {
    pub fn new(schema: Schema, filepath: impl Into<PathBuf>) -> Result<FileRelation, ()> {
        let filepath = Into::<PathBuf>::into(filepath);
        
        match Path::try_exists(&filepath) {
            Ok(true) => Ok(FileRelation {
                schema,
                filepath,
            }),
            _ => Err(()),
        }
    }
}

enum FileRelationRowIter<T: Iterator<Item = io::Result<Row>>> {
    Error(Once<io::Result<Row>>),
    Rows(T),
}

impl<T: Iterator<Item = io::Result<Row>>> Iterator for FileRelationRowIter<T> {
    type Item = io::Result<Row>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            FileRelationRowIter::Error(once) => once.next(),
            FileRelationRowIter::Rows(rows) => rows.next(),
        }
    }
}

impl RelationRef for FileRelation {
    fn schema(&self) -> &Schema {
        &self.schema
    }

    fn iter(&self) -> impl Iterator<Item = io::Result<Row>> {
        let mut file = match File::open(&self.filepath) {
            Ok(file) => file,
            Err(err) => return FileRelationRowIter::Error(std::iter::once(Err(err))),
        };
        let header = match page::Header::read(&mut file).map_err(error::map_binrw_error) {
            Ok(header) => header,
            Err(err) => return FileRelationRowIter::Error(std::iter::once(Err(err))),
        };

        let btree_iter = match btree::BTreePageIter::new(Pager::new(file), header.root_page) {
            Ok(iter) => iter,
            Err(err) => return FileRelationRowIter::Error(std::iter::once(Err(err))),
        };
        
        FileRelationRowIter::Rows(btree_iter)
    }

    fn contains(&self, row: &Row) -> io::Result<bool> {
        if !self.schema.type_check(row) {
            return Err(io::ErrorKind::InvalidInput.into());
        }
        for r in self.iter() {
            if r? == *row {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

impl Relation for FileRelation {
    fn insert(&mut self, new_row: Row) -> io::Result<bool> {
        if !self.schema.type_check(&new_row) {
            return Err(io::ErrorKind::InvalidInput.into());
        }
        
        let mut file = File::options()
            .append(true)
            .open(&self.filepath)?;

        if self.contains(&new_row)? {
            return Ok(false);
        }
        let line = new_row.columns.iter().map(|col| ron::to_string(col).unwrap()).join("\t")
            + "\n";
        
        file.write(line.as_bytes())?;
        Ok(true)
    }

    fn remove(&mut self, row: &Row) -> io::Result<bool> {
        if !self.schema.type_check(row) {
            return Err(io::ErrorKind::InvalidInput.into());
        }
        
        if !self.contains(row)? {
            return Ok(false);
        }

        let mut file = File::create(&self.filepath)?;

        // for new_row in self.iter().filter(|r| r != row) {
        //     let line = new_row.0.iter().map(|col| ron::to_string(col).unwrap()).join("\t")
        //         + "\n";
        //     file.write(line.as_bytes()).map_err(|_| ())?;
        // }

        Ok(true)
    }

    fn retain(&mut self, predicate: impl Fn(&Row) -> bool) -> io::Result<RowSize> {
        let mut file = File::create(&self.filepath)?;

        let count = AtomicU64::new(0);

        // for new_row in self.iter()?.filter(|row| {
        //     let keep = predicate(row);
        //     if !keep {
        //         count.fetch_add(1, Ordering::AcqRel);
        //     }
        //     keep
        // }) {
        //     let line = new_row.0.iter().map(|col| ron::to_string(col).unwrap()).join("\t")
        //         + "\n";
        //     file.write(line.as_bytes()).map_err(|_| ())?;
        // }

        Ok(count.into_inner())
    }
}

#[cfg(test)]
mod tests {
    use std::{io::Seek, num::NonZeroU64};

    use binrw::{io, BinWrite};

    use crate::{relation::{file::page::{Header, BTREE_LEAF_MAX_KEYS}, Column}, ttype::{ScalarType, ScalarValue}};

    use super::{page::{BTreeInteriorPage, BTreeKey, BTreeLeafPage, Page, PAGE_SIZE}, *};

    #[test]
    fn file_relation() {
        let users_path = "target/test_file_relation_users_relation";
        // let inactive_users_path = "target/test_file_relation_inactive_users_relation";

        {
            let mut file = File::create(users_path).unwrap();
            // File::create(inactive_users_path).unwrap();

            Header {
                version: page::FileVersion::V1,
                root_page: NonZeroU64::MIN,
            }.write(&mut file).unwrap();
            
            fn row_to_bytes(row: &Row) -> Vec<u8> {
                let mut row_bytes = io::Cursor::new(vec![]);
                row.write(&mut row_bytes).unwrap();
                return row_bytes.into_inner();
            }

            let keys1 = (0..BTREE_LEAF_MAX_KEYS).map(|i| Row::new(vec![
                ScalarValue::Int32(i as i32),
                ScalarValue::Bool(false),
            ])).map(|r| row_to_bytes(&r)).map(|r| {
                BTreeKey {
                    len: r.len() as u32,
                    overflow_page: None,
                    local_payload: r,
                }
            }).collect::<Vec<_>>();
            
            let keys2 = (BTREE_LEAF_MAX_KEYS..2*BTREE_LEAF_MAX_KEYS).map(|i| Row::new(vec![
                ScalarValue::Int32(i as i32),
                ScalarValue::Bool(false),
            ])).map(|r| row_to_bytes(&r)).map(|r| {
                BTreeKey {
                    len: r.len() as u32,
                    overflow_page: None,
                    local_payload: r,
                }
            }).collect::<Vec<_>>();

            let pages = vec![
                Page::BTreeInterior(BTreeInteriorPage {
                    keys: vec![keys2[0].clone()],
                    ptrs: vec![
                        NonZeroU64::new(2).unwrap(),
                        NonZeroU64::new(3).unwrap(),
                    ],
                }),
                Page::BTreeLeaf(BTreeLeafPage {
                    keys: keys1,
                }),
                Page::BTreeLeaf(BTreeLeafPage {
                    keys: keys2,
                }),
            ];
            
            for (i, page) in pages.into_iter().enumerate() {
                file.seek(io::SeekFrom::Start((i as u64 + 1) * PAGE_SIZE as u64)).unwrap();
                page.write(&mut file).unwrap();
            }
        }

        let mut users = FileRelation::new(Schema {
            columns: vec![
                Column::new("id", ScalarType::Int32),
                Column::new("active", ScalarType::Bool),
            ],
        }, users_path).unwrap();
        // let mut inactive_users = FileRelation::new(Schema {
        //     columns: vec![
        //         Column::new("id", ScalarType::Int32),
        //         Column::new("active", ScalarType::Bool),
        //     ],
        // }, inactive_users_path).unwrap();

        // users.insert(Row::new(vec![
        //     ScalarValue::Int32(0),
        //     ScalarValue::Bool(false),
        // ])).unwrap();
        // users.insert(Row::new(vec![
        //     ScalarValue::Int32(1),
        //     ScalarValue::Bool(true),
        // ])).unwrap();
        // users.insert(Row::new(vec![
        //     ScalarValue::Int32(2),
        //     ScalarValue::Bool(true),
        // ])).unwrap();
        // users.insert(Row::new(vec![
        //     ScalarValue::Int32(3),
        //     ScalarValue::Bool(false),
        // ])).unwrap();
        // users.insert(Row::new(vec![
        //     ScalarValue::Int32(4),
        //     ScalarValue::Bool(true),
        // ])).unwrap();

        let new_rows = users.iter()
            .map(|r| r.unwrap())
            .filter_map(|r| (r.columns[1] == ScalarValue::Bool(false)).then(|| r.clone()));

        for new_row in users.iter() {
            let row = new_row.unwrap();
            println!("{row:?}");
        }
        panic!();

        // inactive_users.extend(new_rows).unwrap();

        // let mut inactive_users_expected = MemoryRelation::new(Schema {
        //     columns: vec![
        //         Column::new("id", ScalarType::Int32),
        //         Column::new("active", ScalarType::Bool),
        //     ],
        // });

        // let user_3 = Row::new(vec![
        //     ScalarValue::Int32(3),
        //     ScalarValue::Bool(false),
        // ]);

        // inactive_users_expected.insert(Row::new(vec![
        //     ScalarValue::Int32(0),
        //     ScalarValue::Bool(false),
        // ])).unwrap();
        // inactive_users_expected.insert(user_3.clone()).unwrap();

        // assert!(RelationRef::eq(&inactive_users_expected, &inactive_users).unwrap());
        
        // // cannot insert again
        // assert_eq!(false, inactive_users.insert(user_3).unwrap());
        // inactive_users.insert(Row::new(vec![])).unwrap_err();
        // inactive_users.insert(Row::new(vec![ScalarValue::Int32(2)])).unwrap_err();
    }
}
