use std::io;

use crate::ttype::{ScalarType, ScalarValue};

mod memory;
mod file;

pub type RowSize = u64;

pub enum RowIterError {
    IoError
}

#[binrw::binrw]
#[brw(big)]
#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub struct Row {
    #[bw(try_calc = columns.len().try_into())]
    #[br(temp)]
    len: u16,
    #[br(count = len)]
    columns: Vec<ScalarValue>,
}

impl Row {
    pub fn new(columns: impl Into<Vec<ScalarValue>>) -> Row {
        Row {
            columns: columns.into(),
        }
    }
}

pub trait RelationRef {
    fn schema(&self) -> &Schema;
    fn iter(&self) -> impl Iterator<Item = io::Result<Row>>;
    fn contains(&self, row: &Row) -> io::Result<bool>;
    
    #[cfg(test)]
    fn eq(&self, other: &impl RelationRef) -> io::Result<bool> {
        if self.schema() != other.schema() {
            return Ok(false);
        }
        for row in self.iter() {
            if !other.contains(&row.unwrap())? {
                return Ok(false);
            }
        }
        Ok(true)
    }
    
    #[cfg(test)]
    fn draw(&self) -> io::Result<String> {
        let mut out_string = String::new();
        
        for col in &self.schema().columns {
            out_string += &format!("{:?}\t", col.name);
        }
        out_string += "\n";

        for row in self.iter() {
            for col in row.unwrap().columns {
                out_string += &format!("{col}\t");
            }
            out_string += "\n";
        }

        // remove final \n
        out_string.pop();

        Ok(out_string)
    }
}

pub trait Relation: RelationRef {
    fn insert(&mut self, new_row: Row) -> io::Result<bool>;
    
    fn extend(&mut self, new_rows: impl IntoIterator<Item = Row>) -> io::Result<RowSize> {
        let mut count = 0;
        for new_row in new_rows {
            self.insert(new_row)?;
            count += 1;
        }
        Ok(count)
    }
    
    fn remove(&mut self, row: &Row) -> io::Result<bool>;
    fn retain(&mut self, predicate: impl Fn(&Row) -> bool) -> io::Result<RowSize>;
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Schema {
    columns: Vec<Column>
}

impl Schema {
    pub fn type_check(&self, row: &Row) -> bool {
        if self.columns.len() != row.columns.len() {
            return false;
        }
        self.columns.iter().zip(&row.columns)
            .all(|(column, value)| column.ttype.type_check(value))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Column {
    name: String,
    ttype: ScalarType,
}

impl Column {
    pub fn new(name: impl Into<String>, ttype: ScalarType) -> Column {
        Column {
            name: name.into(),
            ttype,
        }
    }
}

pub struct RowUpdate {
    index: RowSize,
    new_row: Row,
}
