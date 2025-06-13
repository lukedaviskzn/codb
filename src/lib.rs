use std::{fmt::Debug, fs::File, io, num::NonZeroUsize};

use crate::{db::{pager::Pager, Db}, query::{lexer::LexerInner, parser::{ParseError, Parser}, DataQuery, Query, QueryExecutionError}, typesystem::value::Value};

pub mod query;

mod typesystem;
mod expression;
mod db;
mod error;

#[macro_use]
extern crate maplit;

#[macro_use]
extern crate indexmap;

#[macro_use]
extern crate binrw;

#[macro_use]
extern crate codb_macro;

#[macro_use]
extern crate static_assertions;

#[derive(Debug, thiserror::Error)]
pub enum ConnectionExecutionError {
    #[error("Parse Error: {0}")]
    ParseError(#[from] ParseError),
    #[error("{0}")]
    QueryExecutionError(#[from] QueryExecutionError),
}

pub struct Connection {
    db: Db,
}

impl Connection {
    pub fn create_new(file: File) -> io::Result<Connection> {
        Ok(Connection {
            db: Db::create_new(Pager::open(file))?,
        })
    }

    pub fn open(file: File) -> io::Result<Connection> {
        Ok(Connection {
            db: Db::open(Pager::open(file)),
        })
    }

    pub fn create_new_memory() -> Connection {
        Connection {
            db: Db::create_new(Pager::new_memory()).expect("failed to create new in-memory db"),
        }
    }

    pub fn execute(&self, query: &str) -> Result<Value, ConnectionExecutionError> {
        let expr = {
            let lexer = LexerInner::new(query.chars());
            let mut parser = Parser::new(lexer);

            let manifest = self.db.manifest();
            let registry = manifest.registry();
            let relations = manifest.relations(self.db.pager().clone());

            let expr = parser.parse_data_query(self.db.pager().clone(), registry, &*relations)?;
            expr
        };

        Ok(self.db.execute(Query::Data(DataQuery(expr)))?)
    }

    pub fn execute_schema(&self, query: &str) -> Result<Value, ConnectionExecutionError> {
        let lexer = LexerInner::new(query.chars());
        let mut parser = Parser::new(lexer);

        let query = parser.parse_schema_query()?;

        Ok(self.db.execute(Query::Schema(query))?)
    }
}

struct DebugByteSlice<'a> {
    bytes: &'a [u8],
}

impl<'a> Debug for DebugByteSlice<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct ByteLength {
            byte: u8,
            len: NonZeroUsize,
        }

        impl Debug for ByteLength {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if self.len > NonZeroUsize::MIN {
                    write!(f, "{:02x} * {}", self.byte, self.len)
                } else {
                    write!(f, "{:02x}", self.byte)
                }
            }
        }

        let mut byte_lens: Vec<ByteLength> = Vec::new();

        for byte in self.bytes {
            if let Some(last_byte_len) = byte_lens.last_mut() {
                if last_byte_len.byte == *byte {
                    last_byte_len.len = last_byte_len.len.saturating_add(1);
                    continue;
                }
            }
            byte_lens.push(ByteLength {
                byte: *byte,
                len: NonZeroUsize::MIN,
            });
        }

        Debug::fmt(&byte_lens, f)
    }
}
