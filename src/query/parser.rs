// use std::iter::Peekable;

// use crate::{db::{registry::Registry, relation::Relation, DbRelations}, expression::Expression, typesystem::TypeError};

// use super::lexer::{LexError, Lexer};

// #[derive(Debug, thiserror::Error)]
// pub enum ParseError {
//     #[error("{0}")]
//     LexError(#[from] LexError),
// }

// pub struct Parser<T: Iterator<Item = char>> {
//     lexer: Peekable<Lexer<T>>,
// }

// impl<T: Iterator<Item = char>> Parser<T> {
//     pub fn new(lexer: Lexer<T>) -> Self {
//         Self {
//             lexer: lexer.peekable(),
//         }
//     }

//     pub fn parse<R: Relation>(&self, registry: &Registry, relations: &DbRelations<R>) -> Result<Expression, TypeError> {
//         todo!()
//     }
// }
