mod ident;
mod nested_ident;
mod ident_path;
mod ident_tree;
mod ident_forest;

pub use ident::*;
pub use nested_ident::*;
pub use ident_path::*;
pub use ident_tree::*;
pub use ident_forest::*;

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ParseIdentError {
    #[error("identifier may not be empty")]
    Empty,
    #[error("identifier may not start with {0:?}")]
    InvalidFirstChar(char),
    #[error("identifier may not contain {0:?}")]
    InvalidChar(char),
}

#[macro_use]
extern crate binrw;
