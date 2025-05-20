use codb_core::{Ident, NestedIdent};

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
#[error("{0:?} is already taken")]
pub struct IdentTaken(pub Ident);

// #[derive(Debug, PartialEq, Eq, thiserror::Error)]
// #[error("{ident:?} not found on {context:?}")]
// pub struct FieldNotFoundOn {
//     ident: Ident,
//     context: NestedIdent,
// }
