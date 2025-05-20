use codb_core::Ident;

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
#[error("{0:?} is already taken")]
pub struct IdentTaken(pub Ident);
