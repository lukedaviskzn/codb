use codb_core::{Ident, IdentForest, IdentPath};

use crate::{db::relation::Schema, typesystem::{ttype::{CompositeType, StructType}, TypeError}};

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum SchemaError {
    #[error("invalid path `{0}`")]
    InvalidPath(IdentPath),
    #[error("cannot make primary key: {0}")]
    PrimaryKeyInvalid(TypeError),
}

#[derive(Debug, Clone)]
pub enum SchemaQuery {
    Type(TypeSchemaQuery),
    Relation(RelationSchemaQuery),
}

#[derive(Debug, Clone)]
pub enum TypeSchemaQuery {
    Create {
        name: IdentPath,
        ttype: CompositeType,
    },
    // Drop {
    //     name: IdentPath,
    // },
}

#[derive(Debug, Clone)]
pub enum RelationSchemaQuery {
    Create {
        name: Ident,
        ttype: StructType,
        pkey: IdentForest,
    },
    // Drop {
    //     name: Ident,
    // },
}
