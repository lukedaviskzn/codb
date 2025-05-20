use std::{collections::BTreeMap, fmt::Debug};

use crate::{idents::Ident, typesystem::{ttype::{CompositeType, EnumType, ScalarType, TType}, TypeError}};

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum TypeRegistryError {
    #[error("type with name {0:?} already exists")]
    NameTaken(Ident),
    #[error("type with id {0:?} could not be found")]
    TypeIdNotFound(u64),
    #[error("type {0:?} could not be found")]
    TypeNotFound(String),
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum TTypeId {
    Scalar(ScalarType),
    Id(u64),
    Anonymous(Box<CompositeType>),
}

impl TTypeId {
    pub fn new_anonymous(ttype: TType) -> TTypeId {
        match ttype {
            TType::Composite(composite_type) => TTypeId::Anonymous(Box::new(composite_type)),
            TType::Scalar(scalar_type) => TTypeId::Scalar(scalar_type),
        }
    }
}

impl TTypeId {
    pub const NEVER: TTypeId = TTypeId::Scalar(ScalarType::Never);
    pub const UNIT: TTypeId = TTypeId::Scalar(ScalarType::Unit);
    pub const BOOL: TTypeId = TTypeId::Scalar(ScalarType::Bool);
    pub const INT32: TTypeId = TTypeId::Scalar(ScalarType::Int32);
    pub const STRING: TTypeId = TTypeId::Scalar(ScalarType::String);
}

impl Debug for TTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar(ttype) => Debug::fmt(ttype, f),
            Self::Id(id) => write!(f, "type#{id}"),
            Self::Anonymous(ttype) => {
                write!(f, "anon[")?;
                Debug::fmt(ttype, f)?;
                write!(f, "]")
            },
        }
    }
}

impl From<u64> for TTypeId {
    fn from(id: u64) -> Self {
        TTypeId::Id(id)
    }
}

impl From<ScalarType> for TTypeId {
    fn from(ttype: ScalarType) -> Self {
        TTypeId::Scalar(ttype)
    }
}

pub struct TypeRegistry {
    last_id: u64,
    types: BTreeMap<u64, TypeRegistryEntry>,
}

impl TypeRegistry {
    pub fn new() -> TypeRegistry {
        let mut registry = TypeRegistry {
            last_id: 0,
            types: btreemap! {},
        };

        registry.add("Result".parse().expect("Result is not an ident"), TType::Composite(CompositeType::Enum(EnumType::new(btreemap! {
            "Ok".parse().expect("Ok is not an ident") => TTypeId::UNIT,
            "Err".parse().expect("Err is not an ident") => TTypeId::STRING,
        })))).expect("unreachable");

        registry
    }

    pub fn types(&self) -> impl Iterator<Item = (u64, &TypeRegistryEntry)> {
        self.types.iter().map(|(id, entry)| (*id, entry))
    }

    pub fn get_by_id(&self, ttype_id: &TTypeId) -> Result<TType, TypeRegistryError> {
        match ttype_id {
            TTypeId::Scalar(ttype) => Ok(TType::Scalar(*ttype)),
            TTypeId::Id(id) =>  if let Some(entry) = self.types.get(id) {
                Ok(entry.ttype.clone())
            } else {
                Err(TypeRegistryError::TypeIdNotFound(*id))
            },
            TTypeId::Anonymous(ttype) => Ok((*ttype.clone()).into()),
        }
    }

    pub fn get_id_by_name(&self, name: &str) -> Result<TTypeId, TypeRegistryError> {
        if let Some(ttype) = ScalarType::ALL.iter().find(|t| t.name() == name) {
            return Ok(TTypeId::Scalar(*ttype));
        }
        
        self.types.iter().find(|(_, entry)| entry.name == *name)
            .map(|(id, _)| TTypeId::Id(*id))
            .ok_or_else(|| TypeRegistryError::TypeNotFound(name.into()))
    }

    pub fn get_by_name(&self, name: &str) -> Result<TType, TypeRegistryError> {
        if let Some(ttype) = ScalarType::ALL.iter().find(|t| t.name() == name) {
            return Ok(TType::Scalar(*ttype));
        }
        self.types.iter().find(|(_, entry)| entry.name == *name)
            .map(|(_, entry)| entry.ttype.clone())
            .ok_or_else(|| TypeRegistryError::TypeNotFound(name.into()))
    }

    pub fn get_name(&self, ttype_id: &TTypeId) -> Result<String, TypeRegistryError> {
        match ttype_id {
            TTypeId::Scalar(scalar_type) => Ok(scalar_type.name().into()),
            TTypeId::Id(id) => if let Some(entry) = self.types.get(id) {
                Ok(entry.name.clone().into())
            } else {
                Err(TypeRegistryError::TypeIdNotFound(*id))
            },
            TTypeId::Anonymous(ttype) => Ok(format!("{ttype:?}")),
        }
    }

    pub fn add(&mut self, name: Ident, ttype: TType) -> Result<TTypeId, TypeRegistryError> {
        let mut names = self.types.iter().map(|(_, entry)| &*entry.name)
            .chain(ScalarType::ALL.iter().map(|t| t.name()));

        if names.any(|n| name == *n) {
            return Err(TypeRegistryError::NameTaken(name));
        }

        self.last_id += 1;

        let None = self.types.insert(self.last_id, TypeRegistryEntry {
            name,
            ttype,
        }) else {
            unreachable!("last_id is strictly increasing");
        };

        Ok(self.last_id.into())
    }

    pub fn types_compatible(from: &TTypeId, to: &TTypeId) -> bool {
        from == to || *from == TTypeId::NEVER
    }

    pub fn expect_type(&self, expected: &TTypeId, got: &TTypeId) -> Result<(), TypeError> {
        if Self::types_compatible(got, expected) {
            Ok(())
        } else {
            Err(TypeError::TypeInvalid {
                expected: self.get_by_id(expected)?,
                got: self.get_by_id(got)?,
            })
        }
    }
}

pub struct TypeRegistryEntry {
    name: Ident,
    ttype: TType,
}
