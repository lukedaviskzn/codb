use super::ttype::{ScalarType, TType};

#[derive(Debug, thiserror::Error)]
pub enum TypeRegistryError {
    #[error("type with name {0:?} already exists")]
    NameTaken(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeId {
    Scalar(ScalarType),
    Id(u64),
}

impl From<u64> for TypeId {
    fn from(id: u64) -> Self {
        TypeId::Id(id)
    }
}

impl From<ScalarType> for TypeId {
    fn from(ttype: ScalarType) -> Self {
        TypeId::Scalar(ttype)
    }
}

pub struct TypeRegistry {
    last_id: u64,
    entries: Vec<TypeRegistryEntry>,
}

impl TypeRegistry {
    pub fn new() -> TypeRegistry {
        TypeRegistry {
            last_id: 0,
            entries: vec![],
        }
    }

    pub fn types(&self) -> &[TypeRegistryEntry] {
        &self.entries
    }

    pub fn get_ttype(&self, id: TypeId) -> Option<TType> {
        match id {
            TypeId::Scalar(ttype) => Some(TType::Scalar(ttype)),
            TypeId::Id(id) => Some(self.entries.iter().find(|entry| entry.id == id)?.ttype.clone()),
        }
    }

    pub fn add(&mut self, name: String, ttype: TType) -> Result<TypeId, TypeRegistryError> {
        if self.entries.iter().any(|entry| entry.name() == name) {
            return Err(TypeRegistryError::NameTaken(name));
        }

        self.last_id += 1;

        self.entries.push(TypeRegistryEntry {
            id: self.last_id,
            name,
            ttype,
        });

        Ok(self.last_id.into())
    }
}

pub struct TypeRegistryEntry {
    id: u64,
    name: String,
    ttype: TType,
}

impl TypeRegistryEntry {
    pub fn id(&self) -> u64 {
        self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn ttype(&self) -> &TType {
        &self.ttype
    }
}
