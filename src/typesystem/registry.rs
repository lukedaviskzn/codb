use super::ttype::{CompositeType, EnumType, FieldType, RefinedTType, ScalarType, TType};

#[derive(Debug, thiserror::Error)]
pub enum TypeRegistryError {
    #[error("type with name {0:?} already exists")]
    NameTaken(String),
    #[error("type with id {0:?} could not be found")]
    TypeIdNotFound(u64),
    #[error("type {0:?} could not be found")]
    TypeNotFound(String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum TTypeId {
    Scalar(ScalarType),
    Id(u64),
    Anonymous(Box<TType>),
}

impl TTypeId {
    pub const UNIT: TTypeId = TTypeId::Scalar(ScalarType::Unit);
    pub const BOOL: TTypeId = TTypeId::Scalar(ScalarType::Bool);
    pub const INT32: TTypeId = TTypeId::Scalar(ScalarType::Int32);
    pub const STRING: TTypeId = TTypeId::Scalar(ScalarType::String);
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
    entries: Vec<TypeRegistryEntry>,
}

impl TypeRegistry {
    pub fn new() -> TypeRegistry {
        let mut registry = TypeRegistry {
            last_id: 0,
            entries: vec![],
        };

        registry.add(RefinedTType::RESULT_TYPE_NAME, TType::Composite(CompositeType::Enum(EnumType::new(vec![
            FieldType::new("Ok", TTypeId::UNIT),
            FieldType::new("Err", TTypeId::STRING),
        ]).expect("unreachable"))));

        registry
    }

    pub fn types(&self) -> &[TypeRegistryEntry] {
        &self.entries
    }

    pub fn get_by_id(&self, ttype_id: &TTypeId) -> Result<TType, TypeRegistryError> {
        match ttype_id {
            TTypeId::Scalar(ttype) => Ok(TType::Scalar(*ttype)),
            TTypeId::Id(id) => if let Some(entry) = self.entries.iter().find(|entry| entry.id == *id) {
                Ok(entry.ttype.clone())
            } else {
                Err(TypeRegistryError::TypeIdNotFound(*id))
            },
            TTypeId::Anonymous(ttype) => Ok(*ttype.clone()),
        }
    }

    pub fn get_id_by_name(&self, name: &str) -> Result<TTypeId, TypeRegistryError> {
        if let Some(ttype) = ScalarType::ALL.iter().find(|t| t.name() == name) {
            return Ok(TTypeId::Scalar(*ttype));
        }
        
        self.entries.iter().find(|e| e.name == name)
            .map(|e| TTypeId::Id(e.id))
            .ok_or_else(|| TypeRegistryError::TypeNotFound(name.into()))
    }

    pub fn get_by_name(&self, name: &str) -> Result<TType, TypeRegistryError> {
        if let Some(ttype) = ScalarType::ALL.iter().find(|t| t.name() == name) {
            return Ok(TType::Scalar(*ttype));
        }
        self.entries.iter().find(|e| e.name() == name)
            .map(|e| e.ttype.clone())
            .ok_or_else(|| TypeRegistryError::TypeNotFound(name.into()))
    }

    pub fn get_name(&self, ttype_id: &TTypeId) -> Result<String, TypeRegistryError> {
        match ttype_id {
            TTypeId::Scalar(scalar_type) => Ok(scalar_type.name().into()),
            TTypeId::Id(id) => if let Some(entry) = self.entries.iter().find(|e| e.id == *id) {
                Ok(entry.name.clone())
            } else {
                Err(TypeRegistryError::TypeIdNotFound(*id))
            },
            TTypeId::Anonymous(ttype) => Ok(format!("{ttype:?}")),
        }
    }

    pub fn add(&mut self, name: impl Into<String>, ttype: TType) -> Result<TTypeId, TypeRegistryError> {
        let mut names = self.entries.iter().map(|e| e.name())
            .chain(ScalarType::ALL.iter().map(|t| t.name()));

        let name = name.into();
        
        if names.any(|n| n == name) {
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
