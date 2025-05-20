use std::{collections::BTreeMap, fmt::Debug};

use codb_core::{Ident, IdentTree};

use crate::{registry::{TTypeId, TypeRegistry}};

use super::TypeError;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum TType {
    Composite(CompositeType),
    Scalar(ScalarType),
}

impl TType {
    pub const UNIT: TType = TType::Scalar(ScalarType::Unit);
    pub const BOOL: TType = TType::Scalar(ScalarType::Bool);
    pub const INT32: TType = TType::Scalar(ScalarType::Int32);
    pub const STRING: TType = TType::Scalar(ScalarType::String);

    pub fn select(&self, type_registry: &TypeRegistry, trees: &[IdentTree]) -> Result<TType, TypeError> {
        match self {
            TType::Composite(ttype) => Ok(TType::Composite(ttype.select(type_registry, trees)?)),
            TType::Scalar(ttype) => Ok(TType::Scalar(ttype.select(trees)?)),
        }
    }

    pub fn dot(&self, type_registry: &TypeRegistry, ident: &Ident) -> Result<&TTypeId, TypeError> {
        match self {
            TType::Composite(ttype) => ttype.dot(type_registry, ident),
            TType::Scalar(_) => Err(TypeError::ScalarField(ident.clone())),
        }
    }
}

impl Debug for TType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Composite(ttype) => Debug::fmt(ttype, f),
            Self::Scalar(ttype) => Debug::fmt(ttype, f),
        }
    }
}

impl From<CompositeType> for TType {
    fn from(value: CompositeType) -> Self {
        Self::Composite(value)
    }
}

impl From<StructType> for TType {
    fn from(value: StructType) -> Self {
        Self::Composite(CompositeType::Struct(value))
    }
}

impl From<EnumType> for TType {
    fn from(value: EnumType) -> Self {
        Self::Composite(CompositeType::Enum(value))
    }
}

impl From<ScalarType> for TType {
    fn from(value: ScalarType) -> Self {
        Self::Scalar(value)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum CompositeType {
    Struct(StructType),
    Enum(EnumType),
}

impl CompositeType {
    pub fn select(&self, type_registry: &TypeRegistry, ident_trees: &[IdentTree]) -> Result<CompositeType, TypeError> {
        match self {
            CompositeType::Struct(ttype) => Ok(CompositeType::Struct(ttype.select(type_registry, ident_trees)?)),
            CompositeType::Enum(ttype) => Ok(CompositeType::Enum(ttype.select(ident_trees)?)),
        }
    }

    pub fn dot(&self, type_registry: &TypeRegistry, ident: &Ident) -> Result<&TTypeId, TypeError> {
        match self {
            CompositeType::Struct(ttype) => ttype.dot(type_registry, ident),
            CompositeType::Enum(_) => Err(TypeError::DotTag(ident.clone())),
        }
    }
}

impl Debug for CompositeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Struct(ttype) => Debug::fmt(ttype, f),
            Self::Enum(ttype) => Debug::fmt(ttype, f),
        }
    }
}

impl From<StructType> for CompositeType {
    fn from(value: StructType) -> Self {
        Self::Struct(value)
    }
}

impl From<EnumType> for CompositeType {
    fn from(value: EnumType) -> Self {
        Self::Enum(value)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct StructType {
    fields: BTreeMap<Ident, TTypeId>,
}

impl StructType {
    pub fn new(fields: BTreeMap<Ident, TTypeId>) -> StructType {
        StructType {
            fields: fields.into(),
        }
    }

    pub fn fields(&self) -> &BTreeMap<Ident, TTypeId> {
        &self.fields
    }

    pub fn select(&self, type_registry: &TypeRegistry, ident_trees: &[IdentTree]) -> Result<StructType, TypeError> {
        if ident_trees.is_empty() {
            return Ok(self.clone());
        }

        let mut new_fields = BTreeMap::new();
        
        for tree in ident_trees {
            let ident = tree.ident().clone();

            if let Some(ttype_id) = self.fields.get(&ident) {
                let field_type = type_registry.get_by_id(ttype_id)?;

                new_fields.insert(ident, TTypeId::new_anonymous(
                    field_type.select(type_registry, tree.children())?
                ));
            } else {
                return Err(TypeError::UnknownField(ident));
            }
        }
        
        Ok(StructType {
            fields: new_fields,
        })
    }

    pub fn dot(&self, type_registry: &TypeRegistry, ident: &Ident) -> Result<&TTypeId, TypeError> {
        if let Some(ttype_id) = self.fields.get(ident)  {
            Ok(ttype_id)
        } else {
            Err(TypeError::MissingField(ident.clone()))
        }
    }
}

impl Debug for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct("struct");
        for (name, ttype_id) in &self.fields {
            d.field(name, ttype_id);
        }
        d.finish()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct EnumType {
    tags: BTreeMap<Ident, TTypeId>,
}

impl EnumType {
    pub fn new(tags: BTreeMap<Ident, TTypeId>) -> EnumType {
        EnumType {
            tags,
        }
    }

    pub fn tags(&self) -> &BTreeMap<Ident, TTypeId> {
        &self.tags
    }

    pub fn select(&self, ident_trees: &[IdentTree]) -> Result<EnumType, TypeError> {
        if ident_trees.is_empty() {
            return Ok(self.clone());
        }

        // all tags must be listed
        for (name, _) in &self.tags {
            if !self.tags.contains_key(name) {
                return Err(TypeError::MissingTag(name.clone()));
            }
        }

        // all idents must be valid
        for tree in ident_trees {
            let ident = tree.ident().clone();
            
            if !self.tags.contains_key(&ident) {
                return Err(TypeError::UnknownTag(ident));
            }
        }

        Ok(self.clone())
    }
}

impl Debug for EnumType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct("enum");
        for (name, ttype_id) in &self.tags {
            d.field(name, ttype_id);
        }
        d.finish()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum ScalarType {
    Never,
    Unit,
    Bool,
    Int32,
    String,
}

impl ScalarType {
    pub const ALL: [ScalarType; 5] = [ScalarType::Never, ScalarType::Unit, ScalarType::Bool, ScalarType::Int32, ScalarType::String];

    pub fn from_name(name: &str) -> Option<ScalarType> {
        Self::ALL.iter().find(|st| st.name() == name).map(|st| *st)
    }

    pub fn name(&self) -> &str {
        match self {
            ScalarType::Never => "!",
            ScalarType::Unit => "()",
            ScalarType::Bool => "bool",
            ScalarType::Int32 => "int32",
            ScalarType::String => "string",
        }
    }

    pub fn select(&self, ident_trees: &[IdentTree]) -> Result<ScalarType, TypeError> {
        if let Some(first) = ident_trees.first() {
            Err(TypeError::ScalarField(first.ident().clone()))
        } else {
            Ok(self.clone())
        }
    }
}

impl Debug for ScalarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}
