use std::fmt::{Debug, Display};

use codb_core::{Ident, IdentForest};
use indexmap::IndexMap;

use crate::db::{registry::{Registry, TTypeId}, relation::RowSize};

use super::{TypeError, TypeSet};

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum TType {
    Composite(CompositeType),
    Scalar(ScalarType),
    Array(ArrayType),
}

impl TType {
    #[allow(unused)]
    pub const NEVER: TType = TType::Scalar(ScalarType::Never);
    #[allow(unused)]
    pub const UNIT: TType = TType::Scalar(ScalarType::Unit);
    #[allow(unused)]
    pub const BOOL: TType = TType::Scalar(ScalarType::Bool);
    #[allow(unused)]
    pub const INT32: TType = TType::Scalar(ScalarType::Int32);
    #[allow(unused)]
    pub const INT64: TType = TType::Scalar(ScalarType::Int64);
    #[allow(unused)]
    pub const STRING: TType = TType::Scalar(ScalarType::String);

    pub fn select(&self, registry: &Registry, trees: &IdentForest) -> Result<TType, TypeError> {
        match self {
            TType::Composite(ttype) => Ok(TType::Composite(ttype.select(registry, trees)?)),
            TType::Scalar(ttype) => Ok(TType::Scalar(ttype.select(trees)?)),
            TType::Array(ttype) => Ok(TType::Array(ttype.select(registry, trees)?)),
        }
    }

    pub fn dot(&self, ident: &Ident) -> Option<&TTypeId> {
        match self {
            TType::Composite(ttype) => ttype.dot(ident),
            TType::Scalar(_) => None,
            TType::Array(_) => None,
        }
    }
}

impl Debug for TType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Composite(ttype) => Debug::fmt(ttype, f),
            Self::Scalar(ttype) => Debug::fmt(ttype, f),
            Self::Array(ttype) => Debug::fmt(ttype, f),
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

impl From<ArrayType> for TType {
    fn from(value: ArrayType) -> Self {
        Self::Array(value)
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum CompositeType {
    #[brw(magic = 0u8)]
    Struct(StructType),
    #[brw(magic = 1u8)]
    Enum(EnumType),
}

impl CompositeType {
    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<CompositeType, TypeError> {
        match self {
            CompositeType::Struct(ttype) => Ok(CompositeType::Struct(ttype.select(registry, ident_forest)?)),
            CompositeType::Enum(ttype) => Ok(CompositeType::Enum(ttype.select(ident_forest)?)),
        }
    }

    pub fn dot(&self, ident: &Ident) -> Option<&TTypeId> {
        match self {
            CompositeType::Struct(ttype) => ttype.dot(ident),
            CompositeType::Enum(_) => None,
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

impl TryFrom<TType> for CompositeType {
    type Error = TypeError;

    fn try_from(value: TType) -> Result<Self, Self::Error> {
        match value {
            TType::Composite(ttype) => Ok(ttype),
            ttype => Err(TypeError::TypeSetInvalid {
                expected: TypeSet::Composite,
                got: ttype,
            })
        }
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct StructType {
    #[bw(calc = self.fields.len() as u64)]
    len: u64,
    #[br(count = len, map = |fields: Vec<(Ident, TTypeId)>| IndexMap::from_iter(fields.into_iter()))]
    #[bw(map = |fields| Vec::<(Ident, TTypeId)>::from_iter(fields.clone().into_iter()))]
    fields: IndexMap<Ident, TTypeId>,
}

impl StructType {
    pub fn new(fields: IndexMap<Ident, TTypeId>) -> StructType {
        StructType {
            fields,
        }
    }

    pub fn fields(&self) -> &IndexMap<Ident, TTypeId> {
        &self.fields
    }

    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<StructType, TypeError> {
        if ident_forest.is_empty() {
            return Ok(self.clone());
        }

        let mut new_fields = IndexMap::new();
        
        for tree in ident_forest {
            let ident = tree.ident().clone();

            if let Some(ttype_id) = self.fields.get(&ident) {
                let field_type = registry.ttype(ttype_id)
                    .ok_or_else(|| TypeError::TypeNotFound(ttype_id.clone()))?;

                new_fields.insert(ident, TTypeId::new_anonymous(
                    field_type.select(registry, tree.children())?
                ));
            } else {
                return Err(TypeError::UnknownField(ident));
            }
        }
        
        Ok(StructType {
            fields: new_fields,
        })
    }

    pub fn dot(&self, ident: &Ident) -> Option<&TTypeId> {
        if let Some(ttype_id) = self.fields.get(ident)  {
            Some(ttype_id)
        } else {
            None
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

impl TryFrom<TType> for StructType {
    type Error = TypeError;

    fn try_from(value: TType) -> Result<Self, Self::Error> {
        match value {
            TType::Composite(CompositeType::Struct(ttype)) => Ok(ttype),
            ttype => Err(TypeError::TypeSetInvalid {
                expected: TypeSet::Struct,
                got: ttype,
            })
        }
    }
}

impl TryFrom<CompositeType> for StructType {
    type Error = TypeError;

    fn try_from(value: CompositeType) -> Result<Self, Self::Error> {
        match value {
            CompositeType::Struct(ttype) => Ok(ttype),
            ttype => Err(TypeError::TypeSetInvalid {
                expected: TypeSet::Struct,
                got: ttype.into(),
            })
        }
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct EnumType {
    #[bw(calc = self.tags.len() as u64)]
    len: u64,
    #[br(count = len, map = |tags: Vec<(Ident, TTypeId)>| IndexMap::from_iter(tags.into_iter()))]
    #[bw(map = |tags| Vec::<(Ident, TTypeId)>::from_iter(tags.clone().into_iter()))]
    tags: IndexMap<Ident, TTypeId>,
}

impl EnumType {
    pub fn new(tags: IndexMap<Ident, TTypeId>) -> EnumType {
        EnumType {
            tags,
        }
    }

    pub fn new_option(ttype_id: TTypeId) -> EnumType {
        EnumType {
            tags: indexmap! {
                id!("Some") => ttype_id,
                id!("None") => TTypeId::UNIT,
            }
        }
    }

    pub fn new_result(ok_ttype_id: TTypeId, err_ttype_id: TTypeId) -> EnumType {
        EnumType {
            tags: indexmap! {
                id!("Ok") => ok_ttype_id,
                id!("Err") => err_ttype_id,
            }
        }
    }

    pub fn tags(&self) -> &IndexMap<Ident, TTypeId> {
        &self.tags
    }

    pub fn select(&self, ident_forest: &IdentForest) -> Result<EnumType, TypeError> {
        if ident_forest.is_empty() {
            return Ok(self.clone());
        }

        // all tags must be listed
        for (name, _) in &self.tags {
            if !self.tags.contains_key(name) {
                return Err(TypeError::MissingTag(name.clone()));
            }
        }

        // all idents must be valid
        for tree in ident_forest {
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

impl TryFrom<TType> for EnumType {
    type Error = TypeError;

    fn try_from(value: TType) -> Result<Self, Self::Error> {
        match value {
            TType::Composite(CompositeType::Enum(ttype)) => Ok(ttype),
            ttype => Err(TypeError::TypeSetInvalid {
                expected: TypeSet::Enum,
                got: ttype,
            })
        }
    }
}

impl TryFrom<CompositeType> for EnumType {
    type Error = TypeError;

    fn try_from(value: CompositeType) -> Result<Self, Self::Error> {
        match value {
            CompositeType::Enum(ttype) => Ok(ttype),
            ttype => Err(TypeError::TypeSetInvalid {
                expected: TypeSet::Enum,
                got: ttype.into(),
            })
        }
    }
}

#[binrw]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum ScalarType {
    #[brw(magic = 0u8)]
    Never,
    #[brw(magic = 1u8)]
    Unit,
    #[brw(magic = 2u8)]
    Bool,
    #[brw(magic = 3u8)]
    Int32,
    #[brw(magic = 4u8)]
    Int64,
    #[brw(magic = 5u8)]
    String,
}

impl ScalarType {
    pub const ALL: [ScalarType; 6] = [ScalarType::Never, ScalarType::Unit, ScalarType::Bool, ScalarType::Int32, ScalarType::Int64, ScalarType::String];

    pub fn from_name(name: &str) -> Option<ScalarType> {
        Self::ALL.iter().find(|st| st.name() == name).map(|st| *st)
    }

    pub fn name(&self) -> &str {
        match self {
            ScalarType::Never => "never",
            ScalarType::Unit => "unit",
            ScalarType::Bool => "bool",
            ScalarType::Int32 => "int32",
            ScalarType::Int64 => "int64",
            ScalarType::String => "string",
        }
    }

    pub fn select(&self, ident_forest: &IdentForest) -> Result<ScalarType, TypeError> {
        if let Some(first) = ident_forest.first() {
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

impl Display for ScalarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl TryFrom<TType> for ScalarType {
    type Error = TypeError;

    fn try_from(value: TType) -> Result<Self, Self::Error> {
        match value {
            TType::Scalar(ttype) => Ok(ttype),
            ttype => Err(TypeError::TypeSetInvalid {
                expected: TypeSet::Scalar,
                got: ttype,
            })
        }
    }
}

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct ArrayType {
    inner_ttype_id: TTypeId,
    #[bw(calc = length.is_some() as u8)]
    #[br(assert(is_finite <= 1))]
    is_finite: u8,
    #[br(map = |length: RowSize| if is_finite > 0 { Some(length) } else { None })]
    #[bw(map = |length| length.unwrap_or_default())]
    length: Option<RowSize>,
}

impl ArrayType {
    pub fn new(inner_ttype_id: TTypeId, length: Option<RowSize>) -> ArrayType {
        ArrayType {
            inner_ttype_id,
            length,
        }
    }
    
    pub fn inner_ttype_id(&self) -> &TTypeId {
        &self.inner_ttype_id
    }

    pub fn length(&self) -> &Option<RowSize> {
        &self.length
    }

    pub fn select(&self, registry: &Registry, ident_forest: &IdentForest) -> Result<ArrayType, TypeError> {
        let ttype = registry.ttype(&self.inner_ttype_id)
            .ok_or_else(|| TypeError::TypeNotFound(self.inner_ttype_id.clone()))?;

        Ok(ArrayType {
            inner_ttype_id: TTypeId::new_anonymous(ttype.select(registry, ident_forest)?),
            length: self.length.clone(),
        })
    }
}

impl Debug for ArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(len) = self.length() {
            write!(f, "[{len}]")?;
        } else {
            write!(f, "[]")?;
        }
        Debug::fmt(self.inner_ttype_id(), f)
    }
}

impl TryFrom<TType> for ArrayType {
    type Error = TypeError;

    fn try_from(value: TType) -> Result<Self, Self::Error> {
        match value {
            TType::Array(ttype) => Ok(ttype),
            ttype => Err(TypeError::TypeSetInvalid {
                expected: TypeSet::Array,
                got: ttype,
            })
        }
    }
}
