use std::{collections::BTreeMap, fmt::Debug};

use crate::{expr::Expression, idents::{Ident, IdentTree}};

use super::{registry::{TTypeId, TypeRegistry, TypeRegistryError}, value::{EnumValue, ScalarValueInner, Value}, TypeError};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum TType {
    // Refined(RefinedType),
    Composite(CompositeType),
    Scalar(ScalarType),
}

impl TType {
    pub const UNIT: TType = TType::Scalar(ScalarType::Unit);
    pub const BOOL: TType = TType::Scalar(ScalarType::Bool);
    pub const INT32: TType = TType::Scalar(ScalarType::Int32);
    pub const STRING: TType = TType::Scalar(ScalarType::String);

    pub fn select(&self, registry: &TypeRegistry, trees: &[IdentTree]) -> Result<TType, TypeError> {
        match self {
            // TType::Refined(ttype) => ttype.select(registry, trees),
            TType::Composite(ttype) => Ok(TType::Composite(ttype.select(registry, trees)?)),
            TType::Scalar(ttype) => Ok(TType::Scalar(ttype.select(trees)?)),
        }
    }

    pub fn dot(&self, registry: &TypeRegistry, ident: &Ident) -> Result<TType, TypeError> {
        match self {
            // TType::Refined(ttype) => ttype.dot(registry, ident),
            TType::Composite(ttype) => ttype.dot(registry, ident),
            TType::Scalar(_) => Err(TypeError::ScalarField(ident.clone())),
        }
    }

    pub fn unrefined(self, registry: &TypeRegistry) -> Result<(TType, Box<[Expression]>), TypeRegistryError> {
        fn unrefine(ttype: TType, registry: &TypeRegistry, conditions: &mut Vec<Expression>) -> Result<TType, TypeRegistryError> {
            match ttype {
                // TType::Refined(ttype) => {
                //     conditions.push(*ttype.condition);
                //     unrefine(registry.get_by_id(&ttype.ttype_id)?, registry, conditions)
                // },
                TType::Composite(ttype) => Ok(TType::Composite(ttype)),
                TType::Scalar(ttype) => Ok(TType::Scalar(ttype)),
            }
        }

        let mut conditions = vec![];
        
        let ttype = unrefine(self, registry, &mut conditions)?;

        Ok((ttype, conditions.into()))
    }
}

impl Debug for TType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Self::Refined(ttype) => Debug::fmt(ttype, f),
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
pub struct RefinedType {
    ttype_id: TTypeId,
    condition: Box<Expression>,
}

impl RefinedType {
    pub const RESULT_TYPE_NAME: &str = "RefinementResult";
    pub const RESULT_TYPE_OK: &str = "Ok";
    pub const RESULT_TYPE_ERR: &str = "Err";

    pub fn result_ok(registry: &TypeRegistry) -> Value {
        let ttype_id = registry.get_id_by_name(Self::RESULT_TYPE_NAME).expect("unreachable");
        
        EnumValue::new(
            registry, ttype_id,
            Self::RESULT_TYPE_OK.parse().expect("unreachable"),
            Value::Scalar(ScalarValueInner::Unit.into()),
        ).expect("unreachable").into()
    }

    pub fn new(registry: &TypeRegistry, ttype_id: TTypeId, condition: Expression) -> Result<RefinedType, TypeError> {
        let refinement_ttype = condition.eval_types(registry, &[&StructType {
            fields: btreemap! {
                "this".parse().expect("unreachable") => ttype_id.clone(),
            },
        }])?;

        let result_ttype = registry.get_by_name(Self::RESULT_TYPE_NAME)?;

        if result_ttype != refinement_ttype {
            return Err(TypeError::TypeInvalid { expected: result_ttype, got: refinement_ttype });
        }

        Ok(RefinedType {
            ttype_id: ttype_id,
            condition: Box::new(condition),
        })
    }

    pub fn ttype_id(&self) -> &TTypeId {
        &self.ttype_id
    }

    pub fn condition(&self) -> &Expression {
        &self.condition
    }

    pub fn select(&self, registry: &TypeRegistry, ident_trees: &[IdentTree]) -> Result<TType, TypeError> {
        registry.get_by_id(&self.ttype_id)?.select(registry, ident_trees)
    }

    pub fn dot(&self, registry: &TypeRegistry, ident: &Ident) -> Result<TType, TypeError> {
        registry.get_by_id(&self.ttype_id)?.dot(registry, ident)
    }
}

impl Debug for RefinedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.ttype_id, f)?;
        write!(f, " {{ {:?} }}", &self.condition)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum CompositeType {
    Struct(StructType),
    Enum(EnumType),
}

impl CompositeType {
    pub fn select(&self, registry: &TypeRegistry, ident_trees: &[IdentTree]) -> Result<CompositeType, TypeError> {
        match self {
            CompositeType::Struct(ttype) => Ok(CompositeType::Struct(ttype.select(registry, ident_trees)?)),
            CompositeType::Enum(ttype) => Ok(CompositeType::Enum(ttype.select(ident_trees)?)),
        }
    }

    pub fn dot(&self, registry: &TypeRegistry, ident: &Ident) -> Result<TType, TypeError> {
        match self {
            CompositeType::Struct(ttype) => ttype.dot(registry, ident),
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

    pub fn select(&self, registry: &TypeRegistry, ident_trees: &[IdentTree]) -> Result<StructType, TypeError> {
        if ident_trees.is_empty() {
            return Ok(self.clone());
        }

        let mut new_fields = BTreeMap::new();
        
        for tree in ident_trees {
            let ident = tree.ident().clone();

            if let Some(ttype_id) = self.fields.get(&ident) {
                let field_type = registry.get_by_id(ttype_id)?;

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

    pub fn dot(&self, registry: &TypeRegistry, ident: &Ident) -> Result<TType, TypeError> {
        if let Some(field_ttype_id) = self.fields.get(ident)  {
            let ttype = registry.get_by_id(field_ttype_id)?;
            Ok(ttype)
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
