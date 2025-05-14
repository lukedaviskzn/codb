use std::{collections::BTreeMap, fmt::{Debug, Display}};

use crate::{idents::{Ident, IdentTree}, typesystem::{registry::{TTypeId, TypeRegistry}, ttype::{CompositeType, EnumType, RefinedType, ScalarType, StructType, TType}, RefinementFailedError, TypeError}};

use super::{CompositeLiteral, EnumLiteral, Literal, LiteralType, ScalarLiteral, ScalarLiteralInner, StructLiteral};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum Value {
    Composite(CompositeValue),
    Scalar(ScalarValue),
}

impl Value {
    pub const UNIT: Value = Value::Scalar(ScalarValue::from_inner(ScalarValueInner::Unit));
    pub const TRUE: Value = Value::Scalar(ScalarValue::from_inner(ScalarValueInner::Bool(true)));
    pub const FALSE: Value = Value::Scalar(ScalarValue::from_inner(ScalarValueInner::Bool(false)));

    pub fn from_literal(registry: &TypeRegistry, literal: Literal) -> Result<Value, TypeError> {
        match literal {
            Literal::Composite(literal) => Ok(Value::Composite(CompositeValue::from_literal(registry, literal)?)),
            Literal::Scalar(value) => Ok(Value::Scalar(ScalarValue::from_literal(registry, value)?)),
        }
    }

    pub fn ttype_id(&self) -> TTypeId {
        match self {
            Value::Composite(value) => value.ttype_id(),
            Value::Scalar(value) => value.ttype_id(),
        }
    }

    pub fn select(&self, registry: &TypeRegistry, ident_trees: &[IdentTree]) -> Result<Value, TypeError> {
        match self {
            Value::Composite(value) => Ok(Value::Composite(value.select(registry, ident_trees)?)),
            Value::Scalar(value) => Ok(Value::Scalar(value.select(ident_trees)?)),
        }
    }

    pub fn dot(&self, ident: &Ident) -> Result<Value, TypeError> {
        match self {
            Value::Composite(value) => value.dot(ident),
            Value::Scalar(_) => Err(TypeError::ScalarField(ident.clone())),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Composite(val) => val.fmt(f),
            Self::Scalar(val) => Debug::fmt(val, f),
        }
    }
}

impl From<CompositeValue> for Value {
    fn from(value: CompositeValue) -> Self {
        Self::Composite(value)
    }
}

impl From<StructValue> for Value {
    fn from(value: StructValue) -> Self {
        Self::Composite(CompositeValue::Struct(value))
    }
}

impl From<EnumValue> for Value {
    fn from(value: EnumValue) -> Self {
        Self::Composite(CompositeValue::Enum(value))
    }
}

impl From<ScalarValue> for Value {
    fn from(value: ScalarValue) -> Self {
        Self::Scalar(value)
    }
}

impl From<ScalarValueInner> for Value {
    fn from(value: ScalarValueInner) -> Self {
        Self::Scalar(value.into())
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum CompositeValue {
    Struct(StructValue),
    Enum(EnumValue),
}

impl CompositeValue {
    pub fn from_literal(registry: &TypeRegistry, literal: CompositeLiteral) -> Result<CompositeValue, TypeError> {
        match literal {
            CompositeLiteral::Struct(literal) => Ok(Self::Struct(StructValue::from_literal(registry, literal)?)),
            CompositeLiteral::Enum(literal) => Ok(Self::Enum(EnumValue::from_literal(registry, literal)?)),
        }
    }

    pub fn ttype_id(&self) -> TTypeId {
        match self {
            CompositeValue::Struct(value) => value.ttype_id(),
            CompositeValue::Enum(value) => value.ttype_id(),
        }
    }

    pub fn select(&self, registry: &TypeRegistry, ident_trees: &[IdentTree]) -> Result<CompositeValue, TypeError> {
        match self {
            CompositeValue::Struct(value) => Ok(CompositeValue::Struct(value.select(registry, ident_trees)?)),
            CompositeValue::Enum(value) => Ok(CompositeValue::Enum(value.select(registry, ident_trees)?)),
        }
    }

    pub fn dot(&self, ident: &Ident) -> Result<Value, TypeError> {
        match self {
            CompositeValue::Struct(value) => value.dot(ident),
            CompositeValue::Enum(_) => Err(TypeError::DotTag(ident.clone())),
        }
    }
}

impl Debug for CompositeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Struct(value) => value.fmt(f),
            Self::Enum(value) => value.fmt(f),
        }
    }
}

impl From<StructValue> for CompositeValue {
    fn from(value: StructValue) -> Self {
        CompositeValue::Struct(value)
    }
}

impl From<EnumValue> for CompositeValue {
    fn from(value: EnumValue) -> Self {
        CompositeValue::Enum(value)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct StructValue {
    ttype_id: TTypeId,
    fields: BTreeMap<Ident, Value>,
}

impl StructValue {
    pub fn new(registry: &TypeRegistry, ttype_id: TTypeId, fields: BTreeMap<Ident, Value>) -> Result<StructValue, TypeError> {
        let (ttype, refinement_conditions) = registry.get_by_id(&ttype_id)?.unrefined(registry)?;

        let TType::Composite(CompositeType::Struct(ttype)) = ttype else {
            return Err(TypeError::TypeInvalid {
                expected: StructType::new(btreemap! {}).into(),
                got: ttype
            });
        };

        let mut new_fields = btreemap! {};

        // check all fields exist in type
        for (field_name, field_value) in &fields {
            // field exists
            if let Some(field_ttype_id) = ttype.fields().get(field_name) {
                if *field_ttype_id != field_value.ttype_id() {
                    return Err(TypeError::ValueTypeInvalid {
                        expected: registry.get_by_id(field_ttype_id)?,
                        got: field_value.clone(),
                    });
                }

                new_fields.insert(field_name.clone(), field_value);
            } else {
                return Err(TypeError::UnknownField(field_name.clone()));
            }
        }

        // check literal has all fields
        for name in fields.keys() {
            if !fields.contains_key(name) {
                return Err(TypeError::MissingField(name.clone()));
            }
        }

        let value = StructValue {
            ttype_id,
            fields,
        };

        // check refinement conditions

        let scope_ttype = TTypeId::Anonymous(Box::new(StructType::new(btreemap! {
            "this".parse().expect("unreachable") => value.ttype_id(),
        }).into()));

        for condition in refinement_conditions {
            let expr = condition.eval(registry, &[&StructValue::new(registry, scope_ttype.clone(), btreemap! {
                "this".parse().expect("unreachable") => value.clone().into(),
            }).expect("unreachable")]).map_err(|err| RefinementFailedError::Expr(Box::new(err)))?;

            let result_ttype = registry.get_id_by_name(RefinedType::RESULT_TYPE_NAME)?;

            if result_ttype != expr.ttype_id() {
                return Err(TypeError::TypeInvalid {
                    expected: registry.get_by_id(&result_ttype)?,
                    got: registry.get_by_id(&expr.ttype_id())?,
                })
            }

            if expr != RefinedType::result_ok(registry) {
                let Value::Composite(CompositeValue::Enum(expr)) = expr else { unreachable!() };
                let Value::Scalar(ScalarValue {
                    inner: ScalarValueInner::String(string),
                    ..
                }) = expr.into_value() else { unreachable!() };

                return Err(TypeError::RefinementFailed(RefinementFailedError::Refinement(string)))
            }
        }

        Ok(value)
    }

    pub fn from_literal(registry: &TypeRegistry, literal: StructLiteral) -> Result<StructValue, TypeError> {
        let ttype_id = match literal.ttype {
            LiteralType::Name(ttype) => registry.get_id_by_name(&ttype)?,
            LiteralType::Anonymous(ttype) => TTypeId::Anonymous(Box::new(ttype)),
        };

        let mut new_fields = btreemap! {};

        for (name, value) in literal.fields {
            new_fields.insert(name, Value::from_literal(registry, value)?);
        }

        StructValue::new(registry, ttype_id, new_fields)
    }

    pub fn fields(&self) -> &BTreeMap<Ident, Value> {
        &self.fields
    }

    pub fn ttype_id(&self) -> TTypeId {
        self.ttype_id.clone()
    }

    pub fn select(&self, registry: &TypeRegistry, ident_trees: &[IdentTree]) -> Result<StructValue, TypeError> {
        if ident_trees.is_empty() {
            return Ok(self.clone());
        }

        let mut new_fields = BTreeMap::new();
        
        for tree in ident_trees {
            let ident = tree.ident().clone();
            if let Some(value) = self.fields.get(&ident) {
                new_fields.insert(ident, value.select(registry, tree.children())?);
            } else {
                return Err(TypeError::UnknownField(ident));
            }
        }

        let ttype = registry.get_by_id(&self.ttype_id)?;
        let TType::Composite(CompositeType::Struct(struct_type)) = ttype else {
            return Err(TypeError::TypeInvalid {
                expected: StructType::new(btreemap! {}).into(),
                got: ttype,
            })
        };
        
        let selection_type = struct_type.select(registry, ident_trees)?;
        
        Ok(StructValue {
            ttype_id: TTypeId::Anonymous(Box::new(selection_type.into())),
            fields: new_fields,
        })
    }

    pub fn dot(&self, ident: &Ident) -> Result<Value, TypeError> {
        if let Some(value) = self.fields.get(ident) {
            Ok(value.clone())
        } else {
            Err(TypeError::UnknownField(ident.clone()))
        }
    }
}

impl Debug for StructValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct(&format!("{:?}", self.ttype_id));
        
        for (name, value) in &self.fields {
            d.field(name, value);
        }
        
        d.finish()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct EnumValue {
    ttype_id: TTypeId,
    tag: Ident,
    value: Box<Value>,
}

impl EnumValue {
    pub fn new(registry: &TypeRegistry, ttype_id: TTypeId, tag: Ident, value: Value) -> Result<EnumValue, TypeError> {
        let ttype = registry.get_by_id(&ttype_id)?;

        let TType::Composite(CompositeType::Enum(ttype)) = ttype else {
            return Err(TypeError::TypeInvalid {
                expected: EnumType::new(btreemap! {}).into(),
                got: ttype,
            })
        };

        let tag_name = tag.clone();
        
        if let Some(tag_ttype_id) = ttype.tags().get(&tag_name) {
            if *tag_ttype_id != value.ttype_id() {
                return Err(TypeError::ValueTypeInvalid {
                    expected: registry.get_by_id(tag_ttype_id)?,
                    got: value,
                });
            }

            Ok(EnumValue {
                ttype_id,
                tag: tag_name,
                value: Box::new(value),
            })
        } else {
            Err(TypeError::UnknownTag(tag_name))
        }
    }

    pub fn from_literal(registry: &TypeRegistry, literal: EnumLiteral) -> Result<EnumValue, TypeError> {
        let ttype_id = match literal.ttype {
            LiteralType::Name(ttype) => registry.get_id_by_name(&ttype)?,
            LiteralType::Anonymous(ttype) => TTypeId::Anonymous(Box::new(ttype)),
        };

        let value = Value::from_literal(registry, *literal.value)?;

        EnumValue::new(registry, ttype_id, literal.tag, value)
    }

    pub fn ttype_id(&self) -> TTypeId {
        self.ttype_id.clone()
    }

    pub fn tag(&self) -> &Ident {
        &self.tag
    }

    pub fn value(&self) -> &Value {
        &self.value
    }

    pub fn into_value(self) -> Value {
        *self.value
    }

    pub fn select(&self, registry: &TypeRegistry, ident_trees: &[IdentTree]) -> Result<EnumValue, TypeError> {
        if ident_trees.is_empty() {
            return Ok(self.clone());
        }

        let ttype = registry.get_by_id(&self.ttype_id)?;

        let TType::Composite(CompositeType::Enum(ttype)) = ttype else {
            unreachable!();
        };

        for tag in ttype.tags().keys() {
            if ident_trees.iter().all(|tree| tree.ident() != tag) {
                return Err(TypeError::MissingTag(tag.clone()));
            }
        }

        for tree in ident_trees {
            if ttype.tags().keys().all(|tag| tag != tree.ident()) {
                return Err(TypeError::UnknownTag(tree.ident().clone()));
            }
        }

        Ok(self.clone())
    }
}

impl Debug for EnumValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&format!("{:?}::{:?}", self.ttype_id, self.tag))
            .field(&self.value)
            .finish()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct ScalarValue {
    ttype_id: TTypeId,
    inner: ScalarValueInner,
}

impl ScalarValue {
    pub fn new(registry: &TypeRegistry, ttype_id: TTypeId, inner: ScalarValueInner) -> Result<ScalarValue, TypeError> {
        let (ttype, refinement_conditions) = registry.get_by_id(&ttype_id)?.unrefined(registry)?;

        let TType::Scalar(ttype) = ttype else {
            return Err(TypeError::TypeInvalid {
                // todo: properly handle
                expected: ScalarType::Never.into(),
                got: ttype
            });
        };

        let inner = match inner {
            ScalarValueInner::Unit => if let ScalarType::Unit = &ttype {
                ScalarValueInner::Unit
            } else {
                return Err(TypeError::TypeInvalid {
                    expected: ttype.into(),
                    got: ScalarType::Unit.into(),
                });
            },
            ScalarValueInner::Bool(value) => if let ScalarType::Bool = &ttype {
                ScalarValueInner::Bool(value)
            } else {
                return Err(TypeError::TypeInvalid {
                    expected: ttype.into(),
                    got: ScalarType::Bool.into(),
                });
            },
            ScalarValueInner::Int32(value) => if let ScalarType::Int32 = &ttype {
                ScalarValueInner::Int32(value)
            } else {
                return Err(TypeError::TypeInvalid {
                    expected: ttype.into(),
                    got: ScalarType::Int32.into(),
                });
            },
            ScalarValueInner::String(value) => if let ScalarType::String = &ttype {
                ScalarValueInner::String(value)
            } else {
                return Err(TypeError::TypeInvalid {
                    expected: ttype.into(),
                    got: ScalarType::String.into(),
                });
            },
        };

        let value = ScalarValue {
            ttype_id,
            inner,
        };

        // check refinement conditions

        let scope_ttype = TTypeId::Anonymous(Box::new(StructType::new(btreemap! {
            "this".parse().expect("unreachable") => value.ttype_id(),
        }).into()));

        for condition in refinement_conditions {
            let expr = condition.eval(registry, &[&StructValue::new(registry, scope_ttype.clone(), btreemap! {
                "this".parse().expect("unreachable") => value.clone().into(),
            }).expect("unreachable")]).map_err(|err| RefinementFailedError::Expr(Box::new(err)))?;

            let result_ttype = registry.get_id_by_name(RefinedType::RESULT_TYPE_NAME)?;

            if result_ttype != expr.ttype_id() {
                return Err(TypeError::TypeInvalid {
                    expected: registry.get_by_id(&result_ttype)?,
                    got: registry.get_by_id(&expr.ttype_id())?,
                })
            }

            if expr != RefinedType::result_ok(registry) {
                let Value::Composite(CompositeValue::Enum(expr)) = expr else { unreachable!() };
                let Value::Scalar(ScalarValue {
                    inner: ScalarValueInner::String(string),
                    ..
                }) = expr.into_value() else { unreachable!() };

                return Err(TypeError::RefinementFailed(RefinementFailedError::Refinement(string)))
            }
        }

        Ok(value)
    }

    pub fn from_literal(registry: &TypeRegistry, literal: ScalarLiteral) -> Result<ScalarValue, TypeError> {
        let ttype_id = match literal.ttype {
            LiteralType::Name(ttype) => registry.get_id_by_name(&ttype)?,
            LiteralType::Anonymous(ttype) => TTypeId::Anonymous(Box::new(ttype)),
        };

        ScalarValue::new(registry, ttype_id, ScalarValueInner::from_literal(literal.inner))
    }

    pub fn ttype_id(&self) -> TTypeId {
        self.ttype_id.clone()
    }

    pub fn inner(&self) -> &ScalarValueInner {
        &self.inner
    }

    pub fn select(&self, ident_trees: &[IdentTree]) -> Result<ScalarValue, TypeError> {
        if let Some(first) = ident_trees.first() {
            Err(TypeError::ScalarField(first.ident().clone()))
        } else {
            Ok(self.clone())
        }
    }

    pub const fn from_inner(inner: ScalarValueInner) -> Self {
        Self {
            ttype_id: match &inner {
                ScalarValueInner::Unit => TTypeId::Scalar(ScalarType::Unit),
                ScalarValueInner::Bool(_) => TTypeId::Scalar(ScalarType::Bool),
                ScalarValueInner::Int32(_) => TTypeId::Scalar(ScalarType::Int32),
                ScalarValueInner::String(_) => TTypeId::Scalar(ScalarType::String),
            },
            inner,
        }
    }
}

impl From<ScalarValueInner> for ScalarValue {
    fn from(value: ScalarValueInner) -> Self {
        Self::from_inner(value)
    }
}

impl Debug for ScalarValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?})", self.ttype_id)?;
        Debug::fmt(&self.inner, f)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum ScalarValueInner {
    Unit,
    Bool(bool),
    Int32(i32),
    String(String),
}

impl ScalarValueInner {
    pub fn from_literal(literal: ScalarLiteralInner) -> ScalarValueInner {
        match literal {
            ScalarLiteralInner::Unit => ScalarValueInner::Unit,
            ScalarLiteralInner::Bool(value) => ScalarValueInner::Bool(value),
            ScalarLiteralInner::Int32(value) => ScalarValueInner::Int32(value),
            ScalarLiteralInner::String(value) => ScalarValueInner::String(value),
        }
    }
}

impl Debug for ScalarValueInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Bool(value) => Debug::fmt(value, f),
            Self::Int32(value) => Debug::fmt(value, f),
            Self::String(value) => Debug::fmt(value, f),
        }
    }
}

impl Display for ScalarValueInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(value) => Display::fmt(value, f),
            value => Debug::fmt(value, f),
        }
    }
}
