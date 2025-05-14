use std::{collections::BTreeMap, fmt::{Debug, Display}};

use crate::{idents::{Ident, NestedIdent}, typesystem::{registry::{TTypeId, TypeRegistry}, ttype::{CompositeType, EnumType, ScalarType, StructType, TType}, value::{CompositeValue, LiteralType, ScalarValue, ScalarValueInner, StructLiteral, StructValue, Value}, TypeError}};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum Expression {
    NestedIdent(NestedIdent),
    Value(Value),
    Op(Box<Op>),
    ControlFlow(Box<ControlFlow>),
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NestedIdent(nested_idents) => Display::fmt(&nested_idents.join("."), f),
            Self::Value(value) => Debug::fmt(value, f),
            Self::Op(op) => Debug::fmt(op, f),
            Self::ControlFlow(cf) => Debug::fmt(cf, f)
        }
    }
}

impl Expression {
    pub fn eval_types(&self, registry: &TypeRegistry, scopes: &[&StructType]) -> Result<TType, TypeError> {
        match self {
            Expression::NestedIdent(nested_idents) => {
                let mut nested_idents = nested_idents.to_vec();
                let first = nested_idents.remove(0);

                let mut ttype = None;

                for scope in scopes {
                    if let Some(ttype_id) = scope.fields().get(&first) {
                        ttype = Some(registry.get_by_id(ttype_id)?);
                        break;
                    }
                }

                let Some(mut ttype) = ttype else {
                    return Err(TypeError::MissingField(first));
                };

                for ident in nested_idents {
                    ttype = ttype.dot(registry, &ident)?;
                }
                
                Ok(ttype)
            },
            Expression::Value(value) => Ok(registry.get_by_id(&value.ttype_id())?),
            Expression::Op(op) => op.eval_types(registry, scopes),
            Expression::ControlFlow(control_flow) => control_flow.eval_types(registry, scopes),
        }
    }

    pub fn eval(&self, registry: &TypeRegistry, scopes: &[&StructValue]) -> Result<Value, ExprError> {
        match self {
            Expression::NestedIdent(nested_idents) => {
                let mut nested_idents = nested_idents.to_vec();
                let first = nested_idents.remove(0);

                let mut value = None;

                for scope in scopes {
                    if let Some(val) = scope.fields().get(&first) {
                        value = Some(val.clone());
                        break;
                    }
                }

                let Some(mut value) = value else {
                    return Err(ExprError::TTypeError(TypeError::MissingField(first)));
                };

                for ident in nested_idents {
                    value = value.dot(&ident)?;
                }
                
                Ok(value)
            },
            Expression::Value(value) => Ok(value.clone()),
            Expression::Op(op) => op.eval(registry, scopes),
            Expression::ControlFlow(cf) => cf.eval(registry, scopes),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum Op {
    Logical(LogicalOp),
    Arithmetic(ArithmeticOp),
}

impl Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Logical(op) => Debug::fmt(op, f),
            Self::Arithmetic(op) => Debug::fmt(op, f),
        }
    }
}

impl Op {
    pub fn eval_types(&self, registry: &TypeRegistry, scopes: &[&StructType]) -> Result<TType, TypeError> {
        match self {
            Op::Logical(op) => op.eval_types(registry, scopes),
            Op::Arithmetic(op) => op.eval_types(registry, scopes),
        }
    }
    
    pub fn eval(&self, registry: &TypeRegistry, scopes: &[&StructValue]) -> Result<Value, ExprError> {
        match self {
            Op::Logical(op) => op.eval(registry, scopes),
            Op::Arithmetic(op) => op.eval(registry, scopes),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum LogicalOp {
    And(Expression, Expression),
    Or(Expression, Expression),
    Not(Expression),
    Eq(Expression, Expression),
    Lt(Expression, Expression),
    Gt(Expression, Expression),
    Lte(Expression, Expression),
    Gte(Expression, Expression),
}

impl Debug for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::And(left, right) => write!(f, "({left:?} && {right:?})"),
            Self::Or(left, right) => write!(f, "({left:?} || {right:?})"),
            Self::Not(value) => write!(f, "!({value:?})"),
            Self::Eq(left, right) => write!(f, "({left:?} == {right:?})"),
            Self::Lt(left, right) => write!(f, "({left:?} < {right:?})"),
            Self::Gt(left, right) => write!(f, "({left:?} > {right:?})"),
            Self::Lte(left, right) => write!(f, "({left:?} >= {right:?})"),
            Self::Gte(left, right) => write!(f, "({left:?} <= {right:?})"),
        }
    }
}

impl LogicalOp {
    pub fn eval_types(&self, registry: &TypeRegistry, scopes: &[&StructType]) -> Result<TType, TypeError> {
        match self {
            LogicalOp::And(left, right) |
            LogicalOp::Or(left, right) => {
                let (left_type, _) = left.eval_types(registry, scopes)?.unrefined(registry)?;
                if TType::BOOL != left_type {
                    return Err(TypeError::TypeInvalid { expected: TType::BOOL, got: left_type });
                }
                
                let (right_type, _) = right.eval_types(registry, scopes)?.unrefined(registry)?;
                if TType::BOOL != right_type {
                    return Err(TypeError::TypeInvalid { expected: TType::BOOL, got: right_type });
                }

                Ok(TType::BOOL)
            },
            LogicalOp::Lt(left, right) |
            LogicalOp::Gt(left, right) |
            LogicalOp::Lte(left, right) |
            LogicalOp::Gte(left, right) => {
                let (left_type, _) = left.eval_types(registry, scopes)?.unrefined(registry)?;
                if TType::INT32 != left_type {
                    return Err(TypeError::TypeInvalid { expected: TType::INT32, got: left_type });
                }
                
                let (right_type, _) = right.eval_types(registry, scopes)?.unrefined(registry)?;
                if TType::INT32 != right_type {
                    return Err(TypeError::TypeInvalid { expected: TType::INT32, got: right_type });
                }

                Ok(TType::BOOL)
            },
            LogicalOp::Eq(left, right) => {
                let (left_type, _) = left.eval_types(registry, scopes)?.unrefined(registry)?;
                let (right_type, _) = right.eval_types(registry, scopes)?.unrefined(registry)?;

                if left_type != right_type {
                    return Err(TypeError::TypeInvalid { expected: left_type, got: right_type });
                }

                Ok(TType::BOOL)
            },
            LogicalOp::Not(expr) => {
                let (expr_ttype, _) = expr.eval_types(registry, scopes)?.unrefined(registry)?;
                if TType::BOOL != expr_ttype {
                    return Err(TypeError::TypeInvalid { expected: TType::BOOL, got: expr_ttype });
                }
                
                Ok(TType::BOOL)
            },
        }
    }

    pub fn eval(&self, registry: &TypeRegistry, scopes: &[&StructValue]) -> Result<Value, ExprError> {
        match self {
            LogicalOp::And(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                if left.ttype_id() == TTypeId::BOOL {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::BOOL,
                        got: left,
                    }))
                }
                
                if right.ttype_id() == TTypeId::BOOL {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::BOOL,
                        got: right,
                    }))
                }
                
                Ok(ScalarValueInner::Bool(left == Value::TRUE && right == Value::TRUE).into())
            },
            LogicalOp::Or(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                if left.ttype_id() == TTypeId::BOOL {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::BOOL,
                        got: left,
                    }))
                }
                
                if right.ttype_id() == TTypeId::BOOL {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::BOOL,
                        got: right,
                    }))
                }
                
                Ok(ScalarValueInner::Bool(left == Value::TRUE || right == Value::TRUE).into())
            },
            LogicalOp::Not(expr) => {
                let value = expr.eval(registry, scopes)?;
                
                if value.ttype_id() == TTypeId::BOOL {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::BOOL,
                        got: value,
                    }))
                }
                
                Ok(ScalarValueInner::Bool(value == Value::FALSE).into())
            },
            LogicalOp::Eq(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;

                if left.ttype_id() != right.ttype_id() {
                    return Err(TypeError::TypeInvalid {
                        expected: registry.get_by_id(&left.ttype_id()).map_err(|err| TypeError::from(err))?,
                        got: registry.get_by_id(&right.ttype_id()).map_err(|err| TypeError::from(err))?,
                    }.into());
                }
                
                Ok(ScalarValueInner::Bool(left == right).into())
            },
            LogicalOp::Lt(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                if left.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: left,
                    }))
                }
                
                if right.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: right,
                    }))
                }
                
                Ok(ScalarValueInner::Bool(left < right).into())
            },
            LogicalOp::Gt(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                if left.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: left,
                    }))
                }
                
                if right.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: right,
                    }))
                }
                
                Ok(ScalarValueInner::Bool(left > right).into())
            },
            LogicalOp::Lte(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                if left.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: left,
                    }))
                }
                
                if right.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: right,
                    }))
                }
                
                Ok(ScalarValueInner::Bool(left <= right).into())
            },
            LogicalOp::Gte(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                if left.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: left,
                    }))
                }
                
                if right.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: right,
                    }))
                }
                
                Ok(ScalarValueInner::Bool(left >= right).into())
            },
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum ArithmeticOp {
    Add(Expression, Expression),
    Sub(Expression, Expression),
    Mul(Expression, Expression),
    Div(Expression, Expression),
}

impl Debug for ArithmeticOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add(left, right) => write!(f, "({left:?} + {right:?})"),
            Self::Sub(left, right) => write!(f, "({left:?} - {right:?})"),
            Self::Mul(left, right) => write!(f, "({left:?} * {right:?})"),
            Self::Div(left, right) => write!(f, "({left:?} / {right:?})"),
        }
    }
}

impl ArithmeticOp {
    pub fn eval_types(&self, registry: &TypeRegistry, scopes: &[&StructType]) -> Result<TType, TypeError> {
        match self {
            ArithmeticOp::Add(left, right) |
            ArithmeticOp::Sub(left, right) |
            ArithmeticOp::Mul(left, right) |
            ArithmeticOp::Div(left, right) => {
                let (left_type, _) = left.eval_types(registry, scopes)?.unrefined(registry)?;
                let (right_type, _) = right.eval_types(registry, scopes)?.unrefined(registry)?;

                if TType::INT32 != left_type {
                    return Err(TypeError::TypeInvalid { expected: TType::INT32, got: left_type });
                }
                if TType::INT32 != right_type {
                    return Err(TypeError::TypeInvalid { expected: TType::INT32, got: right_type });
                }

                Ok(TType::INT32)
            },
        }
    }

    pub fn eval(&self, registry: &TypeRegistry, scopes: &[&StructValue]) -> Result<Value, ExprError> {
        match self {
            ArithmeticOp::Add(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                if left.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: left,
                    }))
                }
                
                if right.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: right,
                    }))
                }

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValueInner::Int32(left) = left.inner() else { unreachable!() };
                let ScalarValueInner::Int32(right) = right.inner() else { unreachable!() };
                
                Ok(ScalarValueInner::Int32(left + right).into())
            },
            ArithmeticOp::Sub(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                if left.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: left,
                    }))
                }
                
                if right.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: right,
                    }))
                }

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValueInner::Int32(left) = left.inner() else { unreachable!() };
                let ScalarValueInner::Int32(right) = right.inner() else { unreachable!() };
                
                Ok(ScalarValueInner::Int32(left - right).into())
            },
            ArithmeticOp::Mul(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                if left.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: left,
                    }))
                }
                
                if right.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: right,
                    }))
                }

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValueInner::Int32(left) = left.inner() else { unreachable!() };
                let ScalarValueInner::Int32(right) = right.inner() else { unreachable!() };
                
                Ok(ScalarValueInner::Int32(left * right).into())
            },
            ArithmeticOp::Div(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                if left.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: left,
                    }))
                }
                
                if right.ttype_id() == TTypeId::INT32 {
                    return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                        expected: TType::INT32,
                        got: right,
                    }))
                }

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValueInner::Int32(left) = left.inner() else { unreachable!() };
                let ScalarValueInner::Int32(right) = right.inner() else { unreachable!() };
                
                Ok(ScalarValueInner::Int32(left / right).into())
            },
        }
    }
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum ExprError {
    #[error("{0}")]
    TTypeError(#[from] TypeError),
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum ControlFlow {
    If(IfControlFlow),
    Match(MatchControlFlow),
}

impl Debug for ControlFlow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::If(cf) => Debug::fmt(cf, f),
            Self::Match(cf) => Debug::fmt(cf, f),
        }
    }
}

impl ControlFlow {
    pub fn eval_types(&self, registry: &TypeRegistry, scopes: &[&StructType]) -> Result<TType, TypeError> {
        match self {
            ControlFlow::If(cf) => cf.eval_types(registry, scopes),
            ControlFlow::Match(cf) => cf.eval_types(registry, scopes),
        }
    }

    pub fn eval(&self, registry: &TypeRegistry, scopes: &[&StructValue]) -> Result<Value, ExprError> {
        match self {
            ControlFlow::If(cf) => cf.eval(registry, scopes),
            ControlFlow::Match(cf) => cf.eval(registry, scopes),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct IfControlFlow {
    pub condition: Expression,
    pub then: Expression,
    pub otherwise: Expression,
}

impl Debug for IfControlFlow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {:?} {{ {:?} }} else {{ {:?} }}", self.condition, self.then, self.otherwise)
    }
}

impl IfControlFlow {
    pub fn eval_types(&self, registry: &TypeRegistry, scopes: &[&StructType]) -> Result<TType, TypeError> {
        let (cond_type, _) = self.condition.eval_types(registry, scopes)?.unrefined(registry)?;

        if TType::BOOL != cond_type {
            return Err(TypeError::TypeInvalid { expected: TType::BOOL, got: cond_type });
        }
        
        let then_type = self.then.eval_types(registry, scopes)?;
        let otherwise_type = self.then.eval_types(registry, scopes)?;

        if then_type != otherwise_type {
            return Err(TypeError::TypeInvalid { expected: then_type, got: otherwise_type });
        }

        Ok(then_type)
    }

    pub fn eval(&self, registry: &TypeRegistry, scopes: &[&StructValue]) -> Result<Value, ExprError> {
        let cond_value = self.condition.eval(registry, scopes)?;
                
        if cond_value.ttype_id() == TTypeId::BOOL {
            return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                expected: TType::BOOL,
                got: cond_value,
            }))
        }

        if cond_value == Value::TRUE {
            self.then.eval(registry, scopes)
        } else {
            self.otherwise.eval(registry, scopes)
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct MatchControlFlow {
    pub param: Expression,
    pub branches: BTreeMap<Ident, Branch>,
}

impl Debug for MatchControlFlow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "match {:?} {{ ", self.param)?;

        for (i, (pattern, branch)) in self.branches.iter().enumerate() {
            write!(f, "{pattern:?}{branch:?}")?;
            if i < self.branches.len() - 1 {
                write!(f, ", ")?;
            }
        }

        write!(f, " }}")
    }
}

impl MatchControlFlow {
    fn extract_enum(registry: &TypeRegistry, mut ttype: TType) -> Result<EnumType, TypeError> {
        let example_enum = TType::Composite(CompositeType::Enum(EnumType::new(btreemap! {})));

        loop {
            match ttype {
                TType::Composite(CompositeType::Enum(_)) => break,
                // TType::Refined(refined_type) => ttype = registry.get_by_id(refined_type.ttype_id())?,
                ttype => return Err(TypeError::TypeInvalid {
                    expected: example_enum,
                    got: ttype,
                }),
            };
        }

        let TType::Composite(CompositeType::Enum(ttype)) = ttype else {
            unreachable!();
        };

        Ok(ttype)
    }
    
    pub fn eval_types(&self, registry: &TypeRegistry, scopes: &[&StructType]) -> Result<TType, TypeError> {
        let param_type = self.param.eval_types(registry, scopes)?;
        let param_type = Self::extract_enum(registry, param_type)?;

        for tag in param_type.tags().keys() {
            if self.branches.keys().all(|pattern| pattern != tag) {
                return Err(TypeError::MissingTag(tag.clone()));
            }
        }

        for pattern in self.branches.keys() {
            if param_type.tags().keys().all(|tag| tag != pattern) {
                return Err(TypeError::UnknownTag(pattern.clone()));
            }
        }

        let Some((first_pattern_tag, first_branch)) = self.branches.first_key_value() else {
            return Ok(TType::Scalar(ScalarType::Never));
        };
        
        let new_scope = StructType::new(btreemap! {
            first_branch.ident.clone() => param_type.tags()[first_pattern_tag].clone(),
        });

        let mut new_scopes = scopes.to_vec();
        new_scopes.push(&new_scope);

        let first_branch_type = first_branch.expression.eval_types(registry, &new_scopes)?;

        // all branches return same type
        for (tag, branch) in &self.branches {
            let new_scope = StructType::new(btreemap! {
                branch.ident.clone() => param_type.tags()[tag].clone(),
            });

            let mut new_scopes = scopes.to_vec();
            new_scopes.push(&new_scope);

            let branch_type = branch.expression.eval_types(registry, &new_scopes)?;
            if branch_type != first_branch_type {
                return Err(TypeError::TypeInvalid {
                    expected: first_branch_type,
                    got: branch_type,
                });
            }
        }
        
        Ok(first_branch_type)
    }

    pub fn eval(&self, registry: &TypeRegistry, scopes: &[&StructValue]) -> Result<Value, ExprError> {
        let mut scope_types = vec![];

        for scope in scopes {
            let ttype = registry.get_by_id(&scope.ttype_id()).map_err(|err| TypeError::from(err))?;
            let TType::Composite(CompositeType::Struct(ttype)) = ttype else {
                unreachable!();
            };
            scope_types.push(ttype);
        }

        let scope_types = scope_types.iter().collect::<Vec<_>>();
        
        let param_type = self.param.eval_types(registry, &scope_types)?;
        let param_type = Self::extract_enum(registry, param_type)?;

        let param_value = self.param.eval(registry, scopes)?;

        let param_value = match param_value {
            Value::Composite(CompositeValue::Enum(param_value)) => param_value,
            param_value => return Err(ExprError::TTypeError(TypeError::ValueTypeInvalid {
                expected: TType::Composite(CompositeType::Enum(EnumType::new(btreemap! {}))),
                got: param_value,
            })),
        };

        let Some(branch) = self.branches.get(param_value.tag()) else {
            return Err(ExprError::TTypeError(TypeError::UnknownTag(param_value.tag().clone())));
        };

        let scope_type = TTypeId::Anonymous(Box::new(StructType::new(btreemap! {
            branch.ident.clone() => param_type.tags()[param_value.tag()].clone(),
        }).into()));
        
        let new_scope = StructValue::new(registry, scope_type, btreemap! {
            branch.ident.clone() => param_value.into_value(),
        })?;

        let mut new_scopes = scopes.to_vec();
        new_scopes.push(&new_scope);
        
        branch.expression.eval(registry, &new_scopes)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct Branch {
    ident: Ident,
    expression: Expression,
}

impl Debug for Branch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}) => {:?}", self.ident, self.expression)
    }
}

#[cfg(test)]
mod tests {
    use crate::typesystem::{ttype::RefinedType, value::{EnumLiteral, EnumValue, LiteralType, ScalarLiteral, ScalarLiteralInner}};

    use super::*;

    #[test]
    fn match_expr() {
        let registry = TypeRegistry::new();

        let result_ttype = registry.get_id_by_name(RefinedType::RESULT_TYPE_NAME).unwrap();

        let expr = Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
            param: Expression::Value(EnumValue::new(
                &registry, result_ttype.clone(),
                RefinedType::RESULT_TYPE_OK.parse().unwrap(),
                ScalarValueInner::Unit.into()
            ).unwrap().into()),
            branches: btreemap! {
                RefinedType::RESULT_TYPE_OK.parse().unwrap() => Branch {
                    ident: "_".parse().unwrap(),
                    expression: Expression::Value(ScalarValueInner::Int32(10).into()),
                },
                RefinedType::RESULT_TYPE_ERR.parse().unwrap() => Branch {
                    ident: "_".parse().unwrap(),
                    expression: Expression::Value(ScalarValueInner::Int32(8).into()),
                },
            },
        })));

        expr.eval_types(&registry, &[]).unwrap();
        assert_eq!(Value::Scalar(ScalarValueInner::Int32(10).into()), expr.eval(&registry, &[]).unwrap());

        let expr = Expression::ControlFlow(Box::new(
            ControlFlow::Match(MatchControlFlow {
                param: Expression::Value(EnumValue::new(&registry, result_ttype, RefinedType::RESULT_TYPE_OK.parse().unwrap(), ScalarValueInner::Unit.into()).unwrap().into()),
                branches: btreemap! {
                    RefinedType::RESULT_TYPE_OK.parse().unwrap() => Branch {
                        ident: "_".parse().unwrap(),
                        expression: Expression::Value(ScalarValueInner::Int32(10).into()),
                    },
                },
            })
        ));

        assert_eq!(TypeError::MissingTag(RefinedType::RESULT_TYPE_ERR.parse().unwrap()), expr.eval_types(&registry, &[]).unwrap_err());

        let expr = Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
            param: Expression::Value(
                EnumValue::from_literal(
                    &registry,
                    EnumLiteral {
                        ttype: LiteralType::Name(RefinedType::RESULT_TYPE_NAME.parse().expect("unreachable")),
                        tag: RefinedType::RESULT_TYPE_ERR.parse().expect("unreachable"),
                        value: Box::new(ScalarLiteralInner::String("my_error".into()).into()),
                    }
                ).unwrap().into()
            ),
            branches: btreemap! {
                RefinedType::RESULT_TYPE_OK.parse().unwrap() => Branch {
                    ident: "_".parse().unwrap(),
                    expression: Expression::Value(ScalarValueInner::String("Ok".into()).into()),
                },
                RefinedType::RESULT_TYPE_ERR.parse().unwrap() => Branch {
                    ident: "error".parse().unwrap(),
                    expression: Expression::NestedIdent("error".parse().unwrap()),
                },
            },
        })));

        assert_eq!(TType::Scalar(ScalarType::String), expr.eval_types(&registry, &[]).unwrap());
        assert_eq!(Value::Scalar(ScalarValueInner::String("my_error".into()).into()), expr.eval(&registry, &[]).unwrap());
    }
}
