use std::{borrow::Cow, collections::{BTreeMap, HashSet}, fmt::{Debug, Display}};

use codb_core::{Ident, IdentPath, NestedIdent};

use crate::{db::registry::{Registry, TTypeId}, scope::{ScopeTypes, ScopeValues}, typesystem::{function::{FunctionEntry, InterpreterFunctionAction}, ttype::{CompositeType, EnumType, ScalarType, StructType, TType}, value::{CompositeValue, ScalarValue, StructValue, Value}, TypeError}};

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum EvalError {
    #[error("{0:?}")]
    TypeError(#[from] TypeError),
    #[error("evaluation panicked{}{}", if .0.is_empty() { "!" } else { ": " }, .0)]
    Panic(String),
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum Expression {
    NestedIdent(NestedIdent),
    Value(Value),
    Op(Box<Op>),
    ControlFlow(Box<ControlFlow>),
    FunctionInvocation(FunctionInvocation),
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NestedIdent(nested_ident) => Display::fmt(&nested_ident.join("."), f),
            Self::Value(value) => Debug::fmt(value, f),
            Self::Op(op) => Debug::fmt(op, f),
            Self::ControlFlow(cf) => Debug::fmt(cf, f),
            Self::FunctionInvocation(function) => Debug::fmt(function, f),
        }
    }
}

impl Expression {
    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            Expression::NestedIdent(nested_ident) => scopes.get_nested(registry, nested_ident),
            Expression::Value(value) => Ok(value.ttype_id()),
            Expression::Op(op) => op.eval_types(registry, scopes),
            Expression::ControlFlow(control_flow) => control_flow.eval_types(registry, scopes),
            Expression::FunctionInvocation(function) => function.eval_types(registry, scopes),
        }
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            Expression::NestedIdent(nested_ident) => Ok(scopes.get_nested(nested_ident)?),
            Expression::Value(value) => Ok(value.clone()),
            Expression::Op(op) => Ok(op.eval(registry, scopes)?),
            Expression::ControlFlow(cf) => Ok(cf.eval(registry, scopes)?),
            Expression::FunctionInvocation(function) => function.eval(registry, scopes),
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
    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            Op::Logical(op) => op.eval_types(registry, scopes),
            Op::Arithmetic(op) => op.eval_types(registry, scopes),
        }
    }
    
    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
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
    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            LogicalOp::And(left, right) |
            LogicalOp::Or(left, right) => {
                let left_type_id = left.eval_types(registry, scopes)?;
                let right_type_id = right.eval_types(registry, scopes)?;

                left_type_id.must_eq(&TTypeId::BOOL)?;
                right_type_id.must_eq(&TTypeId::BOOL)?;
                
                Ok(TTypeId::BOOL)
            },
            LogicalOp::Lt(left, right) |
            LogicalOp::Gt(left, right) |
            LogicalOp::Lte(left, right) |
            LogicalOp::Gte(left, right) => {
                let left_type_id = left.eval_types(registry, scopes)?;
                let right_type_id = right.eval_types(registry, scopes)?;

                left_type_id.must_eq(&TTypeId::BOOL)?;
                right_type_id.must_eq(&TTypeId::BOOL)?;

                Ok(TTypeId::BOOL)
            },
            LogicalOp::Eq(left, right) => {
                let left_type_id = left.eval_types(registry, scopes)?;
                let right_type_id = right.eval_types(registry, scopes)?;

                right_type_id.must_eq(&left_type_id)?;

                Ok(TTypeId::BOOL)
            },
            LogicalOp::Not(expr) => {
                let expr_type_id = expr.eval_types(registry, scopes)?;

                expr_type_id.must_eq(&TTypeId::BOOL);
                
                Ok(TTypeId::BOOL)
            },
        }
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            LogicalOp::And(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;

                left.ttype_id().must_eq(&TTypeId::BOOL)?;
                right.ttype_id().must_eq(&TTypeId::BOOL)?;
                
                Ok(ScalarValue::Bool(left == Value::TRUE && right == Value::TRUE).into())
            },
            LogicalOp::Or(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::BOOL)?;
                right.ttype_id().must_eq(&TTypeId::BOOL)?;
                
                Ok(ScalarValue::Bool(left == Value::TRUE || right == Value::TRUE).into())
            },
            LogicalOp::Not(expr) => {
                let value = expr.eval(registry, scopes)?;
                
                value.ttype_id().must_eq(&TTypeId::BOOL)?;
                
                Ok(ScalarValue::Bool(value == Value::FALSE).into())
            },
            LogicalOp::Eq(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                right.ttype_id().must_eq(&left.ttype_id());
                
                Ok(ScalarValue::Bool(left == right).into())
            },
            LogicalOp::Lt(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;
                
                Ok(ScalarValue::Bool(left < right).into())
            },
            LogicalOp::Gt(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;
                
                Ok(ScalarValue::Bool(left > right).into())
            },
            LogicalOp::Lte(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;
                
                Ok(ScalarValue::Bool(left <= right).into())
            },
            LogicalOp::Gte(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;
                
                Ok(ScalarValue::Bool(left >= right).into())
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
    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            ArithmeticOp::Add(left, right) |
            ArithmeticOp::Sub(left, right) |
            ArithmeticOp::Mul(left, right) |
            ArithmeticOp::Div(left, right) => {
                let left_ttype_id = left.eval_types(registry, scopes)?;
                let right_ttype_id = right.eval_types(registry, scopes)?;
                
                left_ttype_id.must_eq(&TTypeId::INT32)?;
                right_ttype_id.must_eq(&TTypeId::INT32)?;

                Ok(TTypeId::INT32)
            },
        }
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            ArithmeticOp::Add(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValue::Int32(left) = left else { unreachable!() };
                let ScalarValue::Int32(right) = right else { unreachable!() };
                
                Ok(ScalarValue::Int32(left + right).into())
            },
            ArithmeticOp::Sub(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValue::Int32(left) = left else { unreachable!() };
                let ScalarValue::Int32(right) = right else { unreachable!() };
                
                Ok(ScalarValue::Int32(left - right).into())
            },
            ArithmeticOp::Mul(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValue::Int32(left) = left else { unreachable!() };
                let ScalarValue::Int32(right) = right else { unreachable!() };
                
                Ok(ScalarValue::Int32(left * right).into())
            },
            ArithmeticOp::Div(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                left.ttype_id().must_eq(&TTypeId::INT32)?;
                right.ttype_id().must_eq(&TTypeId::INT32)?;

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValue::Int32(left) = left else { unreachable!() };
                let ScalarValue::Int32(right) = right else { unreachable!() };
                
                Ok(ScalarValue::Int32(left / right).into())
            },
        }
    }
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
    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            ControlFlow::If(cf) => cf.eval_types(registry, scopes),
            ControlFlow::Match(cf) => cf.eval_types(registry, scopes),
        }
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            ControlFlow::If(cf) => cf.eval(registry, scopes),
            ControlFlow::Match(cf) => cf.eval(registry, scopes),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct IfControlFlow {
    pub condition: Expression,
    pub ret_type: TTypeId,
    pub then: Expression,
    pub otherwise: Expression,
}

impl Debug for IfControlFlow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {:?} {{ {:?} }} else {{ {:?} }}", self.condition, self.then, self.otherwise)
    }
}

impl IfControlFlow {
    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        let cond_type_id = self.condition.eval_types(registry, scopes)?;

        cond_type_id.must_eq(&TTypeId::BOOL)?;
        
        let then_type_id = self.then.eval_types(registry, scopes)?;
        let otherwise_type_id = self.then.eval_types(registry, scopes)?;

        then_type_id.must_eq(&self.ret_type)?;
        otherwise_type_id.must_eq(&self.ret_type)?;

        Ok(then_type_id)
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let cond_value = self.condition.eval(registry, scopes)?;
                
        cond_value.ttype_id().must_eq(&TTypeId::BOOL)?;

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
    pub ret_type: TTypeId,
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
    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        let param_type_id = self.param.eval_types(registry, scopes)?;

        if param_type_id == TTypeId::NEVER {
            return Ok(TTypeId::NEVER);
        }
        
        let param_type = registry.ttype(&param_type_id)
            .ok_or_else(|| TypeError::TypeNotFound(param_type_id.clone()))?;

        let TType::Composite(CompositeType::Enum(param_type)) = param_type else {
            return Err(TypeError::TypeInvalid {
                expected: EnumType::new(btreemap! {}).into(),
                got: param_type,
            });
        };

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
            return Ok(ScalarType::Never.into());
        };
        
        let new_scope = StructType::new(btreemap! {
            first_branch.ident.clone() => param_type.tags()[first_pattern_tag].clone(),
        });

        let mut new_scopes = scopes.clone();
        new_scopes.push(Cow::Owned(new_scope));

        // all branches return same type
        for (tag, branch) in &self.branches {
            let new_scope = StructType::new(btreemap! {
                branch.ident.clone() => param_type.tags()[tag].clone(),
            });

            let mut new_scopes = scopes.clone();
            new_scopes.push(Cow::Owned(new_scope));

            let branch_type_id = branch.expression.eval_types(registry, &new_scopes)?;

            branch_type_id.must_eq(&self.ret_type)?;
        }
        
        Ok(self.ret_type.clone())
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let scope_types = scopes.types()?;
        
        let param_type = self.param.eval_types(registry, &scope_types)?;
        let param_type = registry.ttype(&param_type)
            .ok_or_else(|| TypeError::from(TypeError::TypeNotFound(param_type.clone())))?;
        let TType::Composite(CompositeType::Enum(param_type)) = param_type else {
            return Err(TypeError::TypeInvalid {
                expected: EnumType::new(btreemap! {}).into(),
                got: param_type,
            }.into());
        };

        let param_value = self.param.eval(registry, scopes)?;

        let param_value = match param_value {
            Value::Composite(CompositeValue::Enum(param_value)) => param_value,
            param_value => return Err(TypeError::ValueTypeInvalid {
                expected: EnumType::new(btreemap! {}).into(),
                got: param_value,
            }.into()),
        };

        let Some(branch) = self.branches.get(param_value.tag()) else {
            return Err(TypeError::UnknownTag(param_value.tag().clone()).into());
        };

        let scope_type = TTypeId::new_anonymous(StructType::new(btreemap! {
            branch.ident.clone() => param_type.tags()[param_value.tag()].clone(),
        }).into());
        
        let new_scope = StructValue::new(registry, scope_type, btreemap! {
            branch.ident.clone() => param_value.into_value(),
        })?;

        let mut new_scopes = scopes.clone();
        new_scopes.push(Cow::Owned(new_scope));
        
        branch.expression.eval(registry, &new_scopes)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct Branch {
    ident: Ident,
    expression: Expression,
}

impl Branch {
    pub fn new(ident: Ident, expression: Expression) -> Branch {
        Branch {
            ident,
            expression,
        }
    }
}

impl Debug for Branch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}) => {:?}", self.ident, self.expression)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct FunctionInvocation {
    function: IdentPath,
    args: Box<[Expression]>,
}

impl FunctionInvocation {
    pub fn new(function: IdentPath, args: impl Into<Box<[Expression]>>) -> FunctionInvocation {
        FunctionInvocation {
            function,
            args: args.into(),
        }
    }

    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        let function = registry.function(&self.function)
            .ok_or_else(|| TypeError::FunctionNotFound(self.function.clone()))?;

        let ret_type = function.result_type();

        if function.args().len() != self.args.len() {
            return Err(TypeError::FunctionArgLen {
                expected: function.args().len(),
                got: self.args.len(),
            });
        }

        let mut arg_names = HashSet::new();

        for (arg, expression) in function.args().iter().zip(self.args.iter()) {
            let expression_type_id = expression.eval_types(registry, scopes)?;

            arg.ttype_id().must_eq(&expression_type_id)?;
            
            if !arg_names.insert(arg.name()) {
                return Err(TypeError::FunctionDuplicateArg {
                    arg: arg.name().clone(),
                })
            }
        }

        Ok(ret_type.clone())
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let function = registry.function(&self.function).ok_or_else(
            || TypeError::FunctionNotFound(self.function.clone()))?;
        let mut args = Vec::new();

        for arg in &self.args {
            args.push(arg.eval(registry, scopes)?);
        }

        match function {
            FunctionEntry::UserFunction(function) => Ok(function.invoke(registry, args)?),
            FunctionEntry::InterpreterFunction(function) => match function.action() {
                InterpreterFunctionAction::Panic => {
                    let message = args.get(0).map(|value| {
                        match value {
                            Value::Scalar(scalar_value) => if let ScalarValue::String(string) = scalar_value {
                                string.clone()
                            } else {
                                Default::default()
                            },
                            _ => Default::default(),
                        }
                    }).unwrap_or_default();

                    Err(EvalError::Panic(message))
                },
            },
        }
    }
}

impl Debug for FunctionInvocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_tuple("");
        for arg in &self.args {
            d.field(arg);
        }
        d.finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::typesystem::value::EnumValue;

    use super::*;

    #[test]
    fn match_expr() {
        let registry = Registry::new();

        let result_ttype_id = TTypeId::from(id_path!("Result"));

        let expr = Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
            param: Expression::Value(EnumValue::new(
                &registry, result_ttype_id.clone(),
                id!("Ok"),
                ScalarValue::Unit.into()
            ).unwrap().into()),
            ret_type: TTypeId::INT32,
            branches: btreemap! {
                id!("Ok") => Branch {
                    ident: id!("_"),
                    expression: Expression::Value(ScalarValue::Int32(10).into()),
                },
                id!("Err") => Branch {
                    ident: id!("_"),
                    expression: Expression::Value(ScalarValue::Int32(8).into()),
                },
            },
        })));

        expr.eval_types(&registry, &ScopeTypes::EMPTY).unwrap();
        assert_eq!(Value::Scalar(ScalarValue::Int32(10).into()), expr.eval(&registry, &ScopeValues::EMPTY).unwrap());

        let expr = Expression::ControlFlow(Box::new(
            ControlFlow::Match(MatchControlFlow {
                param: Expression::Value(EnumValue::new(&registry, result_ttype_id, id!("Ok"), ScalarValue::Unit.into()).unwrap().into()),
                ret_type: TTypeId::INT32,
                branches: btreemap! {
                    id!("Ok") => Branch {
                        ident: id!("_"),
                        expression: Expression::Value(ScalarValue::Int32(10).into()),
                    },
                },
            })
        ));

        assert_eq!(TypeError::MissingTag(id!("Err")), expr.eval_types(&registry, &ScopeTypes::EMPTY).unwrap_err());

        let expr = Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
            param: Expression::Value(
                EnumValue::new(
                    &registry,
                    id_path!("Result").into(),
                    id!("Err"),
                    ScalarValue::String("my_error".into()).into(),
                ).unwrap().into()
            ),
            ret_type: TTypeId::STRING,
            branches: btreemap! {
                id!("Ok") => Branch {
                    ident: id!("_"),
                    expression: Expression::Value(ScalarValue::String("Ok".into()).into()),
                },
                id!("Err") => Branch {
                    ident: id!("error"),
                    expression: Expression::NestedIdent(id!("error").into()),
                },
            },
        })));

        assert_eq!(TTypeId::Scalar(ScalarType::String), expr.eval_types(&registry, &ScopeTypes::EMPTY).unwrap());
        assert_eq!(Value::Scalar(ScalarValue::String("my_error".into()).into()), expr.eval(&registry, &ScopeValues::EMPTY).unwrap());
    }

    #[test]
    fn functions() {
        let registry = Registry::new();

        let expr_panic = Expression::FunctionInvocation(FunctionInvocation {
            function: id_path!("unwrap"),
            args: [
                Expression::Value(
                    EnumValue::new(
                        &registry,
                        id_path!("Result").into(),
                        id!("Err"),
                        ScalarValue::String("my_error".into()).into(),
                    ).unwrap().into()
                ),
            ].into(),
        });

        expr_panic.eval_types(&registry, &Default::default()).unwrap();
        let panic_err = expr_panic.eval(&registry, &Default::default()).unwrap_err();

        assert_eq!(EvalError::Panic("my_error".into()), panic_err);

        let expr_pass = Expression::FunctionInvocation(FunctionInvocation {
            function: id_path!("unwrap"),
            args: [
                Expression::Value(
                    EnumValue::new(
                        &registry,
                        id_path!("Result").into(),
                        id!("Ok"),
                        ScalarValue::Unit.into(),
                    ).unwrap().into()
                ),
            ].into(),
        });

        expr_panic.eval_types(&registry, &Default::default()).unwrap();
        let value = expr_pass.eval(&registry, &Default::default()).unwrap();

        assert_eq!(Value::UNIT, value);
    }
}
