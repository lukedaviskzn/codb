use std::{borrow::Cow, collections::{BTreeMap, HashSet}, fmt::{Debug, Display}};

use codb_core::{Ident, IdentPath, NestedIdent};

use crate::{registry::{Registry, TTypeId}, scope::{ScopeTypes, ScopeValues}, typesystem::{function::{FunctionEntry, InterpreterFunctionAction}, ttype::{CompositeType, EnumType, ScalarType, StructType, TType}, value::{CompositeValue, ScalarValueInner, StructValue, Value}, TypeError}};

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
            Expression::NestedIdent(nested_ident) => {
                let ttype_id = scopes.get_nested(registry.types(), nested_ident)?;
                Ok(ttype_id)
            },
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

                registry.types().expect_type(&TTypeId::BOOL, &left_type_id)?;
                registry.types().expect_type(&TTypeId::BOOL, &right_type_id)?;
                
                Ok(TTypeId::BOOL)
            },
            LogicalOp::Lt(left, right) |
            LogicalOp::Gt(left, right) |
            LogicalOp::Lte(left, right) |
            LogicalOp::Gte(left, right) => {
                let left_type_id = left.eval_types(registry, scopes)?;
                let right_type_id = right.eval_types(registry, scopes)?;

                registry.types().expect_type(&TTypeId::BOOL, &left_type_id)?;
                registry.types().expect_type(&TTypeId::BOOL, &right_type_id)?;

                Ok(TTypeId::BOOL)
            },
            LogicalOp::Eq(left, right) => {
                let left_type_id = left.eval_types(registry, scopes)?;
                let right_type_id = right.eval_types(registry, scopes)?;

                registry.types().expect_type(&left_type_id, &right_type_id)?;

                Ok(TTypeId::BOOL)
            },
            LogicalOp::Not(expr) => {
                let expr_type_id = expr.eval_types(registry, scopes)?;

                registry.types().expect_type(&TTypeId::BOOL, &expr_type_id)?;
                
                Ok(TTypeId::BOOL)
            },
        }
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            LogicalOp::And(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;

                registry.types().expect_type(&TTypeId::BOOL, &left.ttype_id())?;
                registry.types().expect_type(&TTypeId::BOOL, &right.ttype_id())?;
                
                Ok(ScalarValueInner::Bool(left == Value::TRUE && right == Value::TRUE).into())
            },
            LogicalOp::Or(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                registry.types().expect_type(&TTypeId::BOOL, &left.ttype_id())?;
                registry.types().expect_type(&TTypeId::BOOL, &right.ttype_id())?;
                
                Ok(ScalarValueInner::Bool(left == Value::TRUE || right == Value::TRUE).into())
            },
            LogicalOp::Not(expr) => {
                let value = expr.eval(registry, scopes)?;
                
                registry.types().expect_type(&TTypeId::BOOL, &value.ttype_id())?;
                
                Ok(ScalarValueInner::Bool(value == Value::FALSE).into())
            },
            LogicalOp::Eq(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                registry.types().expect_type(&left.ttype_id(), &right.ttype_id())?;
                
                Ok(ScalarValueInner::Bool(left == right).into())
            },
            LogicalOp::Lt(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                registry.types().expect_type(&TTypeId::INT32, &left.ttype_id())?;
                registry.types().expect_type(&TTypeId::INT32, &right.ttype_id())?;
                
                Ok(ScalarValueInner::Bool(left < right).into())
            },
            LogicalOp::Gt(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                registry.types().expect_type(&TTypeId::INT32, &left.ttype_id())?;
                registry.types().expect_type(&TTypeId::INT32, &right.ttype_id())?;
                
                Ok(ScalarValueInner::Bool(left > right).into())
            },
            LogicalOp::Lte(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                registry.types().expect_type(&TTypeId::INT32, &left.ttype_id())?;
                registry.types().expect_type(&TTypeId::INT32, &right.ttype_id())?;
                
                Ok(ScalarValueInner::Bool(left <= right).into())
            },
            LogicalOp::Gte(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                registry.types().expect_type(&TTypeId::INT32, &left.ttype_id())?;
                registry.types().expect_type(&TTypeId::INT32, &right.ttype_id())?;
                
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
    pub fn eval_types(&self, registry: &Registry, scopes: &ScopeTypes) -> Result<TTypeId, TypeError> {
        match self {
            ArithmeticOp::Add(left, right) |
            ArithmeticOp::Sub(left, right) |
            ArithmeticOp::Mul(left, right) |
            ArithmeticOp::Div(left, right) => {
                let left_type_id = left.eval_types(registry, scopes)?;
                let right_type_id = right.eval_types(registry, scopes)?;

                registry.types().expect_type(&TTypeId::INT32, &left_type_id)?;
                registry.types().expect_type(&TTypeId::INT32, &right_type_id)?;

                Ok(TTypeId::INT32)
            },
        }
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        match self {
            ArithmeticOp::Add(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                registry.types().expect_type(&TTypeId::INT32, &left.ttype_id())?;
                registry.types().expect_type(&TTypeId::INT32, &right.ttype_id())?;

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValueInner::Int32(left) = left.inner() else { unreachable!() };
                let ScalarValueInner::Int32(right) = right.inner() else { unreachable!() };
                
                Ok(ScalarValueInner::Int32(left + right).into())
            },
            ArithmeticOp::Sub(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                registry.types().expect_type(&TTypeId::INT32, &left.ttype_id())?;
                registry.types().expect_type(&TTypeId::INT32, &right.ttype_id())?;

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValueInner::Int32(left) = left.inner() else { unreachable!() };
                let ScalarValueInner::Int32(right) = right.inner() else { unreachable!() };
                
                Ok(ScalarValueInner::Int32(left - right).into())
            },
            ArithmeticOp::Mul(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                registry.types().expect_type(&TTypeId::INT32, &left.ttype_id())?;
                registry.types().expect_type(&TTypeId::INT32, &right.ttype_id())?;

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValueInner::Int32(left) = left.inner() else { unreachable!() };
                let ScalarValueInner::Int32(right) = right.inner() else { unreachable!() };
                
                Ok(ScalarValueInner::Int32(left * right).into())
            },
            ArithmeticOp::Div(left, right) => {
                let left = left.eval(registry, scopes)?;
                let right = right.eval(registry, scopes)?;
                
                registry.types().expect_type(&TTypeId::INT32, &left.ttype_id())?;
                registry.types().expect_type(&TTypeId::INT32, &right.ttype_id())?;

                let Value::Scalar(left) = left else { unreachable!() };
                let Value::Scalar(right) = right else { unreachable!() };

                let ScalarValueInner::Int32(left) = left.inner() else { unreachable!() };
                let ScalarValueInner::Int32(right) = right.inner() else { unreachable!() };
                
                Ok(ScalarValueInner::Int32(left / right).into())
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

        registry.types().expect_type(&TTypeId::BOOL, &cond_type_id)?;
        
        let then_type_id = self.then.eval_types(registry, scopes)?;
        let otherwise_type_id = self.then.eval_types(registry, scopes)?;

        registry.types().expect_type(&self.ret_type, &then_type_id)?;
        registry.types().expect_type(&self.ret_type, &otherwise_type_id)?;

        Ok(then_type_id)
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let cond_value = self.condition.eval(registry, scopes)?;
                
        registry.types().expect_type(&TTypeId::BOOL, &cond_value.ttype_id())?;

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
        
        let param_type = registry.types().get_by_id(&param_type_id)?;

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

            registry.types().expect_type(&self.ret_type, &branch_type_id)?;
        }
        
        Ok(self.ret_type.clone())
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let scope_types = scopes.types(registry.types())?;
        
        let param_type = self.param.eval_types(registry, &scope_types)?;
        let param_type = registry.types().get_by_id(&param_type).map_err(|err| TypeError::from(err))?;
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

        let scope_type = TTypeId::Anonymous(Box::new(StructType::new(btreemap! {
            branch.ident.clone() => param_type.tags()[param_value.tag()].clone(),
        }).into()));
        
        let new_scope = StructValue::new(registry.types(), scope_type, btreemap! {
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
        let function = registry.functions().get(&self.function)?;

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

            registry.types().expect_type(&expression_type_id, arg.ttype_id())?;
            
            if !arg_names.insert(arg.name()) {
                return Err(TypeError::FunctionDuplicateArg {
                    arg: arg.name().clone(),
                })
            }
        }

        Ok(ret_type.clone())
    }

    pub fn eval(&self, registry: &Registry, scopes: &ScopeValues) -> Result<Value, EvalError> {
        let function = registry.functions().get(&self.function).map_err(|err| TypeError::from(err))?;
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
                            Value::Scalar(scalar_value) => if let ScalarValueInner::String(string) = scalar_value.inner() {
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
    use crate::typesystem::value::{EnumLiteral, EnumValue, LiteralType, ScalarLiteralInner};

    use super::*;

    #[test]
    fn match_expr() {
        let registry = Registry::new();

        let result_ttype = registry.types().get_id_by_name("Result").unwrap();

        let expr = Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
            param: Expression::Value(EnumValue::new(
                registry.types(), result_ttype.clone(),
                id!("Ok"),
                ScalarValueInner::Unit.into()
            ).unwrap().into()),
            ret_type: TTypeId::INT32,
            branches: btreemap! {
                id!("Ok") => Branch {
                    ident: id!("_"),
                    expression: Expression::Value(ScalarValueInner::Int32(10).into()),
                },
                id!("Err") => Branch {
                    ident: id!("_"),
                    expression: Expression::Value(ScalarValueInner::Int32(8).into()),
                },
            },
        })));

        expr.eval_types(&registry, &ScopeTypes::EMPTY).unwrap();
        assert_eq!(Value::Scalar(ScalarValueInner::Int32(10).into()), expr.eval(&registry, &ScopeValues::EMPTY).unwrap());

        let expr = Expression::ControlFlow(Box::new(
            ControlFlow::Match(MatchControlFlow {
                param: Expression::Value(EnumValue::new(registry.types(), result_ttype, id!("Ok"), ScalarValueInner::Unit.into()).unwrap().into()),
                ret_type: TTypeId::INT32,
                branches: btreemap! {
                    id!("Ok") => Branch {
                        ident: id!("_"),
                        expression: Expression::Value(ScalarValueInner::Int32(10).into()),
                    },
                },
            })
        ));

        assert_eq!(TypeError::MissingTag(id!("Err")), expr.eval_types(&registry, &ScopeTypes::EMPTY).unwrap_err());

        let expr = Expression::ControlFlow(Box::new(ControlFlow::Match(MatchControlFlow {
            param: Expression::Value(
                EnumValue::from_literal(
                    registry.types(),
                    EnumLiteral {
                        ttype: LiteralType::Name(id!("Result").into()),
                        tag: id!("Err"),
                        value: Box::new(ScalarLiteralInner::String("my_error".into()).into()),
                    }
                ).unwrap().into()
            ),
            ret_type: TTypeId::STRING,
            branches: btreemap! {
                id!("Ok") => Branch {
                    ident: id!("_"),
                    expression: Expression::Value(ScalarValueInner::String("Ok".into()).into()),
                },
                id!("Err") => Branch {
                    ident: id!("error"),
                    expression: Expression::NestedIdent(id!("error").into()),
                },
            },
        })));

        assert_eq!(TTypeId::Scalar(ScalarType::String), expr.eval_types(&registry, &ScopeTypes::EMPTY).unwrap());
        assert_eq!(Value::Scalar(ScalarValueInner::String("my_error".into()).into()), expr.eval(&registry, &ScopeValues::EMPTY).unwrap());
    }

    #[test]
    fn functions() {
        let registry = Registry::new();

        let expr_panic = Expression::FunctionInvocation(FunctionInvocation {
            function: id_path!("core::unwrap"),
            args: [
                Expression::Value(
                    EnumValue::from_literal(
                        registry.types(),
                        EnumLiteral {
                            ttype: LiteralType::Name(id!("Result").into()),
                            tag: id!("Err"),
                            value: Box::new(ScalarLiteralInner::String("my_error".into()).into()),
                        }
                    ).unwrap().into()
                ),
            ].into(),
        });

        expr_panic.eval_types(&registry, &Default::default()).unwrap();
        let panic_err = expr_panic.eval(&registry, &Default::default()).unwrap_err();

        assert_eq!(EvalError::Panic("".into()), panic_err);

        let expr_pass = Expression::FunctionInvocation(FunctionInvocation {
            function: id_path!("core::unwrap"),
            args: [
                Expression::Value(
                    EnumValue::from_literal(
                        registry.types(),
                        EnumLiteral {
                            ttype: LiteralType::Name(id!("Result").into()),
                            tag: id!("Ok"),
                            value: Box::new(ScalarLiteralInner::Unit.into()),
                        }
                    ).unwrap().into()
                ),
            ].into(),
        });

        expr_panic.eval_types(&registry, &Default::default()).unwrap();
        let value = expr_pass.eval(&registry, &Default::default()).unwrap();

        assert_eq!(Value::UNIT, value);
    }
}
