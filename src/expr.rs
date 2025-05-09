use std::fmt::{Debug, Display};

use crate::typesystem::{registry::TypeRegistry, ttype::{CompositeType, ScalarType, StructType, TType}, value::{CompositeValue, ScalarValue, StructValue, Value}, TypeError};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum Expression {
    Ident(Box<[String]>),
    Value(Value),
    Op(Box<Op>),
    ControlFlow(Box<ControlFlow>),
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(idents) => Display::fmt(&idents.join("."), f),
            Self::Value(value) => Debug::fmt(value, f),
            Self::Op(op) => Debug::fmt(op, f),
            Self::ControlFlow(cf) => Debug::fmt(cf, f)
        }
    }
}

impl Expression {
    pub fn eval_types(&self, registry: &TypeRegistry, variables: &StructType) -> Result<TType, TypeError> {
        match self {
            Expression::Ident(idents) => {
                let mut ttype = TType::Composite(CompositeType::Struct(variables.clone()));
                for ident in idents {
                    ttype = ttype.dot(registry, ident)?;
                }
                Ok(ttype)
            },
            Expression::Value(value) => Ok(registry.get_by_id(&value.ttype_id())?),
            Expression::Op(op) => op.eval_types(registry, variables),
            Expression::ControlFlow(control_flow) => control_flow.eval_types(registry, variables),
        }
    }

    pub fn eval(&self, registry: &TypeRegistry, variables: &StructValue) -> Result<Value, ExprError> {
        match self {
            Expression::Ident(idents) => {
                if idents.is_empty() {
                    return Err(ExprError::InvalidIdents(idents.clone()));
                }

                let mut value_selected = Value::Composite(CompositeValue::Struct(variables.clone()));

                for ident in idents {
                    value_selected = value_selected.dot(ident)?;
                }

                Ok(value_selected)
            },
            Expression::Value(value) => Ok(value.clone()),
            Expression::Op(op) => op.eval(registry, variables),
            Expression::ControlFlow(cf) => cf.eval(registry, variables),
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
    pub fn eval_types(&self, registry: &TypeRegistry, variables: &StructType) -> Result<TType, TypeError> {
        match self {
            Op::Logical(op) => op.eval_types(registry, variables),
            Op::Arithmetic(op) => op.eval_types(registry, variables),
        }
    }
    
    pub fn eval(&self, registry: &TypeRegistry, variables: &StructValue) -> Result<Value, ExprError> {
        match self {
            Op::Logical(op) => op.eval(registry, variables),
            Op::Arithmetic(op) => op.eval(registry, variables),
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
    pub fn eval_types(&self, registry: &TypeRegistry, variables: &StructType) -> Result<TType, TypeError> {
        match self {
            LogicalOp::And(left, right) |
            LogicalOp::Or(left, right) => {
                let left_type = left.eval_types(registry, variables)?;
                if TType::BOOL != left_type {
                    return Err(TypeError::TypeInvalid { expected: TType::BOOL, got: left_type });
                }
                
                let right_type = right.eval_types(registry, variables)?;
                if TType::BOOL != right_type {
                    return Err(TypeError::TypeInvalid { expected: TType::BOOL, got: right_type });
                }

                Ok(TType::BOOL)
            },
            LogicalOp::Lt(left, right) |
            LogicalOp::Gt(left, right) |
            LogicalOp::Lte(left, right) |
            LogicalOp::Gte(left, right) => {
                let left_type = left.eval_types(registry, variables)?;
                if TType::INT32 != left_type {
                    return Err(TypeError::TypeInvalid { expected: TType::INT32, got: left_type });
                }
                
                let right_type = right.eval_types(registry, variables)?;
                if TType::INT32 != right_type {
                    return Err(TypeError::TypeInvalid { expected: TType::INT32, got: right_type });
                }

                Ok(TType::BOOL)
            },
            LogicalOp::Eq(_left, _right) => Ok(TType::BOOL),
            LogicalOp::Not(expr) => {
                let expr_ttype = expr.eval_types(registry, variables)?;
                if TType::BOOL != expr_ttype {
                    return Err(TypeError::TypeInvalid { expected: TType::BOOL, got: expr_ttype });
                }
                
                Ok(TType::BOOL)
            },
        }
    }

    pub fn eval(&self, registry: &TypeRegistry, variables: &StructValue) -> Result<Value, ExprError> {
        match self {
            LogicalOp::And(left, right) => {
                let left = left.eval(registry, variables)?;
                let right = right.eval(registry, variables)?;
                
                TType::Scalar(ScalarType::Bool).check(registry, &left)?;
                TType::Scalar(ScalarType::Bool).check(registry, &right)?;
                
                Ok(Value::Scalar(ScalarValue::Bool(left == Value::TRUE && right == Value::TRUE)))
            },
            LogicalOp::Or(left, right) => {
                let left = left.eval(registry, variables)?;
                let right = right.eval(registry, variables)?;
                
                TType::Scalar(ScalarType::Bool).check(registry, &left)?;
                TType::Scalar(ScalarType::Bool).check(registry, &right)?;
                
                Ok(Value::Scalar(ScalarValue::Bool(left == Value::TRUE || right == Value::TRUE)))
            },
            LogicalOp::Not(expr) => {
                let value = expr.eval(registry, variables)?;
                
                TType::Scalar(ScalarType::Bool).check(registry, &value)?;
                
                Ok(Value::Scalar(ScalarValue::Bool(value == Value::FALSE)))
            },
            LogicalOp::Eq(left, right) => {
                let left = left.eval(registry, variables)?;
                let right = right.eval(registry, variables)?;
                
                Ok(Value::Scalar(ScalarValue::Bool(left == right)))
            },
            LogicalOp::Lt(left, right) => {
                let left = left.eval(registry, variables)?;
                let right = right.eval(registry, variables)?;

                TType::Scalar(ScalarType::Int32).check(registry, &left)?;
                TType::Scalar(ScalarType::Int32).check(registry, &right)?;
                
                Ok(Value::Scalar(ScalarValue::Bool(left < right)))
            },
            LogicalOp::Gt(left, right) => {
                let left = left.eval(registry, variables)?;
                let right = right.eval(registry, variables)?;

                TType::Scalar(ScalarType::Int32).check(registry, &left)?;
                TType::Scalar(ScalarType::Int32).check(registry, &right)?;
                
                Ok(Value::Scalar(ScalarValue::Bool(left > right)))
            },
            LogicalOp::Lte(left, right) => {
                let left = left.eval(registry, variables)?;
                let right = right.eval(registry, variables)?;

                TType::Scalar(ScalarType::Int32).check(registry, &left)?;
                TType::Scalar(ScalarType::Int32).check(registry, &right)?;
                
                Ok(Value::Scalar(ScalarValue::Bool(left <= right)))
            },
            LogicalOp::Gte(left, right) => {
                let left = left.eval(registry, variables)?;
                let right = right.eval(registry, variables)?;

                TType::Scalar(ScalarType::Int32).check(registry, &left)?;
                TType::Scalar(ScalarType::Int32).check(registry, &right)?;
                
                Ok(Value::Scalar(ScalarValue::Bool(left >= right)))
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
    pub fn eval_types(&self, registry: &TypeRegistry, variables: &StructType) -> Result<TType, TypeError> {
        match self {
            ArithmeticOp::Add(left, right) |
            ArithmeticOp::Sub(left, right) |
            ArithmeticOp::Mul(left, right) |
            ArithmeticOp::Div(left, right) => {
                let left_type = left.eval_types(registry, variables)?;
                let right_type = right.eval_types(registry, variables)?;

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

    pub fn eval(&self, registry: &TypeRegistry, variables: &StructValue) -> Result<Value, ExprError> {
        match self {
            ArithmeticOp::Add(left, right) => {
                let left = left.eval(registry, variables)?;
                let right = right.eval(registry, variables)?;

                TType::Scalar(ScalarType::Int32).check(registry, &left)?;
                TType::Scalar(ScalarType::Int32).check(registry, &right)?;

                let Value::Scalar(ScalarValue::Int32(left)) = left else { unreachable!() };
                let Value::Scalar(ScalarValue::Int32(right)) = right else { unreachable!() };
                
                Ok(Value::Scalar(ScalarValue::Int32(left + right)))
            },
            ArithmeticOp::Sub(left, right) => {
                let left = left.eval(registry, variables)?;
                let right = right.eval(registry, variables)?;

                TType::Scalar(ScalarType::Int32).check(registry, &left)?;
                TType::Scalar(ScalarType::Int32).check(registry, &right)?;

                let Value::Scalar(ScalarValue::Int32(left)) = left else { unreachable!() };
                let Value::Scalar(ScalarValue::Int32(right)) = right else { unreachable!() };
                
                Ok(Value::Scalar(ScalarValue::Int32(left - right)))
            },
            ArithmeticOp::Mul(left, right) => {
                let left = left.eval(registry, variables)?;
                let right = right.eval(registry, variables)?;

                TType::Scalar(ScalarType::Int32).check(registry, &left)?;
                TType::Scalar(ScalarType::Int32).check(registry, &right)?;

                let Value::Scalar(ScalarValue::Int32(left)) = left else { unreachable!() };
                let Value::Scalar(ScalarValue::Int32(right)) = right else { unreachable!() };
                
                Ok(Value::Scalar(ScalarValue::Int32(left * right)))
            },
            ArithmeticOp::Div(left, right) => {
                let left = left.eval(registry, variables)?;
                let right = right.eval(registry, variables)?;

                TType::Scalar(ScalarType::Int32).check(registry, &left)?;
                TType::Scalar(ScalarType::Int32).check(registry, &right)?;

                let Value::Scalar(ScalarValue::Int32(left)) = left else { unreachable!() };
                let Value::Scalar(ScalarValue::Int32(right)) = right else { unreachable!() };
                
                Ok(Value::Scalar(ScalarValue::Int32(left / right)))
            },
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ExprError {
    #[error("variable not found {0:?}")]
    VariableNotFound(String),
    #[error("invalid ident {:?}", .0.join("."))]
    InvalidIdents(Box<[String]>),
    #[error("{0}")]
    TTypeError(#[from] TypeError),
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub enum ControlFlow {
    If(IfControlFlow),
}

impl Debug for ControlFlow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::If(cf) => Debug::fmt(cf, f),
        }
    }
}

impl ControlFlow {
    pub fn eval_types(&self, registry: &TypeRegistry, variables: &StructType) -> Result<TType, TypeError> {
        match self {
            ControlFlow::If(cf) => cf.eval_types(registry, variables),
        }
    }

    pub fn eval(&self, registry: &TypeRegistry, variables: &StructValue) -> Result<Value, ExprError> {
        match self {
            ControlFlow::If(cf) => cf.eval(registry, variables),
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
    pub fn eval_types(&self, registry: &TypeRegistry, variables: &StructType) -> Result<TType, TypeError> {
        let cond_type = self.condition.eval_types(registry, variables)?;

        if TType::BOOL != cond_type {
            return Err(TypeError::TypeInvalid { expected: TType::BOOL, got: cond_type });
        }
        
        let then_type = self.then.eval_types(registry, variables)?;
        let otherwise_type = self.then.eval_types(registry, variables)?;

        if then_type != otherwise_type {
            return Err(TypeError::TypeInvalid { expected: then_type, got: otherwise_type });
        }

        Ok(then_type)
    }

    pub fn eval(&self, registry: &TypeRegistry, variables: &StructValue) -> Result<Value, ExprError> {
        let cond_value = self.condition.eval(registry, variables)?;

        TType::BOOL.check(registry, &cond_value)?;

        if cond_value == Value::TRUE {
            self.then.eval(registry, variables)
        } else {
            self.otherwise.eval(registry, variables)
        }
    }
}
