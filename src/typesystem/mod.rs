use std::fmt::{Debug, Display};

use itertools::Itertools;
use ttype::TType;
use value::Value;

use crate::expr::ExprError;

pub mod registry;
pub mod ttype;
pub mod value;

#[derive(Debug, thiserror::Error)]
pub enum TypeError {
    #[error("unknown field '{0}'")]
    UnknownField(String),
    #[error("missing field '{0}'")]
    MissingField(String),
    #[error("unknown tag '{0}'")]
    UnknownTag(String),
    #[error("missing tag '{0}'")]
    MissingTag(String),
    #[error("cannot access enum tag '{0}' using '.' operator, use match")]
    DotTag(String),
    #[error("cannot access field '{0}' on scalar")]
    ScalarField(String),
    #[error("value type invalid, expected {expected:?} got {got:?}")]
    ValueTypeInvalid {
        expected: TType,
        got: Value,
    },
    #[error("type invalid, expected {expected:?} got {got:?}")]
    TypeInvalid {
        expected: TType,
        got: TType,
    },
    #[error("{0}")]
    DuplicateField(#[from] DuplicateField),
    #[error("refinement failed: {0}")]
    RefinementFailed(#[from] RefinementFailedError),
}

#[derive(Debug, thiserror::Error)]
pub enum RefinementFailedError {
    #[error("{0}")]
    Expr(#[from] Box<ExprError>),
    #[error("{0:?}")]
    Refinement(String),
}

#[derive(Debug, thiserror::Error)]
#[error("duplicate field")]
pub struct DuplicateField;

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct NestedIdents {
    pub ident: String,
    pub children: Box<[NestedIdents]>,
}

impl Debug for NestedIdents {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut t = if self.children.is_empty() {
            f.debug_tuple(&self.ident)
        } else {
            f.debug_tuple(&format!("{}.", self.ident))
        };
        for c in &self.children {
            t.field(c);
        }
        t.finish()
    }
}

impl NestedIdents {
    pub fn from_string(ident: &str) -> Option<NestedIdents> {
        let mut nested_ident = None;
        
        for ident in ident.split('.').rev() {
            if let Some(old_nested_ident) = nested_ident {
                nested_ident = Some(NestedIdents {
                    ident: ident.into(),
                    children: vec![old_nested_ident].into(),
                });
            } else {
                nested_ident = Some(NestedIdents {
                    ident: ident.into(),
                    children: Box::new([])
                });
            }
        }
        
        nested_ident
    }

    pub fn from_strings<T: Into<String>>(idents: Vec<T>) -> Box<[NestedIdents]> {
        if idents.is_empty() {
            return Box::new([]);
        }
        let mut idents = {
            let mut new = Vec::with_capacity(idents.len());
            for ident in idents {
                new.push(Into::<String>::into(ident));
            }
            new
        };
        
        let roots = idents.iter().map(|ident| ident.splitn(2, '.').next().expect("unreachable").to_owned()).unique().collect_vec();

        let mut nested_idents = Vec::new();

        for root in roots {
            let root_dot = format!("{root}.");

            idents.retain(|i| i != &root);

            let mut old_idents = Vec::with_capacity(idents.len());
            let mut children = Vec::new();

            for mut ident in idents {
                if ident.starts_with(&root_dot) {
                    children.push(ident.split_off(root_dot.len()));
                } else {
                    old_idents.push(ident);
                }
            }

            idents = old_idents;

            nested_idents.push(NestedIdents {
                ident: root.into(),
                children: NestedIdents::from_strings(children),
            });
        }

        nested_idents.into()
    }
}

impl Display for NestedIdents {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut t = f.debug_tuple(&self.ident);
        for c in &self.children {
            t.field(c);
        }
        t.finish()
    }
}
