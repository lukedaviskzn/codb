use std::fmt::{Debug, Display};

use crate::{Ident, NestedIdent};

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct IdentTree {
    ident: Ident,
    children: Box<[IdentTree]>,
}

impl IdentTree {
    pub fn from_nested_idents(nested_idents: impl Into<Box<[NestedIdent]>>) -> Box<[IdentTree]> {
        let mut roots: Vec<&Ident> = vec![];

        let nested_idents: Box<[NestedIdent]> = nested_idents.into();

        for nested_ident in &nested_idents {
            let ident = &nested_ident[0];

            if !roots.contains(&ident) {
                roots.push(ident);
            }
        }

        let mut trees = Vec::new();

        for root in roots {
            let mut child_nested_idents = Vec::new();

            for nested_ident in nested_idents.iter().filter(|ni| &ni[0] == root) {
                let rest = nested_ident.to_vec().split_off(1);
                
                if let Ok(child_nested_ident) = NestedIdent::try_from(rest) {
                    child_nested_idents.push(child_nested_ident);
                }
            }

            trees.push(IdentTree {
                ident: root.clone(),
                children: Self::from_nested_idents(child_nested_idents),
            });
        }

        trees.into()
    }

    pub fn ident(&self) -> &Ident {
        &self.ident
    }

    pub fn children(&self) -> &[IdentTree] {
        &self.children
    }
}

impl Debug for IdentTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut t = if self.children.is_empty() {
            f.debug_tuple(self.ident.as_ref())
        } else {
            f.debug_tuple(&format!("{}.", self.ident.as_ref()))
        };
        for c in &self.children {
            t.field(c);
        }
        t.finish()
    }
}

impl Display for IdentTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut t = f.debug_tuple(self.ident.as_ref());
        for c in &self.children {
            t.field(c);
        }
        t.finish()
    }
}
