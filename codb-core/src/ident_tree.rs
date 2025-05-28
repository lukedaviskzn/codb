use std::fmt::{Debug, Display};

use crate::{Ident, IdentForest};

#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct IdentTree {
    pub(super) ident: Ident,
    pub(super) children: IdentForest,
}

impl IdentTree {
    pub fn new(ident: Ident, children: IdentForest) -> IdentTree {
        IdentTree {
            ident,
            children,
        }
    }

    pub fn ident(&self) -> &Ident {
        &self.ident
    }

    pub fn children(&self) -> &IdentForest {
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
