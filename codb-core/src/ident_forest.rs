use std::{borrow::Borrow, fmt::Debug, ops::{Deref, Index}};

use crate::{Ident, IdentTree, NestedIdent};

#[binrw]
#[derive(Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct IdentForest {
    #[bw(calc = trees.len() as u64)]
    len: u64,
    #[br(count = len)]
    trees: Vec<IdentTree>,
}

impl IdentForest {
    pub fn empty() -> IdentForest {
        IdentForest {
            trees: Vec::new(),
        }
    }

    pub fn new(trees: impl Into<Vec<IdentTree>>) -> IdentForest {
        IdentForest {
            trees: trees.into(),
        }
    }

    pub fn from_nested_idents(nested_idents: impl Into<Box<[NestedIdent]>>) -> IdentForest {
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

        IdentForest {
            trees: trees.into(),
        }
    }

    pub fn trees(&self) -> &[IdentTree] {
        &self.trees
    }
}

impl Debug for IdentForest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut t = f.debug_tuple("");
        for c in &self.trees {
            t.field(c);
        }
        t.finish()
    }
}

impl From<IdentTree> for IdentForest {
    fn from(value: IdentTree) -> Self {
        Self {
            trees: vec![value],
        }
    }
}

impl Into<Box<[IdentTree]>> for IdentForest {
    fn into(self) -> Box<[IdentTree]> {
        self.trees.into()
    }
}

impl From<Box<[IdentTree]>> for IdentForest {
    fn from(trees: Box<[IdentTree]>) -> Self {
        Self {
            trees: trees.into(),
        }
    }
}

impl Into<Vec<IdentTree>> for IdentForest {
    fn into(self) -> Vec<IdentTree> {
        self.trees
    }
}

impl From<Vec<IdentTree>> for IdentForest {
    fn from(value: Vec<IdentTree>) -> Self {
        Self {
            trees: value,
        }
    }
}

impl AsRef<[IdentTree]> for IdentForest {
    fn as_ref(&self) -> &[IdentTree] {
        &self.trees
    }
}

impl Borrow<[IdentTree]> for IdentForest {
    fn borrow(&self) -> &[IdentTree] {
        &self.trees
    }
}

impl Deref for IdentForest {
    type Target = [IdentTree];

    fn deref(&self) -> &Self::Target {
        &self.trees
    }
}

impl Index<usize> for IdentForest {
    type Output = IdentTree;

    fn index(&self, index: usize) -> &Self::Output {
        self.trees.index(index)
    }
}

impl IntoIterator for IdentForest {
    type Item = IdentTree;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.trees.into_iter()
    }
}

impl<'a> IntoIterator for &'a IdentForest {
    type Item = &'a IdentTree;

    type IntoIter = std::slice::Iter<'a, IdentTree>;

    fn into_iter(self) -> Self::IntoIter {
        self.trees.iter()
    }
}

impl<'a> IntoIterator for &'a mut IdentForest {
    type Item = &'a mut IdentTree;

    type IntoIter = std::slice::IterMut<'a, IdentTree>;

    fn into_iter(self) -> Self::IntoIter {
        self.trees.iter_mut()
    }
}
