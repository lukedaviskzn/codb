use std::{borrow::Borrow, fmt::{Debug, Display}, ops::{Deref, Index}, str::FromStr};

use crate::{NestedIdent, ParseIdentError};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct Ident(String);

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Into<String> for Ident {
    fn into(self) -> String {
        self.0
    }
}

impl FromStr for Ident {
    type Err = ParseIdentError;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        Ident::try_from(string.to_owned()).map_err(|_| ParseIdentError)
    }
}

impl TryFrom<String> for Ident {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        if let Some(char) = value.chars().next() {
            if (char.is_alphabetic() || char == '_') && value.chars().all(|c| c.is_alphanumeric() || c == '_') {
                Ok(Ident(value))
            } else {
                Err(value)
            }
        } else {
            Err(value)
        }
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for Ident {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl Deref for Ident {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl PartialEq<String> for Ident {
    fn eq(&self, other: &String) -> bool {
        &self.0 == other
    }
}
