use std::{borrow::Borrow, fmt::{Debug, Display}, ops::Deref, str::FromStr};

use crate::{ParseIdentError};

#[binrw]
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
pub struct Ident {
    #[bw(calc = self.inner.len() as u64)]
    len: u64,
    #[br(count = len, try_map = |bytes: Vec<u8>| String::from_utf8(bytes))]
    #[bw(map = |string| string.as_bytes())]
    inner: String,
}

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl Into<String> for Ident {
    fn into(self) -> String {
        self.inner
    }
}

impl FromStr for Ident {
    type Err = ParseIdentError;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        Ident::try_from(string.to_owned())
    }
}

impl TryFrom<String> for Ident {
    type Error = ParseIdentError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        let Some(first_char) = value.chars().next() else {
            return Err(ParseIdentError::Empty);
        };
        
        if !first_char.is_alphabetic() && first_char != '_' {
            return Err(ParseIdentError::InvalidFirstChar(first_char));
        }

        if let Some(invalid_char) = value.chars().find(|c| !c.is_alphanumeric() && *c != '_') {
            Err(ParseIdentError::InvalidChar(invalid_char))
        } else {
            Ok(Self {
                inner: value,
            })
        }
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.inner
    }
}

impl Borrow<str> for Ident {
    fn borrow(&self) -> &str {
        &self.inner
    }
}

impl Deref for Ident {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, other: &str) -> bool {
        self.inner == other
    }
}

impl PartialEq<String> for Ident {
    fn eq(&self, other: &String) -> bool {
        &self.inner == other
    }
}
