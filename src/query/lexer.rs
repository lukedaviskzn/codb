// use std::{iter::{Enumerate, Peekable}, num::ParseIntError};

// use codb_core::Ident;

// #[derive(Debug, thiserror::Error)]
// #[error("[{index}]: {kind}")]
// pub struct LexError {
//     index: usize,
//     kind: LexErrorKind,
// }

// #[derive(Debug, thiserror::Error)]
// pub enum LexErrorKind {
//     #[error("unexpected end of input")]
//     UnexpectedEnd,
//     #[error("unexpected char {0:?}")]
//     UnexpectedChar(char),
//     #[error("{0:#x} is not a valid unicode character")]
//     NotUnicode(u32),
//     #[error("{0}")]
//     ParseIntError(#[from] ParseIntError),
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct Token {
//     index: usize,
//     kind: TokenKind,
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum TokenKind {
//     Ident(Ident),
//     Integer(i64),
//     String(String),
//     Symbol(Symbol),
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum Symbol {
//     Dot, // .
//     Comma, // ,
//     Colon, // :
//     PathSep, // ::
//     BracketOpen, // (
//     BracketClose, // )
//     SquareBracketOpen, // [
//     SquareBracketClose, // ]
//     CurlyBracketOpen, // {
//     CurlyBracketClose, // }
//     Plus, // +
//     Minus, // -
//     Asterisk, // *
//     ForwardSlash, // /
//     Equal, // =
// }

// pub struct Lexer<T: Iterator<Item = char>> {
//     chars: Peekable<Enumerate<T>>,
// }

// impl<T: Iterator<Item = char>> Lexer<T> {
//     pub fn new(chars: T) -> Self {
//         Self {
//             chars: chars.enumerate().peekable(),
//         }
//     }
// }

// impl<T: Iterator<Item = char>> Iterator for Lexer<T> {
//     type Item = Result<Token, LexError>;

//     fn next(&mut self) -> Option<Self::Item> {
//         let (mut index, mut next_char) = self.chars.next()?;
        
//         // whitespace
//         if next_char.is_whitespace() {
//             // consume all whitespace
//             while let Some(_) = self.chars.next_if(|(_, c)| c.is_whitespace()) {}
//             let (i, c) = self.chars.next()?;
//             index = i;
//             next_char = c;
//         }

//         let (index, next_char) = (index, next_char);

//         let mut token_kind = None;

//         // identifiers
//         if next_char.is_alphabetic() || next_char == '_' {
//             let mut ident = String::from(next_char);

//             while let Some((_, char)) = self.chars.next_if(|(_, c)| c.is_alphanumeric() || *c == '_') {
//                 ident.push(char);
//             }
            
//             token_kind = Some(TokenKind::Ident(Ident::try_from(ident).expect("unreachable")));
//         }

//         if next_char.is_numeric() {
//             let mut number_string = String::from(next_char);

//             while let Some((_, char)) = self.chars.next_if(|(_, c)| c.is_numeric() || *c == '_') {
//                 if char.is_numeric() {
//                     number_string.push(char);
//                 }
//             }

//             let number: i64 = match number_string.parse() {
//                 Ok(number) => number,
//                 Err(err) => return Some(Err(LexError {
//                     index,
//                     kind: LexErrorKind::ParseIntError(err),
//                 })),
//             };

//             token_kind = Some(TokenKind::Integer(number))
//         }

//         // string
//         if next_char == '"' {
//             let mut string_contents = String::new();

//             loop {
//                 let Some((index, next_char)) = self.chars.next() else {
//                     return Some(Err(LexError {
//                         index,
//                         kind: LexErrorKind::UnexpectedEnd,
//                     }));
//                 };

//                 if next_char == '"' {
//                     break;
//                 }

//                 if next_char != '\\' {
//                     string_contents.push(next_char);
//                     continue;
//                 }

//                 let Some((index, next_char)) = self.chars.next() else {
//                     return Some(Err(LexError {
//                         index,
//                         kind: LexErrorKind::UnexpectedEnd,
//                     }));
//                 };

//                 let replacement_char = match next_char {
//                     'n' => '\n',
//                     'r' => '\r',
//                     't' => '\t',
//                     '\\' => '\\',
//                     '0' => '\0',
//                     '\'' => '\'',
//                     '"' => '"',
//                     'x' => {
//                         let Some((index, first_char)) = self.chars.next() else {
//                             return Some(Err(LexError {
//                                 index,
//                                 kind: LexErrorKind::UnexpectedEnd,
//                             }));
//                         };
//                         let Some((index, second_char)) = self.chars.next() else {
//                             return Some(Err(LexError {
//                                 index,
//                                 kind: LexErrorKind::UnexpectedEnd,
//                             }));
//                         };

//                         let mut hex_string = String::from(first_char);
//                         hex_string.push(second_char);

//                         let byte = match u8::from_str_radix(&hex_string, 16) {
//                             Ok(byte) => byte,
//                             Err(err) => return Some(Err(LexError {
//                                 index,
//                                 kind: LexErrorKind::ParseIntError(err),
//                             })),
//                         };

//                         byte as char
//                     },
//                     'u' => {
//                         let Some((index, first_char)) = self.chars.next() else {
//                             return Some(Err(LexError {
//                                 index,
//                                 kind: LexErrorKind::UnexpectedEnd,
//                             }));
//                         };
//                         let Some((index, second_char)) = self.chars.next() else {
//                             return Some(Err(LexError {
//                                 index,
//                                 kind: LexErrorKind::UnexpectedEnd,
//                             }));
//                         };
//                         let Some((index, third_char)) = self.chars.next() else {
//                             return Some(Err(LexError {
//                                 index,
//                                 kind: LexErrorKind::UnexpectedEnd,
//                             }));
//                         };
//                         let Some((index, fourth_char)) = self.chars.next() else {
//                             return Some(Err(LexError {
//                                 index,
//                                 kind: LexErrorKind::UnexpectedEnd,
//                             }));
//                         };

//                         let mut hex_string = String::from(first_char);
//                         hex_string.push(second_char);
//                         hex_string.push(third_char);
//                         hex_string.push(fourth_char);

//                         let char_u32 = match u32::from_str_radix(&hex_string, 16) {
//                             Ok(byte) => byte,
//                             Err(err) => return Some(Err(LexError {
//                                 index,
//                                 kind: LexErrorKind::ParseIntError(err),
//                             })),
//                         };

//                         match char::from_u32(char_u32) {
//                             Some(c) => c,
//                             None => return Some(Err(LexError {
//                                 index,
//                                 kind: LexErrorKind::NotUnicode(char_u32),
//                             })),
//                         }
//                     },
                    
//                     c => return Some(Err(LexError {
//                         index,
//                         kind: LexErrorKind::UnexpectedChar(c),
//                     }))
//                 };

//                 string_contents.push(replacement_char);
//             }

//             token_kind = Some(TokenKind::String(string_contents))
//         }

//         if next_char == '.' {
//             token_kind = Some(TokenKind::Symbol(Symbol::Dot));
//         }

//         if next_char == ',' {
//             token_kind = Some(TokenKind::Symbol(Symbol::Comma));
//         }

//         if next_char == ':' {
//             // symbol '::'
//             if let Some((_, _)) = self.chars.next_if(|(_, c)| *c == ':') {
//                 token_kind = Some(TokenKind::Symbol(Symbol::PathSep));
//             } else { // just ':'
//                 token_kind = Some(TokenKind::Symbol(Symbol::Colon));
//             }
//         }

//         if next_char == '(' {
//             token_kind = Some(TokenKind::Symbol(Symbol::BracketOpen));
//         }

//         if next_char == ')' {
//             token_kind = Some(TokenKind::Symbol(Symbol::BracketClose));
//         }

//         if next_char == '[' {
//             token_kind = Some(TokenKind::Symbol(Symbol::SquareBracketOpen));
//         }

//         if next_char == ']' {
//             token_kind = Some(TokenKind::Symbol(Symbol::SquareBracketClose));
//         }

//         if next_char == '{' {
//             token_kind = Some(TokenKind::Symbol(Symbol::CurlyBracketOpen));
//         }

//         if next_char == '}' {
//             token_kind = Some(TokenKind::Symbol(Symbol::CurlyBracketClose));
//         }

//         if next_char == '+' {
//             token_kind = Some(TokenKind::Symbol(Symbol::Plus));
//         }

//         if next_char == '-' {
//             token_kind = Some(TokenKind::Symbol(Symbol::Minus));
//         }

//         if next_char == '*' {
//             token_kind = Some(TokenKind::Symbol(Symbol::Asterisk));
//         }

//         if next_char == '/' {
//             token_kind = Some(TokenKind::Symbol(Symbol::ForwardSlash));
//         }

//         if next_char == '=' {
//             token_kind = Some(TokenKind::Symbol(Symbol::Equal));
//         }


//         if let Some(token_kind) = token_kind {
//             Some(Ok(Token {
//                 index,
//                 kind: token_kind,
//             }))
//         } else {
//             Some(Err(LexError {
//                 index,
//                 kind: LexErrorKind::UnexpectedChar(next_char),
//             }))
//         }
//     }
// }

// #[cfg(test)]
// mod tests {
//     use itertools::Itertools;

//     use super::Lexer;

//     #[test]
//     fn lex() {
//         let text_string = r#"
//         value = (anon[struct { active: bool, id: int32, name: string }][])[
//             anon[struct { active: bool, id: int32, name: string }] {
//                 active: false,
//                 id: -1,
//                 name: "El Jones, Jim",
//             },
//             anon[struct { active: bool, id: int32, name: string }] {
//                 active: true,
//                 id: 1,
//                 name: "Jim Jones",
//             },
//             anon[struct { active: bool, id: int32, name: string }] {
//                 active: true,
//                 id: 2,
//                 name: "Jimboni Jonesi",
//             },
//         ]
//         "#;

//         let token_kinds = Lexer::new(text_string.chars()).map(|t| t.unwrap().kind).collect_vec();

//         use super::TokenKind::*;
//         use super::Symbol::*;

//         assert_eq!(token_kinds, vec![
//             Ident(
//                 id!("value"),
//             ),
//             Symbol(
//                 Equal,
//             ),
//             Symbol(
//                 BracketOpen,
//             ),
//             Ident(
//                 id!("anon"),
//             ),
//             Symbol(
//                 SquareBracketOpen,
//             ),
//             Ident(
//                 id!("struct"),
//             ),
//             Symbol(
//                 CurlyBracketOpen,
//             ),
//             Ident(
//                 id!("active"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("bool"),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("id"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("int32"),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("name"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("string"),
//             ),
//             Symbol(
//                 CurlyBracketClose,
//             ),
//             Symbol(
//                 SquareBracketClose,
//             ),
//             Symbol(
//                 SquareBracketOpen,
//             ),
//             Symbol(
//                 SquareBracketClose,
//             ),
//             Symbol(
//                 BracketClose,
//             ),
//             Symbol(
//                 SquareBracketOpen,
//             ),
//             Ident(
//                 id!("anon"),
//             ),
//             Symbol(
//                 SquareBracketOpen,
//             ),
//             Ident(
//                 id!("struct"),
//             ),
//             Symbol(
//                 CurlyBracketOpen,
//             ),
//             Ident(
//                 id!("active"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("bool"),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("id"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("int32"),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("name"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("string"),
//             ),
//             Symbol(
//                 CurlyBracketClose,
//             ),
//             Symbol(
//                 SquareBracketClose,
//             ),
//             Symbol(
//                 CurlyBracketOpen,
//             ),
//             Ident(
//                 id!("active"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("false"),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("id"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Symbol(
//                 Minus,
//             ),
//             Integer(
//                 1,
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("name"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             String(
//                 "El Jones, Jim".into(),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Symbol(
//                 CurlyBracketClose,
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("anon"),
//             ),
//             Symbol(
//                 SquareBracketOpen,
//             ),
//             Ident(
//                 id!("struct"),
//             ),
//             Symbol(
//                 CurlyBracketOpen,
//             ),
//             Ident(
//                 id!("active"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("bool"),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("id"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("int32"),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("name"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("string"),
//             ),
//             Symbol(
//                 CurlyBracketClose,
//             ),
//             Symbol(
//                 SquareBracketClose,
//             ),
//             Symbol(
//                 CurlyBracketOpen,
//             ),
//             Ident(
//                 id!("active"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("true"),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("id"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Integer(
//                 1,
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("name"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             String(
//                 "Jim Jones".into(),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Symbol(
//                 CurlyBracketClose,
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("anon"),
//             ),
//             Symbol(
//                 SquareBracketOpen,
//             ),
//             Ident(
//                 id!("struct"),
//             ),
//             Symbol(
//                 CurlyBracketOpen,
//             ),
//             Ident(
//                 id!("active"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("bool"),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("id"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("int32"),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("name"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("string"),
//             ),
//             Symbol(
//                 CurlyBracketClose,
//             ),
//             Symbol(
//                 SquareBracketClose,
//             ),
//             Symbol(
//                 CurlyBracketOpen,
//             ),
//             Ident(
//                 id!("active"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Ident(
//                 id!("true"),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("id"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             Integer(
//                 2,
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Ident(
//                 id!("name"),
//             ),
//             Symbol(
//                 Colon,
//             ),
//             String(
//                 "Jimboni Jonesi".into(),
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Symbol(
//                 CurlyBracketClose,
//             ),
//             Symbol(
//                 Comma,
//             ),
//             Symbol(
//                 SquareBracketClose,
//             ),
//         ]);
//     }
// }
