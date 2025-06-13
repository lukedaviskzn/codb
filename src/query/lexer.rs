use std::{fmt::{Debug, Display}, iter::{Enumerate, Peekable}, num::ParseIntError, ops::{Deref, DerefMut}};

use codb_core::Ident;

use crate::{query::parser::{ParseError, ParseErrorKind}, typesystem::{ttype::ScalarType, value::ScalarValue}};

use super::Span;

#[derive(Debug, Clone, thiserror::Error)]
#[error("[{span}] error while{}: {kind}", .context.as_ref().map(|ctx| format!(" {ctx}")).unwrap_or(" tokenising".into()))]
pub struct LexError {
    pub span: Span,
    pub context: Option<LexContext>,
    pub kind: LexErrorKind,
}

impl LexError {
    pub fn with_context(mut self, context: LexContext) -> LexError {
        self.context = Some(context);
        self
    }
}

#[derive(Debug, Clone)]
pub enum LexContext {
    LexingString,
    LexingInteger,
    LexingSymbol,
}

impl Display for LexContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexContext::LexingString => write!(f, "tokenising string"),
            LexContext::LexingInteger => write!(f, "tokenising integer"),
            LexContext::LexingSymbol => write!(f, "tokenising symbol"),
        }
    }
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum LexErrorKind {
    #[error("unexpected end of input")]
    UnexpectedEnd,
    #[error("unexpected char `{0:?}`")]
    UnexpectedChar(char),
    #[error("expected char `{expected:?}` got `{got:?}`")]
    ExpectedChar {
        expected: char,
        got: char,
    },
    #[error("{0:#x} is not a valid unicode character")]
    NotUnicode(u32),
    #[error("no such type `{0}-bit integer`")]
    IntegerBits(u8),
    #[error("{0}")]
    ParseIntError(#[from] ParseIntError),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Clone, PartialEq, Eq)]
pub enum TokenKind {
    Keyword(Keyword),
    Ident(Ident),
    Symbol(Symbol),
    ScalarType(ScalarType),
    ScalarLiteral(ScalarValue),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TokenTag {
    Keyword,
    Ident,
    Symbol,
    ScalarType,
    ScalarLiteral,
}

impl TokenKind {
    pub fn tag(&self) -> TokenTag {
        match self {
            TokenKind::Keyword(_) => TokenTag::Keyword,
            TokenKind::Ident(_) => TokenTag::Ident,
            TokenKind::Symbol(_) => TokenTag::Symbol,
            TokenKind::ScalarType(_) => TokenTag::ScalarType,
            TokenKind::ScalarLiteral(_) => TokenTag::ScalarLiteral,
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Keyword(token) => write!(f, "{token}"),
            TokenKind::Ident(token) => write!(f, "{token}"),
            TokenKind::Symbol(token) => write!(f, "{token}"),
            TokenKind::ScalarType(token) => write!(f, "{token}"),
            TokenKind::ScalarLiteral(token) => write!(f, "{token:?}"),
        }
    }
}

impl Debug for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for TokenTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenTag::Keyword => write!(f, "keyword"),
            TokenTag::Ident => write!(f, "ident"),
            TokenTag::Symbol => write!(f, "symbol"),
            TokenTag::ScalarType => write!(f, "scalar type"),
            TokenTag::ScalarLiteral => write!(f, "scalar value"),
        }
    }
}

impl Debug for TokenTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    If,
    Else,
    Match,
    Struct,
    Enum,
    Type,
    Relation,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::Match => write!(f, "match"),
            Keyword::Struct => write!(f, "struct"),
            Keyword::Enum => write!(f, "enum"),
            Keyword::Type => write!(f, "type"),
            Keyword::Relation => write!(f, "relation"),
        }
    }
}

impl Debug for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Symbol {
    Dot, // .
    Comma, // ,
    Colon, // :
    PathSep, // ::
    RightArrow, // ->
    RightFatArrow, // =>
    BracketOpen, // (
    BracketClose, // )
    SquareBracketOpen, // [
    SquareBracketClose, // ]
    CurlyBracketOpen, // {
    CurlyBracketClose, // }
    Plus, // +
    Minus, // -
    Asterisk, // *
    ForwardSlash, // /
    Equal, // =
    DoubleEqual, // =
    LessThan, // <
    LessThanOrEqual, // <=
    GreaterThan, // >
    GreaterThanOrEqual, // >=
    Hash, // #
    And, // &&
    Or, // ||
    Not, // !
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Symbol::Dot => ".",
            Symbol::Comma => ",",
            Symbol::Colon => ":",
            Symbol::PathSep => "::",
            Symbol::RightArrow => "->",
            Symbol::RightFatArrow => "=>",
            Symbol::BracketOpen => "(",
            Symbol::BracketClose => ")",
            Symbol::SquareBracketOpen => "[",
            Symbol::SquareBracketClose => "]",
            Symbol::CurlyBracketOpen => "{",
            Symbol::CurlyBracketClose => "}",
            Symbol::Plus => "+",
            Symbol::Minus => "-",
            Symbol::Asterisk => "*",
            Symbol::ForwardSlash => "/",
            Symbol::Equal => "=",
            Symbol::DoubleEqual => "==",
            Symbol::LessThan => "<",
            Symbol::LessThanOrEqual => "<=",
            Symbol::GreaterThan => ">",
            Symbol::GreaterThanOrEqual => ">=",
            Symbol::Hash => "#",
            Symbol::And => "&&",
            Symbol::Or => "||",
            Symbol::Not => "!",
        };
        f.write_str(string)
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

pub(crate) struct Lexer<T: Iterator<Item = char>> {
    inner: Peekable<LexerInner<T>>,
}

impl<T: Iterator<Item = char>> Lexer<T> {
    pub fn new(chars: T) -> Self {
        Self {
            inner: LexerInner::new(chars).peekable(),
        }
    }

    pub fn expect_peek(&mut self) -> Result<&Token, ParseError> {
        let token = self.peek().ok_or_else(|| ParseError {
            span: Span::ALL,
            context: None,
            kind: ParseErrorKind::UnexpectedEnd,
        })?.as_ref().map_err(|err| err.clone())?;

        Ok(token)
    }

    pub fn expect_token(&mut self) -> Result<Token, ParseError> {
        let token = self.next().ok_or_else(|| ParseError {
            span: Span::ALL,
            context: None,
            kind: ParseErrorKind::UnexpectedEnd,
        })??;

        Ok(token)
    }

    pub fn expect_token_tag(&mut self, tag: TokenTag) -> Result<Token, ParseError> {
        let token = self.expect_token()?;

        let token_tag = token.kind.tag();

        if token_tag != tag {
            return Err(ParseError {
                span: token.span,
                context: None,
                kind: ParseErrorKind::ExpectedTokenTag {
                    expected: tag,
                    got: token_tag,
                },
            });
        }

        Ok(token)
    }

    pub fn expect_token_kind(&mut self, kind: &TokenKind) -> Result<Token, ParseError> {
        let token = self.expect_token()?;

        if token.kind != *kind {
            return Err(ParseError {
                span: token.span,
                context: None,
                kind: ParseErrorKind::ExpectedTokenKind {
                    expected: kind.clone(),
                    got: token.kind,
                },
            });
        }

        Ok(token)
    }

    pub fn expect_symbol(&mut self, symbol: Symbol) -> Result<Token, ParseError> {
        let span = self.expect_token_kind(&TokenKind::Symbol(symbol))?;
        Ok(span)
    }
    
    pub fn expect_keyword(&mut self, keyword: Keyword) -> Result<Token, ParseError> {
        let span = self.expect_token_kind(&TokenKind::Keyword(keyword))?;
        Ok(span)
    }
    
    pub fn next_if_token_tag(&mut self, tag: TokenTag) -> Result<Option<Token>, ParseError> {
        let token = self.next_if(|token| match token {
            Ok(token) if token.kind.tag() == tag => true,
            _ => false,
        });

        match token {
            Some(Ok(ok)) => Ok(Some(ok)),
            Some(Err(err)) => Err(err.into()),
            None => Ok(None),
        }
    }

    pub fn next_if_token_kind(&mut self, kind: &TokenKind) -> Result<Option<Token>, ParseError> {
        let token = self.next_if(|token| match token {
            Ok(token) if token.kind == *kind => true,
            _ => false,
        });

        match token {
            Some(Ok(ok)) => Ok(Some(ok)),
            Some(Err(err)) => Err(err.into()),
            None => Ok(None),
        }
    }

    pub fn next_if_symbol(&mut self, symbol: Symbol) -> Result<Option<Span>, ParseError> {
        let Some(token) = self.next_if_token_kind(&TokenKind::Symbol(symbol))? else { return Ok(None) };
        Ok(Some(token.span))
    }
}

impl<T: Iterator<Item = char>> Deref for Lexer<T> {
    type Target = Peekable<LexerInner<T>>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: Iterator<Item = char>> DerefMut for Lexer<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T: Iterator<Item = char>> Iterator for Lexer<T> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

    pub(crate) struct LexerInner<T: Iterator<Item = char>> {
    chars: Peekable<Enumerate<T>>,
}

impl<T: Iterator<Item = char>> LexerInner<T> {
    fn new(chars: T) -> Self {
        Self {
            chars: chars.enumerate().peekable(),
        }
    }

    fn expect_next(&mut self) -> Result<(usize, char), LexError> {
        self.chars.next().ok_or(LexError {
            span: (..).into(),
            context: None,
            kind: LexErrorKind::UnexpectedEnd,
        })
    }

    fn expect_char(&mut self, char: char) -> Result<usize, LexError> {
        let (index, next_char) = self.expect_next()?;
        
        if next_char != char {
            return Err(LexError {
                span: Span::at(index),
                context: None,
                kind: LexErrorKind::ExpectedChar {
                    expected: char,
                    got: next_char,
                }
            });
        }
        
        Ok(index)
    }
}

impl<T: Iterator<Item = char>> Iterator for LexerInner<T> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        let (index, next_char) = self.chars.next()?;
        
        // whitespace
        if next_char.is_whitespace() {
            // consume all whitespace
            while let Some(_) = self.chars.next_if(|(_, c)| c.is_whitespace()) {}
            return self.next();
        }

        if next_char == '/' {
            if let Some((_, char_after)) = self.chars.peek() {
                if *char_after == '/' {
                    // consume all chars until new line
                    while let Some(_) = self.chars.next_if(|(_, c)| *c != '\n') {}
                    return self.next();
                } else if *char_after == '*' {
                    let mut nesting = 1;
                    // consume all chars until */
                    while let Some((_, first)) = self.chars.next() {
                        if nesting <= 0 {
                            break;
                        }
                        if first == '*' {
                            if let Some((_, '/')) = self.chars.peek() {
                                nesting -= 1;
                            }
                        }
                    }
                    return self.next();
                }
            }
        }

        let (index, next_char) = (index, next_char);

        let mut token = None;

        // identifiers
        if next_char.is_alphabetic() || next_char == '_' {
            let mut ident = String::from(next_char);
            let mut ident_len = 1;

            while let Some((_, char)) = self.chars.next_if(|(_, c)| c.is_alphanumeric() || *c == '_') {
                ident.push(char);
                ident_len += 1;
            }

            let kind;

            if ident == "if" {
                kind = TokenKind::Keyword(Keyword::If);
            } else if ident == "else" {
                kind = TokenKind::Keyword(Keyword::Else);
            } else if ident == "match" {
                kind = TokenKind::Keyword(Keyword::Match);
            } else if ident == "struct" {
                kind = TokenKind::Keyword(Keyword::Struct);
            } else if ident == "enum" {
                kind = TokenKind::Keyword(Keyword::Enum);
            } else if ident == "type" {
                kind = TokenKind::Keyword(Keyword::Type);
            } else if ident == "relation" {
                kind = TokenKind::Keyword(Keyword::Relation);
            } else if ident == "unit" {
                kind = TokenKind::ScalarLiteral(ScalarValue::Unit);
            } else if ident == "true" {
                kind = TokenKind::ScalarLiteral(ScalarValue::Bool(true));
            } else if ident == "false" {
                kind = TokenKind::ScalarLiteral(ScalarValue::Bool(false));
            } else if let Some(scalar) = ScalarType::from_name(&ident) {
                kind = TokenKind::ScalarType(scalar);
            } else {
                kind = TokenKind::Ident(Ident::try_from(ident).expect("unreachable"));
            }

            token = Some(Token {
                span: Span::with_len(index, ident_len),
                kind,
            });
        }

        if next_char.is_numeric() {
            let mut number_string_length = 1;

            let mut number_string = String::from(next_char);

            while let Some((_, char)) = self.chars.next_if(|(_, c)| c.is_numeric() || *c == '_') {
                if char.is_numeric() {
                    number_string.push(char);
                }
                number_string_length += 1;
            }

            let i_index = match self.expect_char('i') {
                Ok(index) => index,
                Err(err) => return Some(Err(err.with_context(LexContext::LexingInteger))),
            };

            let mut number_bit_string_length = 0;

            let mut number_bits_string = String::new();

            while let Some((_, char)) = self.chars.next_if(|(_, c)| c.is_numeric() || *c == '_') {
                if char.is_numeric() {
                    number_bits_string.push(char);
                }
                number_bit_string_length += 1;
            }

            let number: i64 = match number_string.parse() {
                Ok(number) => number,
                Err(err) => return Some(Err(LexError {
                    span: Span::with_len(index, number_string_length),
                    context: Some(LexContext::LexingInteger),
                    kind: LexErrorKind::ParseIntError(err),
                })),
            };

            let number_bits: u8 = match number_bits_string.parse() {
                Ok(number) => number,
                Err(err) => return Some(Err(LexError {
                    span: Span::with_len(index, number_string_length),
                    context: Some(LexContext::LexingInteger),
                    kind: LexErrorKind::ParseIntError(err),
                })),
            };

            match number_bits {
                32 => token = Some(Token {
                    span: Span::with_len(i_index + 1, number_bit_string_length),
                    kind: TokenKind::ScalarLiteral(ScalarValue::Int32(number as i32)),
                }),
                64 => token = Some(Token {
                    span: Span::with_len(i_index + 1, number_bit_string_length),
                    kind: TokenKind::ScalarLiteral(ScalarValue::Int64(number)),
                }),
                bits => return Some(Err(LexError {
                    span: Span::with_len(i_index + 1, number_bit_string_length),
                    context: Some(LexContext::LexingInteger),
                    kind: LexErrorKind::IntegerBits(bits),
                })),
            }
        }

        // string
        if next_char == '"' {
            let mut string_contents = String::new();

            let mut token_length = 1;

            loop {
                let Some((index, next_char)) = self.chars.next() else {
                    return Some(Err(LexError {
                        span: Span::beyond(index),
                        context: Some(LexContext::LexingString),
                        kind: LexErrorKind::UnexpectedEnd,
                    }));
                };

                token_length += 1;

                if next_char == '"' {
                    break;
                }

                if next_char != '\\' {
                    string_contents.push(next_char);
                    continue;
                }

                let Some((index, next_char)) = self.chars.next() else {
                    token_length += 1;
                    return Some(Err(LexError {
                        span: Span::beyond(index),
                        context: Some(LexContext::LexingString),
                        kind: LexErrorKind::UnexpectedEnd,
                    }));
                };

                let replacement_char = match next_char {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '0' => '\0',
                    '\'' => '\'',
                    '"' => '"',
                    'x' => {
                        let (_, first_char) = match self.expect_next() {
                            Ok(next) => next,
                            Err(err) => return Some(Err(err.with_context(LexContext::LexingString))),
                        };
                        let (index, second_char) = match self.expect_next() {
                            Ok(next) => next,
                            Err(err) => return Some(Err(err.with_context(LexContext::LexingString))),
                        };
                        token_length += 2;

                        let mut hex_string = String::from(first_char);
                        hex_string.push(second_char);

                        let byte = match u8::from_str_radix(&hex_string, 16) {
                            Ok(byte) => byte,
                            Err(err) => return Some(Err(LexError {
                                span: Span::with_len(index-3, 4),
                                context: Some(LexContext::LexingString),
                                kind: LexErrorKind::ParseIntError(err),
                            })),
                        };

                        byte as char
                    },
                    'u' => {
                        
                        let (_, first_char) = match self.expect_next() {
                            Ok(next) => next,
                            Err(err) => return Some(Err(err.with_context(LexContext::LexingString))),
                        };
                        let (_, second_char) = match self.expect_next() {
                            Ok(next) => next,
                            Err(err) => return Some(Err(err.with_context(LexContext::LexingString))),
                        };
                        let (_, third_char) = match self.expect_next() {
                            Ok(next) => next,
                            Err(err) => return Some(Err(err.with_context(LexContext::LexingString))),
                        };
                        let (index, fourth_char) = match self.expect_next() {
                            Ok(next) => next,
                            Err(err) => return Some(Err(err.with_context(LexContext::LexingString))),
                        };
                        token_length += 4;

                        let mut hex_string = String::from(first_char);
                        hex_string.push(second_char);
                        hex_string.push(third_char);
                        hex_string.push(fourth_char);

                        let char_u32 = match u32::from_str_radix(&hex_string, 16) {
                            Ok(byte) => byte,
                            Err(err) => return Some(Err(LexError {
                                span: Span::with_len(index-5, 6),
                                context: Some(LexContext::LexingString),
                                kind: LexErrorKind::ParseIntError(err),
                            })),
                        };

                        match char::from_u32(char_u32) {
                            Some(c) => c,
                            None => return Some(Err(LexError {
                                span: Span::with_len(index-5, 6),
                                context: Some(LexContext::LexingString),
                                kind: LexErrorKind::NotUnicode(char_u32),
                            })),
                        }
                    },
                    c => return Some(Err(LexError {
                        span: Span::at(index),
                        context: Some(LexContext::LexingString),
                        kind: LexErrorKind::UnexpectedChar(c),
                    }))
                };

                string_contents.push(replacement_char);
            }

            token = Some(Token {
                span: Span::with_len(index, token_length),
                kind: TokenKind::ScalarLiteral(ScalarValue::String(string_contents)),
            })
        }

        if next_char == '.' {
            token = Some(Token {
                span: Span::at(index),
                kind: TokenKind::Symbol(Symbol::Dot),
            });
        }

        if next_char == ',' {
            token = Some(Token {
                span: Span::at(index),
                kind: TokenKind::Symbol(Symbol::Comma),
            });
        }

        if next_char == ':' {
            // symbol '::'
            if let Some((_, _)) = self.chars.next_if(|(_, c)| *c == ':') {
                token = Some(Token {
                    span: Span::with_len(index, 2),
                    kind: TokenKind::Symbol(Symbol::PathSep),
                });
            } else { // just ':'
                token = Some(Token {
                    span: Span::at(index),
                    kind: TokenKind::Symbol(Symbol::Colon),
                });
            }
        }

        if next_char == '(' {
            token = Some(Token {
                span: Span::at(index),
                kind: TokenKind::Symbol(Symbol::BracketOpen),
            });
        }

        if next_char == ')' {
            token = Some(Token {
                span: Span::at(index),
                kind: TokenKind::Symbol(Symbol::BracketClose),
            });
        }

        if next_char == '[' {
            token = Some(Token {
                span: Span::at(index),
                kind: TokenKind::Symbol(Symbol::SquareBracketOpen),
            });
        }

        if next_char == ']' {
            token = Some(Token {
                span: Span::at(index),
                kind: TokenKind::Symbol(Symbol::SquareBracketClose),
            });
        }

        if next_char == '{' {
            token = Some(Token {
                span: Span::at(index),
                kind: TokenKind::Symbol(Symbol::CurlyBracketOpen),
            });
        }

        if next_char == '}' {
            token = Some(Token {
                span: Span::at(index),
                kind: TokenKind::Symbol(Symbol::CurlyBracketClose),
            });
        }

        if next_char == '+' {
            token = Some(Token {
                span: Span::at(index),
                kind: TokenKind::Symbol(Symbol::Plus),
            });
        }

        if next_char == '-' {
            // symbol '->'
            if let Some((_, _)) = self.chars.next_if(|(_, c)| *c == '>') {
                token = Some(Token {
                    span: Span::with_len(index, 2),
                    kind: TokenKind::Symbol(Symbol::RightArrow),
                });
            } else { // just '-'
                token = Some(Token {
                    span: Span::at(index),
                    kind: TokenKind::Symbol(Symbol::Minus),
                });
            }
        }

        if next_char == '*' {
            token = Some(Token {
                span: Span::at(index),
                kind: TokenKind::Symbol(Symbol::Asterisk),
            });
        }

        if next_char == '/' {
            token = Some(Token {
                span: Span::at(index),
                kind: TokenKind::Symbol(Symbol::ForwardSlash),
            });
        }

        if next_char == '=' {
            if let Some((_, _)) = self.chars.next_if(|(_, c)| *c == '=') {
                // symbol '=='
                token = Some(Token {
                    span: Span::with_len(index, 2),
                    kind: TokenKind::Symbol(Symbol::DoubleEqual),
                });
            } else if let Some((_, _)) = self.chars.next_if(|(_, c)| *c == '>') {
                // symbol '=>'
                token = Some(Token {
                    span: Span::with_len(index, 2),
                    kind: TokenKind::Symbol(Symbol::RightFatArrow),
                });
            } else {
                // just '='
                token = Some(Token {
                    span: Span::at(index),
                    kind: TokenKind::Symbol(Symbol::Equal),
                });
            }
        }

        if next_char == '<' {
            // symbol '<='
            if let Some((_, _)) = self.chars.next_if(|(_, c)| *c == '=') {
                token = Some(Token {
                    span: Span::with_len(index, 2),
                    kind: TokenKind::Symbol(Symbol::LessThanOrEqual),
                });
            } else { // just '<'
                token = Some(Token {
                    span: Span::at(index),
                    kind: TokenKind::Symbol(Symbol::LessThan),
                });
            }
        }

        if next_char == '>' {
            // symbol '>='
            if let Some((_, _)) = self.chars.next_if(|(_, c)| *c == '=') {
                token = Some(Token {
                    span: Span::with_len(index, 2),
                    kind: TokenKind::Symbol(Symbol::GreaterThanOrEqual),
                });
            } else { // just '<'
                token = Some(Token {
                    span: Span::at(index),
                    kind: TokenKind::Symbol(Symbol::GreaterThan),
                });
            }
        }

        if next_char == '#' {
            token = Some(Token {
                span: Span::at(index),
                kind: TokenKind::Symbol(Symbol::Hash),
            });
        }

        if next_char == '&' {
            if let Err(err) = self.expect_char('&') {
                return Some(Err(err.with_context(LexContext::LexingSymbol)));
            }
            token = Some(Token {
                span: Span::with_len(index, 2),
                kind: TokenKind::Symbol(Symbol::And),
            });
        }

        if next_char == '|' {
            if let Err(err) = self.expect_char('|') {
                return Some(Err(err.with_context(LexContext::LexingSymbol)));
            }
            token = Some(Token {
                span: Span::with_len(index, 2),
                kind: TokenKind::Symbol(Symbol::Or),
            });
        }

        if next_char == '!' {
            token = Some(Token {
                span: Span::at(index),
                kind: TokenKind::Symbol(Symbol::Not),
            });
        }

        Some(token.ok_or_else(|| LexError {
            span: Span::at(index),
            context: None,
            kind: LexErrorKind::UnexpectedChar(next_char),
        }))
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::Lexer;

    #[test]
    fn lex() {
        let text_string = r#"
        value = (anon[struct { active: bool, id: int32, name: string }][])[
            anon[struct { active: bool, id: int32, name: string }] {
                active: false,
                id: -1_i32,
                name: "El Jones, Jim",
            },
            anon[struct { active: bool, id: int32, name: string }] {
                active: true,
                id: 1_i32,
                name: "Jim Jones",
            },
            anon[struct { active: bool, id: int32, name: string }] {
                active: true,
                id: 2_i32,
                name: "Jimboni Jonesi",
            },
        ]
        "#;

        let token_kinds = Lexer::new(text_string.chars()).map(|t| t.unwrap().kind).collect_vec();

        use super::TokenKind as T;
        use super::Symbol as S;
        use super::Keyword as K;
        use super::ScalarType as ST;
        use super::ScalarValue as SV;

        assert_eq!(token_kinds, vec![
            T::Ident(
                id!("value"),
            ),
            T::Symbol(
                S::Equal,
            ),
            T::Symbol(
                S::BracketOpen,
            ),
            T::Ident(
                id!("anon")
            ),
            T::Symbol(
                S::SquareBracketOpen,
            ),
            T::Keyword(
                K::Struct
            ),
            T::Symbol(
                S::CurlyBracketOpen,
            ),
            T::Ident(
                id!("active"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarType(
                ST::Bool
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("id"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarType(
                ST::Int32,
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("name"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarType(
                ST::String,
            ),
            T::Symbol(
                S::CurlyBracketClose,
            ),
            T::Symbol(
                S::SquareBracketClose,
            ),
            T::Symbol(
                S::SquareBracketOpen,
            ),
            T::Symbol(
                S::SquareBracketClose,
            ),
            T::Symbol(
                S::BracketClose,
            ),
            T::Symbol(
                S::SquareBracketOpen,
            ),
            T::Ident(
                id!("anon")
            ),
            T::Symbol(
                S::SquareBracketOpen,
            ),
            T::Keyword(
                K::Struct
            ),
            T::Symbol(
                S::CurlyBracketOpen,
            ),
            T::Ident(
                id!("active"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarType(
                ST::Bool
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("id"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarType(
                ST::Int32,
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("name"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarType(
                ST::String,
            ),
            T::Symbol(
                S::CurlyBracketClose,
            ),
            T::Symbol(
                S::SquareBracketClose,
            ),
            T::Symbol(
                S::CurlyBracketOpen,
            ),
            T::Ident(
                id!("active"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarLiteral(
                SV::Bool(false)
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("id"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::Symbol(
                S::Minus,
            ),
            T::ScalarLiteral(
                SV::Int32(1),
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("name"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarLiteral(
                SV::String("El Jones, Jim".into()),
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Symbol(
                S::CurlyBracketClose,
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("anon")
            ),
            T::Symbol(
                S::SquareBracketOpen,
            ),
            T::Keyword(
                K::Struct
            ),
            T::Symbol(
                S::CurlyBracketOpen,
            ),
            T::Ident(
                id!("active"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarType(
                ST::Bool
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("id"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarType(
                ST::Int32,
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("name"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarType(
                ST::String,
            ),
            T::Symbol(
                S::CurlyBracketClose,
            ),
            T::Symbol(
                S::SquareBracketClose,
            ),
            T::Symbol(
                S::CurlyBracketOpen,
            ),
            T::Ident(
                id!("active"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarLiteral(
                SV::Bool(true)
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("id"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarLiteral(
                SV::Int32(1),
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("name"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarLiteral(
                SV::String("Jim Jones".into()),
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Symbol(
                S::CurlyBracketClose,
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("anon")
            ),
            T::Symbol(
                S::SquareBracketOpen,
            ),
            T::Keyword(
                K::Struct
            ),
            T::Symbol(
                S::CurlyBracketOpen,
            ),
            T::Ident(
                id!("active"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarType(
                ST::Bool
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("id"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarType(
                ST::Int32,
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("name"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarType(
                ST::String,
            ),
            T::Symbol(
                S::CurlyBracketClose,
            ),
            T::Symbol(
                S::SquareBracketClose,
            ),
            T::Symbol(
                S::CurlyBracketOpen,
            ),
            T::Ident(
                id!("active"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarLiteral(
                SV::Bool(true)
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("id"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarLiteral(
                SV::Int32(2),
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Ident(
                id!("name"),
            ),
            T::Symbol(
                S::Colon,
            ),
            T::ScalarLiteral(
                SV::String("Jimboni Jonesi".into()),
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Symbol(
                S::CurlyBracketClose,
            ),
            T::Symbol(
                S::Comma,
            ),
            T::Symbol(
                S::SquareBracketClose,
            ),
        ]);
    }
}
