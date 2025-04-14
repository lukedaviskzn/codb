use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
#[binrw::binrw]
pub enum ScalarType {
    #[brw(magic(0u8))]
    Bool,
    #[brw(magic(1u8))]
    Int32,
}

impl ScalarType {
    pub fn type_check(&self, value: &ScalarValue) -> bool {
        match self {
            ScalarType::Bool => matches!(value, ScalarValue::Bool(_)),
            ScalarType::Int32 => matches!(value, ScalarValue::Int32(_)),
        }
    }
}

#[binrw::binread]
struct MyType {
    #[bw(map = |x: &bool| *x as u8)]
    #[br(map = |x: u8| x > 0)]
    value: bool
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize)]
#[binrw::binrw]
pub enum ScalarValue {
    #[brw(magic(0u8))]
    Bool(
        #[bw(map = |x| *x as u8)]
        #[br(map = |x: u8| x > 0u8)]
        bool
    ),
    #[brw(magic(1u8))]
    Int32(i32),
}

impl ScalarValue {
    pub fn ttype(&self) -> ScalarType {
        match self {
            ScalarValue::Bool(_) => ScalarType::Bool,
            ScalarValue::Int32(_) => ScalarType::Int32,
        }
    }
}

impl Display for ScalarValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScalarValue::Bool(value) => value.fmt(f),
            ScalarValue::Int32(value) => value.fmt(f),
        }
    }
}
