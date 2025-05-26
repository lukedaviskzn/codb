use crate::expression::Expression;

pub mod lexer;
pub mod parser;

pub enum Query {
    Data(Expression),
    // Schema(/* schema alteration instructions */),
}
