pub mod parser;
pub mod summary_parser;
mod common;

use chrono::prelude::*;

use nom::character::complete::line_ending;
use nom::multi::separated_list1;
use pyo3::methods::OkWrap;
use pyo3::prelude::*;
use crate::parser::PokerType;
use crate::parser::{Blinds, Card, DealtToHero, GameInfo, Hand, HandInfo, HoleCards, Rank, Suit, TournamentInfo};

#[pyfunction]
fn parse_hands(input: &str) -> PyResult<Vec<Hand>> {
    let (_, hands) = separated_list1(line_ending, Hand::parse)(input).unwrap();
    Ok(hands)
}

#[pymodule]
fn holdem_suite_parser(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(parse_hands, m)?)?;
    m.add_class::<DealtToHero>()?;
    m.add_class::<HoleCards>()?;
    m.add_class::<Card>()?;
    m.add_class::<HandInfo>()?;
    m.add_class::<Hand>()?;
    Ok(())
}