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
fn parse_hands(input: &str) -> PyResult<HandInfo> {
    let (_, hands) = separated_list1(line_ending, Hand::parse)(input).unwrap();
    let hand = HandInfo {
            game_info: GameInfo::Tournament(TournamentInfo {
                name: String::from("WESTERN"),
                buy_in: 0.90,
                rake: 0.10,
                level: 6,
            }),
            hand_id: String::from("2815488303912976462-15-1684698584"),
            poker_type: PokerType::HoldemNoLimit,
            blinds: Blinds {
                ante: Some(60.0),
                small_blind: 250.0,
                big_blind: 500.0,
            },
            // datetime: String::from("2023/05/21 19:49:44 UTC"),
            datetime: Utc.with_ymd_and_hms(2023, 5, 21, 19, 49, 44).unwrap(),
        };
    Ok(hand)
}

#[pymodule]
fn holdem_suite_parser(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(parse_hands, m)?)?;
    m.add_class::<DealtToHero>()?;
    m.add_class::<HoleCards>()?;
    m.add_class::<Card>();
    m.add_class::<HandInfo>();
    Ok(())
}