use std::fmt;
use std::fmt::{Display, Formatter};

use chrono::prelude::*;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_till, take_until, take_while};
use nom::character::complete::{alpha1, anychar, char, line_ending, none_of, not_line_ending};
use nom::combinator::{eof, map, map_res, opt};
use nom::multi::{many0, many1, many_till, separated_list0, separated_list1};
use nom::number::complete::double;
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple, Tuple};
use nom::{IResult, Parser};
use pyo3::{IntoPy, pyclass, pymethods, PyObject, PyResult, Python};
use pyo3::types::{PyFloat, PyString};
use serde::Serialize;

#[pyclass(module = "holdem_suite_parser", get_all)]
#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct TournamentInfo {
    pub name: String,
    pub buy_in: f64,
    pub rake: f64,
    pub level: u32,
}

impl TournamentInfo {
    fn parse(input: &str) -> IResult<&str, TournamentInfo> {
        let name_parser = delimited(tag("\""), take_while(|c: char| c != '"'), tag("\""));

        let buyin_parser = terminated(double, opt(tag("€")));
        let rake_parser = terminated(double, opt(tag("€")));

        let buyin_rake_parser = preceded(
            tag("buyIn: "),
            separated_pair(buyin_parser, tag(" + "), rake_parser),
        );

        let level_parser = preceded(tag("level: "), nom::character::complete::u32);

        let (input, (name, _, (buy_in, rake), _, level)) = (
            name_parser,
            char(' '),
            buyin_rake_parser,
            char(' '),
            level_parser,
        )
            .parse(input)?;

        Ok((
            input,
            TournamentInfo {
                name: name.to_owned(),
                buy_in,
                rake,
                level,
            },
        ))
    }
}

#[derive(Debug, PartialEq, Serialize, Clone)]
#[serde(tag = "type")]
pub enum GameInfo {
    Tournament(TournamentInfo),
    CashGame,
    HoldUp,
}

impl GameInfo {
    fn parse(input: &str) -> IResult<&str, GameInfo> {
        let winamax = tag("Winamax Poker - ");
        let tournament = preceded(
            tag("Tournament "),
            map(TournamentInfo::parse, GameInfo::Tournament),
        );
        let cashgame = map(tag("CashGame"), |_| GameInfo::CashGame);
        let hold_up = map(
            preceded(tag("HOLD-UP"), delimited(tag(" \""), alpha1, char('"'))),
            |_| GameInfo::HoldUp,
        );
        let (input, game_info) =
            preceded(winamax, alt((tournament, cashgame, hold_up))).parse(input)?;
        Ok((input, game_info))
    }
}


impl IntoPy<PyObject> for GameInfo {
    fn into_py(self, py: Python<'_>) -> PyObject {
        match self {
            GameInfo::Tournament(tournament) => PyString::new(py, "WESTERN").into(),
            GameInfo::CashGame => PyString::new(py, "CG").into(),
            GameInfo::HoldUp => PyString::new(py, "holdup").into(),
        }
    }
}



#[pyclass(module = "holdem_suite_parser", get_all)]
#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct Blinds {
    pub ante: Option<f64>,
    pub small_blind: f64,
    pub big_blind: f64,
}

fn parse_blind(input: &str) -> IResult<&str, f64> {
    let (input, blind) = terminated(double, opt(tag("€"))).parse(input)?;
    Ok((input, blind))
}

impl Blinds {
    fn parse(input: &str) -> IResult<&str, Blinds> {
        let small_big_pair = separated_pair(parse_blind, tag("/"), parse_blind);
        let small_big = map(small_big_pair, |(small_blind, big_blind)| Blinds {
            ante: None,
            small_blind,
            big_blind,
        });

        let ante_blinds_tuple = tuple((
            terminated(parse_blind, tag("/")),
            terminated(parse_blind, tag("/")),
            parse_blind,
        ));

        let ante_blinds = map(ante_blinds_tuple, |(ante, small_blind, big_blind)| Blinds {
            ante: Some(ante),
            small_blind,
            big_blind,
        });
        let (input, blinds) = alt((ante_blinds, small_big)).parse(input)?;
        Ok((input, blinds))
    }
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum PokerType {
    HoldemNoLimit,
}

impl PokerType {
    fn parse(input: &str) -> IResult<&str, PokerType> {
        let (input, _) = tag("Holdem no limit").parse(input)?;
        Ok((input, PokerType::HoldemNoLimit))
    }
}

impl fmt::Display for PokerType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let poker_type = match self {
            PokerType::HoldemNoLimit => "Holdem No limit"
        };
        write!(f, "{}", poker_type)
    }
}

impl IntoPy<PyObject> for PokerType {
    fn into_py(self, py: Python) -> PyObject {
        PyString::new(py, &self.to_string()).into()
    }
}

#[pyclass(module = "holdem_suite_parser", get_all)]
#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct HandInfo {
    pub game_info: GameInfo,
    pub hand_id: String,
    pub poker_type: PokerType,
    pub blinds: Blinds,
    pub datetime: DateTime<Utc>,
}

impl HandInfo {
    fn parse(input: &str) -> IResult<&str, HandInfo> {
        let hand_id = preceded(tag("HandId: #"), take_while(|c: char| c != ' '));
        let datetime = terminated(not_line_ending, line_ending);
        let (input, (game_info, _, hand_id, _, poker_type, _, blinds, _, datetime)) = (
            GameInfo::parse,
            tag(" - "),
            hand_id,
            tag(" - "),
            PokerType::parse,
            char(' '),
            delimited(char('('), Blinds::parse, char(')')),
            tag(" - "),
            map_res(datetime, |s: &str| {
                Utc.datetime_from_str(s, "%Y/%m/%d %H:%M:%S %Z")
            }),
        )
            .parse(input)?;
        Ok((
            input,
            HandInfo {
                game_info,
                hand_id: hand_id.to_owned(),
                poker_type,
                blinds,
                datetime,
            },
        ))
    }
}

fn parse_table_name_tournament(input: &str) -> IResult<&str, TableName> {
    let (input, (name, tournament_id, table_id)) = tuple((
        terminated(take_while(|c| c != '('), tag("(")),
        terminated(nom::character::complete::u32, tag(")#")),
        nom::character::complete::u32,
    ))
    .parse(input)?;
    Ok((
        input,
        TableName::Tournament(name.to_owned(), tournament_id, table_id),
    ))
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum TableName {
    Tournament(String, u32, u32),
    CashGame(String),
}

impl TableName {
    fn parse(input: &str) -> IResult<&str, TableName> {
        let parse_cashgame = map(take_while(|c| c != '\''), |name: &str| {
            TableName::CashGame(name.to_owned())
        });
        let (input, table_name) = delimited(
            tag("'"),
            alt((parse_table_name_tournament, parse_cashgame)),
            tag("'"),
        )
        .parse(input)?;
        Ok((input, table_name))
    }
}

impl Display for TableName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let table_name = match self {
            TableName::Tournament(name, tournament_id, table_id) =>  name,
            TableName::CashGame(name) => name,
        };
        write!(f, "{}", table_name)
    }
}

impl IntoPy<PyObject> for TableName {
    fn into_py(self, py: Python<'_>) -> PyObject {
        PyString::new(py, &self.to_string()).into()
    }
}


#[derive(Debug, PartialEq, Serialize, Clone)]
#[serde(tag = "type")]
pub enum MoneyType {
    RealMoney,
    PlayMoney,
}

impl MoneyType {
    fn parse(input: &str) -> IResult<&str, MoneyType> {
        let (input, money_type) = alt((tag("real money"), tag("play money"))).parse(input)?;
        Ok((
            input,
            match money_type {
                "real money" => MoneyType::RealMoney,
                "play money" => MoneyType::PlayMoney,
                _ => unreachable!(),
            },
        ))
    }
}

impl Display for MoneyType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let money_type = match self {
            MoneyType::PlayMoney => "play_money",
            MoneyType::RealMoney => "real_money",
        };
        write!(f, "{}", money_type)
    }
}

impl IntoPy<PyObject> for MoneyType {
    fn into_py(self, py: Python<'_>) -> PyObject {
       PyString::new(py, &self.to_string()).into()
    }
}

#[pyclass(module = "holdem_suite_parser", get_all)]
#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct TableInfo {
    pub table_name: TableName,
    pub max_players: u32,
    pub currency: MoneyType,
    pub button: u32,
}

impl TableInfo {
    fn parse(input: &str) -> IResult<&str, TableInfo> {
        let (input, (table_name, _, max_players, currency, _, button, _, _)) = tuple((
            preceded(tag("Table: "), TableName::parse),
            tag(" "),
            terminated(nom::character::complete::u32, tag("-max ")),
            delimited(tag("("), MoneyType::parse, tag(")")),
            tag(" "),
            preceded(tag("Seat #"), nom::character::complete::u32),
            tag(" is the button"),
            line_ending,
        ))
        .parse(input)?;
        Ok((
            input,
            TableInfo {
                table_name,
                max_players,
                currency,
                button,
            },
        ))
    }
}

#[pyclass(module = "holdem_suite_parser", get_all)]
#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct Seat {
    pub seat_number: u32,
    pub player_name: String,
    pub stack: f64,
    pub bounty: Option<f64>,
}

impl Seat {
    fn parse(input: &str) -> IResult<&str, Seat> {
        let stack_bounty = tuple((
            terminated(double, opt(tag("€"))),
            opt(preceded(
                tag(", "),
                terminated(double, terminated(opt(tag("€")), tag(" bounty"))),
            )),
        ));
        let (input, (seat_number, _, player_name, _, (stack, bounty))) = tuple((
            preceded(tag("Seat "), nom::character::complete::u32),
            tag(": "),
            take_until(" ("),
            tag(" "),
            delimited(tag("("), stack_bounty, tag(")")),
        ))
        .parse(input)?;
        Ok((
            input,
            Seat {
                seat_number,
                player_name: player_name.to_owned(),
                stack,
                bounty,
            },
        ))
    }
}

fn parse_seats(input: &str) -> IResult<&str, Vec<Seat>> {
    let (input, seats) = many1(terminated(Seat::parse, line_ending)).parse(input)?;
    Ok((input, seats))
}

fn parse_amount(input: &str) -> IResult<&str, f64> {
    let (input, amount) = terminated(double, opt(tag("€"))).parse(input)?;
    Ok((input, amount))
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum PostType {
    BigBlind(f64),
    SmallBlind(f64),
    Ante(f64),
}

impl PostType {
    fn parse(input: &str) -> IResult<&str, PostType> {
        let small_blind = map(
            preceded(tag("small blind "), parse_amount),
            PostType::SmallBlind,
        );
        let big_blind = map(
            preceded(tag("big blind "), parse_amount),
            PostType::BigBlind,
        );
        let ante = map(preceded(tag("ante "), parse_amount), PostType::Ante);
        let (input, post_type) = alt((small_blind, big_blind, ante)).parse(input)?;
        Ok((input, post_type))
    }
}

#[derive(Debug, PartialEq, Serialize, Clone)]
#[serde(tag = "type")]
pub enum ActionType {
    Bet { amount: f64 },
    Call { amount: f64 },
    Check,
    Fold,
    Post(PostType),
    Raise { to_call: f64, amount: f64 },
    Collect,
    Shows,
}

impl ActionType {
    fn parse(input: &str) -> IResult<&str, ActionType> {
        let (input, action_type) = alt((
            map(preceded(tag("posts "), PostType::parse), ActionType::Post),
            map(tag("checks"), |_| ActionType::Check),
            map(tag("folds"), |_| ActionType::Fold),
            // TODO: handle "all-in"
            map(preceded(tag("calls "), parse_amount), |x| {
                ActionType::Call { amount: x }
            }),
            map(preceded(tag("bets "), parse_amount), |x| ActionType::Bet {
                amount: x,
            }),
            map(
                preceded(
                    tag("raises "),
                    tuple((parse_amount, tag(" to "), parse_amount)),
                ),
                |(to_call, _, amount)| ActionType::Raise { to_call, amount },
            ),
            // "collected" and "shows" are not actual actions, we don't care about the rest
            // of the line
            map(preceded(tag("collected"), take_until("\n")), |_| {
                ActionType::Collect
            }),
            map(preceded(tag("shows"), take_until("\n")), |_| {
                ActionType::Shows
            }),
        ))(input)?;
        Ok((input, action_type))
    }
}

impl fmt::Display for ActionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ActionType::Bet { .. } => "bet",
                ActionType::Call { .. } => "call",
                ActionType::Check => "check",
                ActionType::Fold => "fold",
                ActionType::Post(_) => "post",
                ActionType::Raise { .. } => "raise",
                ActionType::Collect => "collect",
                ActionType::Shows => "show",
            }
        )
    }
}

#[pyclass(module = "holdem_suite_parser")]
#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct Action {
    pub player_name: String,
    pub action: ActionType,
    pub is_all_in: bool,
}

impl Action {
    fn parse(input: &str) -> IResult<&str, Action> {
        let (input, (player_name_vec, (action_type, all_in))) =
            // anychar would work too, but we want to fail on newlines for robustness
            many_till(none_of("\n"), delimited(tag(" "), pair(ActionType::parse, opt(tag(" and is all-in"))), tag("\n")))(input)?;
        Ok((
            input,
            Action {
                player_name: player_name_vec.into_iter().collect(),
                action: action_type,
                is_all_in: all_in.is_some(),
            },
        ))
    }
}


#[pymethods]
impl Action {
    #[getter]
    fn player_name(&self) -> PyResult<String> {
        Ok(self.player_name.to_owned())
    }


    #[getter]
    fn action_type(&self) -> PyResult<String> {
        Ok(self.action.to_string())
    }

    #[getter]
    fn action_amount(&self) -> PyResult<Option<f64>> {
        let amount = match self.action {
            ActionType::Bet {amount} => Some(amount),
            ActionType::Call {amount} => Some(amount),
            ActionType::Raise {to_call: _, amount} => Some(amount),
            _ => None,
        };
        Ok(amount)

    }
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum Rank {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

impl Rank {
    fn parse(input: &str) -> IResult<&str, Rank> {
        let (input, rank) = alt((
            map(tag("2"), |_| Rank::Two),
            map(tag("3"), |_| Rank::Three),
            map(tag("4"), |_| Rank::Four),
            map(tag("5"), |_| Rank::Five),
            map(tag("6"), |_| Rank::Six),
            map(tag("7"), |_| Rank::Seven),
            map(tag("8"), |_| Rank::Eight),
            map(tag("9"), |_| Rank::Nine),
            map(tag("T"), |_| Rank::Ten),
            map(tag("J"), |_| Rank::Jack),
            map(tag("Q"), |_| Rank::Queen),
            map(tag("K"), |_| Rank::King),
            map(tag("A"), |_| Rank::Ace),
        ))(input)?;
        Ok((input, rank))
    }

    fn parse2(input: &str) -> IResult<&str, Rank> {
        let (input, rank) = alt((
            map(tag("2"), |_| Rank::Two),
            map(tag("3"), |_| Rank::Three),
            map(tag("4"), |_| Rank::Four),
            map(tag("5"), |_| Rank::Five),
            map(tag("6"), |_| Rank::Six),
            map(tag("7"), |_| Rank::Seven),
            map(tag("8"), |_| Rank::Eight),
            map(tag("9"), |_| Rank::Nine),
            map(tag("Tens"), |_| Rank::Ten),
            map(tag("Ten"), |_| Rank::Ten),
            map(tag("Jacks"), |_| Rank::Jack),
            map(tag("Jack"), |_| Rank::Jack),
            map(tag("Queens"), |_| Rank::Queen),
            map(tag("Queen"), |_| Rank::Queen),
            map(tag("Kings"), |_| Rank::King),
            map(tag("King"), |_| Rank::King),
            map(tag("Aces"), |_| Rank::Ace),
            map(tag("Ace"), |_| Rank::Ace),
        ))(input)?;
        Ok((input, rank))
    }
}

impl fmt::Display for Rank {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Rank::Two => "2",
                Rank::Three => "3",
                Rank::Four => "4",
                Rank::Five => "5",
                Rank::Six => "6",
                Rank::Seven => "7",
                Rank::Eight => "8",
                Rank::Nine => "9",
                Rank::Ten => "T",
                Rank::Jack => "J",
                Rank::Queen => "Q",
                Rank::King => "K",
                Rank::Ace => "A",
            }
        )
    }
}

impl IntoPy<PyObject> for Rank {
    fn into_py(self, py: Python<'_>) -> PyObject {
        PyString::new(py, &self.to_string()).into()
    }
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum Suit {
    Spades,
    Hearts,
    Diamonds,
    Clubs,
}

impl Suit {
    fn parse(input: &str) -> IResult<&str, Suit> {
        let (input, suit) = alt((
            map(tag("s"), |_| Suit::Spades),
            map(tag("h"), |_| Suit::Hearts),
            map(tag("d"), |_| Suit::Diamonds),
            map(tag("c"), |_| Suit::Clubs),
        ))(input)?;
        Ok((input, suit))
    }
}

impl fmt::Display for Suit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Suit::Spades => "s",
                Suit::Hearts => "h",
                Suit::Diamonds => "d",
                Suit::Clubs => "c",
            }
        )
    }
}

impl IntoPy<PyObject> for Suit {
    fn into_py(self, py: Python<'_>) -> PyObject {
        PyString::new(py, &self.to_string()).into()
    }
}

#[pyclass(module = "holdem_suite_parser", get_all)]
#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct Card {
    pub rank: Rank,
    pub suit: Suit,
}

impl Card {
    fn parse(input: &str) -> IResult<&str, Card> {
        let (input, (rank, suit)) = tuple((Rank::parse, Suit::parse))(input)?;
        Ok((input, Card { rank, suit }))
    }
}

impl fmt::Display for Card {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.rank, self.suit)
    }
}


#[pyclass(module = "holdem_suite_parser", get_all)]
#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct HoleCards {
    pub card1: Card,
    pub card2: Card,
}

impl HoleCards {
    fn parse(input: &str) -> IResult<&str, HoleCards> {
        let (input, (card1, card2)) = separated_pair(Card::parse, tag(" "), Card::parse)(input)?;
        Ok((input, HoleCards { card1, card2 }))
    }
}



#[pyclass(module = "holdem_suite_parser", get_all)]
#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct DealtToHero {
    pub player_name: String,
    pub hole_cards: HoleCards,
}

impl DealtToHero {
    fn parse(input: &str) -> IResult<&str, DealtToHero> {
        let hole_cards = delimited(tag(" ["), HoleCards::parse, tag("]"));
        let (input, (player_name_vec, hole_cards)) = delimited(
            tag("Dealt to "),
            many_till(anychar, hole_cards),
            line_ending,
        )(input)?;
        Ok((
            input,
            DealtToHero {
                player_name: player_name_vec.into_iter().collect(),
                hole_cards,
            },
        ))
    }
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum StreetType {
    Preflop,
    Flop,
    Turn,
    River,
    Showdown,
}

impl fmt::Display for StreetType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                StreetType::Preflop => "preflop",
                StreetType::Flop => "flop",
                StreetType::Turn => "turn",
                StreetType::River => "river",
                StreetType::Showdown => "showdown",
            }
        )
    }
}

impl IntoPy<PyObject> for StreetType {
    fn into_py(self, py: Python<'_>) -> PyObject {
       PyString::new(py, &self.to_string()).into()
    }
}

#[pyclass(module = "holdem_suite_parser", get_all)]
#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct Street {
    pub street_type: StreetType,
    pub actions: Vec<Action>,
}

impl Street {
    fn parse(input: &str) -> IResult<&str, Street> {
        let street_type = alt((
            map(tag("*** PRE-FLOP ***"), |_| StreetType::Preflop),
            map(tag("*** FLOP ***"), |_| StreetType::Flop),
            map(tag("*** TURN ***"), |_| StreetType::Turn),
            map(tag("*** RIVER ***"), |_| StreetType::River),
            map(tag("*** SHOW DOWN ***"), |_| StreetType::Showdown),
        ));

        let (input, (street_type, _, actions)) = tuple((
            street_type,
            many_till(anychar, line_ending), // ignore partial boards
            many0(Action::parse),
        ))(input)?;
        Ok((
            input,
            Street {
                street_type,
                actions,
            },
        ))
    }
}

#[pyclass(module = "holdem_suite_parser", get_all)]
#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct Board {
    pub cards: Vec<Option<Card>>,
}

impl Board {
    fn parse(input: &str) -> IResult<&str, Board> {
        let (input, (_, cards, _)) = tuple((
            tag("Board: ["),
            terminated(separated_list0(tag(" "), Card::parse), tag("]")),
            line_ending,
        ))(input)?;
        let mut cards: Vec<Option<Card>> = cards.into_iter().map(Some).collect();
        cards.resize_with(5, || None);
        Ok((input, Board { cards }))
    }
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum SummaryResult {
    Won(f64),
    Lost,
}

impl IntoPy<PyObject> for SummaryResult {
    fn into_py(self, py: Python<'_>) -> PyObject {
        let amount = match self {
            SummaryResult::Won(amount) => amount,
            SummaryResult::Lost => 0.0
        };
       PyFloat::new(py, amount).into()
    }
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum HandCategory {
    HighCard(Rank),
    Pair(Rank),
    TwoPair(Rank, Rank),
    ThreeOfAKind(Rank),
    Straight(Rank),
    Flush(Rank),
    Full(Rank, Rank),
    FourOfAKind(Rank),
    StraightFlush(Rank),
}

impl HandCategory {
    fn parse(input: &str) -> IResult<&str, HandCategory> {
        let high_card = preceded(
            tag("High card : "),
            map(Rank::parse2, HandCategory::HighCard),
        );
        let pair = preceded(tag("One pair : "), map(Rank::parse2, HandCategory::Pair));

        let two_pairs = preceded(
            tag("Two pairs : "),
            map(
                separated_pair(Rank::parse2, tag(" and "), Rank::parse2),
                |(rank1, rank2)| HandCategory::TwoPair(rank1, rank2),
            ),
        );

        let three_of_a_kind = preceded(
            tag("Trips of "),
            map(Rank::parse2, HandCategory::ThreeOfAKind),
        );

        let four_of_a_kind = preceded(
            tag("Four of a kind : "),
            map(Rank::parse2, HandCategory::FourOfAKind),
        );

        let full = preceded(
            tag("Full of "),
            map(
                separated_pair(Rank::parse2, tag(" and "), Rank::parse2),
                |(rank1, rank2)| HandCategory::Full(rank1, rank2),
            ),
        );

        let straight = delimited(
            tag("Straight "),
            map(Rank::parse2, HandCategory::Straight),
            tag(" high"),
        );

        let flush = preceded(tag("Flush "), map(Rank::parse2, HandCategory::Flush));

        let straight_flush = preceded(
            tag("Straight flush "),
            map(Rank::parse2, HandCategory::StraightFlush),
        );

        let (input, result_cards) = alt((
            high_card,
            pair,
            two_pairs,
            three_of_a_kind,
            four_of_a_kind,
            full,
            straight,
            flush,
            straight_flush,
        ))(input)?;
        Ok((input, result_cards))
    }
}

impl IntoPy<PyObject> for HandCategory {
    fn into_py(self, py: Python<'_>) -> PyObject {
       PyString::new(py, "Not implemented").into()
    }
}

#[pyclass(module = "holdem_suite_parser", get_all)]
#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct SummaryPlayer {
    pub name: String,
    pub seat: u32,
    pub hole_cards: Option<HoleCards>,
    pub result: SummaryResult,
    pub hand_category: Option<HandCategory>,
}

impl SummaryPlayer {
    fn parse(input: &str) -> IResult<&str, SummaryPlayer> {
        let position = delimited(tag(" ("), take_until(")"), tag(")"));
        let showed = delimited(tag(" showed ["), HoleCards::parse, tag("] and"));
        let result = alt((
            map(preceded(tag(" won "), parse_amount), |amount| {
                SummaryResult::Won(amount)
            }),
            map(tag(" lost"), |_| SummaryResult::Lost),
        ));
        let position_show_result = tuple((
            opt(position),
            opt(showed),
            result,
            opt(preceded(tag(" with "), HandCategory::parse)),
            alt((line_ending, eof)),
        ));

        let winner_seat = preceded(tag("Seat "), nom::character::complete::u32);
        let winner_name_vec = preceded(tag(": "), many_till(anychar, position_show_result));
        let (input, (winner_seat, (winner_name_vec, (_, showed, result, hand_category, _)))) =
            tuple((winner_seat, winner_name_vec))(input)?;
        Ok((
            input,
            SummaryPlayer {
                name: winner_name_vec.into_iter().collect(),
                seat: winner_seat,
                hole_cards: showed,
                result,
                hand_category,
            },
        ))
    }
}

#[pyclass(module = "holdem_suite_parser", get_all)]
#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct Summary {
    pub pot: f64,
    pub rake: Option<f64>,
    pub players: Vec<SummaryPlayer>,
    pub board: Option<Board>,
}

impl Summary {
    fn parse(input: &str) -> IResult<&str, Summary> {
        let pot_amount = delimited(tag("Total pot "), parse_amount, tag(" | "));
        let rake = alt((
            map(preceded(tag("Rake "), parse_amount), Some),
            map(tag("No rake"), |_| None),
        ));
        let (input, (pot_amount, rake, _, board, players)) = tuple((
            pot_amount,
            rake,
            line_ending,
            opt(Board::parse),
            many1(SummaryPlayer::parse),
        ))(input)?;
        Ok((
            input,
            Summary {
                pot: pot_amount,
                rake,
                players,
                board,
            },
        ))
    }
}

#[pyclass(module = "holdem_suite_parser", get_all)]
#[derive(Debug, PartialEq, Serialize)]
pub struct Hand {
    pub hand_info: HandInfo,
    pub table_info: TableInfo,
    pub seats: Vec<Seat>,
    pub dealt_cards: DealtToHero,
    pub streets: Vec<Street>,
    pub summary: Summary,
}

impl Hand {
    pub fn parse(input: &str) -> IResult<&str, Hand> {
        let (input, (_, hand_info, table_info, seats, _, dealt_cards, (streets, _), summary)) =
            tuple((
                take_till(|c: char| c.is_alphabetic()),
                HandInfo::parse,
                TableInfo::parse,
                parse_seats,
                take_until("Dealt to"),
                DealtToHero::parse,
                many_till(
                    Street::parse,
                    terminated(tag("*** SUMMARY ***"), line_ending),
                ),
                Summary::parse,
            ))(input)?;
        Ok((
            input,
            Hand {
                hand_info,
                table_info,
                seats,
                dealt_cards,
                summary,
                streets,
            },
        ))
    }
}

pub fn parse_hands(input: &str) -> IResult<&str, Vec<Hand>> {
    separated_list1(line_ending, Hand::parse)(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_tournament_info() {
        let input = "\"WESTERN\" buyIn: 0.90€ + 0.10€ level: 6";
        let expected = TournamentInfo {
            name: String::from("WESTERN"),
            buy_in: 0.90,
            rake: 0.10,
            level: 6,
        };
        let (_, actual) = TournamentInfo::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_game_info_tournament() {
        let input = "Winamax Poker - Tournament \"WESTERN\" buyIn: 0.90€ + 0.10€ level: 6";
        let expected = GameInfo::Tournament(TournamentInfo {
            name: String::from("WESTERN"),
            buy_in: 0.90,
            rake: 0.10,
            level: 6,
        });
        let (_, actual) = GameInfo::parse(input).unwrap();
        assert_eq!(expected, actual);
    }
    #[test]
    fn test_parse_game_info_cashgame() {
        let input = "Winamax Poker - CashGame";
        let expected = GameInfo::CashGame;
        let (_, actual) = GameInfo::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_blind_chips() {
        let input = "60";
        let expected = 60.0;
        assert_eq!(expected, parse_blind(input).unwrap().1);
    }

    #[test]
    fn test_parse_blind_money() {
        let input = "60€";
        let expected = 60.0;
        assert_eq!(expected, parse_blind(input).unwrap().1);
    }

    #[test]
    fn test_parse_blinds_chips() {
        let input = "60/250/500";
        let expected = Blinds {
            ante: Some(60.0),
            small_blind: 250.0,
            big_blind: 500.0,
        };
        let (_, actual) = Blinds::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_blinds_no_ante() {
        let input = "250/500";
        let expected = Blinds {
            ante: None,
            small_blind: 250.0,
            big_blind: 500.0,
        };
        let (_, actual) = Blinds::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_hand_info_tournament() {
        let input = "Winamax Poker - Tournament \"WESTERN\" buyIn: 0.90€ + 0.10€ level: 6 - HandId: \
        #2815488303912976462-15-1684698584 - Holdem no limit (60/250/500) - 2023/05/21 19:49:44 UTC\n";

        let expected = HandInfo {
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
        let (_, actual) = HandInfo::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_hand_info_hold_up() {
        let input = "Winamax Poker - HOLD-UP \"Colorado\" - HandId: #18559747-238220-1687014585 - Holdem no limit (0.01€/0.02€) - 2023/06/17 15:09:45 UTC\n";

        let expected = HandInfo {
            game_info: GameInfo::HoldUp,
            hand_id: String::from("18559747-238220-1687014585"),
            poker_type: PokerType::HoldemNoLimit,
            blinds: Blinds {
                ante: None,
                small_blind: 0.01,
                big_blind: 0.02,
            },
            datetime: Utc.with_ymd_and_hms(2023, 6, 17, 15, 9, 45).unwrap(),
        };
        let (_, actual) = HandInfo::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_hand_info_cash_game() {
        let input = "Winamax Poker - CashGame - HandId: #18567763-280-1687022958 - Holdem no limit (0.01€/0.02€) - 2023/06/17 17:29:18 UTC\n";
        let expected = HandInfo {
            game_info: GameInfo::CashGame,
            hand_id: String::from("18567763-280-1687022958"),
            poker_type: PokerType::HoldemNoLimit,
            blinds: Blinds {
                ante: None,
                small_blind: 0.01,
                big_blind: 0.02,
            },
            datetime: Utc.with_ymd_and_hms(2023, 6, 17, 17, 29, 18).unwrap(),
        };

        let (_, actual) = HandInfo::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_table_name_tournament() {
        let input = "'Kill The Fish(651864208)#003'";
        let expected = TableName::Tournament(String::from("Kill The Fish"), 651864208, 3);
        let (_, actual) = TableName::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_table_name_cashgame() {
        let input = "'Nice 17'";
        let expected = TableName::CashGame(String::from("Nice 17"));
        let (_, actual) = TableName::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_table_name_holdup() {
        let input = "'Colorado'";
        let expected = TableName::CashGame(String::from("Colorado"));
        let (_, actual) = TableName::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_table_info_tournament() {
        let input = "Table: 'WESTERN(655531954)#077' 6-max (real money) Seat #3 is the button\n";
        let expected = TableInfo {
            table_name: TableName::Tournament(String::from("WESTERN"), 655531954, 77),
            max_players: 6,
            currency: MoneyType::RealMoney,
            button: 3,
        };
        let (_, actual) = TableInfo::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_table_info_cash_game_play_money() {
        let input = "Table: 'Wichita 05' 5-max (play money) Seat #1 is the button\n";
        let expected = TableInfo {
            table_name: TableName::CashGame(String::from("Wichita 05")),
            max_players: 5,
            currency: MoneyType::PlayMoney,
            button: 1,
        };
        let (_, actual) = TableInfo::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_seat_chips_with_bounty() {
        let input = "Seat 5: WinterSound (20000, 0.45€ bounty)\n";
        let expected = Seat {
            seat_number: 5,
            player_name: String::from("WinterSound"),
            stack: 20000.0,
            bounty: Some(0.45),
        };
        let (_, actual) = Seat::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_seat_chips_with_bounty_play_money() {
        let input = "Seat 5: WinterSound (20000, 13.50 bounty)\n";
        let expected = Seat {
            seat_number: 5,
            player_name: String::from("WinterSound"),
            stack: 20000.0,
            bounty: Some(13.50),
        };
        let (_, actual) = Seat::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_seat_chips_without_bounty() {
        let input = "Seat 3: WinterSound (18744)\n";
        let expected = Seat {
            seat_number: 3,
            player_name: String::from("WinterSound"),
            stack: 18744.0,
            bounty: None,
        };
        let (_, actual) = Seat::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_seat_cashgame() {
        let input = "Seat 3: Winter Sound (0.50€)\n";
        let expected = Seat {
            seat_number: 3,
            player_name: String::from("Winter Sound"),
            stack: 0.50,
            bounty: None,
        };
        let (_, actual) = Seat::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_seats() {
        let input = "Seat 1: WinterSound (20000, 0.45€ bounty)\nSeat 2: Player Two (18744)\n";
        let expected = vec![
            Seat {
                seat_number: 1,
                player_name: String::from("WinterSound"),
                stack: 20000.0,
                bounty: Some(0.45),
            },
            Seat {
                seat_number: 2,
                player_name: String::from("Player Two"),
                stack: 18744.0,
                bounty: None,
            },
        ];
        let (_, actual) = parse_seats(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_seats_6() {
        let input = concat!(
            "Seat 1: Anonymous1 (23940, 0.45€ bounty)\n",
            "Seat 2: Anonymous 2 (14388, 0.45€ bounty)\n",
            "Seat 3: Anonymous 3 (20410, 0.45€ bounty)\n",
            "Seat 4: Anonymous4 (15425, 0.45€ bounty)\n",
            "Seat 5: WinterSound (14285, 0.45€ bounty)\n",
            "Seat 6: Anonymous5 (109973, 1€ bounty)\n",
            "*** ANTE/BLINDS ***\n",
        );
        let expected = vec![
            Seat {
                seat_number: 1,
                player_name: String::from("Anonymous1"),
                stack: 23940.0,
                bounty: Some(0.45),
            },
            Seat {
                seat_number: 2,
                player_name: String::from("Anonymous 2"),
                stack: 14388.0,
                bounty: Some(0.45),
            },
            Seat {
                seat_number: 3,
                player_name: String::from("Anonymous 3"),
                stack: 20410.0,
                bounty: Some(0.45),
            },
            Seat {
                seat_number: 4,
                player_name: String::from("Anonymous4"),
                stack: 15425.0,
                bounty: Some(0.45),
            },
            Seat {
                seat_number: 5,
                player_name: String::from("WinterSound"),
                stack: 14285.0,
                bounty: Some(0.45),
            },
            Seat {
                seat_number: 6,
                player_name: String::from("Anonymous5"),
                stack: 109973.0,
                bounty: Some(1.0),
            },
        ];
        let (_, actual) = parse_seats(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_post_type_sb() {
        let input = "small blind 250\n";
        let expected = PostType::SmallBlind(250.0);
        let (_, actual) = PostType::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_post_type_bb_cash() {
        let input = "big blind 0.02€\n";
        let expected = PostType::BigBlind(0.02);
        let (_, actual) = PostType::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_post_type_ante_chips() {
        let input = "big blind 60\n";
        let expected = PostType::BigBlind(60.0);
        let (_, actual) = PostType::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_action_type_post_bb() {
        let input = "posts big blind 500\n";
        let expected = ActionType::Post(PostType::BigBlind(500.0));
        let (_, actual) = ActionType::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_action_type_check() {
        let input = "checks\n";
        let expected = ActionType::Check;
        let (_, actual) = ActionType::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_action_type_call() {
        let input = "calls 500\n";
        let expected = ActionType::Call { amount: 500.0 };
        let (_, actual) = ActionType::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_action_type_bet() {
        let input = "bets 500\n";
        let expected = ActionType::Bet { amount: 500.0 };
        let (_, actual) = ActionType::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_action_type_raise() {
        let input = "raises 500 to 1000\n";
        let expected = ActionType::Raise {
            to_call: 500.0,
            amount: 1000.0,
        };
        let (_, actual) = ActionType::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_action_fold() {
        let input = "As 2 carrot folds\n";
        let expected = Action {
            player_name: String::from("As 2 carrot"),
            action: ActionType::Fold,
            is_all_in: false,
        };
        let (_, actual) = Action::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_action_check() {
        let input = "Player One checks\n";
        let expected = Action {
            player_name: String::from("Player One"),
            action: ActionType::Check,
            is_all_in: false,
        };
        let (_, actual) = Action::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_action_raises() {
        let input = "Player One raises 500 to 1000\n";
        let expected = Action {
            player_name: String::from("Player One"),
            action: ActionType::Raise {
                to_call: 500.0,
                amount: 1000.0,
            },
            is_all_in: false,
        };
        let (_, actual) = Action::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_dealt_to() {
        let input = "Dealt to Player One [Ks 9s]\n";
        let expected = DealtToHero {
            player_name: String::from("Player One"),
            hole_cards: HoleCards {
                card1: Card {
                    rank: Rank::King,
                    suit: Suit::Spades,
                },
                card2: Card {
                    rank: Rank::Nine,
                    suit: Suit::Spades,
                },
            },
        };
        let (_, actual) = DealtToHero::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_street() {
        let input =
            "*** FLOP *** [8s 7h 4h]\nPlayer One raises 500 to 1000\nPlayer Two calls 1000\n";
        let expected = Street {
            street_type: StreetType::Flop,
            actions: vec![
                Action {
                    player_name: String::from("Player One"),
                    action: ActionType::Raise {
                        to_call: 500.0,
                        amount: 1000.0,
                    },
                    is_all_in: false,
                },
                Action {
                    player_name: String::from("Player Two"),
                    action: ActionType::Call { amount: 1000.0 },
                    is_all_in: false,
                },
            ],
        };
        let (_, actual) = Street::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_board() {
        let input = "Board: [8s 7h 4h 3s 2h]\n";
        let expected = Board {
            cards: vec![
                Some(Card {
                    rank: Rank::Eight,
                    suit: Suit::Spades,
                }),
                Some(Card {
                    rank: Rank::Seven,
                    suit: Suit::Hearts,
                }),
                Some(Card {
                    rank: Rank::Four,
                    suit: Suit::Hearts,
                }),
                Some(Card {
                    rank: Rank::Three,
                    suit: Suit::Spades,
                }),
                Some(Card {
                    rank: Rank::Two,
                    suit: Suit::Hearts,
                }),
            ],
        };
        let (_, actual) = Board::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_hand_category_high() {
        let input = "High card : Ace";
        let expected = HandCategory::HighCard(Rank::Ace);
        let (_, actual) = HandCategory::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_hand_category_pair() {
        let input = "One pair : Aces";
        let expected = HandCategory::Pair(Rank::Ace);
        let (_, actual) = HandCategory::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_hand_category_two_pair() {
        let input = "Two pairs : Queens and 2";
        let expected = HandCategory::TwoPair(Rank::Queen, Rank::Two);
        let (_, actual) = HandCategory::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_hand_category_flush() {
        let input = "Flush Jack high";
        let expected = HandCategory::Flush(Rank::Jack);
        let (_, actual) = HandCategory::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_hand_category_full() {
        let input = "Full of 6 and 4";
        let expected = HandCategory::Full(Rank::Six, Rank::Four);
        let (_, actual) = HandCategory::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_hand_category_straight() {
        let input = "Straight Ten high";
        let expected = HandCategory::Straight(Rank::Ten);
        let (_, actual) = HandCategory::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_summary_player() {
        let input = "Seat 6: Alexarango (button) won 0.31€";
        let expected = SummaryPlayer {
            seat: 6,
            name: String::from("Alexarango"),
            result: SummaryResult::Won(0.31),
            hole_cards: None,
            hand_category: None,
        };
        let (_, actual) = SummaryPlayer::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_summary_player_showdown() {
        let input =
            "Seat 6: Alexarango (button) showed [8d Td] and won 0.36€ with Straight Ten high\n";
        let expected = SummaryPlayer {
            seat: 6,
            name: String::from("Alexarango"),
            result: SummaryResult::Won(0.36),
            hole_cards: Some(HoleCards {
                card1: Card {
                    rank: Rank::Eight,
                    suit: Suit::Diamonds,
                },
                card2: Card {
                    rank: Rank::Ten,
                    suit: Suit::Diamonds,
                },
            }),
            hand_category: Some(HandCategory::Straight(Rank::Ten)),
        };
        let (_, actual) = SummaryPlayer::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_summary_player_showdown_lost() {
        let input =
            "Seat 3: Player Two showed [Qd As] and won 0.36€ with Two pairs : Queens and 2\n";
        let expected = SummaryPlayer {
            seat: 3,
            name: String::from("Player Two"),
            result: SummaryResult::Won(0.36),
            hole_cards: Some(HoleCards {
                card1: Card {
                    rank: Rank::Queen,
                    suit: Suit::Diamonds,
                },
                card2: Card {
                    rank: Rank::Ace,
                    suit: Suit::Spades,
                },
            }),
            hand_category: Some(HandCategory::TwoPair(Rank::Queen, Rank::Two)),
        };
        let (_, actual) = SummaryPlayer::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_summary_no_flop_no_rake() {
        let input = "Total pot 2670 | No rake\nSeat 3: Player One won 2670\n\n";
        let expected = Summary {
            pot: 2670.0,
            rake: None,
            players: vec![SummaryPlayer {
                seat: 3,
                name: String::from("Player One"),
                result: SummaryResult::Won(2670.0),
                hole_cards: None,
                hand_category: None,
            }],
            board: None,
        };
        let (_, actual) = Summary::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_summary_with_rake() {
        let input =
            "Total pot 0.79€ | Rake 0.01€\nBoard: [8c 5h Ts Kd Td]\nSeat 3: Player One won 0.79€";
        let expected = Summary {
            pot: 0.79,
            rake: Some(0.01),
            players: vec![SummaryPlayer {
                seat: 3,
                name: String::from("Player One"),
                result: SummaryResult::Won(0.79),
                hole_cards: None,
                hand_category: None,
            }],
            board: Some(Board {
                cards: vec![
                    Some(Card {
                        rank: Rank::Eight,
                        suit: Suit::Clubs,
                    }),
                    Some(Card {
                        rank: Rank::Five,
                        suit: Suit::Hearts,
                    }),
                    Some(Card {
                        rank: Rank::Ten,
                        suit: Suit::Spades,
                    }),
                    Some(Card {
                        rank: Rank::King,
                        suit: Suit::Diamonds,
                    }),
                    Some(Card {
                        rank: Rank::Ten,
                        suit: Suit::Diamonds,
                    }),
                ],
            }),
        };
        let (_, actual) = Summary::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_summary_with_board() {
        let input =
            "Total pot 2670 | No rake\nBoard: [8s 7h 4h 3s 2h]\nSeat 3: Player One won 2670\n\n";
        let expected = Summary {
            pot: 2670.0,
            rake: None,
            players: vec![SummaryPlayer {
                seat: 3,
                name: String::from("Player One"),
                result: SummaryResult::Won(2670.0),
                hole_cards: None,
                hand_category: None,
            }],
            board: Some(Board {
                cards: vec![
                    Some(Card {
                        rank: Rank::Eight,
                        suit: Suit::Spades,
                    }),
                    Some(Card {
                        rank: Rank::Seven,
                        suit: Suit::Hearts,
                    }),
                    Some(Card {
                        rank: Rank::Four,
                        suit: Suit::Hearts,
                    }),
                    Some(Card {
                        rank: Rank::Three,
                        suit: Suit::Spades,
                    }),
                    Some(Card {
                        rank: Rank::Two,
                        suit: Suit::Hearts,
                    }),
                ],
            }),
        };
        let (_, actual) = Summary::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_summary_with_showdown() {
        let input = concat!(
            "Total pot 0.30€ | Rake 0.03€\n",
            "Board: [3s Ks Qh 2s 2c]\n",
            "Seat 2: Player One (big blind) showed [9c Kd] and won ",
            "0.30€ with One pair : Kings\n",
            "Seat 3: Player Two showed [Qd As] and lost with Two pairs : Queens and 2\n\n"
        );

        let expected = Summary {
            pot: 0.30,
            rake: Some(0.03),
            players: vec![
                SummaryPlayer {
                    seat: 2,
                    name: String::from("Player One"),
                    result: SummaryResult::Won(0.30),
                    hole_cards: Some(HoleCards {
                        card1: Card {
                            rank: Rank::Nine,
                            suit: Suit::Clubs,
                        },
                        card2: Card {
                            rank: Rank::King,
                            suit: Suit::Diamonds,
                        },
                    }),
                    hand_category: Some(HandCategory::Pair(Rank::King)),
                },
                SummaryPlayer {
                    seat: 3,
                    name: String::from("Player Two"),
                    result: SummaryResult::Lost,
                    hole_cards: Some(HoleCards {
                        card1: Card {
                            rank: Rank::Queen,
                            suit: Suit::Diamonds,
                        },
                        card2: Card {
                            rank: Rank::Ace,
                            suit: Suit::Spades,
                        },
                    }),
                    hand_category: Some(HandCategory::TwoPair(Rank::Queen, Rank::Two)),
                },
            ],
            board: Some(Board {
                cards: vec![
                    Some(Card {
                        rank: Rank::Three,
                        suit: Suit::Spades,
                    }),
                    Some(Card {
                        rank: Rank::King,
                        suit: Suit::Spades,
                    }),
                    Some(Card {
                        rank: Rank::Queen,
                        suit: Suit::Hearts,
                    }),
                    Some(Card {
                        rank: Rank::Two,
                        suit: Suit::Spades,
                    }),
                    Some(Card {
                        rank: Rank::Two,
                        suit: Suit::Clubs,
                    }),
                ],
            }),
        };
        let (_, actual) = Summary::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_hand() {
        let input = concat!(
            "Winamax Poker - Tournament \"WESTERN\" buyIn: 0.90€ + 0.10€ level: 7 - ",
            "HandId: #2815488303912976462-17-1684698755 - Holdem no limit (70/300/600)",
            " - 2023/05/21 19:52:35 UTC\n",
            "Table: 'WESTERN(1684698755)#004' 6-max (real money) Seat #3 is the button\n",
            "Seat 1: Anonymous1 (23940, 0.45€ bounty)\n",
            "Seat 2: Anonymous 2 (14388, 0.45€ bounty)\n",
            "Seat 3: Anonymous 3 (20410, 0.45€ bounty)\n",
            "Seat 4: Anonymous4 (15425, 0.45€ bounty)\n",
            "Seat 5: WinterSound (14285, 0.45€ bounty)\n",
            "Seat 6: Anonymous5 (109973, 1€ bounty)\n",
            "*** ANTE/BLINDS ***\n",
            "Anonymous5 posts ante 70\n",
            "Anonymous1 posts ante 70\n",
            "Anonymous 2 posts ante 70\n",
            "Anonymous 3 posts ante 70\n",
            "Anonymous4 posts ante 70\n",
            "WinterSound posts ante 70\n",
            "Anonymous5 posts small blind 300\n",
            "Anonymous1 posts big blind 60\n",
            "Dealt to WinterSound [6s Qh]\n",
            "*** PRE-FLOP ***\n",
            "Anonymous 2 folds\n",
            "Anonymous 3 raises 750 to 1350\n",
            "Anonymous4 folds\n",
            "WinterSound folds\n",
            "Anonymous5 folds\n",
            "Anonymous1 folds\n",
            "Anonymous 3 collected 2670 from pot\n",
            "*** SUMMARY ***\n",
            "Total pot 2670 | No rake\n",
            "Seat 3: Anonymous 3 won 2670\n\n"
        );

        let expected = Hand {
            hand_info: HandInfo {
                game_info: GameInfo::Tournament(TournamentInfo {
                    name: String::from("WESTERN"),
                    buy_in: 0.90,
                    rake: 0.10,
                    level: 7,
                }),
                hand_id: String::from("2815488303912976462-17-1684698755"),
                poker_type: PokerType::HoldemNoLimit,
                blinds: Blinds {
                    ante: Some(70.0),
                    small_blind: 300.0,
                    big_blind: 600.0,
                },
                datetime: Utc.with_ymd_and_hms(2023, 5, 21, 19, 52, 35).unwrap(),
            },
            table_info: TableInfo {
                table_name: TableName::Tournament(String::from("WESTERN"), 1684698755, 4),
                max_players: 6,
                currency: MoneyType::RealMoney,
                button: 3,
            },
            seats: vec![
                Seat {
                    seat_number: 1,
                    player_name: String::from("Anonymous1"),
                    stack: 23940.0,
                    bounty: Some(0.45),
                },
                Seat {
                    seat_number: 2,
                    player_name: String::from("Anonymous 2"),
                    stack: 14388.0,
                    bounty: Some(0.45),
                },
                Seat {
                    seat_number: 3,
                    player_name: String::from("Anonymous 3"),
                    stack: 20410.0,
                    bounty: Some(0.45),
                },
                Seat {
                    seat_number: 4,
                    player_name: String::from("Anonymous4"),
                    stack: 15425.0,
                    bounty: Some(0.45),
                },
                Seat {
                    seat_number: 5,
                    player_name: String::from("WinterSound"),
                    stack: 14285.0,
                    bounty: Some(0.45),
                },
                Seat {
                    seat_number: 6,
                    player_name: String::from("Anonymous5"),
                    stack: 109973.0,
                    bounty: Some(1.0),
                },
            ],
            dealt_cards: DealtToHero {
                player_name: String::from("WinterSound"),
                hole_cards: HoleCards {
                    card1: Card {
                        rank: Rank::Six,
                        suit: Suit::Spades,
                    },
                    card2: Card {
                        rank: Rank::Queen,
                        suit: Suit::Hearts,
                    },
                },
            },
            streets: vec![Street {
                street_type: StreetType::Preflop,
                actions: vec![
                    Action {
                        player_name: String::from("Anonymous 2"),
                        action: ActionType::Fold,
                        is_all_in: false,
                    },
                    Action {
                        player_name: String::from("Anonymous 3"),
                        action: ActionType::Raise {
                            to_call: 750.0,
                            amount: 1350.0,
                        },
                        is_all_in: false,
                    },
                    Action {
                        player_name: String::from("Anonymous4"),
                        action: ActionType::Fold,
                        is_all_in: false,
                    },
                    Action {
                        player_name: String::from("WinterSound"),
                        action: ActionType::Fold,
                        is_all_in: false,
                    },
                    Action {
                        player_name: String::from("Anonymous5"),
                        action: ActionType::Fold,
                        is_all_in: false,
                    },
                    Action {
                        player_name: String::from("Anonymous1"),
                        action: ActionType::Fold,
                        is_all_in: false,
                    },
                    Action {
                        player_name: String::from("Anonymous 3"),
                        action: ActionType::Collect,
                        is_all_in: false,
                    },
                ],
            }],
            summary: Summary {
                pot: 2670.0,
                rake: None,
                players: vec![SummaryPlayer {
                    name: String::from("Anonymous 3"),
                    seat: 3,
                    hole_cards: None,
                    result: SummaryResult::Won(2670.0),
                    hand_category: None,
                }],
                board: None,
            },
        };
        let (_, actual) = Hand::parse(input).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_hands() {
        let data = include_str!("../samples/sample1.txt");
        let (_, hands) = parse_hands(data).unwrap();
        assert_eq!(hands.len(), 3);
    }

    #[test]
    fn test_parse_empty_streets() {
        let data = include_str!("../samples/sample2.txt");
        let (_, hands) = parse_hands(data).unwrap();
        println!("{:?}", hands);
        assert_eq!(hands.len(), 1);
    }

    #[test]
    fn test_parse_hands_play_money() {
        let data = include_str!("../samples/sample_expresso_play_money.txt");
        let (_, hands) = parse_hands(data).unwrap();
        assert_eq!(hands.len(), 1);
    }

    #[test]
    fn test_parse_hands_cash_play_money() {
        let data = include_str!("../samples/sample_cash_play_money.txt");
        let (_, hands) = parse_hands(data).unwrap();
        assert_eq!(hands.len(), 1);
    }
}
