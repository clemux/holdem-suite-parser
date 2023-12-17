from datetime import datetime

class TableInfo:
    table_name: str
    max_players: int
    currency: str
    button: int

class Blinds:
    ante: float | None
    small_blind: float
    big_bling: float

class HandInfo:
    game_info: str
    hand_id: str
    poker_type: str
    blinds: Blinds
    datetime: datetime

class Seat:
    seat_number: int
    player_name: str
    stack: float
    bounty: float | None

class Card:
    rank: str
    suit: str

class HoleCards:
    card1: Card
    card2: Card

class DealtToHero:
    player_name: str
    hole_cards: HoleCards

class Action:
    player_name: str
    action_type: str
    action_amount: float | None

class Street:
    street_type: str
    actions: list[Action]


class SummaryPlayer:
    name: str
    seat: int
    hole_cards: HoleCards | None
    result: float



class Board:
    cards: list[Card|None]

class Summary:
    pot: float
    rake: float | None
    players: list[SummaryPlayer]
    board: Board | None

class Hand:
    hand_info: HandInfo
    table_info: TableInfo
    seats: list[Seat]
    dealt_cards: DealtToHero
    streets: list[Street]
    summary: Summary

def parse_hands(input: str) -> list[Hand]:
    ...