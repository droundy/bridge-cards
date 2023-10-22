pub use ai::{choose_play, BridgeAi, Convention};
use bridge_deck::{Card, Cards};
use display_as::{with_template, DisplayAs, HTML};
use serde::{Deserialize, Serialize};

mod ai;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Bid {
    Pass,
    Double,
    Redouble,
    Suit(usize, bridge_deck::Suit),
    NT(usize),
}
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Action {
    Redeal,
    Bid(Seat, Bid),
    Play(Seat, Card),
    Name(String),
    ToggleCountForMe,
}
#[with_template(r#" onclick="send_message('"# serde_json::to_string(self).unwrap() r#"')""#)]
impl DisplayAs<HTML> for Action {}

impl PartialOrd for Bid {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Bid::*;
        if let Some(self_l) = self.level() {
            if let Some(other_l) = other.level() {
                if self == other {
                    return Some(std::cmp::Ordering::Equal);
                } else if self_l != other_l {
                    return self_l.partial_cmp(&other_l);
                } else if let NT(_) = self {
                    return Some(std::cmp::Ordering::Greater);
                } else if let NT(_) = other {
                    return Some(std::cmp::Ordering::Less);
                } else if let (Suit(_, me), Suit(_, them)) = (*self, *other) {
                    return me.partial_cmp(&them);
                }
            }
        }
        if self == other {
            return Some(std::cmp::Ordering::Equal);
        }
        if *self == Pass || other.is_contract() || *self == Redouble {
            return Some(std::cmp::Ordering::Greater);
        }
        if *other == Pass || self.is_contract() || *other == Redouble {
            return Some(std::cmp::Ordering::Less);
        }
        None
    }
}

#[test]
fn bid_comparisons() {
    assert!(Bid::NT(1) == Bid::NT(1));
    assert!(!(Bid::NT(1) > Bid::NT(1)));
    assert!(Bid::NT(1).is_contract());
    assert_eq!(Some(1), Bid::NT(1).level());
    assert_eq!(Some(3), Bid::NT(3).level());
}

#[with_template("[%" "%]" "bid.html")]
impl<'a> DisplayAs<HTML> for Bid {}

impl Bid {
    pub fn level(self) -> Option<usize> {
        match self {
            Bid::Suit(l, _) => Some(l),
            Bid::NT(l) => Some(l),
            _ => None,
        }
    }

    pub fn is_contract(self) -> bool {
        self.level().is_some()
    }

    pub fn same_suit(self, other: Bid) -> bool {
        match (self, other) {
            (Bid::Suit(_, me), Bid::Suit(_, she)) => me == she,
            (Bid::NT(_), Bid::NT(_)) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum PlayerName {
    #[default]
    None,
    Human(String),
    Robot(String),
}

impl PlayerName {
    pub fn not_human(&self) -> bool {
        !matches!(self, PlayerName::Human(_))
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct GameState<C> {
    pub names: Seated<PlayerName>,
    #[serde(skip)]
    pub connections: Seated<Option<C>>,
    #[serde(skip)]
    pub ai: Option<C>,
    pub hands: Seated<Cards>,
    pub original_hands: Seated<Cards>,

    pub count_for_me: Seated<bool>,

    pub hand_done: bool,

    pub dealer: Seat,
    pub bids: Vec<Bid>,

    pub lead: Option<Seat>,
    pub played: Vec<Card>,
    pub ns_tricks: usize,
    pub ew_tricks: usize,

    #[serde(skip)]
    pub last_action: Option<std::time::Instant>,

    #[serde(skip)]
    conventions: Vec<Convention>,
}

impl<C> Default for GameState<C> {
    fn default() -> Self {
        Self::new()
    }
}

impl<C> GameState<C> {
    pub fn new() -> GameState<C> {
        let mut deck = Cards::ALL;
        let north = deck.pick(13).unwrap();
        let south = deck.pick(13).unwrap();
        let east = deck.pick(13).unwrap();
        let west = deck;
        GameState {
            connections: Default::default(),
            ai: Default::default(),
            names: [
                PlayerName::None,
                PlayerName::None,
                PlayerName::None,
                PlayerName::None,
            ]
            .into(),
            hands: [south, west, north, east].into(),
            original_hands: [south, west, north, east].into(),
            count_for_me: [false, false, false, false].into(),
            hand_done: false,
            dealer: Seat::South,
            bids: Vec::new(),
            lead: None,
            played: Vec::new(),
            ns_tricks: 0,
            ew_tricks: 0,
            last_action: Some(std::time::Instant::now()),
            conventions: vec![Convention::sheets()],
        }
    }

    pub fn randomseat(&self) -> Option<Seat> {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        for _ in 0..30 {
            let seat: Seat = rng.gen::<usize>().into();
            if self.connections[seat].is_none() {
                return Some(seat);
            }
        }
        None
    }
    fn starting(&self) -> Option<bridge_solver::Starting> {
        let lead = self.lead?;
        let playing = self.turn()?;
        let mut unknown = self.hands[Seat::South]
            + self.hands[Seat::North]
            + self.hands[Seat::East]
            + self.hands[Seat::West];
        let mut hands = [Cards::EMPTY, Cards::EMPTY, Cards::EMPTY, Cards::EMPTY];
        unknown -= self.hands[playing];
        hands[(playing as usize + 4 - lead as usize) % 4] = self.hands[playing];
        if self.played.len() < 4 {
            for (i, c) in self.played.iter().cloned().enumerate() {
                hands[i] += Cards::singleton(c);
                unknown -= Cards::singleton(c);
            }
        }
        bridge_solver::Starting { hands, unknown }.check();
        if let Some(dummy) = self.dummy() {
            hands[(dummy as usize + 4 - lead as usize) % 4] += self.hands[dummy];
            unknown -= self.hands[dummy];
            Some(bridge_solver::Starting { hands, unknown })
        } else {
            Some(bridge_solver::Starting { hands, unknown })
        }
    }
    pub fn bid_convention(&self, bids: &[Bid]) -> Option<impl DisplayAs<HTML>> {
        self.conventions[0].refine(bids).map(|c| c.description())
    }
    pub fn bid_convention2(&self, bid: Bid, oldbids: &[Bid]) -> Option<impl DisplayAs<HTML>> {
        let mut bids = oldbids.to_vec();
        bids.push(bid);
        self.bid_convention(&bids)
    }
    pub fn bid_understood2(&self, bid: Bid, otherbids: &[Bid]) -> bool {
        self.conventions[0].refine2(bid, otherbids).is_some()
    }
    pub fn check_timeout(&mut self) -> bool {
        let now = std::time::Instant::now();
        if let Some(last_action) = self.last_action {
            if now.duration_since(last_action) > std::time::Duration::from_secs(60 * 60) {
                *self = GameState::new();
                true
            } else {
                self.last_action = Some(now);
                false
            }
        } else {
            false
        }
    }
    pub fn redeal(&mut self) {
        let dealer = self.dealer.next();
        let names = self.names.clone();
        let ai = std::mem::take(&mut self.ai);
        let connections = std::mem::take(&mut self.connections);
        let count_for_me = self.count_for_me;
        *self = Self {
            names,
            dealer,
            connections,
            ai,
            count_for_me,
            ..GameState::new()
        }
    }

    pub fn bidder(&self) -> Option<Seat> {
        let n = self.bids.len();
        if n > 3 && self.bids[n - 3..] == [Bid::Pass, Bid::Pass, Bid::Pass] {
            None
        } else {
            Some(self.dealer + n)
        }
    }

    fn highest_contract_bid(&self) -> Option<Bid> {
        self.bids.iter().rev().find(|x| x.is_contract()).copied()
    }

    pub fn find_declarer(&self) -> Option<Seat> {
        let contract = self.highest_contract_bid()?;
        let mut winning_seat = self.dealer;
        for &x in self.bids.iter() {
            if x == contract {
                break;
            }
            winning_seat = winning_seat.next();
        }
        let mut declarer = self.dealer;
        let mut bids = self.bids.iter();
        if winning_seat != self.dealer && winning_seat != self.dealer.next().next() {
            bids.next();
            declarer = declarer.next();
        }
        for &x in bids.step_by(2) {
            if x.same_suit(contract) {
                return Some(declarer);
            }
            declarer = declarer.next().next();
        }
        None
    }

    pub fn dummy(&self) -> Option<Seat> {
        if self.bids.len() < 4
            || self.bids[self.bids.len() - 3..] != [Bid::Pass, Bid::Pass, Bid::Pass]
        {
            return None;
        }
        self.find_declarer().map(|s| s.next().next())
    }

    pub fn turn(&self) -> Option<Seat> {
        if let Some(seat) = self.bidder() {
            Some(seat)
        } else if let (Some(lead), Some(dummy)) = (self.lead, self.dummy()) {
            let declarer = dummy.next().next();
            let play_seat = match self.played.len() {
                0 | 4 => lead,
                n => lead + n,
            };
            if self.hands[play_seat].is_empty() {
                None
            } else if play_seat == dummy {
                Some(declarer)
            } else {
                Some(play_seat)
            }
        } else {
            None
        }
    }

    pub fn hand_playing(&self) -> Option<Seat> {
        if let Some(seat) = self.bidder() {
            Some(seat)
        } else {
            self.lead.map(|s| s + self.played.len())
        }
    }

    pub fn hand_visible_to(&self, hand: Seat, who: Seat) -> bool {
        let dummy = self.dummy();
        hand == who
            || (Some(hand) == dummy && Some(who.next().next()) == dummy)
            || (Some(hand.next().next()) == dummy && Some(who) == dummy)
            || (Some(hand) == dummy && self.hands.iter().map(|h| h.len()).sum::<usize>() < 52)
    }

    pub fn bid_is_legal(&self, seat: Seat, bid: Bid) -> bool {
        if Some(seat) != self.bidder() {
            return false;
        }
        let n = self.bids.len();
        match bid {
            Bid::Pass => true,
            Bid::Double => {
                (n >= 1 && self.bids[n - 1].is_contract())
                    || (n >= 3
                        && self.bids[n - 2..] == [Bid::Pass, Bid::Pass]
                        && self.bids[n - 3].is_contract())
            }
            Bid::Redouble => false,
            _ => {
                if let Some(hb) = self.highest_contract_bid() {
                    bid > hb
                } else {
                    true
                }
            }
        }
    }

    pub fn playable_cards(&self, seat: Seat, player: Seat) -> PlayableHand {
        let hand = self.hands[seat];
        let played_already = if self.count_for_me[player] {
            self.original_hands[seat] - hand
        } else {
            Cards::EMPTY
        };
        if !self.hand_visible_to(seat, player) {
            return PlayableHand {
                seat,
                hand: Cards::EMPTY,
                playable: Cards::EMPTY,
                played_already,
            };
        }
        let playable = if let (Some(lead), Some(dummy)) = (self.lead, self.dummy()) {
            let declarer = dummy.next().next();
            if (player != seat && seat != dummy) || (seat == dummy && player != declarer) {
                return PlayableHand {
                    seat,
                    hand,
                    playable: Cards::EMPTY,
                    played_already,
                };
            }
            match self.played.len() {
                0 | 4 => {
                    if seat == lead {
                        hand
                    } else {
                        Cards::EMPTY
                    }
                }
                n => {
                    if seat == lead + n {
                        let suit = self.played[0].suit();
                        let mysuit = hand.in_suit(suit);
                        if mysuit.is_empty() {
                            hand
                        } else {
                            mysuit
                        }
                    } else {
                        Cards::EMPTY
                    }
                }
            }
        } else {
            Cards::EMPTY
        };
        PlayableHand {
            seat,
            hand,
            playable,
            played_already,
        }
    }

    pub fn could_be_played(&self) -> Cards {
        if let Some(lead) = self.lead {
            match self.played.len() {
                0 | 4 => self.hands[lead],
                n => {
                    let seat = lead + n;
                    let suit = self.played[0].suit();
                    let mysuit = self.hands[seat].in_suit(suit);
                    if mysuit.is_empty() {
                        self.hands[seat]
                    } else {
                        mysuit
                    }
                }
            }
        } else {
            Cards::EMPTY
        }
    }

    pub fn trick_finish(&mut self) {
        if self.played.len() == 4 {
            let led = self.played[0].suit();
            let mut cards = Cards::EMPTY;
            for c in self.played.iter().cloned() {
                cards += Cards::singleton(c);
            }
            let mut winner = cards.in_suit(led).rev().next().unwrap();
            if let Some(Bid::Suit(_, trump)) = self.highest_contract_bid() {
                if let Some(trump_winner) = cards.in_suit(trump).rev().next() {
                    winner = trump_winner;
                }
            }
            let mut winning_seat = self.lead.unwrap();
            for c in self.played.iter().cloned() {
                if c == winner {
                    break;
                }
                winning_seat = winning_seat.next();
            }
            // Shift is how far the lead has shifted from last time.
            let shift = winning_seat as i32 - self.lead.unwrap() as i32;
            let shift = (shift + 4) % 4;
            self.lead = Some(winning_seat);
            if winning_seat == Seat::North || winning_seat == Seat::South {
                self.ns_tricks += 1;
            } else {
                self.ew_tricks += 1;
            }
            // Adjust the "played" cards so they will appear in front of the
            // proper player.
            for _ in 0..shift {
                let x = self.played.remove(0);
                self.played.push(x);
            }
        }
    }

    pub fn most_recent_bid(&self, seat: Seat) -> Option<Bid> {
        let mut my_bid = None;

        let mut s = self.dealer;
        for &bid in self.bids.iter() {
            if s == seat {
                my_bid = Some(bid);
            }
            s = s.next();
        }
        my_bid
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PlayableHand {
    seat: Seat,
    pub hand: Cards,
    pub playable: Cards,
    played_already: Cards,
}

#[with_template("[%" "%]" "hand.html")]
impl DisplayAs<HTML> for PlayableHand {}

impl PlayableHand {
    pub fn hcp(&self) -> usize {
        (self.hand + self.played_already).high_card_points()
    }
    pub fn shcp(&self) -> usize {
        (self.hand + self.played_already).protected_high_card_points()
            + (self.hand + self.played_already).short_card_points()
    }
    pub fn lhcp(&self) -> usize {
        (self.hand + self.played_already).high_card_points()
            + (self.hand + self.played_already).long_card_points()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Seat {
    South = 0,
    West = 1,
    North = 2,
    East = 3,
}
impl From<usize> for Seat {
    fn from(s: usize) -> Self {
        match s % 4 {
            x if x == Seat::South as usize => Seat::South,
            x if x == Seat::West as usize => Seat::West,
            x if x == Seat::North as usize => Seat::North,
            x if x == Seat::East as usize => Seat::East,
            _ => unreachable!(),
        }
    }
}
impl std::ops::Add<usize> for Seat {
    type Output = Seat;

    fn add(self, rhs: usize) -> Self::Output {
        (self as usize + rhs).into()
    }
}
impl Seat {
    pub fn next(self) -> Self {
        self + 1
    }
    pub fn name(self) -> &'static str {
        const NAMES: Seated<&'static str> = Seated::new(["S", "W", "N", "E"]);
        NAMES[self]
    }
    pub fn long_name(self) -> &'static str {
        const NAMES: Seated<&'static str> = Seated::new(["south", "west", "north", "east"]);
        NAMES[self]
    }
}

impl std::str::FromStr for Seat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "north" => Ok(Seat::North),
            "south" => Ok(Seat::South),
            "west" => Ok(Seat::West),
            "east" => Ok(Seat::East),
            _ => Err(format!("invalid seat: {}", s)),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Seated<T> {
    internal: [T; 4],
}

impl<T> From<[T; 4]> for Seated<T> {
    fn from(internal: [T; 4]) -> Self {
        Seated { internal }
    }
}
impl<T> Seated<T> {
    pub const fn new(internal: [T; 4]) -> Self {
        Seated { internal }
    }
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.internal.iter()
    }
}

impl<T> std::ops::Index<Seat> for Seated<T> {
    type Output = T;

    fn index(&self, index: Seat) -> &T {
        &self.internal[index as usize]
    }
}

impl<T> std::ops::IndexMut<Seat> for Seated<T> {
    fn index_mut(&mut self, index: Seat) -> &mut T {
        &mut self.internal[index as usize]
    }
}
