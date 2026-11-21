//! A player that chooses a card without searching: an n-tuple network over
//! what the player can see (their hand, the other hand in sight, which cards
//! are still hidden, the trick so far and the auction), whose entries are
//! fitted to the double-dummy value of each card averaged over deals of the
//! hidden hands that fit what is known, as `EnsemblePlay` values them.  It is
//! for the stages of the play that are too slow to search, and for choosing
//! which few cards a search should value.  Nothing here says which card is
//! good, only which facts each table sees; the opening lead has its own
//! tables (`opening_lead`), since the dummy is not yet in sight then.

use super::fitting::Network;
#[cfg(test)]
mod model_file;
use super::Convention;
use crate::{Bid, GameState, Seat, Seated};
use bridge_deck::{Card, Cards, Suit};

/// A table of the play model: the facts about a card, and the moment it is
/// played, that pick its entry.  The role is 0 for the declarer playing
/// their own hand, 1 for the declarer playing the dummy's, and 2 and 3 for
/// the defenders on the declarer's left and right; the place is how many
/// cards have been played to the trick; how a card is played is whether it
/// leads, follows suit, ruffs or is discarded.  The play is the place and
/// how, of which there are ten: a lead, or a card that follows, ruffs or is
/// discarded in second, third or fourth place.
///
/// The tables that end in 1 are those of [`V1`], laid out with room for
/// combinations that cannot happen (the place and how as 16 rather than the
/// 10 plays, and five holders of a card where there are four).  Each is
/// otherwise the table of the same name.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Table {
    /// The role, the play, whether partner is winning the trick, and
    /// whether the card beats the trick so far.
    Trick,
    /// The place, and whether each of the three cards just above is the
    /// player's, in the other hand in sight, hidden, or not left at all.
    Above,
    /// The place, and the same of the three cards just below.
    Below,
    /// The play, how many cards of its suit are left above it, and whether
    /// it is the player's lowest of the suit.
    Rank,
    /// The place, the player's length in the suit, the length of the other
    /// hand in sight, and whether the suit is trumps.
    Lengths,
    /// The play, and how many tricks are left.
    TricksLeft,
    /// The role, the play, and how the tricks the declarer still needs for
    /// the contract compare with the tricks left: already made, beaten, or
    /// with none, one, two, or more to spare.
    Contract,
    /// The play, and the fewest and most cards of its suit that the first
    /// hidden hand after the player can hold, by the auction and the play.
    FirstLength,
    /// The same of the second hidden hand.
    SecondLength,
    /// The role, the place, the fewest and most points that the first hidden
    /// hand can still hold, by the auction and the play, and whether the card
    /// is an honour.
    FirstPoints,
    /// The same of the second hidden hand.
    SecondPoints,
    /// The role, the play, who first led the card's suit this hand (nobody,
    /// the player, or which other seat), and in how many tricks it has been
    /// led (none, one, two, or more): to return partner's suit, or not to
    /// help the declarer's, and to see a ruff coming.
    History,
    /// The play, how many trumps the player has (none to three or more),
    /// how many the other hand in sight has, how many are hidden (none, one
    /// or two, three or four, or more), and whether the contract is in
    /// notrump: for drawing trumps, ruffing and overruffing.
    Trumps,
    /// The role, the play, whether the other hand in sight plays to the
    /// trick after the player and, if so, whether it can beat the card and
    /// the trick so far, and whether a hidden hand plays after the player
    /// and, if so, whether one that does is known to be void in the suit led.
    Later,
    /// The role, the play, and which seats bid the card's suit.
    Bids,
    Trick1,
    Above1,
    Below1,
    Rank1,
    TricksLeft1,
    Contract1,
    FirstLength1,
    SecondLength1,
}

/// How many plays there are: see [`Table`].
const PLAYS: usize = 10;

/// The play of a card in `place` played as `how` (see [`Table`]), or `None`
/// if no card is played so.
fn play_of(place: usize, how: usize) -> Option<usize> {
    match (place, how) {
        (0, 0) => Some(0),
        (1..=3, 1..=3) => Some(1 + (place - 1) * 3 + how - 1),
        _ => None,
    }
}

/// The holders of three cards as [`Table::Above`] has them, from the base-5
/// number of [`Cards::holders_above`], or `None` if a card is held by the
/// fourth hand, which [`View::entries`] never has.
fn holders_of(base_5: usize) -> Option<usize> {
    let digits = [base_5 % 5, base_5 / 5 % 5, base_5 / 25];
    digits.iter().all(|&d| d < 4).then(|| digits[0] + 4 * digits[1] + 16 * digits[2])
}

impl Table {
    /// How many entries the table has.
    pub(crate) const fn size(self) -> usize {
        match self {
            Table::Trick => 4 * PLAYS * 2 * 2,
            Table::Above | Table::Below => 4 * 64,
            Table::Rank => PLAYS * 6 * 2,
            Table::Lengths => 4 * 4 * 4 * 2,
            Table::TricksLeft => PLAYS * 6,
            Table::Contract => 4 * PLAYS * 6,
            Table::FirstLength | Table::SecondLength => PLAYS * 20,
            Table::FirstPoints | Table::SecondPoints => 4 * 4 * 20 * 2,
            Table::History => 4 * PLAYS * 5 * 4,
            Table::Trumps => PLAYS * 4 * 4 * 4 * 2,
            Table::Later => 4 * PLAYS * 3 * 3,
            Table::Bids => 4 * PLAYS * 16,
            Table::Trick1 => 4 * 4 * 4 * 2 * 2,
            Table::Above1 | Table::Below1 => 4 * 125,
            Table::Rank1 => 4 * 4 * 6 * 2,
            Table::TricksLeft1 => 4 * 4 * 6,
            Table::Contract1 => 4 * 4 * 4 * 6,
            Table::FirstLength1 | Table::SecondLength1 => 4 * 4 * 20,
        }
    }

    /// The table that this one of [`V1`] is laid out anew as, and the entry
    /// of that table that its entry `index` becomes, or `None` for an entry
    /// that no card looks up.  The other tables are as they were.
    #[cfg(test)]
    fn without_room(self, index: usize) -> (Table, Option<usize>) {
        // The play of a v1 index whose lowest digits are `rest` values and
        // whose next are the place and how.
        let play = |index: usize, rest: usize| {
            let (high, low) = (index / rest, index % rest);
            Some((high / 16, play_of(high / 4 % 4, high % 4)? * rest + low))
        };
        match self {
            Table::Trick1 => (Table::Trick, play(index, 4).map(|(role, p)| role * PLAYS * 4 + p)),
            Table::Above1 => (Table::Above, holders_of(index % 125).map(|h| index / 125 * 64 + h)),
            Table::Below1 => (Table::Below, holders_of(index % 125).map(|h| index / 125 * 64 + h)),
            Table::Rank1 => (Table::Rank, play(index, 12).map(|(_, p)| p)),
            Table::TricksLeft1 => (Table::TricksLeft, play(index, 6).map(|(_, p)| p)),
            Table::Contract1 => (Table::Contract, play(index, 6).map(|(role, p)| role * PLAYS * 6 + p)),
            Table::FirstLength1 => (Table::FirstLength, play(index, 20).map(|(_, p)| p)),
            Table::SecondLength1 => (Table::SecondLength, play(index, 20).map(|(_, p)| p)),
            table => (table, Some(index)),
        }
    }
}

/// The facts that each entry of a table stands for, in words, to say where a
/// model goes wrong.
#[cfg(test)]
impl Table {
    /// Entry `index` of this table in words, laid out as
    /// [`View::entries`] numbers it.  The tables of [`V1`] are left as
    /// numbers.
    fn describe(self, index: usize) -> String {
        const ROLES: [&str; 4] = ["declarer", "dummy", "left defender", "right defender"];
        const PLACES: [&str; 4] = ["lead", "2nd hand", "3rd hand", "4th hand"];
        const HOLDERS: [&str; 4] = ["none", "mine", "in sight", "hidden"];
        let play = |play: usize| match play {
            0 => "lead".to_string(),
            p => format!("{} {}", PLACES[1 + (p - 1) / 3], ["follows", "ruffs", "discards"][(p - 1) % 3]),
        };
        let role_play = |rp: usize| format!("{}, {}", ROLES[rp / PLAYS], play(rp % PLAYS));
        let holders = |h: usize| (0..3).map(|k| HOLDERS[(h >> (2 * k)) & 3]).collect::<Vec<_>>().join(" ");
        let length = |l: usize| {
            let (least, most) = (l / 5, l % 5);
            format!("{least}{}-{most}{}", if least == 3 { "+" } else { "" }, if most == 4 { "+" } else { "" })
        };
        let points = |p: usize| {
            let least = ["0-5", "6-10", "11-15", "16+"][p / 5];
            let most = ["0-7", "8-11", "12-15", "16-20", "21+"][p % 5];
            format!("fewest {least}, most {most} points")
        };
        let three = |n: usize| if n == 3 { "3+".to_string() } else { n.to_string() };
        match self {
            Table::Trick => format!(
                "{}, partner {}winning, {}",
                role_play(index / 4),
                if index & 2 == 0 { "not " } else { "" },
                if index & 1 == 0 { "does not beat" } else { "beats" }
            ),
            Table::Above | Table::Below => {
                format!("{}, {}: {}", PLACES[index / 64], if self == Table::Above { "above" } else { "below" }, holders(index % 64))
            }
            Table::Rank => {
                let above = index / 2 % 6;
                format!(
                    "{}, {}{} left above, {}",
                    play(index / 12),
                    above,
                    if above == 5 { "+" } else { "" },
                    if index & 1 == 1 { "lowest" } else { "not lowest" }
                )
            }
            Table::Lengths => format!(
                "{}, length {}{}, in sight {}, {}",
                PLACES[index / 32],
                index / 8 % 4 + 1,
                if index / 8 % 4 == 3 { "+" } else { "" },
                three(index / 2 % 4),
                if index & 1 == 1 { "trumps" } else { "not trumps" }
            ),
            Table::TricksLeft => {
                format!("{}, {} cards left", play(index / 6), ["1", "2", "3", "4-5", "6-8", "9+"][index % 6])
            }
            Table::Contract => format!(
                "{}, {}",
                role_play(index / 6),
                ["beaten", "none to spare", "1 to spare", "2 to spare", "3+ to spare", "made"][index % 6]
            ),
            Table::FirstLength | Table::SecondLength => format!(
                "{}, {} hidden hand holds {} of the suit",
                play(index / 20),
                if self == Table::FirstLength { "first" } else { "second" },
                length(index % 20)
            ),
            Table::FirstPoints | Table::SecondPoints => format!(
                "{}, {}, {} hidden hand {}, {}",
                ROLES[index / 160],
                PLACES[index / 40 % 4],
                if self == Table::FirstPoints { "first" } else { "second" },
                points(index / 2 % 20),
                if index & 1 == 1 { "honour" } else { "not honour" }
            ),
            Table::History => format!(
                "{}, suit first led by {}, {} rounds",
                role_play(index / 20),
                ["nobody", "me", "left", "partner", "right"][index / 4 % 5],
                three(index % 4)
            ),
            Table::Trumps if index & 1 == 1 => format!("{}, notrump", play(index / 128)),
            Table::Trumps => format!(
                "{}, trumps: mine {}, in sight {}, hidden {}",
                play(index / 128),
                three(index / 32 % 4),
                three(index / 8 % 4),
                ["0", "1-2", "3-4", "5+"][index / 2 % 4]
            ),
            Table::Later => format!(
                "{}, in sight after: {}, hidden after: {}",
                role_play(index / 9),
                ["no", "cannot beat", "can beat"][index / 3 % 3],
                ["no", "yes", "known void"][index % 3]
            ),
            Table::Bids => {
                let bidders: Vec<&str> =
                    ["me", "left", "partner", "right"].into_iter().enumerate().filter(|(k, _)| ((index % 16) >> k) & 1 == 1).map(|(_, s)| s).collect();
                format!("{}, suit bid by {}", role_play(index / 16), if bidders.is_empty() { "nobody".to_string() } else { bidders.join(", ") })
            }
            table => format!("{table:?} entry {index}"),
        }
    }
}

#[cfg(test)]
impl Shape {
    /// The table that holds `entry` of the tables laid end to end, and the
    /// entry in words (see [`Table::describe`]).
    fn describe(&self, mut entry: usize) -> (Table, String) {
        for &table in self.tables {
            if entry < table.size() {
                return (table, table.describe(entry));
            }
            entry -= table.size();
        }
        panic!("{} has no entry {entry} more", self.name)
    }
}

/// A shape of the play model, one version of it: the tables it reads, end to
/// end, and the widths of its two hidden layers (see [`Network`]).  Weights
/// fit only the shape they were trained for, so a model file names the shape
/// of each of its models, and a new table or width is a new shape, which can
/// be trained beside the others in self-play to see whether it pays.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Shape {
    /// What a model file calls it.
    pub(crate) name: &'static str,
    pub(crate) tables: &'static [Table],
    /// How many units the first hidden layer has: each table entry feeds
    /// them as well as the score, so that the model can weigh combinations
    /// of the facts of different tables.
    pub(crate) hidden: usize,
    /// How many units the second hidden layer has (0 for none): each takes
    /// all the units of the first, so that the model can weigh combinations
    /// of combinations.
    pub(crate) second: usize,
}

/// The first shape, of the model trained by self-play from round 0.
pub(crate) const V1: Shape = Shape {
    name: "v1",
    tables: &[
        Table::Trick1,
        Table::Above1,
        Table::Below1,
        Table::Rank1,
        Table::Lengths,
        Table::TricksLeft1,
        Table::Contract1,
        Table::FirstLength1,
        Table::SecondLength1,
        Table::FirstPoints,
        Table::SecondPoints,
    ],
    hidden: 16,
    second: 8,
};

/// [`V1`] without the entries that no card looks up: the same facts, and
/// the same model, in 2,900 entries rather than 3,976 (see
/// [`Model::without_room`]).
pub(crate) const V2: Shape = Shape {
    name: "v2",
    tables: &[
        Table::Trick,
        Table::Above,
        Table::Below,
        Table::Rank,
        Table::Lengths,
        Table::TricksLeft,
        Table::Contract,
        Table::FirstLength,
        Table::SecondLength,
        Table::FirstPoints,
        Table::SecondPoints,
    ],
    hidden: 16,
    second: 8,
};

/// [`V2`] with hidden layers twice as wide, to see whether its width is what
/// limits it.
pub(crate) const V2_WIDE: Shape = Shape { name: "v2-wide", tables: V2.tables, hidden: 32, second: 16 };

/// [`V2`] and more tables, to see whether they pay.
const fn v2_and(name: &'static str, tables: &'static [Table]) -> Shape {
    Shape { name, tables, hidden: 16, second: 8 }
}

/// The tables of [`V2`] and then `$table`s.
macro_rules! v2_tables_and {
    ($($table:expr),*) => {
        &[
            Table::Trick,
            Table::Above,
            Table::Below,
            Table::Rank,
            Table::Lengths,
            Table::TricksLeft,
            Table::Contract,
            Table::FirstLength,
            Table::SecondLength,
            Table::FirstPoints,
            Table::SecondPoints,
            $($table,)*
        ]
    };
}

/// [`V2`] and [`Table::History`].
pub(crate) const V2_HISTORY: Shape = v2_and("v2-history", v2_tables_and!(Table::History));
/// [`V2`] and [`Table::Trumps`].
pub(crate) const V2_TRUMPS: Shape = v2_and("v2-trumps", v2_tables_and!(Table::Trumps));
/// [`V2`] and [`Table::Later`].
pub(crate) const V2_LATER: Shape = v2_and("v2-later", v2_tables_and!(Table::Later));
/// [`V2`] and [`Table::Bids`].
pub(crate) const V2_BIDS: Shape = v2_and("v2-bids", v2_tables_and!(Table::Bids));
/// [`V2`] and both [`Table::Later`] and [`Table::History`], the two that
/// paid in the first screen.
pub(crate) const V2_LATER_HISTORY: Shape =
    v2_and("v2-later-history", v2_tables_and!(Table::Later, Table::History));
/// [`V2`] and both [`Table::Later`] and [`Table::Trumps`], the two that paid
/// in the second screen.
pub(crate) const V2_LATER_TRUMPS: Shape = v2_and("v2-later-trumps", v2_tables_and!(Table::Later, Table::Trumps));

/// Every shape there is.
pub(crate) const SHAPES: &[&Shape] = &[&V1, &V2, &V2_WIDE, &V2_HISTORY, &V2_TRUMPS, &V2_LATER, &V2_BIDS, &V2_LATER_HISTORY, &V2_LATER_TRUMPS];

impl Shape {
    /// The shape called `name`, if there is one.
    pub(crate) fn named(name: &str) -> Option<&'static Shape> {
        SHAPES.iter().copied().find(|shape| shape.name == name)
    }

    /// How many entries its tables have, end to end.
    pub(crate) const fn entries(&self) -> usize {
        let mut entries = 0;
        let mut i = 0;
        while i < self.tables.len() {
            entries += self.tables[i].size();
            i += 1;
        }
        entries
    }

    /// How many weights a model of this shape has.
    pub(crate) fn size(&self) -> usize {
        Network::size(self.entries(), self.hidden, self.second)
    }
}

/// A play model: a network of a shape.
#[derive(Clone)]
pub(crate) struct Model {
    pub(crate) shape: &'static Shape,
    pub(crate) network: Network,
}

impl Model {
    /// The model of `shape` with `params`, such as one written out before.
    pub(crate) fn with_params(shape: &'static Shape, params: Vec<f64>) -> Model {
        let network = Network::with_params(shape.entries(), shape.hidden, shape.second, params);
        Model { shape, network }
    }

    /// A model of `shape` that starts by choosing at random (see
    /// [`Network::new`]).
    #[cfg(test)]
    pub(crate) fn new(shape: &'static Shape, seed: u64) -> Model {
        let network = Network::new(shape.entries(), shape.hidden, shape.second, seed);
        Model { shape, network }
    }

    /// Adds `shift` to the score of every card, exactly: each card looks up
    /// one entry of each table, so the linear weights of all of the first
    /// table's entries are moved by it.
    #[cfg(test)]
    pub(crate) fn shift_scores(&mut self, shift: f64) {
        for p in &mut self.network.params[..self.shape.tables[0].size()] {
            *p += shift;
        }
    }

    /// The entries of its tables that playing `card` looks up in `view`.
    pub(crate) fn entries(&self, view: &View, card: Card) -> Vec<usize> {
        view.entries(card, self.shape)
    }

    /// Its score for playing `card` in `view`.
    pub(crate) fn score(&self, view: &View, card: Card) -> f64 {
        self.network.score(&self.entries(view, card))
    }

    /// This model of [`V1`] as a model of [`V2`], which scores every card
    /// the same: each entry that a card can look up keeps its weights, and
    /// the others are dropped.
    #[cfg(test)]
    pub(crate) fn without_room(&self) -> Model {
        let moved = Model::entries_without_room();
        let params = self.network.with_entries_moved(&self.network.params, &moved, V2.entries());
        Model::with_params(&V2, params)
    }

    /// This model grown into one of `shape`, which scores every card as it
    /// did until it learns more (see [`Network::grown`]): each of its tables
    /// becomes the same table of `shape`, wherever that is, the tables that
    /// it lacks start adding nothing, and so do the units of layers that are
    /// wider.  A model of [`V1`] is first laid out as one of [`V2`].  `None`
    /// if `shape` lacks one of its tables, or has a narrower layer, or a
    /// second layer where it has none or none where it has one.
    #[cfg(test)]
    pub(crate) fn grown_to(&self, shape: &'static Shape) -> Option<Model> {
        if self.shape == &V1 {
            return self.without_room().grown_to(shape);
        }
        let (hidden, second) = (self.shape.hidden, self.shape.second);
        if shape.hidden < hidden || shape.second < second || (shape.second == 0) != (second == 0) {
            return None;
        }
        // Where each table starts among the entries of `shape`.
        let starts: Vec<usize> = shape
            .tables
            .iter()
            .scan(0, |start, table| {
                *start += table.size();
                Some(*start - table.size())
            })
            .collect();
        let mut moved = Vec::with_capacity(self.shape.entries());
        for table in self.shape.tables {
            let at = shape.tables.iter().position(|t| t == table)?;
            moved.extend((0..table.size()).map(|index| starts[at] + index));
        }
        let network = self.network.grown(&moved, shape.entries(), shape.hidden, shape.second, 11);
        Some(Model { shape, network })
    }

    /// Where each entry of [`V1`] is in [`V2`], if it is there.
    #[cfg(test)]
    fn entries_without_room() -> Vec<Option<usize>> {
        let mut moved = vec![None; V1.entries()];
        let (mut from, mut to) = (0, 0);
        for (&old, &new) in V1.tables.iter().zip(V2.tables) {
            for index in 0..old.size() {
                let (table, entry) = old.without_room(index);
                assert_eq!(table, new);
                moved[from + index] = entry.map(|e| to + e);
            }
            from += old.size();
            to += new.size();
        }
        moved
    }
}

/// What the auction and the play show of a hand that cannot be seen.
#[derive(Clone, Copy)]
struct Shown {
    /// The fewest and most cards it can hold of each suit now.
    length: [(usize, usize); 4],
    /// The fewest and most high card points it can hold now.
    points: (usize, usize),
}

impl Shown {
    /// What the auction of `game` (read by `convention`) and the play show of
    /// the hand of `seat`, which has `played`, has shown out of the suits of
    /// `shown_out`, and holds some of `hidden`.
    fn new<C>(
        game: &GameState<C>,
        convention: &Convention,
        seat: Seat,
        played: Cards,
        shown_out: Cards,
        hidden: Cards,
    ) -> Shown {
        let first = (seat as usize + 4 - game.dealer as usize) % 4;
        let offset = (game.bids.len() + 4 - first) % 4;
        let (min, max) = convention.shown_bounds(&game.bids, offset);
        let length = Suit::ALL.map(|suit| {
            if !shown_out.in_suit(suit).is_empty() {
                return (0, 0);
            }
            let gone = played.in_suit(suit).len();
            let most = (max.length[suit] as usize).saturating_sub(gone);
            let least = (min.length[suit] as usize).saturating_sub(gone);
            let most = most.min(hidden.in_suit(suit).len());
            (least.min(most), most)
        });
        let gone = played.high_card_points();
        Shown {
            length,
            points: (
                (min.hcp as usize).saturating_sub(gone),
                (max.hcp as usize).saturating_sub(gone),
            ),
        }
    }

    /// The fewest and most cards of `suit`, as a number below 20.
    fn length_index(&self, suit: Suit) -> usize {
        let (least, most) = self.length[suit as usize];
        least.min(3) * 5 + most.min(4)
    }

    /// The fewest and most points, as a number below 20.
    fn points_index(&self) -> usize {
        let (least, most) = self.points;
        let least = match least {
            0..=5 => 0,
            6..=10 => 1,
            11..=15 => 2,
            _ => 3,
        };
        let most = match most {
            0..=7 => 0,
            8..=11 => 1,
            12..=15 => 2,
            16..=20 => 3,
            _ => 4,
        };
        least * 5 + most
    }
}

/// What the player to choose a card can see, which every card they could
/// play shares.
pub(crate) struct View {
    role: usize,
    place: usize,
    trump: Option<Suit>,
    /// The hand that the card is played from.
    mine: Cards,
    /// The other hand in sight: the dummy, or for the dummy the declarer's.
    other: Cards,
    /// The cards of the two hands that cannot be seen.
    hidden: Cards,
    /// Every card that has not been played.
    left: Cards,
    lead: Option<Suit>,
    winning: Option<Card>,
    partner_winning: bool,
    /// How the tricks that the declarer still needs compare with the tricks
    /// left, as a number below 6 (see [`Table::Contract`]).
    contract: usize,
    /// What is shown of the two hidden hands, the first after the player
    /// first.
    shown: [Shown; 2],
    /// For each suit, who first led it this hand, as 1 more than their seat
    /// counting clockwise from the player (0 if nobody has), and in how many
    /// tricks it has been led, at most 3.  Nobody, if the game does not keep
    /// its tricks.
    history: [(usize, usize); 4],
    /// For each suit, the seats that bid it, as bits for the seats counting
    /// clockwise from the player.
    bid_by: [usize; 4],
    /// Whether the other hand in sight plays to this trick after the player.
    other_later: bool,
    /// Whether a hidden hand plays to this trick after the player.
    hidden_later: bool,
    /// The suits that a hidden hand that plays to this trick after the
    /// player is known to be void in.
    hidden_later_void: Cards,
}

impl View {
    /// What the player to play a card in `game` can see, reading the auction
    /// by `convention`, or `None` when nobody is to play a card, or it is the
    /// opening lead.
    pub(crate) fn new<C>(game: &GameState<C>, convention: &Convention) -> Option<View> {
        View::at(game, convention, game.hands, current_trick(game))
    }

    /// What the next player to `trick` would see, if the hands were `hands`
    /// then, where `trick` is the current trick of `game`, or the start of
    /// it, or the current trick with cards that might be played after it.
    /// What `game` shows of each hand is taken as it would have been then.
    pub(crate) fn at<C>(
        game: &GameState<C>,
        convention: &Convention,
        hands: Seated<Cards>,
        trick: &[Card],
    ) -> Option<View> {
        View::at_moment(game, convention, hands, played_at(game, trick), game.lead?, trick)
    }

    /// What the next player to `trick`, which `lead_seat` led, would see,
    /// with the hands then `hands` and the cards that each had played by then
    /// `played`, those of `trick` among them: any moment of the play of
    /// `game`, or one that might come.
    pub(crate) fn at_moment<C>(
        game: &GameState<C>,
        convention: &Convention,
        hands: Seated<Cards>,
        played: Seated<Cards>,
        lead_seat: Seat,
        trick: &[Card],
    ) -> Option<View> {
        let dummy = game.dummy()?;
        let declarer = dummy + 2;
        let place = trick.len();
        if place > 3 {
            return None;
        }
        let seat = lead_seat + place;
        let left = (0..4).fold(Cards::EMPTY, |cards, s| cards + hands[Seat::from(s)]);
        if left.len() == 52 {
            return None;
        }
        let role = if seat == declarer {
            0
        } else if seat == dummy {
            1
        } else if seat == declarer + 1 {
            2
        } else {
            3
        };
        let mine = hands[seat];
        let other = if seat == dummy {
            hands[declarer]
        } else {
            hands[dummy]
        };
        let trump = super::ensemble::trump_of(game);
        let mut winner = 0;
        for (i, &card) in trick.iter().enumerate().skip(1) {
            if beats(trump, card, trick[winner]) {
                winner = i;
            }
        }
        let level = match game.highest_contract_bid() {
            Some(Bid::Suit(level, _)) | Some(Bid::NT(level)) => level,
            _ => return None,
        };
        // The tricks that declarer's side had won at this moment, which may
        // be earlier in the play than now, and the suits that each player
        // had shown out of, from the tricks so far; or now's, if the game
        // does not keep them.
        let on_declarers_side = |s: Seat| (s as usize + declarer as usize).is_multiple_of(2);
        let done = (0..4).map(|s| played[Seat::from(s)].len()).sum::<usize>() / 4;
        let (won, shown_out) = match game.tricks.get(..done) {
            Some(tricks) => {
                let won = tricks
                    .iter()
                    .filter(|(leader, cards)| {
                        let mut winner = 0;
                        for i in 1..4 {
                            if beats(trump, cards[i], cards[winner]) {
                                winner = i;
                            }
                        }
                        on_declarers_side(*leader + winner)
                    })
                    .count();
                let mut shown_out = Seated::new([Cards::EMPTY; 4]);
                let so_far = tricks.iter().map(|(leader, cards)| (*leader, &cards[..]));
                for (leader, cards) in so_far.chain([(lead_seat, trick)]) {
                    if let Some(led) = cards.first().map(|card| card.suit()) {
                        for (i, card) in cards.iter().enumerate() {
                            if card.suit() != led {
                                shown_out[leader + i] += Cards::ALL.in_suit(led);
                            }
                        }
                    }
                }
                (won, shown_out)
            }
            None if on_declarers_side(Seat::South) => (game.ns_tricks, game.shown_out),
            None => (game.ew_tricks, game.shown_out),
        };
        let left_to_play = 13 - done;
        let needed = (level + 6).saturating_sub(won);
        let contract = if needed == 0 {
            5
        } else {
            match left_to_play as isize - needed as isize {
                spare if spare < 0 => 0,
                spare => (spare as usize).min(3) + 1,
            }
        };
        let hidden = left - mine - other;
        let in_sight = if seat == dummy { declarer } else { dummy };
        let mut hidden_seats = (1..4).map(|n| seat + n).filter(|&s| s != in_sight);
        let mut shown = || {
            let hidden_seat = hidden_seats.next().unwrap();
            Shown::new(game, convention, hidden_seat, played[hidden_seat], shown_out[hidden_seat], hidden)
        };
        let shown = [shown(), shown()];
        // Seats by how far clockwise from the player they are.
        let from_me = |s: Seat| (s as usize + 4 - seat as usize) % 4;
        let mut history = [(0, 0); 4];
        let so_far = game.tricks.get(..done).unwrap_or_default();
        for (leader, cards) in so_far.iter().map(|(leader, cards)| (*leader, &cards[..])).chain([(lead_seat, trick)]) {
            if let Some(led) = cards.first().map(|card| card.suit()) {
                let (first, rounds) = &mut history[led as usize];
                if *first == 0 {
                    *first = 1 + from_me(leader);
                }
                if cards.len() == 4 {
                    *rounds = (*rounds + 1).min(3);
                }
            }
        }
        let mut bid_by = [0; 4];
        for (i, bid) in game.bids.iter().enumerate() {
            if let Bid::Suit(_, suit) = bid {
                bid_by[*suit as usize] |= 1 << from_me(game.dealer + i);
            }
        }
        let later: Vec<Seat> = (1..4 - place).map(|n| seat + n).collect();
        let hidden_later: Vec<Seat> = later.iter().copied().filter(|&s| s != in_sight).collect();
        Some(View {
            role,
            place,
            trump,
            mine,
            other,
            hidden,
            left,
            lead: trick.first().map(|card| card.suit()),
            winning: trick.get(winner).copied(),
            partner_winning: place > 0 && winner % 2 == place % 2,
            contract,
            shown,
            history,
            bid_by,
            other_later: later.contains(&in_sight),
            hidden_later: !hidden_later.is_empty(),
            hidden_later_void: hidden_later.iter().fold(Cards::EMPTY, |void, &s| void + shown_out[s]),
        })
    }

    /// The card that `model` scores highest.
    pub(crate) fn best(&self, model: &Model) -> Card {
        self.legal()
            .map(|card| (model.score(self, card), card))
            .max_by(|a, b| a.0.total_cmp(&b.0))
            .unwrap()
            .1
    }

    /// The cards that the player could play: those of the suit led if they
    /// have any, and otherwise any.
    pub(crate) fn legal(&self) -> Cards {
        match self.lead {
            Some(lead) if !self.mine.in_suit(lead).is_empty() => self.mine.in_suit(lead),
            _ => self.mine,
        }
    }

    /// The entries of the tables of `shape`, laid end to end, that playing
    /// `card` looks up, one in each table.
    pub(crate) fn entries(&self, card: Card, shape: &Shape) -> Vec<usize> {
        let suit = card.suit();
        let (role, place) = (self.role, self.place);
        let how = match self.lead {
            None => 0,
            Some(lead) if lead == suit => 1,
            Some(_) if self.trump == Some(suit) => 2,
            Some(_) => 3,
        };
        let beats = self
            .winning
            .is_some_and(|winning| beats(self.trump, card, winning));
        // Renumbered among the cards left, the cards just above and below
        // are the next ones still in play.
        let renumbered = |cards: Cards| cards.renumbered_among(self.left);
        let in_sight = [
            renumbered(self.mine),
            renumbered(self.other),
            renumbered(self.hidden),
            Cards::EMPTY,
        ];
        let card_now = renumbered(Cards::singleton(card)).max().unwrap();
        let mine = self.mine.in_suit(suit);
        let above = (self.left.in_suit(suit) & Cards::higher_than(card))
            .len()
            .min(5);
        let lowest = mine.min() == Some(card);
        let length = mine.len().min(4) - 1;
        let other_length = self.other.in_suit(suit).len().min(3);
        let trumps = self.trump == Some(suit);
        let tricks = match self.mine.len() {
            0..=1 => 0,
            2 => 1,
            3 => 2,
            4..=5 => 3,
            6..=8 => 4,
            _ => 5,
        };
        let honour = (card.rank() >= 10) as usize;
        let play = play_of(place, how).unwrap();
        let (above3, below3) = (
            Cards::holders_above(&in_sight, card_now, 3),
            Cards::holders_below(&in_sight, card_now, 3),
        );
        let trick = (self.partner_winning as usize) * 2 + beats as usize;
        let (first, rounds) = self.history[suit as usize];
        let trumps_in = |cards: Cards| self.trump.map_or(0, |trump| cards.in_suit(trump).len());
        let hidden_trumps = match trumps_in(self.hidden) {
            0 => 0,
            1..=2 => 1,
            3..=4 => 2,
            _ => 3,
        };
        let trumps_index = ((trumps_in(self.mine).min(3) * 4 + trumps_in(self.other).min(3)) * 4 + hidden_trumps) * 2
            + self.trump.is_none() as usize;
        let led = self.lead.unwrap_or(suit);
        let later = {
            let top = match self.winning {
                Some(winning) if !beats => winning,
                _ => card,
            };
            let following = self.other.in_suit(led);
            let could = if following.is_empty() { self.other } else { following };
            let other = match self.other_later {
                false => 0,
                true if could.into_iter().any(|c| crate::ai::fitted_play::beats(self.trump, c, top)) => 2,
                true => 1,
            };
            let hidden = match self.hidden_later {
                false => 0,
                true if !self.hidden_later_void.in_suit(led).is_empty() => 2,
                true => 1,
            };
            other * 3 + hidden
        };
        let index = |table: Table| match table {
            Table::Trick => (role * PLAYS + play) * 4 + trick,
            Table::Above => place * 64 + holders_of(above3).unwrap(),
            Table::Below => place * 64 + holders_of(below3).unwrap(),
            Table::Rank => (play * 6 + above) * 2 + lowest as usize,
            Table::Lengths => ((place * 4 + length) * 4 + other_length) * 2 + trumps as usize,
            Table::TricksLeft => play * 6 + tricks,
            Table::Contract => (role * PLAYS + play) * 6 + self.contract,
            Table::FirstLength => play * 20 + self.shown[0].length_index(suit),
            Table::SecondLength => play * 20 + self.shown[1].length_index(suit),
            Table::FirstPoints => ((role * 4 + place) * 20 + self.shown[0].points_index()) * 2 + honour,
            Table::SecondPoints => ((role * 4 + place) * 20 + self.shown[1].points_index()) * 2 + honour,
            Table::History => ((role * PLAYS + play) * 5 + first) * 4 + rounds,
            Table::Trumps => play * 4 * 4 * 4 * 2 + trumps_index,
            Table::Later => (role * PLAYS + play) * 9 + later,
            Table::Bids => (role * PLAYS + play) * 16 + self.bid_by[suit as usize],
            Table::Trick1 => ((role * 4 + place) * 4 + how) * 4 + trick,
            Table::Above1 => place * 125 + above3,
            Table::Below1 => place * 125 + below3,
            Table::Rank1 => ((place * 4 + how) * 6 + above) * 2 + lowest as usize,
            Table::TricksLeft1 => (place * 4 + how) * 6 + tricks,
            Table::Contract1 => ((role * 4 + place) * 4 + how) * 6 + self.contract,
            Table::FirstLength1 => (place * 4 + how) * 20 + self.shown[0].length_index(suit),
            Table::SecondLength1 => (place * 4 + how) * 20 + self.shown[1].length_index(suit),
        };
        let mut offset = 0;
        shape
            .tables
            .iter()
            .map(|&table| {
                let index = index(table);
                debug_assert!(index < table.size(), "{table:?}");
                offset += table.size();
                offset - table.size() + index
            })
            .collect()
    }
}

/// The cards played to the trick that is not yet over in `game`, if any.
fn current_trick<C>(game: &GameState<C>) -> &[Card] {
    if game.played.len() == 4 {
        &[]
    } else {
        &game.played
    }
}

/// The seat that plays the `i`th card of the current trick of `game`.
fn seat_of<C>(game: &GameState<C>, i: usize) -> Seat {
    game.lead.unwrap() + i
}

/// The cards that each hand had played when `trick` (as [`View::at`] takes
/// it) had been played to the current trick of `game`.  What each hand has
/// played is seen by all.
fn played_at<C>(game: &GameState<C>, trick: &[Card]) -> Seated<Cards> {
    let mut played = game.original_hands;
    for s in 0..4 {
        let seat = Seat::from(s);
        played[seat] = game.original_hands[seat] - game.hands[seat];
    }
    let current = current_trick(game);
    for (i, &card) in current.iter().enumerate().skip(trick.len()) {
        played[seat_of(game, i)] -= Cards::singleton(card);
    }
    for (i, &card) in trick.iter().enumerate().skip(current.len()) {
        played[seat_of(game, i)] += Cards::singleton(card);
    }
    played
}

/// The hands of `game` with the hidden hands of `seats` dealt `deal`, as they
/// were, or would be, once `trick` (as [`View::at`] takes it) had been
/// played to the current trick.
#[cfg(test)]
pub(crate) fn hands_at<C>(game: &GameState<C>, seats: [Seat; 2], deal: [Cards; 2], trick: &[Card]) -> Seated<Cards> {
    let mut hands = game.hands;
    for (seat, cards) in seats.into_iter().zip(deal) {
        hands[seat] = cards;
    }
    let current = current_trick(game);
    for (i, &card) in current.iter().enumerate().skip(trick.len()) {
        hands[seat_of(game, i)] += Cards::singleton(card);
    }
    for (i, &card) in trick.iter().enumerate().skip(current.len()) {
        hands[seat_of(game, i)] -= Cards::singleton(card);
    }
    hands
}

/// The chance that the player of `view` plays each card that they could,
/// by the play model `model`: the softmax of its scores divided by
/// `temperature`, which is fitted so that these are chances and not only an
/// order.
#[cfg(test)]
pub(crate) fn chances(view: &View, model: &Model, temperature: f64) -> Vec<(Card, f64)> {
    let scores: Vec<(Card, f64)> = view
        .legal()
        .map(|card| (card, model.score(view, card) / temperature))
        .collect();
    let most = scores.iter().map(|s| s.1).fold(f64::MIN, f64::max);
    let odds: Vec<(Card, f64)> = scores.iter().map(|&(c, s)| (c, (s - most).exp())).collect();
    let sum: f64 = odds.iter().map(|o| o.1).sum();
    odds.into_iter().map(|(c, o)| (c, o / sum)).collect()
}

/// The chance that the next player to `trick` (as [`View::at`] takes it)
/// plays each card, when we cannot see their hand: the chances of
/// [`chances`] in each of `deals` of the hidden hands of `seats`, each with
/// its weight, averaged.  A card that they would always play if they held it
/// has only the chance that they hold it.
#[cfg(test)]
pub(crate) fn predicted_plays<C>(
    game: &GameState<C>,
    convention: &Convention,
    seats: [Seat; 2],
    deals: &[([Cards; 2], f64)],
    trick: &[Card],
    model: &Model,
    temperature: f64,
) -> Vec<(Card, f64)> {
    let mut totals: Vec<(Card, f64)> = Vec::new();
    let total_weight: f64 = deals.iter().map(|d| d.1).sum();
    for &(deal, weight) in deals {
        let hands = hands_at(game, seats, deal, trick);
        let Some(view) = View::at(game, convention, hands, trick) else {
            continue;
        };
        for (card, chance) in chances(&view, model, temperature) {
            match totals.iter_mut().find(|t| t.0 == card) {
                Some(total) => total.1 += weight * chance / total_weight,
                None => totals.push((card, weight * chance / total_weight)),
            }
        }
    }
    totals
}

/// The seat whose player chooses the cards of `seat`: the declarer chooses
/// the dummy's.
#[cfg(test)]
fn chooser<C>(game: &GameState<C>, seat: Seat) -> Seat {
    if Some(seat) == game.dummy() {
        seat + 2
    } else {
        seat
    }
}

/// How likely the cards that the players whose hands are `seats` have
/// chosen are, by the play model, if those hands were dealt `deal`: the
/// product of the chance of each card, in the view its player had when they
/// played it.  That includes the dummy's cards when the declarer is hidden,
/// since how the declarer plays the dummy shows something of their own hand.
/// It covers the tricks in `game.tricks` and the current trick: all of the
/// play, unless the game was saved before tricks were kept.  Weighting
/// deals of an ensemble by it draws what can be drawn from the play.
#[cfg(test)]
pub(crate) fn play_likelihood<C>(
    game: &GameState<C>,
    convention: &Convention,
    seats: [Seat; 2],
    deal: [Cards; 2],
    model: &Model,
    temperature: f64,
) -> f64 {
    let current = current_trick(game);
    let mut tricks: Vec<(Seat, &[Card])> = game
        .tricks
        .iter()
        .map(|(lead, cards)| (*lead, &cards[..]))
        .collect();
    if let Some(lead) = game.lead.filter(|_| !current.is_empty()) {
        tricks.push((lead, current));
    }
    // The hands and what each had played before the first of those tricks.
    let mut hands = hands_at(game, seats, deal, current);
    let mut played = played_at(game, current);
    for &(lead, cards) in &tricks {
        for (i, &card) in cards.iter().enumerate() {
            hands[lead + i] += Cards::singleton(card);
            played[lead + i] -= Cards::singleton(card);
        }
    }
    let mut likelihood = 1.0;
    for (lead, cards) in tricks {
        for (i, &card) in cards.iter().enumerate() {
            let seat = lead + i;
            if seats.contains(&chooser(game, seat)) {
                let chance = View::at_moment(game, convention, hands, played, lead, &cards[..i])
                    .map_or(1.0, |view| {
                        chances(&view, model, temperature)
                            .into_iter()
                            .find(|c| c.0 == card)
                            .map_or(0.0, |c| c.1)
                    });
                likelihood *= chance;
            }
            hands[seat] -= Cards::singleton(card);
            played[seat] += Cards::singleton(card);
        }
    }
    likelihood
}

/// The play model that [`ModelPlay`] plays by: the length of the name of its
/// shape as a little-endian `u32`, the name, the number of its weights as a
/// `u32`, and the weights as `f32`s.  `build.rs` takes it from the play model
/// file `src/ai/play_model.bin` (see `fitted_play::model_file`).  That file is
/// not kept in git, and `build.rs` puts nothing here when it is missing.
static MODEL: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/play_model.bin"));

/// The play model of [`MODEL`], or `None` if there is none, or it is of a
/// shape that this code does not have, or has the wrong number of weights.
pub(crate) fn model() -> Option<&'static Model> {
    static MODEL_PLAYED: std::sync::OnceLock<Option<Model>> = std::sync::OnceLock::new();
    MODEL_PLAYED
        .get_or_init(|| {
            let number = |at: usize| Some(u32::from_le_bytes(MODEL.get(at..at + 4)?.try_into().ok()?) as usize);
            let length = number(0)?;
            let shape = Shape::named(std::str::from_utf8(MODEL.get(4..4 + length)?).ok()?)?;
            let n = number(4 + length)?;
            let weights = MODEL.get(8 + length..)?;
            let params: Vec<f64> = weights.as_chunks::<4>().0.iter().map(|w| f32::from_le_bytes(*w) as f64).collect();
            (n == shape.size() && params.len() == n).then(|| Model::with_params(shape, params))
        })
        .as_ref()
}

/// A player that chooses each card by the play model, without searching:
/// the card that it scores highest, from what the player can see.  The
/// opening lead, before the dummy is seen, is the first of
/// `likely_opening_leads`.  Without weights (see [`MODEL`]) it plays as the
/// robot does.
#[derive(Debug, Default)]
pub struct ModelPlay;

impl super::PlayAI for ModelPlay {
    fn play<C>(&mut self, game: &GameState<C>) -> Card {
        let Some(model) = model() else {
            return super::RandomPlay.play(game);
        };
        model_card(model, game, &game.system.convention())
    }
}

/// The card that `model` scores highest for the player to play in `game`,
/// from what the player can see, or for the opening lead, the first of
/// `likely_opening_leads`.
pub(crate) fn model_card<C>(model: &Model, game: &GameState<C>, convention: &Convention) -> Card {
    match View::new(game, convention) {
        Some(view) => view.best(model),
        None => {
            use super::opening_lead::{likely_opening_leads, Auction};
            match (Auction::new(game), game.lead) {
                (Some(auction), Some(leader)) => {
                    likely_opening_leads(&auction, game.hands[leader])[0]
                }
                _ => game.could_be_played().min().unwrap(),
            }
        }
    }
}

/// Whether `card`, played after `winning`, beats it.
fn beats(trump: Option<Suit>, card: Card, winning: Card) -> bool {
    if card.suit() == winning.suit() {
        card.rank() > winning.rank()
    } else {
        Some(card.suit()) == trump
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ai::ensemble::{card_values, plays_for_the_leaders, starting_with, trump_of, Hidden};
    use crate::ai::fitting;
    use crate::situations::Generator;
    use rand::rngs::StdRng;
    use rayon::prelude::*;
    use rand::{Rng, SeedableRng};

    fn var(name: &str, default: &str) -> String {
        std::env::var(name).unwrap_or_else(|_| default.into())
    }

    /// The shape named by the variable `name`, `v1` if it is not set.
    fn shape_of(name: &str) -> &'static Shape {
        let named = var(name, V1.name);
        Shape::named(&named).unwrap_or_else(|| panic!("{name}: there is no shape {named}"))
    }

    fn number(name: &str, default: usize) -> usize {
        var(name, &default.to_string()).parse().unwrap()
    }

    fn pair(card: Card) -> [u8; 2] {
        [card.suit() as u8, card.rank()]
    }

    #[test]
    fn shifting_the_scores_moves_every_score_alike() {
        let mut model = Model::new(&V2_LATER_TRUMPS, 1);
        let mut rng = StdRng::seed_from_u64(4);
        let cards: Vec<Vec<usize>> = (0..5)
            .map(|_| {
                let mut offset = 0;
                V2_LATER_TRUMPS
                    .tables
                    .iter()
                    .map(|table| {
                        offset += table.size();
                        offset - table.size() + rng.gen_range(0..table.size())
                    })
                    .collect()
            })
            .collect();
        let before: Vec<f64> = cards.iter().map(|entries| model.network.score(entries)).collect();
        model.shift_scores(1.25);
        for (entries, before) in cards.iter().zip(before) {
            let after = model.network.score(entries);
            assert!((after - before - 1.25).abs() < 1e-12, "{before} became {after}");
        }
    }

    #[test]
    fn each_entry_of_a_table_is_described_its_own_way() {
        for table in [V2_HISTORY.tables, V2_LATER_TRUMPS.tables, V2_BIDS.tables].concat() {
            let mut seen = std::collections::HashMap::new();
            for index in 0..table.size() {
                // In notrump the entries of every holding of trumps are one.
                if table == Table::Trumps && index & 1 == 1 {
                    continue;
                }
                let words = table.describe(index);
                if let Some(other) = seen.insert(words.clone(), index) {
                    panic!("{table:?} entries {other} and {index} are both {words}");
                }
            }
        }
    }

    #[test]
    fn every_card_that_could_be_played_has_its_entries() {
        let mut generator = Generator::new();
        let mut rng = StdRng::seed_from_u64(1);
        for tricks_left in 2..=12 {
            for index in 0..16 {
                let Some(json) = generator.situation(&mut rng, index, tricks_left) else {
                    continue;
                };
                let game: GameState<()> = serde_json::from_str(&json).unwrap();
                let view = View::new(&game, &game.system.convention()).unwrap();
                // The robot bid the auction, so the hidden hands are as it
                // showed them, less what they have played.
                let seat = game.lead.unwrap() + game.played.len() % 4;
                let in_sight = if Some(seat) == game.dummy() { seat + 2 } else { game.dummy().unwrap() };
                let hidden_seats = (1..4).map(|n| seat + n).filter(|&s| s != in_sight);
                for (shown, hidden_seat) in view.shown.iter().zip(hidden_seats) {
                    let hand = game.hands[hidden_seat];
                    for suit in Suit::ALL {
                        let (least, most) = shown.length[suit as usize];
                        assert!((least..=most).contains(&hand.in_suit(suit).len()), "{json}");
                    }
                    let (least, most) = shown.points;
                    assert!((least..=most).contains(&hand.high_card_points()), "{json}");
                }
                for card in game.could_be_played() {
                    for shape in SHAPES {
                        let mut start = 0;
                        for (table, entry) in shape.tables.iter().zip(view.entries(card, shape)) {
                            assert!((start..start + table.size()).contains(&entry), "{table:?}");
                            start += table.size();
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn the_model_plays_legal_cards_to_the_end() {
        use crate::ai::PlayAI;
        // Weights that do not fit the model are a mistake, not a choice.
        assert!(MODEL.is_empty() || model().is_some(), "play_model.bin is not the size of the play model");
        for json in crate::ai::opening_lead::candidates::opening_leads(5, 6)
            .iter()
            .map(|game| serde_json::to_string(game).unwrap())
        {
            let mut game: GameState<()> = serde_json::from_str(&json).unwrap();
            while !game.hand_done {
                let card = ModelPlay.play(&game);
                assert!(game.could_be_played().contains(card), "{card} in {json}");
                game.play(card);
            }
            assert_eq!(game.ns_tricks + game.ew_tricks, 13);
        }
    }

    /// Situations of the play at every stage, as games.
    fn some_games() -> Vec<GameState<()>> {
        let mut generator = Generator::new();
        let mut rng = StdRng::seed_from_u64(2);
        let mut games = Vec::new();
        for tricks_left in 2..=12 {
            for index in 0..16 {
                if let Some(json) = generator.situation(&mut rng, index, tricks_left) {
                    games.push(serde_json::from_str(&json).unwrap());
                }
            }
        }
        games
    }

    #[test]
    fn the_tricks_kept_are_the_play_so_far() {
        for game in some_games() {
            let current = current_trick(&game);
            assert_eq!(game.tricks.len(), game.ns_tricks + game.ew_tricks);
            let mut played = Seated::new([Cards::EMPTY; 4]);
            let mut lead = game.dummy().unwrap() + 3;
            for &(leader, cards) in &game.tricks {
                assert_eq!(leader, lead);
                let mut winner = 0;
                for (i, &card) in cards.iter().enumerate() {
                    played[leader + i] += Cards::singleton(card);
                    if beats(trump_of(&game), card, cards[winner]) {
                        winner = i;
                    }
                }
                lead = leader + winner;
            }
            assert_eq!(Some(lead), game.lead);
            for (i, &card) in current.iter().enumerate() {
                played[lead + i] += Cards::singleton(card);
            }
            for s in 0..4 {
                let seat = Seat::from(s);
                assert_eq!(played[seat], game.original_hands[seat] - game.hands[seat]);
            }
        }
    }

    #[test]
    fn a_view_earlier_in_the_trick_could_play_the_card_that_was_played() {
        for game in some_games() {
            let convention = game.system.convention();
            let hidden = Hidden::new(&game, &convention).unwrap();
            let seats = hidden.seats();
            let real = seats.map(|seat| game.hands[seat]);
            let current = current_trick(&game);
            for (i, &card) in current.iter().enumerate() {
                let hands = hands_at(&game, seats, real, &current[..i]);
                let view = View::at(&game, &convention, hands, &current[..i]).unwrap();
                assert!(view.legal().contains(card), "{card} at {i}");
            }
        }
    }

    /// The deals that playouts are played on keep every hand that the player
    /// to play can see, and deal the others the cards that they hold between
    /// them, as many to each as before, none of a suit that it has shown out
    /// of; with no deals asked for, the game is as it was dealt.
    #[test]
    fn playouts_deal_only_the_hands_that_the_player_cannot_see() {
        let mut rng = StdRng::seed_from_u64(5);
        for game in some_games() {
            let convention = game.system.convention();
            let game = crate::ai::playout::copy_of(&game);
            let seats = Hidden::new(&game, &convention).unwrap().seats();
            assert_eq!(deals_of(&game, &convention, 0, &mut rng)[0].hands, game.hands);
            let deals = deals_of(&game, &convention, 5, &mut rng);
            assert_eq!(deals.len(), 5);
            for deal in deals {
                for seat in (0..4).map(Seat::from) {
                    if seats.contains(&seat) {
                        assert_eq!(deal.hands[seat].len(), game.hands[seat].len());
                        assert!((deal.hands[seat] & game.shown_out[seat]).is_empty());
                    } else {
                        assert_eq!(deal.hands[seat], game.hands[seat]);
                    }
                    assert_eq!(deal.original_hands[seat] - deal.hands[seat], game.original_hands[seat] - game.hands[seat]);
                }
                assert_eq!(deal.hands[seats[0]] + deal.hands[seats[1]], game.hands[seats[0]] + game.hands[seats[1]]);
            }
        }
    }

    /// The model that the build includes, when there is one, is of the shape
    /// that this code plays by.
    #[test]
    fn the_included_model_is_of_this_shape() {
        assert_eq!(MODEL.is_empty(), model().is_none());
    }

    /// A model of [`V1`] laid out anew as one of [`V2`], or grown into one of
    /// a shape with more tables, in any order, and wider layers, scores every
    /// card that could be played as it did, and what is new in it can learn.
    #[test]
    fn a_model_without_room_scores_the_same() {
        let mut rng = StdRng::seed_from_u64(5);
        let mut old = Model::new(&V1, 6);
        for p in &mut old.network.params {
            *p = rng.gen_range(-1.0..1.0);
        }
        let new = old.without_room();
        assert_eq!(V2.entries(), 2900);
        // A shape of more tables, in another order, and wider layers.
        const MORE: Shape = Shape {
            name: "more",
            tables: &[
                Table::Later,
                Table::SecondPoints,
                Table::Trick,
                Table::Above,
                Table::Below,
                Table::Rank,
                Table::History,
                Table::Lengths,
                Table::TricksLeft,
                Table::Contract,
                Table::FirstLength,
                Table::SecondLength,
                Table::FirstPoints,
            ],
            hidden: 20,
            second: 11,
        };
        let mut grown: Vec<Model> = SHAPES.iter().filter_map(|&shape| old.grown_to(shape)).collect();
        assert_eq!(grown.len(), SHAPES.len() - 1, "every shape but v1 grows from v1");
        grown.push(old.grown_to(&MORE).unwrap());
        // Nor can a shape that lacks a table, or has narrower layers.
        const FEWER: Shape = Shape { name: "fewer", tables: V2.tables.split_at(1).1, hidden: 16, second: 8 };
        const NARROWER: Shape = Shape { name: "narrower", tables: V2.tables, hidden: 12, second: 8 };
        assert!(new.grown_to(&FEWER).is_none() && new.grown_to(&NARROWER).is_none());
        let more = grown.last().unwrap();
        let mut gradient = vec![0.0; more.network.params.len()];
        for game in some_games() {
            let convention = game.system.convention();
            let view = View::new(&game, &convention).unwrap();
            for card in view.legal() {
                let a = old.score(&view, card);
                for model in std::iter::once(&new).chain(&grown) {
                    let b = model.score(&view, card);
                    assert!((a - b).abs() < 1e-9, "{}: {a} {b}", model.shape.name);
                }
                more.network.add_gradient(&more.entries(&view, card), 1.0, &mut gradient);
            }
        }
        // The output weights of the new units of the last layer come last,
        // and some of the time those units are active, so that they learn.
        let new_outputs = &gradient[gradient.len() - (MORE.second - V2.second)..];
        assert!(new_outputs.iter().all(|&g| g > 0.0), "{new_outputs:?}");
    }

    /// Screens shapes by a supervised fit to the data in `PLAY_DATA`: for each
    /// of the shapes `PLAY_SHAPES` (comma-separated) and each of the penalties
    /// `PLAY_PENALTIES` (0.01), fits a model by `PLAY_STEPS` (300) steps, and
    /// prints the tricks a play its choices lose to the ensemble's best card
    /// on the data in `PLAY_TEST`, with the standard error, and how many more
    /// than the first fit's they are, with the standard error of that paired
    /// difference.  Writes each fit's weights to `PLAY_WEIGHTS_DIR`, if that
    /// is set, as `SHAPE-PENALTY.txt`.
    #[test]
    #[ignore]
    fn screen_play_shapes() {
        let shapes: Vec<&'static Shape> = var("PLAY_SHAPES", "v2")
            .split(',')
            .map(|name| Shape::named(name.trim()).unwrap_or_else(|| panic!("PLAY_SHAPES: there is no shape {name}")))
            .collect();
        let penalties: Vec<f64> = var("PLAY_PENALTIES", "0.01").split(',').map(|p| p.trim().parse().unwrap()).collect();
        let steps = number("PLAY_STEPS", 300);
        let mut first: Option<Vec<f64>> = None;
        let error = |values: &[f64]| {
            let n = values.len() as f64;
            let mean = values.iter().sum::<f64>() / n;
            (mean, (values.iter().map(|v| (v - mean).powi(2)).sum::<f64>() / (n - 1.0) / n).sqrt())
        };
        for shape in shapes {
            let train = read_situations(&std::env::var("PLAY_DATA").expect("PLAY_DATA"), shape);
            let test = read_situations(&std::env::var("PLAY_TEST").expect("PLAY_TEST"), shape);
            let choices: fitting::Choices = train.into_iter().map(|s| s.choice).collect();
            for &penalty in &penalties {
                let started = std::time::Instant::now();
                let network =
                    fitting::fit(&choices.iter().collect::<Vec<_>>(), Model::new(shape, 1).network, penalty, steps);
                let losses: Vec<f64> = test
                    .iter()
                    .map(|s| {
                        let best = s.choice.iter().map(|c| c.1).fold(f64::MIN, f64::max);
                        best - s.choice[network.choice(&s.choice)].1
                    })
                    .collect();
                let (mean, spread) = error(&losses);
                print!(
                    "{:<11} penalty {penalty:<6} {} weights, {:.0} s: loses {mean:.4} ({spread:.4}) tricks a play",
                    shape.name,
                    shape.size(),
                    started.elapsed().as_secs_f64()
                );
                match &first {
                    Some(first) => {
                        let more: Vec<f64> = losses.iter().zip(first).map(|(a, b)| a - b).collect();
                        let (mean, spread) = error(&more);
                        println!(", {mean:+.4} ({spread:.4}) more than the first");
                    }
                    None => {
                        println!();
                        first = Some(losses);
                    }
                }
                if let Ok(directory) = std::env::var("PLAY_WEIGHTS_DIR") {
                    std::fs::create_dir_all(&directory).unwrap();
                    network.write(&format!("{directory}/{}-{penalty}.txt", shape.name));
                }
            }
        }
    }

    /// Measures what each table of the play model in `PLAY_WEIGHTS` (see
    /// [`read_model`]) is worth on the data in `PLAY_TEST`, by the tricks a
    /// play that its choices lose to the best card by the ensemble's values:
    /// with every table, and with each table left out of every card's sum
    /// (its entry dropped, which takes away all it adds, through the units
    /// too), with the standard error of the difference.  Beside that, how
    /// much the table's linear weight differs among the cards of a choice
    /// (only differences within a choice change it), and how many of its
    /// entries the data looks up.  Then how many of the units of each hidden
    /// layer are ever active, and how often.
    #[test]
    #[ignore]
    fn measure_play_tables() {
        let model = read_model(&std::env::var("PLAY_WEIGHTS").expect("PLAY_WEIGHTS"));
        let test = read_situations(&std::env::var("PLAY_TEST").expect("PLAY_TEST"), model.shape);
        let network = &model.network;
        let n = test.len() as f64;
        // The tricks that the card scored highest loses to the best one, with
        // the table `left_out` dropped from every card's entries.
        let loss = |s: &Situation, left_out: Option<usize>| -> f64 {
            let scores: Vec<f64> = s
                .choice
                .iter()
                .map(|(entries, _)| {
                    let kept: Vec<usize> =
                        entries.iter().enumerate().filter(|&(t, _)| Some(t) != left_out).map(|(_, &e)| e).collect();
                    network.score(&kept)
                })
                .collect();
            let mut chosen = 0;
            for (i, &score) in scores.iter().enumerate() {
                if score > scores[chosen] {
                    chosen = i;
                }
            }
            let best = s.choice.iter().map(|c| c.1).fold(f64::MIN, f64::max);
            best - s.choice[chosen].1
        };
        let full: Vec<f64> = test.iter().map(|s| loss(s, None)).collect();
        let random: f64 = test
            .iter()
            .map(|s| {
                let best = s.choice.iter().map(|c| c.1).fold(f64::MIN, f64::max);
                best - s.choice.iter().map(|c| c.1).sum::<f64>() / s.choice.len() as f64
            })
            .sum::<f64>()
            / n;
        println!(
            "{} situations of the shape {}: the model loses {:.4} tricks a play, a card at random {random:.4}",
            test.len(),
            model.shape.name,
            full.iter().sum::<f64>() / n
        );
        println!("{:<14} {:>9} {:>7} {:>8} {:>13}", "table", "left out", "error", "spread", "entries used");
        let mut start = 0;
        for (t, table) in model.shape.tables.iter().enumerate() {
            let more: Vec<f64> = test.iter().zip(&full).map(|(s, f)| loss(s, Some(t)) - f).collect();
            let mean = more.iter().sum::<f64>() / n;
            let error = (more.iter().map(|m| (m - mean).powi(2)).sum::<f64>() / (n - 1.0) / n).sqrt();
            let spread = test
                .iter()
                .map(|s| {
                    let weights = s.choice.iter().map(|(e, _)| network.params[e[t]]);
                    weights.clone().fold(f64::MIN, f64::max) - weights.fold(f64::MAX, f64::min)
                })
                .sum::<f64>()
                / n;
            let used: std::collections::HashSet<usize> =
                test.iter().flat_map(|s| s.choice.iter().map(move |(e, _)| e[t])).collect();
            println!(
                "{:<14} {mean:>+9.4} {error:>7.4} {spread:>8.3} {:>6} of {:<5}",
                format!("{table:?}"),
                used.len(),
                table.size()
            );
            debug_assert!(used.iter().all(|&e| (start..start + table.size()).contains(&e)));
            start += table.size();
        }
        let (mut first, mut second) = (vec![0usize; model.shape.hidden], vec![0usize; model.shape.second]);
        let mut cards = 0;
        for s in &test {
            for (entries, _) in &s.choice {
                let (a, b) = network.active_units(entries);
                for (count, on) in first.iter_mut().zip(a).chain(second.iter_mut().zip(b)) {
                    *count += on as usize;
                }
                cards += 1;
            }
        }
        let share = |counts: &[usize]| {
            counts.iter().map(|&c| format!("{:.0}%", 100.0 * c as f64 / cards as f64)).collect::<Vec<_>>().join(" ")
        };
        println!("how often each unit of the first layer is active: {}", share(&first));
        println!("and of the second: {}", share(&second));
    }

    /// Writes the play model file `PLAY_MODEL_FILE` again to `PLAY_MODEL_OUT`
    /// with each of its models of [`V1`] laid out as one of [`V2`] (see
    /// [`Model::without_room`]), learners with their state of Adam.  They
    /// play just as they did, with a quarter fewer weights.
    #[test]
    #[ignore]
    fn convert_play_model_file() {
        let file = model_file::ModelFile::read(&std::env::var("PLAY_MODEL_FILE").expect("PLAY_MODEL_FILE"));
        let moved = Model::entries_without_room();
        let convert = |model: Model| if model.shape == &V1 { model.without_room() } else { model };
        let learners = file
            .learners
            .into_iter()
            .map(|learner| {
                let adam = learner.adam.map(|adam| {
                    if learner.model.shape != &V1 {
                        return adam;
                    }
                    let (first, second, steps) = adam.parts();
                    let network = &learner.model.network;
                    fitting::Adam::from_parts(
                        network.with_entries_moved(first, &moved, V2.entries()),
                        network.with_entries_moved(second, &moved, V2.entries()),
                        steps,
                    )
                });
                model_file::Learner { model: convert(learner.model), adam }
            })
            .collect();
        model_file::ModelFile {
            round: file.round,
            best: convert(file.best),
            learners,
            league: file.league.into_iter().map(|(round, model)| (round, convert(model))).collect(),
        }
        .write(&std::env::var("PLAY_MODEL_OUT").expect("PLAY_MODEL_OUT"));
    }

    /// A view of an earlier moment of the play, such as `play_likelihood`
    /// takes, is the view that the player had then, though tricks have been
    /// won since.
    #[test]
    fn a_view_of_an_earlier_moment_is_the_view_then() {
        let mut rng = StdRng::seed_from_u64(4);
        for mut game in some_games().into_iter().step_by(7) {
            let convention = game.system.convention();
            let start = game.tricks.len();
            // The cards of the trick under way that were played before.
            let before = current_trick(&game).len();
            let mut seen = Vec::new();
            while let Some(view) = View::new(&game, &convention) {
                let legal: Vec<Card> = view.legal().collect();
                if legal.is_empty() {
                    break;
                }
                let entries = |card| SHAPES.iter().map(|shape| view.entries(card, shape)).collect::<Vec<_>>();
                seen.push(legal.iter().map(|&card| entries(card)).collect::<Vec<_>>());
                game.play(legal[rng.gen_range(0..legal.len())]);
            }
            // Back to the start of the tricks played since `start`.
            let mut hands = game.hands;
            let mut played = played_at(&game, &[]);
            for (lead, cards) in &game.tricks[start..] {
                for (i, &card) in cards.iter().enumerate() {
                    hands[*lead + i] += Cards::singleton(card);
                    played[*lead + i] -= Cards::singleton(card);
                }
            }
            let mut seen = seen.into_iter();
            for (t, &(lead, cards)) in game.tricks[start..].iter().enumerate() {
                for (i, &card) in cards.iter().enumerate() {
                    if t == 0 && i < before {
                        hands[lead + i] -= Cards::singleton(card);
                        played[lead + i] += Cards::singleton(card);
                        continue;
                    }
                    let then = seen.next().unwrap();
                    let view = View::at_moment(&game, &convention, hands, played, lead, &cards[..i]).unwrap();
                    let now: Vec<_> = view
                        .legal()
                        .map(|card| SHAPES.iter().map(|shape| view.entries(card, shape)).collect::<Vec<_>>())
                        .collect();
                    assert_eq!(then, now, "trick led by {lead:?}, card {i}");
                    hands[lead + i] -= Cards::singleton(card);
                    played[lead + i] += Cards::singleton(card);
                }
            }
        }
    }

    #[test]
    fn the_chances_add_up_and_cover_only_cards_that_could_be_held() {
        let mut rng = StdRng::seed_from_u64(3);
        const SMALL: Shape = Shape { name: "small", tables: V1.tables, hidden: 4, second: 3 };
        let mut weights = Model::new(&SMALL, 3);
        for p in &mut weights.network.params {
            *p = rng.gen_range(-1.0..1.0);
        }
        for game in some_games() {
            let convention = game.system.convention();
            let view = View::new(&game, &convention).unwrap();
            let sum: f64 = chances(&view, &weights, 1.0).iter().map(|c| c.1).sum();
            assert!((sum - 1.0).abs() < 1e-9);
            let hidden = Hidden::new(&game, &convention).unwrap();
            let seats = hidden.seats();
            let real = seats.map(|seat| game.hands[seat]);
            assert!(play_likelihood(&game, &convention, seats, real, &weights, 1.0) > 0.0);
            // After the first card we could play, what would the next
            // player play, if we cannot see their hand?
            let trick: Vec<Card> = current_trick(&game)
                .iter()
                .copied()
                .chain(view.legal().min())
                .collect();
            // The next player is predicted when they, or the declarer
            // choosing the dummy's card, cannot be seen.
            let next = seat_of(&game, trick.len());
            if trick.len() == 4 || !seats.contains(&chooser(&game, next)) {
                continue;
            }
            let deals: Vec<([Cards; 2], f64)> = hidden
                .ensemble(&mut rng, 4, None)
                .into_iter()
                .map(|deal| (deal, 1.0))
                .collect();
            let predicted = predicted_plays(&game, &convention, seats, &deals, &trick, &weights, 1.0);
            let sum: f64 = predicted.iter().map(|p| p.1).sum();
            assert!((sum - 1.0).abs() < 1e-9, "{sum}");
            let could_hold = if seats.contains(&next) {
                game.hands[seats[0]] + game.hands[seats[1]]
            } else {
                // The dummy is in sight: only its own cards can be played.
                game.hands[next]
            };
            assert!(predicted.iter().all(|&(card, _)| could_hold.contains(card)));
        }
    }

    /// The temperature for [`chances`] that makes the chance of a best card
    /// likeliest, by the mean log of it, over the data in `PLAY_TEST`, for
    /// the model in the file `PLAY_WEIGHTS` (see [`read_model`]): the chances
    /// that a player who plays as the ensemble's values say would play each
    /// card.
    #[test]
    #[ignore]
    fn fit_play_temperature() {
        let path = std::env::var("PLAY_WEIGHTS").expect("PLAY_WEIGHTS");
        let model = read_model(&path);
        let weights = &model.network;
        let test = read_situations(&std::env::var("PLAY_TEST").expect("PLAY_TEST"), model.shape);
        for temperature in [0.25, 0.5, 1.0, 2.0, 4.0, 8.0] {
            let total: f64 = test
                .iter()
                .map(|s| {
                    let scores: Vec<f64> = s
                        .choice
                        .iter()
                        .map(|(e, _)| weights.score(e) / temperature)
                        .collect();
                    let most = scores.iter().copied().fold(f64::MIN, f64::max);
                    let odds: Vec<f64> = scores.iter().map(|x| (x - most).exp()).collect();
                    let best = s.choice.iter().map(|c| c.1).fold(f64::MIN, f64::max);
                    let on_best: f64 = odds
                        .iter()
                        .zip(&s.choice)
                        .filter(|(_, c)| c.1 >= best - 1e-9)
                        .map(|(o, _)| o)
                        .sum();
                    (on_best / odds.iter().sum::<f64>()).ln()
                })
                .sum();
            println!("temperature {temperature}: mean log chance of a best card {:.4}", total / test.len() as f64);
        }
    }

    /// Writes situations of the play, with the double-dummy value of each
    /// card that could be played on each of an ensemble of deals of the
    /// hidden hands, one JSON object per line, to the file `PLAY_DATA`: the
    /// stage (tricks left), the game as JSON, whether the player plays for
    /// the side that led the trick, and for each card the tricks that side
    /// takes after it in each deal.  The stages are `PLAY_STAGES` (7, 6, 5,
    /// 4, 3, 2), each with `PLAY_SITUATIONS` (200) situations from the seed
    /// `PLAY_SEED` (1), and the ensembles have `PLAY_DEALS` (6) deals, which
    /// replay the auction as `EnsemblePlay::replaying_the_auction` does.  It
    /// uses every core.
    #[test]
    #[ignore]
    fn collect_play_data() {
        let path = std::env::var("PLAY_DATA").expect("PLAY_DATA");
        let stages: Vec<usize> = var("PLAY_STAGES", "7,6,5,4,3,2")
            .split(',')
            .map(|s| s.trim().parse().unwrap())
            .collect();
        let count = number("PLAY_SITUATIONS", 200);
        let deals = number("PLAY_DEALS", 6);
        let seed = number("PLAY_SEED", 1) as u64;
        let mut generator = Generator::new();
        let mut situations = Vec::new();
        for &stage in &stages {
            let mut rng = StdRng::seed_from_u64(seed * 1000 + stage as u64);
            let mut index = 0;
            let start = situations.len();
            while situations.len() < start + count {
                if let Some(json) = generator.situation(&mut rng, index, stage) {
                    situations.push((stage, json));
                }
                index += 1;
            }
        }
        let threads = std::thread::available_parallelism().map_or(4, |n| n.get());
        let next = std::sync::atomic::AtomicUsize::new(0);
        let lines = std::sync::Mutex::new(Vec::new());
        std::thread::scope(|scope| {
            for _ in 0..threads {
                scope.spawn(|| {
                    let mut conventions = std::collections::HashMap::new();
                    loop {
                        let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        let Some((stage, json)) = situations.get(i) else {
                            break;
                        };
                        let game: GameState<()> = serde_json::from_str(json).unwrap();
                        let convention = conventions
                            .entry(format!("{:?}", game.system))
                            .or_insert_with(|| game.system.convention());
                        let mut rng = StdRng::seed_from_u64(seed * 1_000_000 + i as u64);
                        let Some(values) = ensemble_values(&game, convention, deals, &mut rng) else {
                            continue;
                        };
                        let legal = game.could_be_played();
                        let line = serde_json::json!({
                            "stage": stage,
                            "situation": json,
                            "leaders": plays_for_the_leaders(&game),
                            "values": legal.map(pair).zip(values).collect::<Vec<_>>(),
                        });
                        lines.lock().unwrap().push((i, line.to_string()));
                    }
                });
            }
        });
        let mut lines = lines.into_inner().unwrap();
        lines.sort();
        let text: String = lines.into_iter().map(|(_, line)| line + "\n").collect();
        std::fs::write(path, text).unwrap();
    }

    /// The double-dummy tricks of the side that led the trick, after each
    /// card that the player to play in `game` could play (in the order of
    /// `could_be_played`), on each of up to `deals` deals of the hidden hands
    /// that fit what the player has seen and that the robot would have bid
    /// the auction with, or that come closest to it.  `None` if no deal fits.
    fn ensemble_values(
        game: &GameState<()>,
        convention: &Convention,
        deals: usize,
        rng: &mut StdRng,
    ) -> Option<Vec<Vec<f64>>> {
        let hidden = Hidden::new(game, convention)?;
        let legal = game.could_be_played();
        let trump = trump_of(game);
        let mut values: Vec<Vec<f64>> = legal.map(|_| Vec::new()).collect();
        for deal in hidden.ensemble(rng, deals, Some(convention)) {
            let Some((starting, in_trick)) = starting_with(game, &hidden, deal) else {
                continue;
            };
            for (all, (_, value)) in values.iter_mut().zip(card_values(starting, trump, &in_trick, legal)) {
                all.push(value);
            }
        }
        (!values.iter().any(|v| v.is_empty())).then_some(values)
    }

    /// A situation from the data that `collect_play_data` writes, as the
    /// choices of `fitting`: each card's entries and its mean value to the
    /// player, the tricks of their side, over the ensemble.
    struct Situation {
        stage: usize,
        choice: Vec<(Vec<usize>, f64)>,
    }

    /// The situations in the files `paths`, comma-separated, each card with
    /// its entries in the tables of `shape`.
    fn read_situations(paths: &str, shape: &Shape) -> Vec<Situation> {
        let mut conventions = std::collections::HashMap::new();
        let mut situations = Vec::new();
        for path in paths.split(',') {
            for line in std::fs::read_to_string(path.trim()).unwrap().lines() {
                let json: serde_json::Value = serde_json::from_str(line).unwrap();
                let game: GameState<()> =
                    serde_json::from_str(json["situation"].as_str().unwrap()).unwrap();
                let convention = conventions
                    .entry(format!("{:?}", game.system))
                    .or_insert_with(|| game.system.convention());
                let view = View::new(&game, convention).unwrap();
                let leaders = json["leaders"].as_bool().unwrap();
                let choice = json["values"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|v| {
                        let card = Card::new(
                            Suit::ALL[v[0][0].as_u64().unwrap() as usize],
                            v[0][1].as_u64().unwrap() as u8,
                        );
                        let deals = v[1].as_array().unwrap();
                        let mean = deals.iter().map(|d| d.as_f64().unwrap()).sum::<f64>()
                            / deals.len() as f64;
                        // The tricks of the player's side, up to a number that
                        // is the same for every card.
                        let value = if leaders { mean } else { -mean };
                        (view.entries(card, shape), value)
                    })
                    .collect();
                situations.push(Situation {
                    stage: json["stage"].as_u64().unwrap() as usize,
                    choice,
                });
            }
        }
        situations
    }

    /// Fits the play model to the data in the files `PLAY_DATA`, picking the
    /// penalty by five-fold cross-validation, and prints, for each stage of
    /// the data in `PLAY_TEST`, the tricks that its choice loses to the best
    /// card by the ensemble's values, beside those that a card at random
    /// loses.  Writes the weights, one a line, to the file `PLAY_WEIGHTS` if
    /// it is set.  `PLAY_STEPS` (500) is the number of steps of each fit, and
    /// the model is of the shape `PLAY_SHAPE` (`v1`), so that shapes can be
    /// compared.
    #[test]
    #[ignore]
    fn fit_play_model() {
        let shape = shape_of("PLAY_SHAPE");
        let train = read_situations(&std::env::var("PLAY_DATA").expect("PLAY_DATA"), shape);
        let test = read_situations(&std::env::var("PLAY_TEST").expect("PLAY_TEST"), shape);
        let steps = number("PLAY_STEPS", 500);
        let choices: fitting::Choices = train.into_iter().map(|s| s.choice).collect();
        let start = || Model::new(shape, 1).network;
        let (penalty, held_out) =
            fitting::cross_validate(&choices, &start, &[0.0, 0.0001, 0.001, 0.01], steps);
        for (penalty, value) in held_out {
            println!("penalty {penalty}: {value:.4} held out");
        }
        let weights = fitting::fit(&choices.iter().collect::<Vec<_>>(), start(), penalty, steps);
        let mut stages: Vec<usize> = test.iter().map(|s| s.stage).collect();
        stages.sort_unstable();
        stages.dedup();
        for stage in stages.into_iter().rev() {
            let at: Vec<&Situation> = test.iter().filter(|s| s.stage == stage).collect();
            let loss = |value: &dyn Fn(&Situation) -> f64| {
                at.iter()
                    .map(|s| {
                        let best = s.choice.iter().map(|c| c.1).fold(f64::MIN, f64::max);
                        best - value(s)
                    })
                    .sum::<f64>()
                    / at.len() as f64
            };
            let model = loss(&|s| s.choice[weights.choice(&s.choice)].1);
            let random = loss(&|s| s.choice.iter().map(|c| c.1).sum::<f64>() / s.choice.len() as f64);
            println!(
                "{stage} tricks left, {} situations: the model loses {model:.3} tricks a play, a card at random {random:.3}",
                at.len()
            );
        }
        if let Ok(path) = std::env::var("PLAY_WEIGHTS") {
            weights.write(&path);
        }
    }

    /// One choice of a card in a game of self-play: the learner that made
    /// it, each card that could be played, with its entries and chance,
    /// which one was played, the side that chose it (0 for north and south)
    /// and the tricks that side had already won, and, if it was played out
    /// (see [`playout`]), the tricks that side took after each card, with
    /// the weight of 1 over the chance that it was.
    struct Decision {
        learner: usize,
        cards: Vec<(Vec<usize>, f64)>,
        chosen: usize,
        side: usize,
        trick: usize,
        won_before: usize,
        played_out: Option<Vec<f64>>,
        weight: f64,
    }

    /// Plays `card` in `game`, a game of self-play, and the rest of the game
    /// after it, on the same deal and with the same `players`, each model
    /// playing the card it scores highest and the players of any card
    /// drawing from `seed`.  Gives the tricks of north and south.  Played
    /// out after each card that could be played, with the same seed, the
    /// tricks are what each card is worth to its player when the others go
    /// on as they would: the hidden hands are as they were dealt, a fair
    /// draw from the hands that the player could be facing, and no one later
    /// sees more than they could, so on the mean over many games the tricks
    /// after a card are what it is worth.
    fn playout(
        game: &GameState<()>,
        card: Card,
        convention: &Convention,
        learners: &[Model],
        players: [Player; 4],
        seed: u64,
    ) -> [usize; 2] {
        let mut game = crate::ai::playout::copy_of(game);
        let mut rng = StdRng::seed_from_u64(seed);
        game.play(card);
        while !game.hand_done {
            let seat = chooser(&game, game.lead.unwrap() + current_trick(&game).len());
            let model = match players[seat as usize] {
                Player::Learning(learner) => &learners[learner],
                Player::Past(model, _) => model,
                Player::Random => {
                    let legal: Vec<Card> = game.could_be_played().collect();
                    game.play(legal[rng.gen_range(0..legal.len())]);
                    continue;
                }
            };
            game.play(model_card(model, &game, convention));
        }
        [game.ns_tricks, game.ew_tricks]
    }

    /// Which of the learners' choices self-play plays out (see [`playout`]):
    /// a `share` of those of more than one card with up to `most` tricks
    /// left, and `per_game` of those of more than one run of touching cards
    /// in each game with no player of any card, chosen at random once the
    /// game is over, which are played out on `deals` deals of the hands
    /// that the player cannot see (see [`deals_of`]) and fitted to the
    /// `level` and the `apart` accuracies, in tricks (see
    /// [`learn_from_playouts`]): an infinite `level` leaves the level of the
    /// scores out, and the tricks are not put at a level.
    #[derive(Clone, Copy)]
    struct Playouts {
        share: f64,
        most: usize,
        per_game: usize,
        deals: usize,
        level: f64,
        apart: f64,
    }

    /// `count` deals of the hands that the player to play in `game` cannot
    /// see that fit what they have seen (see [`Hidden::ensemble`]), each as
    /// the game would be with them, or none if there is no one to play; or,
    /// with `count` 0, the game as it was dealt.
    fn deals_of(game: &GameState<()>, convention: &Convention, count: usize, rng: &mut StdRng) -> Vec<GameState<()>> {
        if count == 0 {
            return vec![crate::ai::playout::copy_of(game)];
        }
        let Some(hidden) = Hidden::new(game, convention) else {
            return Vec::new();
        };
        hidden
            .ensemble(rng, count, None)
            .iter()
            .map(|deal| crate::ai::playout::dealt_as(game, hidden.seats(), deal))
            .collect()
    }

    /// What a learner's choices played out add up to in self-play: the
    /// tricks that its likeliest cards lose to the best, how many choices,
    /// and, learning from playouts, the sums over the choices of the mean
    /// square of how far its scores are from the tricks that they are
    /// fitted to, of the mean square of how far each of those misses is
    /// from the mean miss of its choice, and of the mean of the squares
    /// that are fitted, and, weighted as the choices are, the sums of the scores
    /// less the mean of their choice times the tricks less theirs, and of
    /// the squares of those scores, to find the factor that makes the
    /// differences of its scores nearest those of the tricks, and the sums
    /// of the weights, scores and tricks, to find the shift that then makes
    /// its scores nearest the tricks.
    #[derive(Clone, Copy, Default)]
    struct Tally {
        lost: f64,
        choices: usize,
        off: f64,
        off_apart: f64,
        chi_squared: f64,
        across: f64,
        squares: f64,
        weights: f64,
        scores: f64,
        tricks: f64,
    }

    impl Tally {
        fn add(&mut self, other: &Tally) {
            self.lost += other.lost;
            self.choices += other.choices;
            self.off += other.off;
            self.off_apart += other.off_apart;
            self.chi_squared += other.chi_squared;
            self.across += other.across;
            self.squares += other.squares;
            self.weights += other.weights;
            self.scores += other.scores;
            self.tricks += other.tricks;
        }

        /// The factor that brings the differences of the scores of a choice
        /// nearest those of the tricks, and the shift that then brings the
        /// scores nearest the tricks, by least squares.  The level of a
        /// self-play model's scores means nothing, as its chances do not
        /// depend on it, so the factor is not fitted to it.
        fn fit(&self) -> (f64, f64) {
            let factor = self.across / self.squares;
            (factor, (self.tricks - factor * self.scores) / self.weights)
        }
    }

    /// How the learners of self-play learn.
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Learning {
        /// Up the slope of the tricks after each choice, and of the choices
        /// played out.
        Outcomes,
        /// By fitting the scores of the cards of each choice played out to
        /// the tricks after them.
        Playouts,
    }

    /// How a seat chooses its cards in self-play.
    #[derive(Clone, Copy)]
    enum Player<'a> {
        /// By this learner of the population, which learns from its choices.
        Learning(usize),
        /// By this model, a snapshot of an earlier one, taken after this
        /// many rounds.
        Past(&'a Model, usize),
        /// Any card that it could play, at random.
        Random,
    }

    /// Plays `json`, a game whose opening lead is next, out to the end, the
    /// opening lead by `likely_opening_leads` and each other card by the
    /// seat that chooses it as its `players` say: a model chooses at random
    /// with its chances at `temperature`, a learner by its weights in
    /// `learners`.  Gives the choices that the learners made and the tricks
    /// each side won, with `record` the whole game, for watching, with some
    /// of the learners' choices played out as `playouts` says.
    #[expect(clippy::too_many_arguments, reason = "the one caller is self-play's round, which has them all at hand")]
    fn play_out(
        json: &str,
        convention: &Convention,
        learners: &[Model],
        temperature: f64,
        players: [Player; 4],
        record: bool,
        playouts: Playouts,
        rng: &mut StdRng,
    ) -> (Vec<Decision>, [usize; 2], Option<serde_json::Value>) {
        use crate::ai::opening_lead::{likely_opening_leads, Auction};
        let mut game: GameState<()> = serde_json::from_str(json).unwrap();
        let leader = game.turn().unwrap();
        let lead = likely_opening_leads(&Auction::new(&game).unwrap(), game.hands[leader])[0];
        let mut plays = vec![serde_json::json!({
            "seat": format!("{leader:?}"),
            "card": pair(lead),
            "by": "the order of opening leads",
        })];
        game.play(lead);
        let mut decisions = Vec::new();
        // The choices that could be played out after the game, by their
        // number among the decisions: the game then, its runs, the cards
        // that could be played and the side to play.
        let mut later = Vec::new();
        while !game.hand_done {
            let Some(view) = View::new(&game, convention) else {
                break;
            };
            let seat = game.lead.unwrap() + current_trick(&game).len();
            let side = chooser(&game, seat) as usize % 2;
            let legal: Vec<Card> = view.legal().collect();
            let player = players[chooser(&game, seat) as usize];
            let weights = match player {
                Player::Random => None,
                Player::Past(past, _) => Some((past, None)),
                Player::Learning(learner) => Some((&learners[learner], Some(learner))),
            };
            let mut considered = Vec::new();
            let card = if let Some((weights, learning)) = weights {
                let cards: Vec<(Vec<usize>, f64)> = chances(&view, weights, temperature)
                    .into_iter()
                    .map(|(card, chance)| (weights.entries(&view, card), chance))
                    .collect();
                let mut pick = rng.gen_range(0.0..1.0);
                let mut chosen = cards.len() - 1;
                for (i, (_, chance)) in cards.iter().enumerate() {
                    if pick < *chance {
                        chosen = i;
                        break;
                    }
                    pick -= chance;
                }
                if record {
                    considered = legal.iter().zip(&cards).map(|(&card, c)| (pair(card), c.1)).collect();
                }
                if let Some(learner) = learning {
                    let Playouts { share, most, per_game, .. } = playouts;
                    if per_game > 0 && legal.len() > 1 && 13 - game.tricks.len() <= most {
                        let runs: Vec<Card> = crate::ai::playout::one_of_each_run(&game).collect();
                        if runs.len() > 1 {
                            later.push((decisions.len(), crate::ai::playout::copy_of(&game), runs, legal.clone(), side));
                        }
                    }
                    let played_out = (share > 0.0
                        && legal.len() > 1
                        && 13 - game.tricks.len() <= most
                        && rng.gen_range(0.0..1.0) < share)
                        .then_some(())
                        .and_then(|()| {
                            // One card of each run of touching cards is
                            // played out, for all of the run.
                            let runs: Vec<Card> = crate::ai::playout::one_of_each_run(&game).collect();
                            if runs.len() < 2 {
                                return None;
                            }
                            let seed = rng.gen::<u64>();
                            let values: Vec<f64> = runs
                                .iter()
                                .map(|&card| playout(&game, card, convention, learners, players, seed)[side] as f64)
                                .collect();
                            Some(legal.iter().map(|&card| values[crate::ai::playout::run_of(&runs, card)]).collect())
                        });
                    decisions.push(Decision {
                        learner,
                        cards,
                        chosen,
                        side,
                        trick: game.tricks.len(),
                        won_before: if side == 0 { game.ns_tricks } else { game.ew_tricks },
                        played_out,
                        weight: if share > 0.0 { 1.0 / share } else { 0.0 },
                    });
                }
                legal[chosen]
            } else {
                legal[rng.gen_range(0..legal.len())]
            };
            if record {
                plays.push(serde_json::json!({
                    "seat": format!("{seat:?}"),
                    "card": pair(card),
                    "by": player_name(player, learners),
                    "chances": considered,
                }));
            }
            game.play(card);
        }
        let any_card = players.iter().any(|p| matches!(p, Player::Random));
        if !later.is_empty() && !any_card {
            let chosen = playouts.per_game.min(later.len());
            let weight = later.len() as f64 / chosen as f64;
            for i in rand::seq::index::sample(rng, later.len(), chosen) {
                let (number, game, runs, legal, side): &(usize, GameState<()>, Vec<Card>, Vec<Card>, usize) = &later[i];
                // Each deal is played out after each card, so that the cards
                // are compared on the same deals.
                let deals: Vec<(GameState<()>, u64)> = deals_of(game, convention, playouts.deals, rng)
                    .into_iter()
                    .map(|deal| (deal, rng.gen::<u64>()))
                    .collect();
                if deals.is_empty() {
                    continue;
                }
                let mean = |card: Card, players: [Player; 4]| -> f64 {
                    deals
                        .iter()
                        .map(|(deal, seed)| playout(deal, card, convention, learners, players, *seed)[*side] as f64)
                        .sum::<f64>()
                        / deals.len() as f64
                };
                let values: Vec<f64> = runs.iter().map(|&card| mean(card, players)).collect();
                let mut tricks: Vec<f64> =
                    legal.iter().map(|&card| values[crate::ai::playout::run_of(runs, card)]).collect();
                let decision = &mut decisions[*number];
                if playouts.level.is_finite() {
                    // The tricks from now on, at the level of the learner
                    // playing every seat after the card that it would play.
                    let learner = decision.learner;
                    let own = model_card(&learners[learner], game, convention);
                    let won = if *side == 0 { game.ns_tricks } else { game.ew_tricks };
                    let alone = mean(own, [Player::Learning(learner); 4]) - won as f64;
                    let shift = alone - values[crate::ai::playout::run_of(runs, own)];
                    tricks.iter_mut().for_each(|t| *t += shift);
                }
                decision.played_out = Some(tricks);
                decision.weight = weight;
            }
        }
        let watched = record.then(|| {
            let start: GameState<()> = serde_json::from_str(json).unwrap();
            let dummy = start.dummy().unwrap();
            serde_json::json!({
                "dealer": format!("{:?}", start.dealer),
                "hands": (0..4)
                    .map(|s| {
                        let seat = Seat::from(s);
                        (format!("{seat:?}"), start.original_hands[seat].map(pair).collect::<Vec<_>>())
                    })
                    .collect::<std::collections::BTreeMap<_, _>>(),
                "bids": start.bids.iter().map(|&bid| call_name(bid)).collect::<Vec<_>>(),
                "declarer": format!("{:?}", dummy + 2),
                "contract": start.highest_contract_bid().map(call_name),
                "players": (0..4)
                    .map(|s| {
                        let seat = Seat::from(s);
                        let name = if seat == dummy {
                            "the dummy".to_string()
                        } else {
                            player_name(players[s], learners)
                        };
                        (format!("{seat:?}"), name)
                    })
                    .collect::<std::collections::BTreeMap<_, _>>(),
                "plays": plays,
                "tricks": { "NS": game.ns_tricks, "EW": game.ew_tricks },
            })
        });
        (decisions, [game.ns_tricks, game.ew_tricks], watched)
    }

    /// What a viewer calls a player of self-play.
    fn player_name(player: Player, learners: &[Model]) -> String {
        match player {
            Player::Learning(learner) => format!("learner {} ({})", learner + 1, learners[learner].shape.name),
            Player::Past(past, 0) => format!("a {} model before it learned", past.shape.name),
            Player::Past(past, round) => format!("a {} model after {round} rounds", past.shape.name),
            Player::Random => "a player of any card".into(),
        }
    }

    /// A call as the viewer writes it: P, X, XX, 1C to 7S, or 1N to 7N.
    fn call_name(bid: Bid) -> String {
        let suit = |suit: Suit| ['C', 'D', 'H', 'S'][suit as usize];
        match bid {
            Bid::Pass => "P".into(),
            Bid::Double => "X".into(),
            Bid::Redouble => "XX".into(),
            Bid::Suit(level, s) => format!("{level}{}", suit(s)),
            Bid::NT(level) => format!("{level}N"),
        }
    }

    /// Learns the play model from nothing, by self-play among a population
    /// of `PLAY_SELF_LEARNERS` (4) learners, each with its own weights and
    /// its own steps of Adam, which share games but are never copied.  Each
    /// round plays `PLAY_SELF_DEALS` (256) deals of a pool of
    /// `PLAY_SELF_POOL` (4000) deals that the robot bid (in each of its
    /// systems in turn), each `PLAY_SELF_GROUP` (8) times, and then replaces
    /// the `PLAY_SELF_FRESH` (0) oldest deals of the pool with new ones,
    /// dealt and bid by the threads that play the games, so that no deal is
    /// played so often that the learners learn it rather than bridge.  When
    /// the pool is the size of a round, each of its deals is played in each
    /// round, and with `PLAY_SELF_FRESH` the same size, every deal is new.
    ///
    /// With `PLAY_SELF_ROUND_ROBIN` set to 1, each deal is played in a round
    /// robin in place of `PLAY_SELF_GROUP` plays, as team play between mixed
    /// partnerships, so that each learner defends beside others and never
    /// beside itself: four learners, chosen at random for each deal when
    /// there are more (and taken again in turn when there are fewer), are
    /// split into two partnerships in each of the three ways, and each split
    /// plays each side of the deal, and each of those with the partners in
    /// either seat, which is twelve plays; and `PLAY_SELF_WILDCARDS` (4) more
    /// plays, two matches at two tables, of a learner partnered by a past
    /// model or a player of any card (in the proportions of `PLAY_SELF_PAST`
    /// and `PLAY_SELF_RANDOM`) against two other learners.  Otherwise, every seat of a game is played by a
    /// learner chosen at random, or with the chance `PLAY_SELF_PAST` (0) by a
    /// past model of the league, or with the chance `PLAY_SELF_RANDOM` (0) by
    /// a player of any card, partners as well as opponents, so that no
    /// learner can count on playing with itself.  A model chooses each card at
    /// random with its chances at `PLAY_SELF_TEMPERATURE` (1).  Each learner
    /// moves its weights by a step of Adam of size `PLAY_SELF_RATE` (0.01) up
    /// the gradient of the tricks its side takes after each of its own
    /// choices, less the mean of that at the same trick over the plays of the
    /// same deal, so that what it learns is what made a play of a deal better
    /// than the others.  So that the learners keep trying other cards where
    /// they are unsure, and so keep finding out which are better, each
    /// update also pushes towards spreading a choice's chances, with a weight
    /// that grows by 5% a round while the mean entropy of the choices is
    /// below `PLAY_SELF_ENTROPY` (0.25) and shrinks by 5% while it is above.
    /// Without that, the choices of the first long run became nearly sure,
    /// the plays of a deal nearly all the same, and its learning stopped.
    /// Each step also shrinks the weights by `PLAY_SELF_DECAY` (0) times its
    /// size, as AdamW's weight decay does, so that they stay small unless the
    /// games keep calling for them.
    ///
    /// Every `PLAY_SELF_SNAPSHOT` (50) rounds, and at the end, it keeps a
    /// snapshot of each learner for the league (written to the directory
    /// `PLAY_SELF_SNAPSHOTS` if that is set, as
    /// `round-NNNNN-learner-K-SHAPE.txt`), up to `PLAY_SELF_LEAGUE` (64) of them:
    /// the first four and the latest, so that a long run does not keep every
    /// snapshot it has taken.  It ranks the learners in a tournament
    /// of two-table matches between each two of them on `PLAY_SELF_TOURNAMENT`
    /// (200) other deals; the best is the one that the game plays by.  On
    /// the same deals it plays each learner in two-table matches against the
    /// model of `PLAY_SELF_REFERENCE` if that names a file (see
    /// [`read_model`]), and every `PLAY_SELF_ROBOT_EVERY` (1000) rounds and
    /// at the end against the robot, which is slow, and prints the tricks a
    /// deal each learner takes more, with the standard error.  Those do not
    /// learn, and so cannot be beaten by learning their habits.  Every
    /// `PLAY_SELF_REPORT` (10) rounds of `PLAY_SELF_ROUNDS` (100), it writes
    /// the best's weights to the file `PLAY_SELF_WEIGHTS` if that is set,
    /// sample games to `PLAY_SELF_WATCH` (see `self_play_viewer.html`), and a
    /// play model file (see `model_file`) to `PLAY_SELF_CHECKPOINT`, and
    /// reports the tricks a play each learner's choices lose by the
    /// ensemble's values on the data in `PLAY_TEST` if that is set.  It uses
    /// `PLAY_THREADS` threads, or else every core.
    ///
    /// With `PLAY_SELF_PLAYOUT_SHARE` above 0, that share of the learners'
    /// choices of more than one card with up to `PLAY_SELF_PLAYOUT_TRICKS`
    /// (13) tricks left are played out (see [`playout`]): each card that the
    /// learner could play is played (one of each run of cards that touch,
    /// for all of the run), and the rest of the game after it on the same
    /// deal, by the same players: what a card is worth against the players
    /// that the learner is playing against.  The learner's step then also
    /// climbs, with the weight of `PLAY_SELF_PLAYOUT_WEIGHTS` (1; weights
    /// separated by commas, one for each learner, taken again in turn if
    /// there are fewer) divided by the share, so that the weight does not
    /// depend on it, the slope of the mean of those tricks over the cards,
    /// each weighted by its chance at `PLAY_SELF_PLAYOUT_TEMPERATURE` (1)
    /// times the temperature of self-play: the tricks that the choice is
    /// worth, if it were made at that temperature.  Since that is linear in
    /// the tricks, the luck of a deal averages out, and as self-play only
    /// learns of the card that it played, a playout learns of all of them.
    /// Above 1, a card that the learner is nearly sure not to play has more
    /// of a chance there, and so more of a push if it is better.  (A push of
    /// each card's score by its tricks more than the mean, with no factor of
    /// its chance, pushed the worse cards down without end, and the learners
    /// lost 1.7 tricks a deal in 100 rounds.)  The
    /// report says how many tricks a play each learner's likeliest cards
    /// lose to the best of the cards played out, on the choices played out
    /// since the last report; as the best on one deal is partly luck, that
    /// is above 0 even for the best play.  Every `PLAY_SELF_ENDGAME_EVERY` (0, never) rounds, at a
    /// snapshot, the learners also play `PLAY_SELF_ENDGAME_DEALS` (100)
    /// endgames from 7 tricks left against the ensemble (see
    /// [`endgame_gains`]), from positions that the reference model reaches.
    ///
    /// With `PLAY_SELF_RESUME` naming a play model file, a run takes up from
    /// it, its rounds numbered on from the file's, with its learners and
    /// league; learners that the file lacks start from nothing.  With
    /// `PLAY_SELF_START` naming one instead, the run takes its rounds and
    /// league, and each learner starts from the file's learner of the same
    /// number (taken again in turn if it has fewer, or its game's model if it
    /// has none), grown into the learner's shape (see [`Model::grown_to`]),
    /// or from nothing if it cannot be; with `#K` after the file's name,
    /// every learner starts from its learner K (from 1).  A learner
    /// with no state of Adam (see `make_play_model_file`) starts with steps
    /// that grow from nothing over 20 rounds, so that its weights are not
    /// jolted while Adam learns their scale.
    #[test]
    #[ignore]
    fn self_play() {
        run_self_play(Learning::Outcomes);
    }

    /// Self-play (see [`self_play`], and its settings) in which the learners
    /// learn only from playouts, fitting their scores to the tricks after
    /// each card.  In each game with no player of any card,
    /// `PLAY_SELF_PLAYOUTS` (1) of the learners' choices of more than one
    /// run of touching cards, with up to `PLAY_SELF_PLAYOUT_TRICKS` (13)
    /// tricks left, are chosen at random once the game is over, and played
    /// out (see [`playout`]) by the players of the game, after each run, on
    /// the same `PLAY_SELF_PLAYOUT_DEALS` (100) deals of the hands that the
    /// player cannot see that fit what they have seen, as the playout
    /// yardstick values cards (see [`score_on_the_playout_yardstick`]), or
    /// with 0 on the deal as it was dealt, and the tricks after each card
    /// are their mean over the deals.  Each learner's step is then down the slope
    /// of the squares of how far its score of each card that it could have
    /// played is from the tricks after it, each choice weighted by 1 over
    /// its chance of being played out, so that a game's choices count the
    /// same however many it has, divided by the square of
    /// `PLAY_SELF_LEVEL_ACCURACY` (0.5 tricks), plus the squares of how far
    /// each of those misses is from the mean miss of the choice, divided by
    /// the square of `PLAY_SELF_CHOICE_ACCURACY` (0.05 tricks): the first
    /// fits the scores, the second their differences, as the sum of those
    /// squares is that of the squared misses of the differences of every
    /// two cards of the choice, divided by the number of cards.  Each is
    /// then a number of accuracies, so the differences of the cards, which
    /// are what choose a card, count a hundred times as much as the level
    /// for the same miss.  The tricks are those that its side takes
    /// from then on, less those after the card that the learner would play,
    /// plus those after that card with the learner playing every seat: the
    /// cards differ as they do against the players of the game, but the
    /// level is the learner's own, so that a score says how many tricks
    /// there are to take, and does not go up when a weak opponent will give
    /// tricks away later, whatever the card.  With `PLAY_SELF_LEVEL_ACCURACY`
    /// set to `inf`, only the differences are fitted, which leaves the level
    /// out, and with `PLAY_SELF_CHOICE_ACCURACY` set to `inf`, only the
    /// scores.
    /// The scores are then tricks: a card a trick worse than another ends
    /// a trick below it, however unlikely it was to be played, and two
    /// cards of nearly the same worth end nearly together; and as the fit
    /// is linear in the tricks, the luck of a deal averages out.  The
    /// cards are chosen in the games at `PLAY_SELF_TEMPERATURE` (0.1)
    /// tricks, so a card a tenth of a trick worse is played e⁻¹ times as
    /// often as the best.  Nothing pushes the chances to spread, as each
    /// card played out learns its worth whatever its chance, and neither
    /// `PLAY_SELF_PLAYOUT_SHARE` nor the tricks after each choice are used.
    /// A partner or an opponent who plays worse makes every card worth less,
    /// which the means take away, but it can change which card is best, so
    /// what a learner learns is the best against the players it meets: none
    /// of the games with a player of any card is played out.  The report
    /// says how far the scores are from the tricks, as well as how many
    /// tricks a play the likeliest cards lose to the best played out.  A
    /// learner that starts from a model of self-play, whose scores are not
    /// tricks, first has its scores all multiplied by the factor, and moved
    /// by the shift, that bring them nearest the tricks of the first round's
    /// playouts (see [`Network::scale_scores`] and [`Model::shift_scores`]),
    /// which keeps the order of its cards, in
    /// place of that round's step; otherwise its first steps, fitting
    /// scores many tricks apart, lost what it knew of which card is best.
    #[test]
    #[ignore]
    fn learn_from_playouts() {
        run_self_play(Learning::Playouts);
    }

    /// Self-play, as [`self_play`] and [`learn_from_playouts`] say, the
    /// learners learning as `learning` says.
    fn run_self_play(learning: Learning) {
        let float = |name: &str, default: f64| -> f64 { var(name, &default.to_string()).parse().unwrap() };
        let rounds = number("PLAY_SELF_ROUNDS", 100);
        let batch = number("PLAY_SELF_DEALS", 256);
        let group = number("PLAY_SELF_GROUP", 8);
        let report = number("PLAY_SELF_REPORT", 10);
        let temperature = float("PLAY_SELF_TEMPERATURE", if learning == Learning::Playouts { 0.1 } else { 1.0 });
        let rate = float("PLAY_SELF_RATE", 0.01);
        let random_share = float("PLAY_SELF_RANDOM", 0.0);
        let past_share = float("PLAY_SELF_PAST", 0.0);
        let entropy_target = float("PLAY_SELF_ENTROPY", 0.25);
        let decay = float("PLAY_SELF_DECAY", 0.0);
        let playouts = match learning {
            Learning::Outcomes => Playouts {
                share: float("PLAY_SELF_PLAYOUT_SHARE", 0.0),
                most: number("PLAY_SELF_PLAYOUT_TRICKS", 13),
                per_game: 0,
                deals: 0,
                level: f64::INFINITY,
                apart: f64::INFINITY,
            },
            Learning::Playouts => Playouts {
                share: 0.0,
                most: number("PLAY_SELF_PLAYOUT_TRICKS", 13),
                per_game: number("PLAY_SELF_PLAYOUTS", 1),
                deals: number("PLAY_SELF_PLAYOUT_DEALS", 100),
                level: float("PLAY_SELF_LEVEL_ACCURACY", 0.5),
                apart: float("PLAY_SELF_CHOICE_ACCURACY", 0.05),
            },
        };
        let softened = float("PLAY_SELF_PLAYOUT_TEMPERATURE", 1.0);
        let playout_weights: Vec<f64> = var("PLAY_SELF_PLAYOUT_WEIGHTS", "1")
            .split(',')
            .map(|w| w.trim().parse().expect("PLAY_SELF_PLAYOUT_WEIGHTS: weights separated by commas"))
            .collect();
        let endgame_every = number("PLAY_SELF_ENDGAME_EVERY", 0);
        let endgame_deals = if endgame_every > 0 {
            crate::ai::opening_lead::candidates::opening_leads(19, number("PLAY_SELF_ENDGAME_DEALS", 100))
        } else {
            Vec::new()
        };
        let mut bonus = 0.01;
        let every = number("PLAY_SELF_SNAPSHOT", 50);
        let population = number("PLAY_SELF_LEARNERS", 4);
        let league = number("PLAY_SELF_LEAGUE", 64).max(5);
        let directory = std::env::var("PLAY_SELF_SNAPSHOTS").ok();
        if let Some(directory) = &directory {
            std::fs::create_dir_all(directory).unwrap();
        }
        let deals = |seed: u64, count: usize| -> Vec<String> {
            crate::ai::opening_lead::candidates::opening_leads(seed, count)
                .iter()
                .map(|game| serde_json::to_string(game).unwrap())
                .collect()
        };
        let mut pool = deals(7, number("PLAY_SELF_POOL", 4000));
        let fresh = number("PLAY_SELF_FRESH", 0);
        let mut oldest = 0;
        let round_robin = number("PLAY_SELF_ROUND_ROBIN", 0) == 1;
        let wildcards = number("PLAY_SELF_WILDCARDS", 4);
        let tournament_deals = deals(17, number("PLAY_SELF_TOURNAMENT", 200));
        let reference = std::env::var("PLAY_SELF_REFERENCE").ok().map(|path| read_model(&path));
        let robot_every = number("PLAY_SELF_ROBOT_EVERY", 1000);
        let test = std::env::var("PLAY_TEST")
            .ok()
            .map(|paths| SHAPES.iter().map(|&shape| (shape.name, read_situations(&paths, shape))).collect::<Vec<_>>());
        let shapes: Vec<&'static Shape> = var("PLAY_SELF_SHAPES", V1.name)
            .split(',')
            .map(|name| Shape::named(name.trim()).unwrap_or_else(|| panic!("PLAY_SELF_SHAPES: there is no shape {name}")))
            .collect();
        let mut learners: Vec<Model> = Vec::new();
        let mut adams: Vec<fitting::Adam> = Vec::new();
        let mut warmups: Vec<usize> = Vec::new();
        let mut snapshots: Vec<(usize, Model)> = Vec::new();
        let mut first_round = 1;
        let mut resumed = false;
        if let Ok(path) = std::env::var("PLAY_SELF_RESUME") {
            let file = model_file::ModelFile::read(&path);
            first_round = file.round + 1;
            resumed = true;
            for learner in file.learners {
                warmups.push(if learner.adam.is_some() { 0 } else { 20 });
                adams.push(learner.adam.unwrap_or_else(|| fitting::Adam::new(learner.model.network.params.len())));
                learners.push(learner.model);
            }
            snapshots = file.league;
            println!(
                "resuming after round {} with {} learners, and past models from rounds {:?}",
                file.round,
                learners.len(),
                snapshots.iter().map(|s| s.0).collect::<Vec<_>>()
            );
        }
        // Or the models of a file, which the learners grow from.
        let mut start = Vec::new();
        if let Ok(path) = std::env::var("PLAY_SELF_START") {
            let (path, only) = match path.split_once('#') {
                Some((file, learner)) => (file.to_string(), Some(learner.parse::<usize>().expect("a learner's number after #"))),
                None => (path, None),
            };
            let file = model_file::ModelFile::read(&path);
            first_round = file.round + 1;
            snapshots = file.league;
            println!(
                "starting from the {} learners of round {} of {path}",
                file.learners.len().max(1),
                file.round
            );
            start = file.learners.into_iter().map(|learner| learner.model).collect();
            if let Some(learner) = only {
                start = vec![start.swap_remove(learner - 1)];
                println!("every learner starts from its learner {learner}");
            }
            if start.is_empty() {
                start.push(file.best);
            }
        }
        // The learners that the file lacks take the shapes in turn.
        while learners.len() < population {
            let shape = shapes[learners.len() % shapes.len()];
            let grown = start.get(learners.len() % start.len().max(1)).and_then(|start| start.grown_to(shape));
            warmups.push(if grown.is_some() { 20 } else { 0 });
            let model = grown.unwrap_or_else(|| Model::new(shape, 11 + learners.len() as u64));
            adams.push(fitting::Adam::new(model.network.params.len()));
            learners.push(model);
        }
        println!(
            "learners of the shapes {}",
            learners.iter().map(|l| l.shape.name).collect::<Vec<_>>().join(", ")
        );
        let mut playout_losses = vec![Tally::default(); learners.len()];
        if snapshots.is_empty() {
            snapshots = learners.iter().map(|l| (first_round - 1, l.clone())).collect();
        }
        let last_round = first_round + rounds - 1;
        let mut best = 0;
        let mut watched: Vec<serde_json::Value> = Vec::new();
        let samples = number("PLAY_SELF_SAMPLES", 2);
        let mut rng = StdRng::seed_from_u64(10 + first_round as u64);
        let threads = number(
            "PLAY_THREADS",
            std::thread::available_parallelism().map_or(4, |n| n.get()),
        );
        // One thread deals the fresh deals into a channel that holds two
        // rounds' worth, keeping ahead of the rounds that take them.  Dealing
        // takes far less time than playing, so it barely adds to the threads
        // that play, and the deals are the same whatever their number.
        let (dealt, fresh_deals) = std::sync::mpsc::sync_channel::<String>(2 * fresh);
        if fresh > 0 {
            let mut dealer = (Generator::new(), StdRng::seed_from_u64(1000 + first_round as u64), 10);
            std::thread::spawn(move || while dealt.send(fresh_deal(&mut dealer)).is_ok() {});
        }
        // The same threads play every round.
        let workers = rayon::ThreadPoolBuilder::new().num_threads(threads).build().unwrap();
        let started = std::time::Instant::now();
        for round in first_round..=last_round {
            let deals: Vec<usize> = if pool.len() == batch {
                (0..batch).collect()
            } else {
                (0..batch).map(|_| rng.gen_range(0..pool.len())).collect()
            };
            let seed = rng.gen::<u64>();
            let chunk = batch.div_ceil(threads);
            let (learners_now, pool_now, past, bonus_now) = (&learners, &pool, &snapshots, bonus);
            let watching = round % report == 0 || round == last_round;
            type Results = (Vec<Vec<f64>>, Vec<usize>, f64, Vec<serde_json::Value>, Vec<Tally>);
            let results: Vec<Results> = workers.install(|| {
                deals
                    .par_chunks(chunk)
                    .enumerate()
                    .map(|(t, deals)| {
                        {
                            let mut rng = StdRng::seed_from_u64(seed + t as u64);
                            let mut conventions = std::collections::HashMap::new();
                            let mut gradients: Vec<Vec<f64>> =
                                learners_now.iter().map(|l| vec![0.0; l.network.params.len()]).collect();
                            let mut counts = vec![0; learners_now.len()];
                            let mut entropy = 0.0;
                            let mut sampled = Vec::new();
                            let mut lost = vec![Tally::default(); learners_now.len()];
                            for (d, &deal) in deals.iter().enumerate() {
                                let json = &pool_now[deal];
                                let system: GameState<()> = serde_json::from_str(json).unwrap();
                                let convention = conventions
                                    .entry(format!("{:?}", system.system))
                                    .or_insert_with(|| system.system.convention());
                                // Who plays each seat (by its index: south,
                                // west, north, east) in each play of the deal.
                                let population = learners_now.len();
                                let lineups: Vec<[Player; 4]> = if round_robin {
                                    use rand::seq::SliceRandom;
                                    let mut order: Vec<usize> = (0..population).collect();
                                    order.shuffle(&mut rng);
                                    let four = |i: usize, order: &[usize]| Player::Learning(order[i % population]);
                                    let [a, b, c, d] = [0, 1, 2, 3].map(|i| four(i, &order));
                                    let mut lineups = Vec::new();
                                    // A partnership of `one` and `two` against
                                    // `three` and `four`, at both tables and
                                    // with the partners in either seat.
                                    let mut matched = |one, two, three, four| {
                                        lineups.push([one, three, two, four]);
                                        lineups.push([two, four, one, three]);
                                        lineups.push([three, one, four, two]);
                                        lineups.push([four, two, three, one]);
                                    };
                                    matched(a, b, c, d);
                                    matched(a, c, b, d);
                                    matched(a, d, b, c);
                                    for _ in 0..wildcards / 2 {
                                        order.shuffle(&mut rng);
                                        let [x, y, z] = [0, 1, 2].map(|i| four(i, &order));
                                        let wild = if random_share == 0.0
                                            || rng.gen_range(0.0..past_share + random_share) < past_share
                                        {
                                            let (round, ref weights) = past[rng.gen_range(0..past.len())];
                                            Player::Past(weights, round)
                                        } else {
                                            Player::Random
                                        };
                                        // `x` and the wildcard against `y` and
                                        // `z`, at both tables.
                                        lineups.push([x, y, wild, z]);
                                        lineups.push([y, x, z, wild]);
                                    }
                                    lineups
                                } else {
                                    (0..group)
                                        .map(|_| {
                                            [0; 4].map(|_| {
                                                let draw = rng.gen_range(0.0..1.0);
                                                if draw < past_share {
                                                    let (round, ref weights) = past[rng.gen_range(0..past.len())];
                                                    Player::Past(weights, round)
                                                } else if draw < past_share + random_share {
                                                    Player::Random
                                                } else {
                                                    Player::Learning(rng.gen_range(0..population))
                                                }
                                            })
                                        })
                                        .collect()
                                };
                                let games: Vec<(Vec<Decision>, [usize; 2])> = lineups
                                    .into_iter()
                                    .enumerate()
                                    .map(|(g, players)| {
                                        let record = watching && t == 0 && g == 0 && d < samples;
                                        let (decisions, won, game) = play_out(
                                            json,
                                            convention,
                                            learners_now,
                                            temperature,
                                            players,
                                            record,
                                            playouts,
                                            &mut rng,
                                        );
                                        sampled.extend(game);
                                        (decisions, won)
                                    })
                                    .collect();
                                // The mean over the plays of this deal of the
                                // tricks each side took from each trick on.
                                let mut sums = [[0.0; 2]; 14];
                                let mut seen = [[0.0; 2]; 14];
                                for (decisions, won) in &games {
                                    for d in decisions {
                                        sums[d.trick][d.side] += (won[d.side] - d.won_before) as f64;
                                        seen[d.trick][d.side] += 1.0;
                                    }
                                }
                                for (decisions, won) in &games {
                                    for d in decisions {
                                        let to_go = (won[d.side] - d.won_before) as f64;
                                        let advantage = to_go - sums[d.trick][d.side] / seen[d.trick][d.side];
                                        let spread: f64 = d
                                            .cards
                                            .iter()
                                            .filter(|c| c.1 > 0.0)
                                            .map(|c| -c.1 * c.1.ln())
                                            .sum();
                                        entropy += spread;
                                        let model = &learners_now[d.learner];
                                        if learning == Learning::Playouts {
                                            if let Some(values) = &d.played_out {
                                                // Fits the scores to the
                                                // tricks, and their
                                                // differences to those of the
                                                // tricks, by least squares,
                                                // each square divided by
                                                // that of its accuracy.
                                                let scores: Vec<f64> =
                                                    d.cards.iter().map(|(entries, _)| model.network.score(entries)).collect();
                                                let n = values.len() as f64;
                                                let value_mean = values.iter().sum::<f64>() / n;
                                                let score_mean = scores.iter().sum::<f64>() / n;
                                                for (((entries, _), &value), &score) in d.cards.iter().zip(values).zip(&scores) {
                                                    let (apart, value_apart) = (score - score_mean, value - value_mean);
                                                    let (miss, miss_apart) = (score - value, apart - value_apart);
                                                    let (level, choice) = (playouts.level.powi(-2), playouts.apart.powi(-2));
                                                    let off = level * miss + choice * miss_apart;
                                                    let w = d.weight;
                                                    let tally = &mut lost[d.learner];
                                                    tally.off += miss * miss / n;
                                                    tally.off_apart += miss_apart * miss_apart / n;
                                                    tally.chi_squared += (level * miss * miss + choice * miss_apart * miss_apart) / n;
                                                    tally.across += w * apart * value_apart;
                                                    tally.squares += w * apart * apart;
                                                    tally.weights += w;
                                                    tally.scores += w * score;
                                                    tally.tricks += w * value;
                                                    model.network.add_gradient(entries, -w * off, &mut gradients[d.learner]);
                                                }
                                            }
                                        } else {
                                            for (j, (entries, chance)) in d.cards.iter().enumerate() {
                                                let wanted = if j == d.chosen { 1.0 } else { 0.0 };
                                                // The slope of the entropy of the
                                                // choice with this card's score.
                                                let log = if *chance > 0.0 { chance.ln() } else { 0.0 };
                                                let spreading = -chance * (log + spread);
                                                let g = (advantage * (wanted - chance) + bonus_now * spreading) / temperature;
                                                learners_now[d.learner].network.add_gradient(entries, g, &mut gradients[d.learner]);
                                            }
                                            if let Some(values) = &d.played_out {
                                                let weight = playout_weights[d.learner % playout_weights.len()];
                                                if weight != 0.0 {
                                                    // The chances at the softened
                                                    // temperature, from those at
                                                    // self-play's.
                                                    let odds: Vec<f64> = d.cards.iter().map(|c| c.1.powf(1.0 / softened)).collect();
                                                    let sum: f64 = odds.iter().sum();
                                                    let soft: Vec<f64> = odds.iter().map(|o| o / sum).collect();
                                                    let mean: f64 = values.iter().zip(&soft).map(|(v, p)| v * p).sum();
                                                    for (((entries, _), value), p) in d.cards.iter().zip(values).zip(&soft) {
                                                        let g = weight * d.weight * p * (value - mean) / (softened * temperature);
                                                        learners_now[d.learner].network.add_gradient(entries, g, &mut gradients[d.learner]);
                                                    }
                                                }
                                            }
                                        }
                                        if let Some(values) = &d.played_out {
                                            let likeliest =
                                                (0..values.len()).max_by(|&a, &b| d.cards[a].1.total_cmp(&d.cards[b].1)).unwrap();
                                            lost[d.learner].lost += values.iter().copied().fold(f64::MIN, f64::max) - values[likeliest];
                                            lost[d.learner].choices += 1;
                                        }
                                        counts[d.learner] += 1;
                                    }
                                }
                            }
                            (gradients, counts, entropy, sampled, lost)
                        }
                    })
                    .collect()
            });
            let counts: Vec<usize> = (0..learners.len())
                .map(|i| results.iter().map(|r| r.1[i]).sum())
                .collect();
            let decisions: usize = counts.iter().sum();
            let entropy: f64 = results.iter().map(|r| r.2).sum::<f64>() / decisions.max(1) as f64;
            bonus = if entropy < entropy_target { bonus * 1.05 } else { bonus * 0.95 }.clamp(1e-4, 1.0);
            for result in &results {
                for (total, tally) in playout_losses.iter_mut().zip(&result.4) {
                    total.add(tally);
                }
            }
            // Learning from playouts, a learner whose scores are not yet
            // tricks, as those of self-play are not, first has them made as
            // near to the tricks as one factor makes them, which keeps the
            // order of its cards: the step of that round is skipped.  A run
            // that resumes one of these has its scores in tricks already.
            let calibrating = learning == Learning::Playouts && round == first_round && !resumed;
            if calibrating {
                for (i, learner) in learners.iter_mut().enumerate() {
                    let mut tally = Tally::default();
                    for result in &results {
                        tally.add(&result.4[i]);
                    }
                    let (factor, shift) = tally.fit();
                    if factor.is_finite() && factor > 0.0 {
                        learner.network.scale_scores(factor);
                        if playouts.level.is_finite() {
                            learner.shift_scores(shift);
                            println!("learner {}'s scores are multiplied by {factor:.4} and {shift:+.3} added, to be tricks", i + 1);
                        } else {
                            println!("learner {}'s scores are multiplied by {factor:.4} to be tricks", i + 1);
                        }
                    } else {
                        println!("learner {}'s scores are left as they are: {factor}", i + 1);
                    }
                }
            }
            for (i, learner) in learners.iter_mut().enumerate().filter(|_| !calibrating) {
                let gradient: Vec<f64> = (0..learner.network.params.len())
                    .map(|j| results.iter().map(|r| r.0[i][j]).sum::<f64>() / counts[i].max(1) as f64)
                    .collect();
                let ramp = if warmups[i] > 0 {
                    ((round - first_round + 1) as f64 / warmups[i] as f64).min(1.0)
                } else {
                    1.0
                };
                for p in &mut learner.network.params {
                    *p *= 1.0 - rate * ramp * decay;
                }
                adams[i].step(&mut learner.network.params, &gradient, rate * ramp);
            }
            for result in results {
                for mut game in result.3 {
                    game["round"] = round.into();
                    watched.push(game);
                }
            }
            for deal in fresh_deals.iter().take(fresh) {
                pool[oldest] = deal;
                oldest = (oldest + 1) % pool.len();
            }
            if round % every == 0 || round == last_round {
                for (i, learner) in learners.iter().enumerate() {
                    snapshots.push((round, learner.clone()));
                    if let Some(directory) = &directory {
                        let name = learner.shape.name;
                        learner.network.write(&format!("{directory}/round-{round:05}-learner-{}-{name}.txt", i + 1));
                    }
                }
                // The first four, and the latest.
                if snapshots.len() > league {
                    snapshots.drain(4..snapshots.len() - league + 4);
                }
                let standings = tournament(&learners, &tournament_deals);
                best = (0..learners.len())
                    .max_by(|&a, &b| standings[a].total_cmp(&standings[b]))
                    .unwrap();
                println!(
                    "round {round}: tournament of the learners, tricks a deal more than the others: {}; the best is learner {}",
                    standings.iter().map(|s| format!("{s:+.3}")).collect::<Vec<_>>().join(", "),
                    best + 1
                );
                // Against opponents that do not learn, which a learner cannot
                // beat by learning the habits of the others.
                let robot_too = round % robot_every == 0 || round == last_round;
                for (name, opponent) in [("the robot", None), ("the reference", reference.as_ref())] {
                    if (name == "the reference" && opponent.is_none()) || (name == "the robot" && !robot_too) {
                        continue;
                    }
                    let results: Vec<String> = workers.install(|| {
                        learners
                            .iter()
                            .map(|learner| {
                                let (mean, error) = match_at_two_tables(learner, opponent, &tournament_deals);
                                format!("{mean:+.3} ({error:.3})")
                            })
                            .collect()
                    });
                    println!("round {round}: against {name}, tricks a deal more: {}", results.join(", "));
                }
                if endgame_every > 0 && (round % endgame_every == 0 || round == last_round) {
                    let contestants: Vec<Contestant> = learners.iter().map(Contestant::Model).collect();
                    let gains = workers.install(|| endgame_gains(&contestants, &endgame_deals, 7, 7, reference.as_ref()));
                    println!(
                        "round {round}: endgames from 7 tricks against the ensemble: {}",
                        (0..learners.len()).map(|i| endgame_summary(&gains, i)).collect::<Vec<_>>().join("; ")
                    );
                }
            }
            if watching {
                if let Ok(path) = std::env::var("PLAY_SELF_WATCH") {
                    // The latest games, enough to watch without the page
                    // growing without end.
                    let start = watched.len().saturating_sub(400);
                    let data = serde_json::to_string(&watched[start..]).unwrap();
                    // A published page gets the document around it; a file
                    // opened in a browser needs its own.
                    let page = format!(
                        "<!doctype html>\n<meta charset=\"utf-8\">\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, viewport-fit=cover\">\n{}",
                        include_str!("self_play_viewer.html")
                            .replace("/*GAME_CSS*/", include_str!("../../../abridge/src/style.css"))
                            .replace("[/*GAMES*/]", &data)
                    );
                    std::fs::write(path, page).unwrap();
                }
                print!(
                    "round {round} at {:.0} s: {decisions} choices {counts:?}, entropy {entropy:.3} a choice, bonus {bonus:.4}",
                    started.elapsed().as_secs_f64()
                );
                if let Some(test) = &test {
                    let losses: Vec<String> = learners
                        .iter()
                        .map(|learner| {
                            let test = &test.iter().find(|t| t.0 == learner.shape.name).unwrap().1;
                            let loss = test
                                .iter()
                                .map(|s| {
                                    let most = s.choice.iter().map(|c| c.1).fold(f64::MIN, f64::max);
                                    most - s.choice[learner.network.choice(&s.choice)].1
                                })
                                .sum::<f64>()
                                / test.len() as f64;
                            format!("{loss:.4}")
                        })
                        .collect();
                    print!(", the test data's choices lose {} tricks a play", losses.join(", "));
                }
                if playouts.share > 0.0 || playouts.per_game > 0 {
                    let lost: Vec<String> = playout_losses
                        .iter()
                        .map(|t| format!("{:.4} ({})", t.lost / t.choices.max(1) as f64, t.choices))
                        .collect();
                    print!(
                        ", the likeliest cards lose {} tricks a play to the best played out",
                        lost.join(", ")
                    );
                    if learning == Learning::Playouts {
                        let mean = |t: &Tally, sum: f64| sum / t.choices.max(1) as f64;
                        let off: Vec<String> =
                            playout_losses.iter().map(|t| format!("{:.3}", mean(t, t.off).sqrt())).collect();
                        let apart: Vec<String> =
                            playout_losses.iter().map(|t| format!("{:.3}", mean(t, t.off_apart).sqrt())).collect();
                        let chi: Vec<String> =
                            playout_losses.iter().map(|t| format!("{:.2}", mean(t, t.chi_squared))).collect();
                        print!(
                            ", their scores miss the tricks by {}, and their differences by {} (root mean square), and the squares fitted come to {} a card",
                            off.join(", "),
                            apart.join(", "),
                            chi.join(", ")
                        );
                    }
                    playout_losses.iter_mut().for_each(|l| *l = Tally::default());
                }
                println!();
                if let Ok(path) = std::env::var("PLAY_SELF_WEIGHTS") {
                    learners[best].network.write(&path);
                }
                if let Ok(path) = std::env::var("PLAY_SELF_CHECKPOINT") {
                    model_file::ModelFile {
                        round,
                        best: learners[best].clone(),
                        learners: learners
                            .iter()
                            .zip(&adams)
                            .map(|(model, adam)| model_file::Learner {
                                model: model.clone(),
                                adam: Some(adam.clone()),
                            })
                            .collect(),
                        league: league_of(&snapshots),
                    }
                    .write(&path);
                }
            }
        }
    }

    /// A new deal that the robot bid, whose opening lead is next, from
    /// `dealer`: a generator, the random numbers it deals with, and the index
    /// of the next situation, which picks the bidding system in turn.
    fn fresh_deal(dealer: &mut (Generator, StdRng, usize)) -> String {
        let (generator, rng, index) = dealer;
        loop {
            // The tenth of each sixteen situations is the defender on lead.
            let found = generator.situation(rng, *index, 13);
            *index += 16;
            if let Some(json) = found {
                let game: GameState<()> = serde_json::from_str(&json).unwrap();
                if game.played.is_empty() {
                    return json;
                }
            }
        }
    }

    /// How many more tricks a deal each of `learners` takes than the others,
    /// on average over two-table matches against each of them on `deals`,
    /// each playing the card it scores highest.
    fn tournament(learners: &[Model], deals: &[String]) -> Vec<f64> {
        let mut conventions = std::collections::HashMap::new();
        let mut standings = vec![0.0; learners.len()];
        for json in deals {
            let game: GameState<()> = serde_json::from_str(json).unwrap();
            let convention = conventions
                .entry(format!("{:?}", game.system))
                .or_insert_with(|| game.system.convention());
            for a in 0..learners.len() {
                for b in a + 1..learners.len() {
                    let gain = play_at_a_table(json, convention, &learners[a], 0, Some(&learners[b])) as f64
                        - play_at_a_table(json, convention, &learners[a], 1, Some(&learners[b])) as f64;
                    let share = (deals.len() * (learners.len() - 1)) as f64;
                    standings[a] += gain / share;
                    standings[b] -= gain / share;
                }
            }
        }
        standings
    }

    /// How many more tricks a deal `model` takes than `other` (or the robot,
    /// if there is none), in two-table matches on `deals`, each playing the
    /// card it scores highest, with the standard error.  The deals are played
    /// on rayon's threads, since the robot is slow.
    fn match_at_two_tables(model: &Model, other: Option<&Model>, deals: &[String]) -> (f64, f64) {
        let gains: Vec<f64> = deals
            .par_iter()
            .map(|json| {
                let game: GameState<()> = serde_json::from_str(json).unwrap();
                let convention = game.system.convention();
                play_at_a_table(json, &convention, model, 0, other) as f64
                    - play_at_a_table(json, &convention, model, 1, other) as f64
            })
            .collect();
        let n = gains.len() as f64;
        let mean = gains.iter().sum::<f64>() / n;
        (mean, (gains.iter().map(|g| (g - mean).powi(2)).sum::<f64>() / (n - 1.0) / n).sqrt())
    }

    /// The past models that a play model file keeps of `snapshots`: the
    /// first four, and the last eight.
    fn league_of(snapshots: &[(usize, Model)]) -> Vec<(usize, Model)> {
        let first = snapshots.len().min(4);
        let last = snapshots.len().saturating_sub(8).max(first);
        snapshots[..first].iter().chain(&snapshots[last..]).cloned().collect()
    }

    /// Writes a play model file (see `model_file`) to `PLAY_MODEL_FILE` for a
    /// run of self-play that did not write one: the weights in the file
    /// `PLAY_WEIGHTS` after round `PLAY_ROUND`, as the game's model and as a
    /// learner with no state of Adam, and as its league, of the snapshots in
    /// the directory `PLAY_SELF_SNAPSHOTS` (whose names start `round-` and
    /// the round), the first four and the last eight, all of the shape
    /// `PLAY_SHAPE` (`v1`).
    #[test]
    #[ignore]
    fn make_play_model_file() {
        let shape = shape_of("PLAY_SHAPE");
        let read = |path: &str| {
            let network = Network::read(path, shape.entries(), shape.hidden, shape.second);
            Model { shape, network }
        };
        let weights = read(&std::env::var("PLAY_WEIGHTS").expect("PLAY_WEIGHTS"));
        let directory = std::env::var("PLAY_SELF_SNAPSHOTS").expect("PLAY_SELF_SNAPSHOTS");
        let mut snapshots: Vec<(usize, Model)> = std::fs::read_dir(&directory)
            .unwrap()
            .filter_map(|entry| {
                let path = entry.unwrap().path();
                let name = path.file_stem()?.to_str()?;
                let digits: String = name.strip_prefix("round-")?.chars().take_while(char::is_ascii_digit).collect();
                Some((digits.parse().ok()?, read(path.to_str()?)))
            })
            .collect();
        snapshots.sort_by_key(|s| s.0);
        model_file::ModelFile {
            round: number("PLAY_ROUND", 0),
            best: weights.clone(),
            learners: vec![model_file::Learner { model: weights, adam: None }],
            league: league_of(&snapshots),
        }
        .write(&std::env::var("PLAY_MODEL_FILE").expect("PLAY_MODEL_FILE"));
    }

    /// The play model in `path`: the game's model of a play model file, or
    /// with `#K` after the file's name, its learner K (from 1), or a file of
    /// its weights one a line, as self-play writes them, of the shape that
    /// its name ends with (as `round-NNNNN-learner-K-SHAPE.txt`), or else of
    /// `PLAY_SHAPE` (`v1`).
    fn read_model(path: &str) -> Model {
        if let Some((file, learner)) = path.split_once('#') {
            let learner: usize = learner.parse().expect("a learner's number after #");
            return model_file::ModelFile::read(file).learners[learner - 1].model.clone();
        }
        let start = std::fs::read(path).unwrap().into_iter().take(8).collect::<Vec<u8>>();
        if start.starts_with(b"PLAYMDL") {
            model_file::ModelFile::read(path).best
        } else {
            // A snapshot of self-play names its shape at the end.
            let named = path
                .strip_suffix(".txt")
                .and_then(|stem| SHAPES.iter().filter(|s| stem.ends_with(&format!("-{}", s.name))).max_by_key(|s| s.name.len()));
            let shape = named.copied().unwrap_or_else(|| shape_of("PLAY_SHAPE"));
            Model { shape, network: Network::read(path, shape.entries(), shape.hidden, shape.second) }
        }
    }

    /// Plays `json`, a game whose opening lead is next, to the end: the side
    /// `model_side` (0 for north and south) by the play model `model`, and
    /// the other by `other` if there is one, or else by the robot.  Gives the
    /// tricks of north and south.
    fn play_at_a_table(
        json: &str,
        convention: &Convention,
        model: &Model,
        model_side: usize,
        other: Option<&Model>,
    ) -> usize {
        use crate::ai::PlayAI;
        let mut game: GameState<()> = serde_json::from_str(json).unwrap();
        let mut robot = super::super::RandomPlay;
        while !game.hand_done {
            let place = current_trick(&game).len();
            let seat = chooser(&game, game.lead.unwrap() + place);
            let card = if seat as usize % 2 == model_side {
                model_card(model, &game, convention)
            } else {
                match other {
                    Some(network) => model_card(network, &game, convention),
                    None => robot.play(&game),
                }
            };
            game.play(card);
        }
        game.ns_tricks
    }

    /// A player in [`match_endgames_against_the_ensemble`]: a play model,
    /// the robot's card play, or [`EnsemblePlay`](crate::EnsemblePlay).
    enum Endgamer<'a> {
        Model(&'a Model),
        Robot,
        Ensemble(Box<crate::EnsemblePlay>),
        Playout(Box<crate::PlayoutPlay>),
    }

    impl Endgamer<'_> {
        fn play(&mut self, game: &GameState<()>, convention: &Convention) -> Card {
            use crate::ai::PlayAI;
            match self {
                Endgamer::Model(model) => model_card(model, game, convention),
                Endgamer::Robot => super::super::RandomPlay.play(game),
                Endgamer::Ensemble(player) => player.play(game),
                Endgamer::Playout(player) => player.play(game),
            }
        }
    }

    /// Plays `game` to the end, north and south by `ns` and east and west by
    /// `ew`, and gives the tricks of north and south.
    fn play_endgame(mut game: GameState<()>, convention: &Convention, ns: &mut Endgamer, ew: &mut Endgamer) -> usize {
        while !game.hand_done {
            let place = current_trick(&game).len();
            let seat = chooser(&game, game.lead.unwrap() + place);
            let card = if (seat as usize).is_multiple_of(2) { ns.play(&game, convention) } else { ew.play(&game, convention) };
            game.play(card);
        }
        game.ns_tricks
    }

    /// Who plays one side in [`endgame_gains`].
    #[derive(Clone, Copy)]
    enum Contestant<'a> {
        Model(&'a Model),
        Robot,
        /// The ensemble, with other random deals than its opponents'.
        Ensemble,
        /// [`PlayoutPlay`](crate::PlayoutPlay) with this model, over this
        /// many deals.
        Playout(&'a Model, usize),
    }

    /// Plays endgames against [`EnsemblePlay`](crate::EnsemblePlay), which
    /// plays the card with the best mean double-dummy value over deals of the
    /// hidden hands that fit what it has seen, with up to `solved` tricks
    /// left (and otherwise plays as the robot does), the nearest to best play
    /// that the robot has.  Each of `deals`, whose opening lead is next, is
    /// played by `lead_in` (or the robot) until `tricks` tricks are left, and
    /// from there at three tables: the ensemble on both sides, and each of
    /// `contestants` for the declaring side and for the defenders, the
    /// ensemble playing the other side.  Gives for each deal and contestant
    /// the tricks that the contestant's sides took more than the ensemble's
    /// would have, declaring and defending.  It plays the deals at once on
    /// the threads of the rayon pool that it is called in.
    fn endgame_gains(
        contestants: &[Contestant],
        deals: &[GameState<()>],
        tricks: usize,
        solved: usize,
        lead_in: Option<&Model>,
    ) -> Vec<Vec<(f64, f64)>> {
        let tricks_left = |game: &GameState<()>| game.hands.iter().map(|hand| hand.len()).max().unwrap_or(0);
        deals
            .par_iter()
            .enumerate()
            .map(|(i, game)| {
                let convention = game.system.convention();
                let mut game: GameState<()> = serde_json::from_str(&serde_json::to_string(game).unwrap()).unwrap();
                let mut lead_in = match lead_in {
                    Some(model) => Endgamer::Model(model),
                    None => Endgamer::Robot,
                };
                while !game.hand_done && !(current_trick(&game).is_empty() && tricks_left(&game) == tricks) {
                    let card = lead_in.play(&game, &convention);
                    game.play(card);
                }
                let position = serde_json::to_string(&game).unwrap();
                let at = || serde_json::from_str::<GameState<()>>(&position).unwrap();
                let ensemble =
                    |seed: u64| Endgamer::Ensemble(Box::new(crate::EnsemblePlay::seeded(seed).solving_up_to(solved)));
                let seed = i as u64;
                let both = play_endgame(at(), &convention, &mut ensemble(seed), &mut ensemble(seed)) as f64;
                let ns_declares = game.dummy().is_some_and(|dummy| (dummy as usize).is_multiple_of(2));
                contestants
                    .iter()
                    .map(|&contestant| {
                        let player = || match contestant {
                            Contestant::Model(model) => Endgamer::Model(model),
                            Contestant::Robot => Endgamer::Robot,
                            Contestant::Ensemble => ensemble(seed + 1_000_000),
                            Contestant::Playout(model, deals) => Endgamer::Playout(Box::new(
                                crate::PlayoutPlay::seeded(seed + 2_000_000).with_model(model.clone()).with_deals(deals),
                            )),
                        };
                        let as_ns = play_endgame(at(), &convention, &mut player(), &mut ensemble(seed)) as f64;
                        let as_ew = play_endgame(at(), &convention, &mut ensemble(seed), &mut player()) as f64;
                        // North and south's tricks with the player in place
                        // of the ensemble, as north and south, and as east
                        // and west.
                        let (ns, ew) = (as_ns - both, both - as_ew);
                        if ns_declares { (ns, ew) } else { (ew, ns) }
                    })
                    .collect()
            })
            .collect()
    }

    /// The mean of `values` and its standard error.
    fn mean_and_error(values: &[f64]) -> (f64, f64) {
        let n = values.len() as f64;
        let mean = values.iter().sum::<f64>() / n;
        (mean, (values.iter().map(|v| (v - mean).powi(2)).sum::<f64>() / (n - 1.0) / n).sqrt())
    }

    /// For each contestant of `gains` (see [`endgame_gains`]), the tricks a
    /// deal more than the ensemble's, declaring, defending and in all, each
    /// with its standard error, as text.
    fn endgame_summary(gains: &[Vec<(f64, f64)>], contestant: usize) -> String {
        let declaring = mean_and_error(&gains.iter().map(|g| g[contestant].0).collect::<Vec<_>>());
        let defending = mean_and_error(&gains.iter().map(|g| g[contestant].1).collect::<Vec<_>>());
        let total = mean_and_error(&gains.iter().map(|g| g[contestant].0 + g[contestant].1).collect::<Vec<_>>());
        format!(
            "declaring {:+.3} ({:.3}), defending {:+.3} ({:.3}), in all {:+.3} ({:.3})",
            declaring.0, declaring.1, defending.0, defending.1, total.0, total.1
        )
    }

    /// Plays endgames against [`EnsemblePlay`](crate::EnsemblePlay) (see
    /// [`endgame_gains`]): `PLAY_ENDGAME_DEALS` (100) deals that the robot
    /// bid, played by the play model of `PLAY_ENDGAME_LEAD_IN` (or else the
    /// first model of the players) until `PLAY_ENDGAME_TRICKS` (7) tricks are
    /// left, with the ensemble solving with up to `PLAY_ENDGAME_SOLVED` (7)
    /// tricks left, for each player of `PLAY_ENDGAME_PLAYERS`
    /// (comma-separated play models as [`read_model`] reads them, `robot`,
    /// `ensemble`, which is the ensemble with other random deals, or
    /// `playout=` and a model, which is [`PlayoutPlay`](crate::PlayoutPlay)
    /// with that model over `PLAY_PLAYOUT_DEALS` (100) deals).
    /// Prints the tricks a deal that each player's sides took more than the
    /// ensemble's would have, declaring, defending and in all, with the
    /// standard errors.  It uses `PLAY_THREADS` (1) threads.
    #[test]
    #[ignore]
    fn match_endgames_against_the_ensemble() {
        let tricks = number("PLAY_ENDGAME_TRICKS", 7);
        let solved = number("PLAY_ENDGAME_SOLVED", 7);
        let count = number("PLAY_ENDGAME_DEALS", 100);
        let names: Vec<String> = var("PLAY_ENDGAME_PLAYERS", "robot").split(',').map(|n| n.trim().to_string()).collect();
        let models: Vec<Option<Model>> = names
            .iter()
            .map(|n| match n.strip_prefix("playout=") {
                Some(path) => Some(read_model(path)),
                None => (n != "robot" && n != "ensemble").then(|| read_model(n)),
            })
            .collect();
        let playout_deals = number("PLAY_PLAYOUT_DEALS", 100);
        let contestants: Vec<Contestant> = models
            .iter()
            .zip(&names)
            .map(|(model, name)| match (model, name.as_str()) {
                (Some(model), name) if name.starts_with("playout=") => Contestant::Playout(model, playout_deals),
                (Some(model), _) => Contestant::Model(model),
                (None, "robot") => Contestant::Robot,
                _ => Contestant::Ensemble,
            })
            .collect();
        let lead_in = std::env::var("PLAY_ENDGAME_LEAD_IN")
            .ok()
            .map(|path| read_model(&path))
            .or_else(|| models.iter().flatten().next().cloned());
        let workers = rayon::ThreadPoolBuilder::new().num_threads(number("PLAY_THREADS", 1)).build().unwrap();
        let started = std::time::Instant::now();
        let deals = crate::ai::opening_lead::candidates::opening_leads(13, count);
        let gains = workers.install(|| endgame_gains(&contestants, &deals, tricks, solved, lead_in.as_ref()));
        println!(
            "{} deals from {tricks} tricks left in {:.0} s; tricks a deal more than the ensemble would have taken:",
            gains.len(),
            started.elapsed().as_secs_f64()
        );
        for (p, name) in names.iter().enumerate() {
            println!("{name}: {}", endgame_summary(&gains, p));
        }
    }

    /// A yardstick for play models that does not move as they learn, and
    /// that is far less noisy than a match.  The competitor, the play model
    /// `PLAY_YARD_COMPETITOR` (see [`read_model`]), plays `PLAY_YARD_GAMES`
    /// (200) deals that the robot bid, and a share `PLAY_YARD_SHARE` (0.25)
    /// of its choices of more than one run of touching cards are kept.  In
    /// each, each run is played out on the same `PLAY_YARD_DEALS` (100) deals
    /// of the hands that the player cannot see that fit what they have seen
    /// (see [`card_values`](crate::ai::playout::card_values)).  For the
    /// competitor and each model of `PLAY_YARD_MODELS` (comma-separated), it
    /// prints the tricks a play that its likeliest cards lose to the best
    /// card, with the standard errors, valued twice: with the competitor
    /// playing on for both sides, which is the same for every model, and
    /// with the model playing on for its own side and the competitor for the
    /// opponents, which is what each card is worth to the model against the
    /// competitor, unless `PLAY_YARD_OWN` is 0; the first costs next to
    /// nothing for each model, and the second as much as the positions.
    /// Each loss is also compared with the competitor's on the same
    /// positions, which has a far smaller error than the losses.  As the
    /// best card on a hundred deals is partly luck, even the best play loses
    /// a little.  With `PLAY_YARD_ENTRIES` set to n, it also prints for each
    /// model the n entries of its tables where it loses the most tricks more
    /// than its mean (see [`entry_losses`]), both of the card it plays and
    /// of the best card, valued with the competitor playing on; and with
    /// `PLAY_YARD_TSV` set to a path, it writes every entry there.  It uses
    /// `PLAY_THREADS` threads, or else every core.
    #[test]
    #[ignore]
    fn score_on_the_playout_yardstick() {
        use crate::ai::playout::{copy_of, one_of_each_run, run_of};
        let competitor = read_model(&std::env::var("PLAY_YARD_COMPETITOR").expect("PLAY_YARD_COMPETITOR"));
        let mut models: Vec<(String, Model)> = vec![("the competitor".to_string(), competitor.clone())];
        for name in var("PLAY_YARD_MODELS", "").split(',').map(str::trim).filter(|n| !n.is_empty()) {
            models.push((name.to_string(), read_model(name)));
        }
        let games = number("PLAY_YARD_GAMES", 200);
        let share: f64 = var("PLAY_YARD_SHARE", "0.25").parse().unwrap();
        let count = number("PLAY_YARD_DEALS", 100);
        let threads = number("PLAY_THREADS", std::thread::available_parallelism().map_or(4, |n| n.get()));
        let workers = rayon::ThreadPoolBuilder::new().num_threads(threads).build().unwrap();
        let started = std::time::Instant::now();
        let deals = crate::ai::opening_lead::candidates::opening_leads(23, games);
        let positions: Vec<Position> = workers.install(|| {
            deals
                .par_iter()
                .enumerate()
                .map(|(i, game)| {
                    let convention: &Convention = game.system.table();
                    let mut rng = StdRng::seed_from_u64(i as u64);
                    let mut game = copy_of(game);
                    let mut kept = Vec::new();
                    while !game.hand_done {
                        let runs: Vec<Card> = one_of_each_run(&game).collect();
                        if runs.len() > 1 && View::new(&game, convention).is_some() && rng.gen_range(0.0..1.0) < share {
                            if let Some(hidden) = Hidden::new(&game, convention) {
                                let dealt = hidden.ensemble(&mut rng, count, None);
                                if !dealt.is_empty() {
                                    kept.push((copy_of(&game), runs, dealt));
                                }
                            }
                        }
                        let card = model_card(&competitor, &game, convention);
                        game.play(card);
                    }
                    kept
                })
                .collect::<Vec<_>>()
                .into_iter()
                .flatten()
                .collect()
        });
        // The value of each run of each position, when `ours` plays on for
        // the side to play and the competitor for the other.
        let values = |ours: &Model| -> Vec<Vec<f64>> {
            workers.install(|| {
                positions
                    .par_iter()
                    .map(|(game, runs, dealt)| {
                        let convention: &Convention = game.system.table();
                        let side = game.turn().unwrap() as usize % 2;
                        let mut both = [&competitor, &competitor];
                        both[side] = ours;
                        crate::ai::playout::card_values(game, runs, dealt, both, convention).unwrap()
                    })
                    .collect()
            })
        };
        let fixed = values(&competitor);
        println!(
            "{} positions from {games} games of the competitor, each played out on {count} deals, in {:.0} s",
            positions.len(),
            started.elapsed().as_secs_f64()
        );
        let losses = |model: &Model, values: &[Vec<f64>]| -> Vec<f64> {
            positions
                .iter()
                .zip(values)
                .map(|((game, runs, _), values)| {
                    let card = model_card(model, game, game.system.table());
                    values.iter().copied().fold(f64::MIN, f64::max) - values[run_of(runs, card)]
                })
                .collect()
        };
        let own_too = number("PLAY_YARD_OWN", 1) == 1;
        // The competitor's own losses are the same both ways.
        let competitor_lost = losses(&competitor, &fixed);
        // A loss, and how much more it is than the competitor's on the same
        // positions, each with its standard error, as text.
        let compared = |lost: &[f64]| {
            let (mean, error) = mean_and_error(lost);
            let more: Vec<f64> = lost.iter().zip(&competitor_lost).map(|(a, b)| a - b).collect();
            let (more, more_error) = mean_and_error(&more);
            format!("{mean:.4} ({error:.4}), {more:+.4} ({more_error:.4}) more than the competitor")
        };
        use std::io::Write;
        let top = number("PLAY_YARD_ENTRIES", 0);
        let mut tsv = std::env::var("PLAY_YARD_TSV").ok().map(|path| {
            let mut file = std::io::BufWriter::new(std::fs::File::create(path).unwrap());
            writeln!(file, "model\tkind\ttable\tentry\tcount\tmean\terror\texcess").unwrap();
            file
        });
        for (i, (name, model)) in models.iter().enumerate() {
            let lost = losses(model, &fixed);
            print!("{name}: its likeliest cards lose {} with the competitor playing on", compared(&lost));
            if own_too && i > 0 {
                print!(", {} with itself playing on for its side", compared(&losses(model, &values(model))));
            }
            println!(" ({:.0} s)", started.elapsed().as_secs_f64());
            if top > 0 || tsv.is_some() {
                let rows = entry_losses(model, &positions, &fixed, &lost);
                let mut sorted: Vec<&EntryLoss> = rows.iter().collect();
                sorted.sort_by(|a, b| b.excess.total_cmp(&a.excess));
                for kind in ["played", "best"] {
                    println!("  where the {kind} card looks up, the entries that lose the most tricks more than the mean:");
                    for row in sorted.iter().filter(|row| row.kind == kind).take(top) {
                        println!(
                            "    {:+8.2} tricks over {:6} plays, {:.4} ({:.4}) a play: {:?}, {}",
                            row.excess, row.count, row.mean, row.error, row.table, row.entry
                        );
                    }
                }
                if let Some(file) = &mut tsv {
                    for row in &rows {
                        writeln!(
                            file,
                            "{name}\t{}\t{:?}\t{}\t{}\t{:.5}\t{:.5}\t{:.3}",
                            row.kind, row.table, row.entry, row.count, row.mean, row.error, row.excess
                        )
                        .unwrap();
                    }
                }
            }
        }
    }

    /// A position of [`score_on_the_playout_yardstick`], with its runs and
    /// the deals that they are played out on.
    type Position = (GameState<()>, Vec<Card>, Vec<[Cards; 2]>);

    /// How much a model loses in the positions of
    /// [`score_on_the_playout_yardstick`], by each entry of its tables.
    struct EntryLoss {
        /// Whether the entry is one that the card the model played looks up
        /// ("played"), or one that the best card looks up ("best"), which is
        /// the card played if that loses nothing.
        kind: &'static str,
        table: Table,
        /// The entry in words.
        entry: String,
        /// How many positions there are with a card that looks it up.
        count: usize,
        /// The mean tricks lost in those, and its standard error.
        mean: f64,
        error: f64,
        /// How many tricks more than the mean of all positions are lost in
        /// those, in all.
        excess: f64,
    }

    /// The losses `lost` of `model` in `positions`, whose runs are worth
    /// `values`, by each entry of its tables that the card it plays, or the
    /// best card, looks up.  The opening lead has its own tables, so no
    /// position is one.
    fn entry_losses(
        model: &Model,
        positions: &[Position],
        values: &[Vec<f64>],
        lost: &[f64],
    ) -> Vec<EntryLoss> {
        use crate::ai::playout::run_of;
        let mean_lost = lost.iter().sum::<f64>() / lost.len() as f64;
        // The count, sum and sum of squares of the losses, by entry, for
        // the card played and the best.
        let mut sums = [vec![(0usize, 0.0, 0.0); model.shape.entries()], vec![(0usize, 0.0, 0.0); model.shape.entries()]];
        for (((game, runs, _), values), &loss) in positions.iter().zip(values).zip(lost) {
            let convention: &Convention = game.system.table();
            let view = View::new(game, convention).unwrap();
            let played = model_card(model, game, convention);
            let best = if loss > 0.0 {
                let most = values.iter().copied().fold(f64::MIN, f64::max);
                runs[values.iter().position(|&v| v == most).unwrap()]
            } else {
                played
            };
            debug_assert!(loss == 0.0 || run_of(runs, best) != run_of(runs, played));
            for (sums, card) in sums.iter_mut().zip([played, best]) {
                for entry in model.entries(&view, card) {
                    let (count, sum, squares) = &mut sums[entry];
                    *count += 1;
                    *sum += loss;
                    *squares += loss * loss;
                }
            }
        }
        let mut rows = Vec::new();
        for (kind, sums) in ["played", "best"].into_iter().zip(sums) {
            for (entry, (count, sum, squares)) in sums.into_iter().enumerate().filter(|(_, s)| s.0 > 0) {
                let n = count as f64;
                let mean = sum / n;
                let variance = if count > 1 { (squares - n * mean * mean).max(0.0) / (n - 1.0) } else { 0.0 };
                let (table, entry) = model.shape.describe(entry);
                rows.push(EntryLoss {
                    kind,
                    table,
                    entry,
                    count,
                    mean,
                    error: (variance / n).sqrt(),
                    excess: sum - n * mean_lost,
                });
            }
        }
        rows
    }

    /// How long each player takes to choose a card, at `PLAY_TIME_TRICKS` (7)
    /// tricks left, in `PLAY_TIME_DEALS` (30) positions reached by the model
    /// `PLAY_TIME_MODEL` playing the deals that the robot bid, with one
    /// thread: the ensemble (with `PLAY_TIME_ENSEMBLE` set) and the playout
    /// player.
    #[test]
    #[ignore]
    fn time_the_players() {
        use crate::ai::PlayAI;
        let tricks = number("PLAY_TIME_TRICKS", 7);
        let count = number("PLAY_TIME_DEALS", 30);
        let model = read_model(&std::env::var("PLAY_TIME_MODEL").expect("PLAY_TIME_MODEL"));
        let deals = crate::ai::opening_lead::candidates::opening_leads(13, count);
        let tricks_left =
            |game: &GameState<()>| game.hands.iter().map(|hand| hand.len()).max().unwrap_or(0);
        let positions: Vec<GameState<()>> = deals
            .iter()
            .filter_map(|game| {
                let convention = game.system.convention();
                let mut game: GameState<()> =
                    serde_json::from_str(&serde_json::to_string(game).unwrap()).unwrap();
                while !game.hand_done
                    && !(current_trick(&game).is_empty() && tricks_left(&game) == tricks)
                {
                    let card = model_card(&model, &game, &convention);
                    game.play(card);
                }
                (!game.hand_done).then_some(game)
            })
            .collect();
        println!("{} positions with {tricks} tricks left", positions.len());
        type Chooser = Box<dyn FnMut(&GameState<()>) -> Card>;
        let time = |name: &str, mut player: Chooser| {
            let started = std::time::Instant::now();
            let mut sum = 0usize;
            for game in &positions {
                sum += player(game).rank() as usize;
            }
            println!(
                "{name:<22} {:9.2} ms a decision ({sum})",
                started.elapsed().as_secs_f64() * 1e3 / positions.len() as f64
            );
        };
        let ensemble_deals = number("PLAY_TIME_ENSEMBLE", 0);
        if ensemble_deals > 0 {
            let mut player = crate::EnsemblePlay::seeded(1).solving_up_to(tricks);
            time("ensemble", Box::new(move |g| player.play(g)));
        }
        let mut player = crate::PlayoutPlay::seeded(3)
            .with_model(model.clone())
            .with_deals(100);
        time("playout (100 deals)", Box::new(move |g| player.play(g)));
    }

    /// Plays the play model with the weights in the file
    /// `PLAY_MATCH_WEIGHTS` against the robot's card play as a team match at
    /// two tables: on each of `PLAY_MATCH_DEALS` (100) deals that the robot
    /// bid, the model plays north and south at one table and east and west
    /// at the other, each time the card it scores highest, and the robot
    /// plays the other seats.  Prints how many more tricks the model's sides
    /// took than the robot's on the same cards, a deal, with its standard
    /// error.  With `PLAY_MATCH_AGAINST` naming other weights, those play in
    /// the robot's place, to compare two snapshots.  It uses `PLAY_THREADS`
    /// (1) threads.
    #[test]
    #[ignore]
    fn match_against_the_robot() {
        let read = |name: &str| std::env::var(name).ok().map(|path| read_model(&path));
        let model = read("PLAY_MATCH_WEIGHTS").expect("PLAY_MATCH_WEIGHTS");
        let other = read("PLAY_MATCH_AGAINST");
        let deals: Vec<String> = crate::ai::opening_lead::candidates::opening_leads(13, number("PLAY_MATCH_DEALS", 100))
            .iter()
            .map(|game| serde_json::to_string(game).unwrap())
            .collect();
        let threads = number("PLAY_THREADS", 1);
        let started = std::time::Instant::now();
        let gains: Vec<f64> = std::thread::scope(|scope| {
            let handles: Vec<_> = deals
                .chunks(deals.len().div_ceil(threads))
                .map(|deals| {
                    let (model, other) = (&model, other.as_ref());
                    scope.spawn(move || {
                        let mut conventions = std::collections::HashMap::new();
                        deals
                            .iter()
                            .map(|json| {
                                let game: GameState<()> = serde_json::from_str(json).unwrap();
                                let convention = conventions
                                    .entry(format!("{:?}", game.system))
                                    .or_insert_with(|| game.system.convention());
                                // North and south's tricks with the model
                                // playing them, less with the other player.
                                let model_ns = play_at_a_table(json, convention, model, 0, other);
                                let other_ns = play_at_a_table(json, convention, model, 1, other);
                                model_ns as f64 - other_ns as f64
                            })
                            .collect::<Vec<f64>>()
                    })
                })
                .collect();
            handles.into_iter().flat_map(|h| h.join().unwrap()).collect()
        });
        let n = gains.len() as f64;
        let mean = gains.iter().sum::<f64>() / n;
        let spread = (gains.iter().map(|g| (g - mean).powi(2)).sum::<f64>() / (n - 1.0)).sqrt();
        println!(
            "{} deals in {:.0} s: the model's sides took {mean:+.3} tricks a deal more than {}'s, standard error {:.3}",
            gains.len(),
            started.elapsed().as_secs_f64(),
            if other.is_some() { "the other model" } else { "the robot" },
            spread / n.sqrt(),
        );
    }
}
