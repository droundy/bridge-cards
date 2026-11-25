//! A player that chooses a card without searching: an n-tuple network over
//! what the player can see (their hand, the other hand in sight, which cards
//! are still hidden, the trick so far and the auction), whose entries are
//! fitted to the double-dummy value of each card averaged over deals of the
//! hidden hands that fit what is known, as `EnsemblePlay` values them.  It is
//! for the stages of the play that are too slow to search, and for choosing
//! which few cards a search should value.  Nothing here says which card is
//! good, only which facts each table sees; the opening lead has its own
//! tables (`opening_lead`), since the dummy is not yet in sight then.

use super::fitting::{Entries, Network};
#[cfg(test)]
mod model_file;
mod suit_play;
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
    /// The role, the play, and, for the hidden hands that play after the
    /// player, whether one of them could win the trick, as [`Table::Later`]
    /// says of the other hand in sight, but only as far as the auction and
    /// the play show: for the later hidden opponents together, and for the
    /// later hidden partner (a defender's partner; the declarer's side has
    /// none, since the other hand of its own is in sight).  Each is: does not
    /// play after the player, plays after and cannot beat the card and the
    /// trick so far, could beat it by following suit, or could beat it by
    /// ruffing, which is when it can be void in the suit led and may hold a
    /// trump that beats it.  Following wins over ruffing when both could do it.
    Later2,
    /// [`Table::Later2`] by the player's beliefs: the same entries, each with
    /// its chance, where a hand's state is what it does hold, not what it
    /// could: it cannot beat the card and the trick so far, it holds a card
    /// of the suit led that beats them, or it holds none of the suit led and
    /// a trump that beats them.  Those are exclusive, since a hand that holds
    /// a card of the suit led must follow.
    Later3,
    /// [`Table::Later3`] with each state's chance p put on a scale that can
    /// bend: an entry for each of [`KNOTS`], weighted by the hat function of
    /// p that is 1 at that knot and falls to 0 at the knots beside it (at 0
    /// below the first), the last rising to 1 at p = 1.  Their weights make a
    /// function of p that is linear between the knots and 0 at p = 0, which
    /// a state's p alone, entering the first layer, cannot be.
    Later3Knots,
    /// The role, the play, and how many tricks of its suit the card gives
    /// away against the best card of the suit (0, 1, 2, or 3 or more), with
    /// the suit played out alone with all four hands known, for each way that
    /// the player's beliefs give the suit's hidden cards, with its chance;
    /// or that the card does not follow the suit led, or that the suit has
    /// too many cards whose place is not known to be worked out.
    SuitTricks,
    /// The role, the play, and what a small search of the card's suit says
    /// of it (see [`suit_play`]): played out from now as though at notrump,
    /// for each way that the hidden cards of the suit could be split between
    /// the hidden hands, by what the auction and the play show, how many
    /// fewer tricks the player's side takes in the suit after this card than
    /// after the best card of the suit (0, 1, 2, or 3 or more), at the worst
    /// split and at the best.  A lead is compared with the other leads of
    /// its suit, a discard by what it costs its own suit against the
    /// cheapest discard, and a ruff is always 0.
    SuitPlay,
    /// The role, the play, and the median number of tricks in a row that the
    /// player's side takes, from this trick on, over the ways that the
    /// hidden cards could lie by the player's beliefs (0 to 4 or more): each
    /// way is a deal of the hidden cards, in which every play of the side's
    /// two hands, and each play that the opponents can make to win a trick,
    /// ruffs and overruffs included, is tried, and the run counts the tricks
    /// that the side takes one after the other until the opponents win one,
    /// the side leading each trick that it has won, whichever hand won it.
    /// So a run can cross to the other hand to lead, or ruff in each hand in
    /// turn.  Only for views that keep beliefs.
    Run,
    /// [`Table::Run`] worked out without dealing: the role, the play, and the
    /// chance of each run, 0 to 4 or more tricks in a row from this trick on.
    /// Each suit's ways of lying come with their exact chances from the
    /// player's beliefs; in each, the side cashes the suit from the top
    /// until an opponent's card beats its best or a void opponent with a trump
    /// ruffs, the opponents playing low otherwise.  The trick in progress is
    /// played out for each way that the suit led lies, and the suits are
    /// added up as though they lay independently given which hidden hands
    /// hold trumps (above the trick's top trump, for this trick), whose
    /// chances come from the trump suit.  The hands are taken to reach each
    /// other at will: there is no transport, and the side does not ruff.
    RunOdds,
    /// [`Table::RunOdds`] with transport and ruffs: the role, the play, and
    /// the chance of each run, 0 to 4 or more.  After the trick in progress,
    /// the side plays its suits one after another, choosing each next suit
    /// by the mean run that it leads to, knowing what the state says (the run
    /// so far, the hand on lead, the trumps) and not how the suits lie, and
    /// carrying from suit to suit which of its hands is on lead and the
    /// trumps that each hand and each opponent holds.  In a
    /// suit it leads from the hand on lead, which needs a card of it (so a
    /// suit can be blocked), cashes while its best card is safe and the lead
    /// goes to the hand that wins, and when it cannot cash it ruffs in the
    /// other hand if that hand has none of the suit and a trump, and no
    /// opponent with none of the suit holds a trump.  The opponents' trumps
    /// are drawn by the side's cashing of trumps.  Each suit's ways of lying
    /// come with their chances from the beliefs, as for RunOdds.
    RunPlan,
    /// The run of [`Table::RunOdds`] that is sure: the fewest tricks in a
    /// row over every way of lying that the player's beliefs allow at all,
    /// which is every way that the cards seen and the suits shown out allow.
    RunSure,
    /// The role, the play, and which seats bid the card's suit.
    Bids,
    /// The place, and whether each of the three cards just above is the
    /// player's, in the other hand in sight, in the first hidden hand after
    /// the player, in the second, or not left at all: with the chance of
    /// each way that the hidden ones could lie, by the player's beliefs
    /// about where the cards are (see [`crate::ai::locations`]), each
    /// entry looked up with its chance.
    AboveSplit,
    /// The same of the three cards just below.
    BelowSplit,
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
            Table::Later2 | Table::Later3 => 4 * PLAYS * 4 * 4,
            Table::Later3Knots => 4 * PLAYS * 4 * 4 * KNOTS.len(),
            Table::SuitTricks => 4 * PLAYS * 5,
            Table::SuitPlay => 4 * PLAYS * 4 * 4,
            Table::Run | Table::RunOdds | Table::RunSure | Table::RunPlan => 4 * PLAYS * 5,
            Table::Bids => 4 * PLAYS * 16,
            Table::AboveSplit | Table::BelowSplit => 4 * 125,
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
            Table::SuitTricks => {
                let what = ["gives away no trick", "gives away 1 trick", "gives away 2 tricks", "gives away 3 or more", "off the suit, or not worked out"];
                format!("{}, in its suit {}", role_play(index / 5), what[index % 5])
            }
            Table::RunOdds => format!("{}, a run of {}{} tricks", role_play(index / 5), index % 5, if index % 5 == RUN_MOST { " or more" } else { "" }),
            Table::RunPlan => format!("{}, a planned run of {}{} tricks", role_play(index / 5), index % 5, if index % 5 == RUN_MOST { " or more" } else { "" }),
            Table::RunSure => format!("{}, a sure run of {}{} tricks", role_play(index / 5), index % 5, if index % 5 == RUN_MOST { " or more" } else { "" }),
            Table::Run => format!("{}, the median run is {}{} tricks", role_play(index / 5), index % 5, if index % 5 == RUN_MOST { " or more" } else { "" }),
            Table::Later3Knots => {
                let (state, knot) = (index / KNOTS.len(), KNOTS[index % KNOTS.len()]);
                format!("{}, at the chance {knot}", Table::Later3.describe(state))
            }
            Table::Later2 | Table::Later3 => {
                const COULD: [&str; 4] = ["no", "cannot beat", "could beat following", "could beat ruffing"];
                format!("{}, opponents after: {}, partner after: {}", role_play(index / 16), COULD[index / 4 % 4], COULD[index % 4])
            }
            Table::SuitPlay => {
                format!("{}, tricks fewer at the worst split: {}, at the best: {}", role_play(index / 16), index / 4 % 4, index % 4)
            }
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
    /// How many units each hidden layer has, the first first: each table
    /// entry feeds the first as well as the score, so that the model can
    /// weigh combinations of the facts of different tables, and each later
    /// layer takes all the units of the one before, so that it can weigh
    /// combinations of those.
    pub(crate) layers: &'static [usize],
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
    layers: &[16, 8],
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
    layers: &[16, 8],
};

/// [`V2`] with hidden layers twice as wide, to see whether its width is what
/// limits it.
pub(crate) const V2_WIDE: Shape = Shape { name: "v2-wide", tables: V2.tables, layers: &[32, 16] };

/// [`V2`] and more tables, to see whether they pay.
const fn v2_and(name: &'static str, tables: &'static [Table]) -> Shape {
    Shape { name, tables, layers: &[16, 8] }
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
/// [`V2_TRUMPS`] with hidden layers twice as wide, to see whether its width
/// is what stops training from going further.
pub(crate) const V2_TRUMPS_WIDE: Shape = Shape { name: "v2-trumps-wide", tables: V2_TRUMPS.tables, layers: &[32, 16] };
/// [`V2_TRUMPS`] and [`Table::History`].
pub(crate) const V2_TRUMPS_HISTORY: Shape = v2_and("v2-trumps-history", v2_tables_and!(Table::Trumps, Table::History));
/// [`V2_TRUMPS_HISTORY`] with hidden layers twice as wide.
pub(crate) const V2_TRUMPS_HISTORY_WIDE: Shape =
    Shape { name: "v2-trumps-history-wide", tables: V2_TRUMPS_HISTORY.tables, layers: &[32, 16] };
/// [`V2_TRUMPS_HISTORY`] with a third hidden layer, to see whether depth
/// pays where width did not.  A model of two layers grows into it with the
/// third passing on the second, so that it starts by playing as it did.
pub(crate) const V2_TRUMPS_HISTORY_DEEP: Shape =
    Shape { name: "v2-trumps-history-deep", tables: V2_TRUMPS_HISTORY.tables, layers: &[16, 8, 8] };
/// Every table that does not read the calls nor the beliefs, and layers
/// that are wider and one deeper, to be trained for far longer than the
/// screens: a model of two layers grows into it playing as it did (see
/// [`Model::grown_to`]).
pub(crate) const V2_BIG: Shape = Shape {
    name: "v2-big",
    tables: v2_tables_and!(Table::Later, Table::Trumps, Table::History, Table::Later2),
    layers: &[64, 32, 16],
};
/// [`V2_TRUMPS_HISTORY`] and [`Table::Later`].
pub(crate) const V2_LATER_TRUMPS_HISTORY: Shape =
    v2_and("v2-later-trumps-history", v2_tables_and!(Table::Later, Table::Trumps, Table::History));

/// [`V2_TRUMPS_HISTORY`] and [`Table::Later2`], to see whether it pays to say
/// whether a hidden hand that plays after the player could win the trick.
pub(crate) const V2_TRUMPS_HISTORY_LATER2: Shape =
    v2_and("v2-trumps-history-later2", v2_tables_and!(Table::Trumps, Table::History, Table::Later2));

/// [`V2_TRUMPS`] reading the player's beliefs about where the hidden cards
/// lie: [`Table::AboveSplit`] and [`Table::BelowSplit`] in place of
/// [`Table::Above`] and [`Table::Below`], which a model of [`V2_TRUMPS`]
/// grows into by giving both hidden hands the weights of "hidden", so that
/// it scores every card as it did.
pub(crate) const V2_TRUMPS_BELIEFS: Shape = v2_and(
    "v2-trumps-beliefs",
    &[
        Table::Trick,
        Table::AboveSplit,
        Table::BelowSplit,
        Table::Rank,
        Table::Lengths,
        Table::TricksLeft,
        Table::Contract,
        Table::FirstLength,
        Table::SecondLength,
        Table::FirstPoints,
        Table::SecondPoints,
        Table::Trumps,
    ],
);

/// The most cards of a suit left in the hands that [`Table::SuitTricks`]
/// works the suit out for: it solves the suit once for each way that those
/// whose place is not known could lie, and a whole suit takes far too long.
#[cfg(test)]
const SUIT_TRICKS_MOST: usize = 8;

/// Plays one suit out alone, with every hand's cards of it known, for the
/// most tricks for the player's side (the hands 0 and 2, counting clockwise
/// from the player): each trick is won by the highest card, a hand with no
/// card of the suit plays none, and the side that wins a trick leads the
/// next from either of its hands that has a card of the suit, or, if
/// neither has, the other side does.  Ruffs and entries are left out.
#[cfg(test)]
#[derive(Default)]
struct SuitSolver {
    known: std::collections::HashMap<(u64, u64), u8>,
}

#[cfg(test)]
thread_local! {
    /// Each thread's [`SuitSolver`], which keeps the positions it has solved
    /// from view to view, its cards numbered by their order (see
    /// `View::suit_losses`), up to a million of them.
    static SUIT_SOLVER: std::cell::RefCell<SuitSolver> = Default::default();
}

#[cfg(test)]
impl SuitSolver {
    /// The tricks of the player's side from here, the trick in progress
    /// with them: the `hands`' cards of the suit (as bits of their ranks),
    /// the trick led by `leader`, of which `turns` hands have played, the
    /// `ranks` they played (0 for none).
    fn tricks(&mut self, hands: [u16; 4], leader: usize, turns: usize, ranks: [u8; 4]) -> u8 {
        let key = (
            hands.iter().fold(0u64, |key, &h| key << 16 | u64::from(h)),
            ranks.iter().fold((leader * 8 + turns) as u64, |key, &r| key << 8 | u64::from(r)),
        );
        if let Some(&tricks) = self.known.get(&key) {
            return tricks;
        }
        if self.known.len() > 1 << 20 {
            self.known.clear();
        }
        let tricks = if turns == 4 {
            let winner = (0..4).max_by_key(|&s| ranks[s]).unwrap();
            let won = u8::from(winner.is_multiple_of(2));
            let leaders = |side: usize| -> Vec<usize> { [side, side + 2].into_iter().filter(|&s| hands[s] != 0).collect() };
            let mut next = leaders(winner % 2);
            if next.is_empty() {
                next = leaders(1 - winner % 2);
            }
            match next.first() {
                None => won,
                Some(&first) => {
                    let values = next.iter().map(|&l| self.tricks(hands, l, 0, [0; 4]));
                    won + if first.is_multiple_of(2) { values.max().unwrap() } else { values.min().unwrap() }
                }
            }
        } else {
            let seat = (leader + turns) % 4;
            if hands[seat] == 0 {
                self.tricks(hands, leader, turns + 1, ranks)
            } else {
                // Cards with no card of another hand, nor one played to this
                // trick, between them are the same: play the highest of each run.
                let others = (0..4).filter(|&s| s != seat).fold(0u16, |o, s| o | hands[s])
                    | ranks.iter().filter(|&&r| r > 0).fold(0u16, |o, &r| o | 1 << r);
                let mut values = Vec::new();
                for rank in 1..=14u8 {
                    if hands[seat] & 1 << rank == 0 {
                        continue;
                    }
                    let higher = hands[seat] & !((2u16 << rank) - 1);
                    if higher != 0 {
                        let next = higher.trailing_zeros();
                        let between = !((2u16 << rank) - 1) & ((1u16 << next) - 1);
                        if others & between == 0 {
                            continue;
                        }
                    }
                    let mut after = hands;
                    after[seat] &= !(1 << rank);
                    let mut played = ranks;
                    played[seat] = rank;
                    values.push(self.tricks(after, leader, turns + 1, played));
                }
                if seat.is_multiple_of(2) {
                    values.into_iter().max().unwrap()
                } else {
                    values.into_iter().min().unwrap()
                }
            }
        };
        self.known.insert(key, tricks);
        tricks
    }
}

/// The chances at which [`Table::Later3Knots`] has a knot, after 0: the
/// scale may bend at ½, where the plan that plays best tends to change (a
/// finesse or the drop, which way to guess), and the units can learn bends
/// elsewhere.
pub(crate) const KNOTS: [f64; 2] = [0.5, 1.0];

/// The hat functions of the chance `p` at each of [`KNOTS`]: so that
/// `Σ knot · hat = p`, and with weights `w` the sum `Σ w · hat` is the
/// function of p that is linear between the knots, `w` at each and 0 at 0.
#[cfg(test)]
fn hats(p: f64) -> [f64; KNOTS.len()] {
    let p = p.clamp(0.0, 1.0);
    std::array::from_fn(|j| {
        let (left, at) = (if j == 0 { 0.0 } else { KNOTS[j - 1] }, KNOTS[j]);
        if p <= left {
            0.0
        } else if p <= at {
            (p - left) / (at - left)
        } else {
            match KNOTS.get(j + 1) {
                Some(&right) if p < right => (right - p) / (right - at),
                _ => 0.0,
            }
        }
    })
}

/// [`V2_TRUMPS_HISTORY_LATER2`] with [`Table::Later3`] in place of
/// [`Table::Later2`], to see what the chances add to what could be.
pub(crate) const V2_TRUMPS_HISTORY_LATER3: Shape =
    v2_and("v2-trumps-history-later3", v2_tables_and!(Table::Trumps, Table::History, Table::Later3));

/// [`V2_TRUMPS_HISTORY_LATER2`] with [`Table::Later3Knots`] in place of
/// [`Table::Later2`], to see what a scale of the chances that can bend adds.
pub(crate) const V2_TRUMPS_HISTORY_LATER3_KNOTS: Shape =
    v2_and("v2-trumps-history-later3-knots", v2_tables_and!(Table::Trumps, Table::History, Table::Later3Knots));

/// [`V2_TRUMPS_HISTORY_LATER2`] and [`Table::RunOdds`].
pub(crate) const V2_TRUMPS_HISTORY_LATER2_RUN_ODDS: Shape =
    v2_and("v2-trumps-history-later2-run-odds", v2_tables_and!(Table::Trumps, Table::History, Table::Later2, Table::RunOdds));

/// [`V2_TRUMPS_HISTORY_LATER2`] and [`Table::RunPlan`].
pub(crate) const V2_TRUMPS_HISTORY_LATER2_RUN_PLAN: Shape =
    v2_and("v2-trumps-history-later2-run-plan", v2_tables_and!(Table::Trumps, Table::History, Table::Later2, Table::RunPlan));

/// [`V2_TRUMPS_HISTORY_LATER2`] and [`Table::RunSure`].
pub(crate) const V2_TRUMPS_HISTORY_LATER2_RUN_SURE: Shape =
    v2_and("v2-trumps-history-later2-run-sure", v2_tables_and!(Table::Trumps, Table::History, Table::Later2, Table::RunSure));

/// [`V2_TRUMPS_HISTORY_LATER2`] and [`Table::Run`].
pub(crate) const V2_TRUMPS_HISTORY_LATER2_RUN: Shape =
    v2_and("v2-trumps-history-later2-run", v2_tables_and!(Table::Trumps, Table::History, Table::Later2, Table::Run));

/// [`V2_TRUMPS_HISTORY_LATER2`] and [`Table::SuitTricks`].
pub(crate) const V2_TRUMPS_HISTORY_LATER2_SUIT_TRICKS: Shape =
    v2_and("v2-trumps-history-later2-suit-tricks", v2_tables_and!(Table::Trumps, Table::History, Table::Later2, Table::SuitTricks));
/// [`V2_TRUMPS_HISTORY_LATER2`] and [`Table::SuitPlay`], to see whether a
/// small search of the card's suit pays.
pub(crate) const V2_TRUMPS_HISTORY_LATER2_SUIT_PLAY: Shape =
    v2_and("v2-trumps-history-later2-suit-play", v2_tables_and!(Table::Trumps, Table::History, Table::Later2, Table::SuitPlay));

/// Every shape there is.
pub(crate) const SHAPES: &[&Shape] = &[
    &V1,
    &V2,
    &V2_WIDE,
    &V2_HISTORY,
    &V2_TRUMPS,
    &V2_LATER,
    &V2_BIDS,
    &V2_LATER_HISTORY,
    &V2_LATER_TRUMPS,
    &V2_TRUMPS_WIDE,
    &V2_TRUMPS_HISTORY,
    &V2_TRUMPS_HISTORY_WIDE,
    &V2_LATER_TRUMPS_HISTORY,
    &V2_TRUMPS_HISTORY_DEEP,
    &V2_BIG,
    &V2_TRUMPS_HISTORY_LATER2,
    &V2_TRUMPS_BELIEFS,
    &V2_TRUMPS_HISTORY_LATER3,
    &V2_TRUMPS_HISTORY_LATER3_KNOTS,
    &V2_TRUMPS_HISTORY_LATER2_SUIT_TRICKS,
    &V2_TRUMPS_HISTORY_LATER2_SUIT_PLAY,
    &V2_TRUMPS_HISTORY_LATER2_RUN,
    &V2_TRUMPS_HISTORY_LATER2_RUN_ODDS,
    &V2_TRUMPS_HISTORY_LATER2_RUN_SURE,
    &V2_TRUMPS_HISTORY_LATER2_RUN_PLAN,
];

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

    /// Whether it reads the players' beliefs about where the hidden cards
    /// lie, which only self-play keeps.
    #[cfg(test)]
    pub(crate) fn reads_beliefs(&self) -> bool {
        self.tables.iter().any(|t| matches!(t, Table::AboveSplit | Table::BelowSplit | Table::Later3 | Table::Later3Knots | Table::SuitTricks | Table::Run | Table::RunOdds | Table::RunSure | Table::RunPlan))
    }

    /// How many weights a model of this shape has.
    pub(crate) fn size(&self) -> usize {
        Network::size(self.entries(), self.layers)
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
        let network = Network::with_params(shape.entries(), shape.layers, params);
        Model { shape, network }
    }

    /// A model of `shape` that starts by choosing at random (see
    /// [`Network::new`]).
    #[cfg(test)]
    pub(crate) fn new(shape: &'static Shape, seed: u64) -> Model {
        let network = Network::new(shape.entries(), shape.layers, seed);
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
    pub(crate) fn entries(&self, view: &View, card: Card) -> Entries {
        view.entries(card, self.shape)
    }

    /// Its score for playing `card` in `view`.
    pub(crate) fn score(&self, view: &View, card: Card) -> f64 {
        self.network.score_of(&self.entries(view, card))
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
    /// wider or added after its last (see [`crate::ai::fitting::grows`]).  A
    /// model of [`V1`] is first laid out as one of [`V2`].  `None` if `shape`
    /// lacks one of its tables, or its layers cannot grow into those of
    /// `shape`.
    #[cfg(test)]
    pub(crate) fn grown_to(&self, shape: &'static Shape) -> Option<Model> {
        if self.shape == &V1 {
            return self.without_room().grown_to(shape);
        }
        if !crate::ai::fitting::grows(self.shape.layers, shape.layers) {
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
        // A table of where the cards near the card lie may become the same
        // with the hidden hands told apart: "hidden" becomes the first
        // hidden hand, and then the second is given its weights too.
        let split = |table: Table| match table {
            Table::Above => Table::AboveSplit,
            Table::Below => Table::BelowSplit,
            Table::Later2 => Table::Later3,
            table => table,
        };
        let mut moved = Vec::with_capacity(self.shape.entries());
        let mut copies = Vec::new();
        for &table in self.shape.tables {
            if let Some(at) = shape.tables.iter().position(|&t| t == table) {
                moved.extend((0..table.size()).map(|index| starts[at] + index));
                continue;
            }
            if let Some(at) = shape.tables.iter().position(|&t| t == Table::Later3Knots).filter(|_| matches!(table, Table::Later2 | Table::Later3)) {
                // Each state's weights go to its last knot, at p = 1, and
                // the others get them times their knots, which scores the
                // chances p as Later3 does (and as Later2 does p = 1).
                let n = KNOTS.len();
                moved.extend((0..table.size()).map(|index| starts[at] + index * n + n - 1));
                for index in 0..table.size() {
                    for (j, &knot) in KNOTS[..n - 1].iter().enumerate() {
                        copies.push((starts[at] + index * n + n - 1, starts[at] + index * n + j, knot));
                    }
                }
                continue;
            }
            let at = shape.tables.iter().position(|&t| t == split(table) && t != table)?;
            if table == Table::Later2 {
                // The same entries, now with their chances.
                moved.extend((0..table.size()).map(|index| starts[at] + index));
                continue;
            }
            moved.extend((0..table.size()).map(|index| {
                let (place, h) = (index / 64, index % 64);
                starts[at] + place * 125 + h % 4 + 5 * (h / 4 % 4) + 25 * (h / 16)
            }));
            for index in 0..shape.tables[at].size() {
                let (place, mut digits) = (index / 125, [index % 5, index / 5 % 5, index / 25]);
                if digits.contains(&4) {
                    digits.iter_mut().filter(|d| **d == 4).for_each(|d| *d = 3);
                    let from = place * 125 + digits[0] + 5 * digits[1] + 25 * digits[2];
                    copies.push((starts[at] + from, starts[at] + index, 1.0));
                }
            }
        }
        let mut network = self.network.grown(&moved, shape.entries(), shape.layers, 11);
        for (from, to, factor) in copies {
            network.copy_entry(from, to, factor);
        }
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
        let first = (game.hand_called_by(seat) as usize + 4 - game.dealer as usize) % 4;
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
    /// For each of the two hidden hands, the first after the player first (as
    /// `shown` has them): whether it plays to this trick after the player,
    /// and whether it is the player's partner.
    hidden_after: [bool; 2],
    hidden_partner: [bool; 2],
    /// How many cards each hand holds, counting clockwise from the player,
    /// and the cards played to this trick before the player, in order.
    sizes: [usize; 4],
    trick: [Option<Card>; 3],
    /// What [`Table::SuitPlay`] says of each card, by its offset, worked out
    /// the first time that it is asked for.
    suit_play: std::cell::OnceCell<[u8; 64]>,
    /// Which hand plays `n` places after the player, for each `n`: 0 the
    /// player's, 1 the other in sight, 2 the first hidden hand, 3 the second.
    #[cfg(test)]
    order: [usize; 4],
    /// What the player believes of where the hidden cards lie, when
    /// [`LOCATIONS`] is set.
    #[cfg(test)]
    beliefs: Option<std::sync::Arc<Seen>>,
}

/// The most tricks in a row that [`Table::Run`] counts.
#[cfg(test)]
const RUN_MOST: usize = 4;

/// One way that the cards of a suit could lie: the ranks (as bits) that each
/// hand holds, the player's, the other in sight, the first hidden and the
/// second, and its chance.
#[cfg(test)]
type SuitSplit = ([u16; 4], f64);

/// The ways of lying of the trump suit that give the hidden hands the same
/// trump states (see `View::run_odds`): the states, their chance, and the
/// ways.
#[cfg(test)]
type TrumpGroup = ([u8; 2], f64, Vec<(usize, SuitSplit)>);

/// The most cards of a suit whose place is not known whose ways of lying
/// [`Table::RunOdds`] tells apart; the lower ones of a longer suit go to the
/// hidden hands in turn.
#[cfg(test)]
const RUN_ODDS_MOST_FREE: usize = 8;

/// The highest rank of `bits`, or 0.
#[cfg(test)]
fn top_rank(bits: u16) -> u8 {
    if bits == 0 { 0 } else { 15 - bits.leading_zeros() as u8 }
}

/// `bits` without their lowest.
#[cfg(test)]
fn without_lowest(bits: u16) -> u16 {
    bits & bits.wrapping_sub(1)
}

/// The tricks in a row, at most [`RUN_MOST`], that the side of the hands
/// `ours` takes by cashing one suit, its hands holding `hands` of it: each
/// round the side's highest card wins unless an opponent holds a higher
/// one, or holds none of the suit and `ruffs`, the side's other hand and
/// the opponents playing their lowest.
#[cfg(test)]
fn cash(mut hands: [u16; 4], ours: [usize; 2], opponents: [usize; 2], ruffs: [bool; 2]) -> usize {
    let mut run = 0;
    while run < RUN_MOST {
        let mine = hands[ours[0]] | hands[ours[1]];
        if mine == 0 {
            break;
        }
        let top = top_rank(mine);
        let stopped = (0..2).any(|k| {
            let o = hands[opponents[k]];
            if o == 0 { ruffs[k] } else { top_rank(o) > top }
        });
        if stopped {
            break;
        }
        run += 1;
        let (winner, other) = if hands[ours[0]] & 1 << top != 0 { (ours[0], ours[1]) } else { (ours[1], ours[0]) };
        hands[winner] &= !(1 << top);
        hands[other] = without_lowest(hands[other]);
        for &o in &opponents {
            hands[o] = without_lowest(hands[o]);
        }
    }
    run
}

/// Where a run of [`Table::RunPlan`] stands between suits.
#[cfg(test)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct RunState {
    /// The tricks taken in a row so far, at most [`RUN_MOST`].
    run: u8,
    /// Which of the side's hands is on lead: 0 the player's, 1 partner's.
    lead: u8,
    /// The trumps that each of the side's hands holds, the player's first.
    ours: [u8; 2],
    /// The trumps that each of the side's hands has ruffed with, which its
    /// cards of the trump suit no longer have.
    ruffed: [u8; 2],
    /// The trumps that each opponent holds.
    theirs: [u8; 2],
}

/// [`RunState`] after the side plays one suit, whose cards the hands hold as
/// `hands` (by the four hands, as rank bits): the side's two hands are
/// `side`, the player's first, and the opponents `opponents`; the suit is
/// trumps if `is_trump`, and there are trumps if `trumps`.
#[cfg(test)]
fn plan_step(mut hands: [u16; 4], mut state: RunState, side: [usize; 2], opponents: [usize; 2], is_trump: bool, trumps: bool) -> RunState {
    if is_trump {
        for (i, &h) in side.iter().enumerate() {
            for _ in 0..state.ruffed[i] {
                hands[h] = without_lowest(hands[h]);
            }
        }
    }
    while (state.run as usize) < RUN_MOST {
        let lead = state.lead as usize;
        let (x, y) = (side[lead], side[1 - lead]);
        if hands[x] == 0 {
            break;
        }
        let top = top_rank(hands[x] | hands[y]);
        let beaten = opponents.iter().any(|&o| hands[o] != 0 && top_rank(hands[o]) > top);
        let ruffer = !is_trump && trumps && (0..2).any(|k| hands[opponents[k]] == 0 && state.theirs[k] > 0);
        if !beaten && !ruffer {
            let winner = if hands[x] & 1 << top != 0 { lead } else { 1 - lead };
            hands[side[winner]] &= !(1 << top);
            hands[side[1 - winner]] = without_lowest(hands[side[1 - winner]]);
            for &o in &opponents {
                hands[o] = without_lowest(hands[o]);
            }
            state.lead = winner as u8;
            state.run += 1;
        } else if !is_trump && trumps && hands[y] == 0 && state.ours[1 - lead] > 0 && !ruffer {
            hands[x] = without_lowest(hands[x]);
            for &o in &opponents {
                hands[o] = without_lowest(hands[o]);
            }
            state.ours[1 - lead] -= 1;
            state.ruffed[1 - lead] += 1;
            state.lead = (1 - lead) as u8;
            state.run += 1;
        } else {
            break;
        }
    }
    if is_trump {
        state.ours = side.map(|h| hands[h].count_ones() as u8);
        state.theirs = opponents.map(|h| hands[h].count_ones() as u8);
        state.ruffed = [0; 2];
    }
    state
}

/// The top card of a trick, as (whether it is a trump, its rank, its hand).
#[cfg(test)]
type TrickTop = Option<(bool, u8, usize)>;

/// Plays out the rest of a trick whose top card so far is `top`, by the
/// hands `later`, which hold `hands` of the suit led (as rank bits, by the
/// four hands); `side` is the player's side, `trump_led` says whether the
/// suit led is trumps, and `ruffs` whether a hand with none of the suit led
/// would ruff above any trump so far.  Each hand knows where the suit's cards
/// lie: a hand whose side is not winning plays the cheapest card that beats
/// the trick and every later opponent's best, or else the cheapest that
/// beats the trick, or else its lowest; a hand whose side is winning keeps
/// its lowest unless a later opponent can beat the trick, and then plays the
/// cheapest card above them; a hand with none of the suit ruffs if it can
/// and its side is not winning, or is winning with a card that a later
/// opponent can beat.  Gives the top card at the end, the hands'
/// cards of the suit led after the trick, and which hands ruffed.
#[cfg(test)]
fn play_out_trick(hands: [u16; 4], mut top: TrickTop, later: &[usize], side: [usize; 2], trump_led: bool, ruffs: impl Fn(usize) -> bool) -> (TrickTop, [u16; 4], [u8; 4]) {
    let mut after = hands;
    let mut ruffed = [0u8; 4];
    // The cards above `rank` in `bits`, and the cheapest of them.
    let cheapest_above = |bits: u16, rank: u8| {
        let above = bits & !((2u16 << rank) - 1);
        (above != 0).then(|| above.trailing_zeros() as u8)
    };
    for (i, &h) in later.iter().enumerate() {
        let ours = side.contains(&h);
        let winning = top.is_some_and(|(_, _, w)| side.contains(&w) == ours);
        // A trump on a trick of another suit cannot be beaten by following.
        let ruffed_trick = top.is_some_and(|(t, _, _)| t && !trump_led);
        let threat = later[i + 1..].iter().filter(|&&o| side.contains(&o) != ours).map(|&o| top_rank(hands[o])).max().unwrap_or(0);
        let holding = hands[h];
        if holding == 0 {
            let threatened = !ruffed_trick && threat > top.map_or(0, |t| t.1);
            if (!winning || threatened) && !ruffed_trick && !trump_led && ruffs(h) {
                top = Some((true, 15, h));
                ruffed[h] = 1;
            }
            continue;
        }
        let top_rank_now = top.map_or(0, |t| t.1);
        let card = if ruffed_trick {
            None
        } else if winning {
            if top_rank_now > threat { None } else { cheapest_above(holding, threat) }
        } else {
            cheapest_above(holding, top_rank_now.max(threat)).or_else(|| cheapest_above(holding, top_rank_now))
        };
        match card {
            Some(rank) => {
                after[h] &= !(1 << rank);
                top = Some((trump_led, rank, h));
            }
            None => after[h] = without_lowest(after[h]),
        }
    }
    (top, after, ruffed)
}

/// The chances of each run from `state` on, with the suits of `remaining`
/// (as bits by suit) still to play, each suit's ways of lying `ways`: the
/// side plays next the suit that gives the most tricks in a row in the mean,
/// which it chooses knowing all that the state says (the run so far, the
/// hand on lead, the trumps) and nothing of how the suits lie.  `known`
/// keeps what has been worked out.
#[cfg(test)]
fn best_rest(
    state: RunState,
    remaining: u8,
    ways: &[Vec<SuitSplit>; 4],
    side: [usize; 2],
    opponents: [usize; 2],
    trump: Option<Suit>,
    known: &mut RunMemo,
) -> [f64; RUN_MOST + 1] {
    let mut runs = [0.0; RUN_MOST + 1];
    if state.run as usize >= RUN_MOST || remaining == 0 {
        runs[(state.run as usize).min(RUN_MOST)] = 1.0;
        return runs;
    }
    if let Some((_, found)) = known.iter().find(|(k, _)| *k == (state, remaining)) {
        return *found;
    }
    let mean = |runs: &[f64; RUN_MOST + 1]| runs.iter().enumerate().map(|(k, p)| k as f64 * p).sum::<f64>();
    let mut best: Option<[f64; RUN_MOST + 1]> = None;
    for suit in Suit::ALL {
        if remaining & 1 << suit as usize == 0 {
            continue;
        }
        let mut these = [0.0; RUN_MOST + 1];
        for &(hands, q) in &ways[suit as usize] {
            let next = plan_step(hands, state, side, opponents, Some(suit) == trump, trump.is_some());
            // A suit that takes nothing is left, and the run goes on by the others.
            let after = best_rest(next, remaining & !(1 << suit as usize), ways, side, opponents, trump, known);
            for (t, a) in these.iter_mut().zip(after) {
                *t += q * a;
            }
        }
        if best.is_none_or(|b| mean(&these) > mean(&b)) {
            best = Some(these);
        }
    }
    let best = best.unwrap();
    known.push(((state, remaining), best));
    best
}

/// What [`best_rest`] has worked out, by the state and the suits left (a
/// list, since it is short and a hash costs more than looking through it).
#[cfg(test)]
type RunMemo = Vec<((RunState, u8), [f64; RUN_MOST + 1])>;

/// The [`RunMemo`]s of a view: one for each way of the trump states, suit
/// led and card that leaves another suit, since those are what the rest of
/// the plan after the trick depends on.
#[cfg(test)]
type RunMemos = std::sync::Mutex<Vec<(([(bool, u8); 4], Suit, Option<Card>, u8), RunMemo)>>;

/// The chances of runs of 0 to [`RUN_MOST`] tricks of two independent
/// parts added up, a run of the most or more counting as the most.
#[cfg(test)]
fn add_runs(a: &[f64; RUN_MOST + 1], b: &[f64; RUN_MOST + 1]) -> [f64; RUN_MOST + 1] {
    let mut sum = [0.0; RUN_MOST + 1];
    for (i, &p) in a.iter().enumerate() {
        for (j, &q) in b.iter().enumerate() {
            sum[(i + j).min(RUN_MOST)] += p * q;
        }
    }
    sum
}

/// How many ways that the hidden cards could lie [`Table::Run`] tries.
#[cfg(test)]
const RUN_SAMPLES: usize = 24;

/// The most plays that a search of [`Table::Run`] makes before it gives up,
/// counting the opponents' as winning.
#[cfg(test)]
const RUN_NODES: usize = 50_000;

/// The search that [`Table::Run`] makes in one deal of all four hands: how
/// many tricks in a row, up to a most, the side of the seats 0 and 2 takes
/// from a trick on, when it chooses every play of its two hands and the
/// opponents choose theirs to stop it.
#[cfg(test)]
struct RunSearch {
    trump: Option<Suit>,
    nodes: usize,
}

#[cfg(test)]
impl RunSearch {
    /// The tricks in a row, at most `depth`, that seats 0 and 2 take from the
    /// trick that `leader` led and `played` (the seat and card of each play so
    /// far) are the plays of, with `hands` what the seats still hold.
    fn trick(&mut self, hands: &mut [Cards; 4], leader: usize, played: &mut Vec<(usize, Card)>, depth: usize) -> usize {
        if played.len() == 4 {
            let mut top = played[0];
            for &(seat, card) in &played[1..] {
                if beats(self.trump, card, top.1) {
                    top = (seat, card);
                }
            }
            if !top.0.is_multiple_of(2) {
                return 0;
            }
            if depth <= 1 || hands.iter().all(|hand| hand.is_empty()) {
                return 1;
            }
            let mut next = Vec::with_capacity(4);
            return 1 + self.trick(hands, top.0, &mut next, depth - 1);
        }
        self.nodes += 1;
        if self.nodes > RUN_NODES {
            return 0;
        }
        let seat = (leader + played.len()) % 4;
        let ours = seat.is_multiple_of(2);
        let mut best = if ours { 0 } else { depth };
        for card in self.candidates(hands, seat, played) {
            hands[seat] -= Cards::singleton(card);
            played.push((seat, card));
            let value = self.trick(hands, leader, played, depth);
            played.pop();
            hands[seat] += Cards::singleton(card);
            if ours {
                best = best.max(value);
                if best >= depth {
                    break;
                }
            } else {
                best = best.min(value);
                if best == 0 {
                    break;
                }
            }
        }
        best
    }

    /// The cards that `seat` could play next that are worth trying: of
    /// cards that touch among those left, the lowest; and for an opponent
    /// only those that beat the trick so far (ruffs and overruffs among them),
    /// the lowest card that follows, and when it cannot follow the lowest card
    /// of each suit, since it plays to take the trick, or else to give
    /// nothing up.
    fn candidates(&self, hands: &[Cards; 4], seat: usize, played: &[(usize, Card)]) -> Vec<Card> {
        let hand = hands[seat];
        let led = played.first().map(|&(_, card)| card.suit());
        let following = led.is_some_and(|led| !hand.in_suit(led).is_empty());
        let legal = if following { hand.in_suit(led.unwrap()) } else { hand };
        let left = hands.iter().fold(Cards::EMPTY, |cards, &hand| cards + hand)
            + played.iter().fold(Cards::EMPTY, |cards, &(_, card)| cards + Cards::singleton(card));
        let mut lowest: Vec<Card> = Vec::new();
        for suit in Suit::ALL {
            let mut previous: Option<Card> = None;
            for card in legal.in_suit(suit) {
                // Cards come lowest first.
                let touches = previous.is_some_and(|before| {
                    !left.in_suit(suit).into_iter().any(|other| other.rank() > before.rank() && other.rank() < card.rank())
                });
                if !touches {
                    lowest.push(card);
                }
                previous = Some(card);
            }
        }
        // The cards most likely to settle the search come first, so that it
        // stops as soon as a line takes every trick it counts (or, for an
        // opponent, wins one): the highest cards, with trumps ahead when the
        // trick is not trumped yet.
        let top = played.iter().map(|&(_, card)| card).reduce(|top, card| if beats(self.trump, card, top) { card } else { top });
        let strength = |card: &Card| {
            let beating = top.is_some_and(|top| beats(self.trump, *card, top));
            (beating, Some(card.suit()) == self.trump, card.rank())
        };
        if seat.is_multiple_of(2) {
            lowest.sort_by_key(|card| std::cmp::Reverse(strength(card)));
            return lowest;
        }
        let mut chosen: Vec<Card> = lowest.iter().copied().filter(|&card| top.is_none_or(|top| beats(self.trump, card, top))).collect();
        if following {
            chosen.extend(legal.min());
        } else {
            for suit in Suit::ALL {
                chosen.extend(legal.in_suit(suit).min());
            }
        }
        chosen.sort_by_key(|card| (card.suit() as usize, card.rank()));
        chosen.dedup();
        chosen.sort_by_key(|card| std::cmp::Reverse(strength(card)));
        chosen
    }
}

/// The prior and the evidence by which the views of self-play keep the
/// players' beliefs about where the hidden cards lie, for the tables that
/// read them ([`Table::AboveSplit`]), set once by self-play when its file
/// is given.
#[cfg(test)]
pub(crate) static LOCATIONS: std::sync::OnceLock<(crate::ai::locations::Prior, crate::ai::locations::Evidence)> =
    std::sync::OnceLock::new();

/// How much the players' beliefs about where the hidden cards lie are the
/// truth, λ from 0 to 1, as the bits of an `f64`: the chance of each way the
/// cards a table reads could lie is λ for the way they do lie and 1 − λ times
/// that of the beliefs.  At 1 the beliefs are the true deal, and no beliefs
/// are kept.  Self-play sets it from `PLAY_SELF_ORACLE`.
#[cfg(test)]
pub(crate) static ORACLE: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// λ of [`ORACLE`].
#[cfg(test)]
fn oracle() -> f64 {
    f64::from_bits(ORACLE.load(std::sync::atomic::Ordering::Relaxed))
}

/// Sets λ of [`ORACLE`].
#[cfg(test)]
fn set_oracle(lambda: f64) {
    ORACLE.store(lambda.to_bits(), std::sync::atomic::Ordering::Relaxed);
}

/// A player's beliefs about where the hidden cards lie, as a [`View`] reads
/// them.
#[cfg(test)]
pub(crate) struct Seen {
    /// The beliefs, unless they are the truth alone.
    beliefs: Option<crate::ai::locations::Beliefs>,
    /// Its joint chances, worked out when a table first reads them.
    joint: std::sync::OnceLock<crate::ai::locations::Joint>,
    /// The hidden cards whose place is not known.
    free: Cards,
    /// Those known to be in the player's first hidden hand.
    first: Cards,
    /// Whether the player's first hidden hand is the beliefs' second, as it
    /// is for the declarer playing from the dummy.
    swapped: bool,
    /// The cards that the player's first hidden hand holds.
    truth: Cards,
    /// How much the beliefs are the truth (see [`ORACLE`]).
    oracle: f64,
    /// What works out the chances of how the hidden hands could beat the
    /// trick, for [`Table::Later3`], once it first reads them; it keeps them,
    /// since the cards of a view that do not beat the trick share them.
    beating: std::sync::OnceLock<crate::ai::locations::Beater>,
    /// The chances of how many tricks of its suit each card the player can
    /// play gives away, for [`Table::SuitTricks`], once worked out.
    suit_losses: std::sync::Mutex<Vec<(Card, [f64; 4])>>,
    /// The ways that the player's first hidden hand could be dealt by the
    /// beliefs, for [`Table::Run`], dealt once for all the cards of a view.
    run_deals: std::sync::OnceLock<Vec<Cards>>,
    /// Each suit's ways of lying among the four hands, for
    /// [`Table::RunOdds`], worked out once for all the cards of a view.
    run_splits: std::sync::OnceLock<[Vec<SuitSplit>; 4]>,
    /// What [`Table::RunPlan`] has worked out of the plans after the trick,
    /// shared by the cards of a view whose plans are the same.
    run_memos: RunMemos,
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
        #[cfg(test)]
        let beliefs = (|| {
            let player_first = (1..4).map(|n| seat + n).find(|&s| s != in_sight)?;
            let (truth, oracle) = (hands[player_first], oracle());
            if oracle >= 1.0 {
                let joint = std::sync::OnceLock::new();
                return Some(std::sync::Arc::new(Seen { beliefs: None, joint, free: Cards::EMPTY, first: truth, swapped: false, truth, oracle, beating: std::sync::OnceLock::new(), suit_losses: Default::default(), run_deals: Default::default(), run_splits: Default::default(), run_memos: Default::default() }));
            }
            let (prior, evidence) = LOCATIONS.get()?;
            let viewer = if seat == dummy { declarer } else { seat };
            let plays = game.tricks.get(..done)?.iter().flat_map(|(_, cards)| cards.iter().copied()).chain(trick.iter().copied());
            let tracker = crate::ai::locations::replayed(game, convention, viewer, plays, prior, evidence)?;
            let swapped = player_first != tracker.first;
            let b = &tracker.beliefs;
            let first = if swapped { b.in_second } else { b.in_first };
            let free = b.unseen - b.in_first - b.in_second;
            let joint = std::sync::OnceLock::new();
            Some(std::sync::Arc::new(Seen { beliefs: Some(tracker.beliefs), joint, free, first, swapped, truth, oracle, beating: std::sync::OnceLock::new(), suit_losses: Default::default(), run_deals: Default::default(), run_splits: Default::default(), run_memos: Default::default() }))
        })();
        let mut hidden_seats = (1..4).map(|n| seat + n).filter(|&s| s != in_sight);
        let hidden_order = [hidden_seats.next().unwrap(), hidden_seats.next().unwrap()];
        let shown = hidden_order.map(|hidden_seat| {
            Shown::new(game, convention, hidden_seat, played[hidden_seat], shown_out[hidden_seat], hidden)
        });
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
                bid_by[*suit as usize] |= 1 << from_me(game.hand_of_caller(game.dealer + i));
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
            hidden_after: hidden_order.map(|s| later.contains(&s)),
            hidden_partner: hidden_order.map(|s| s == seat + 2),
            sizes: std::array::from_fn(|n| hands[seat + n].len()),
            trick: std::array::from_fn(|i| trick.get(i).copied()),
            suit_play: std::cell::OnceCell::new(),
            #[cfg(test)]
            order: std::array::from_fn(|n| match seat + n {
                _ if n == 0 => 0,
                s if s == in_sight => 1,
                s if s == hidden_order[0] => 2,
                _ => 3,
            }),
            #[cfg(test)]
            beliefs,
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

    /// What [`Table::Later2`] says of playing `card`: 4 times the opponents'
    /// value and then the partner's.  Of the cards that a later hidden hand
    /// could hold (it may have a card of a suit if the auction and the play
    /// leave it room for one) those that beat the card and the trick so far
    /// are the ones that could win; they win by following suit if one is of
    /// the suit led, and otherwise by ruffing if the hand can be void in the
    /// suit led.
    fn later2(&self, card: Card) -> usize {
        let led = self.lead.unwrap_or(card.suit());
        let top = match self.winning {
            Some(winning) if !beats(self.trump, card, winning) => winning,
            _ => card,
        };
        let mut beating = Cards::higher_than(top);
        if let Some(trump) = self.trump.filter(|&trump| trump != top.suit()) {
            beating += Cards::ALL.in_suit(trump);
        }
        let beating = beating & self.hidden;
        let following = Cards::ALL.in_suit(led);
        // The opponents together, and then the partner: the value of the
        // hand that could do the most, where beating by following goes before
        // beating by ruffing, then not beating, then not playing.
        let mut values = [0, 0];
        for k in 0..2 {
            if !self.hidden_after[k] {
                continue;
            }
            let holdable = Suit::ALL
                .into_iter()
                .filter(|&suit| self.shown[k].length[suit as usize].1 > 0)
                .fold(Cards::EMPTY, |cards, suit| cards + Cards::ALL.in_suit(suit));
            let could = beating & holdable;
            let value = if !(could & following).is_empty() {
                2
            } else if self.shown[k].length[led as usize].0 == 0 && !(could - following).is_empty() {
                3
            } else {
                1
            };
            let side = &mut values[self.hidden_partner[k] as usize];
            let rank = |value: usize| [0, 1, 3, 2][value];
            if rank(value) > rank(*side) {
                *side = value;
            }
        }
        values[0] * 4 + values[1]
    }

    /// What [`Table::Later3`] says of playing `card`: as
    /// [`View::later2`] gives it, 4 times the opponents' value and then the
    /// partner's, but for each way that the hidden hands could hold the
    /// cards that beat the card and the trick so far, with its chance by the
    /// player's beliefs, mixed with the truth as [`ORACLE`] says.
    #[cfg(test)]
    fn later3(&self, card: Card) -> Vec<(usize, f64)> {
        let seen = self.beliefs.as_ref().expect("the beliefs of PLAY_SELF_LOCATIONS or PLAY_SELF_ORACLE");
        let led = self.lead.unwrap_or(card.suit());
        let top = match self.winning {
            Some(winning) if !beats(self.trump, card, winning) => winning,
            _ => card,
        };
        let mut beating = Cards::higher_than(top);
        if let Some(trump) = self.trump.filter(|&trump| trump != top.suit()) {
            beating += Cards::ALL.in_suit(trump);
        }
        let beating = beating & self.hidden;
        let led_cards = self.hidden.in_suit(led);
        let following = beating.in_suit(led);
        let ruffing = match self.trump {
            Some(trump) if trump != led => beating.in_suit(trump),
            _ => Cards::EMPTY,
        };
        // The chances of the states of the player's first and second hidden
        // hands (see `Beliefs::beating`).
        let mut chances = [[0.0; 3]; 3];
        if seen.oracle < 1.0 {
            let beliefs = seen.beliefs.as_ref().expect("the beliefs");
            let after = if seen.swapped { [self.hidden_after[1], self.hidden_after[0]] } else { self.hidden_after };
            let believed = seen.beating.get_or_init(|| beliefs.beater()).beating(led_cards, following, ruffing, after);
            for (a, row) in believed.iter().enumerate() {
                for (b, &chance) in row.iter().enumerate() {
                    let (first, second) = if seen.swapped { (b, a) } else { (a, b) };
                    chances[first][second] += (1.0 - seen.oracle) * chance;
                }
            }
        }
        if seen.oracle > 0.0 {
            let state = |hand: Cards, after: bool| {
                if !after {
                    0
                } else if !(hand & following).is_empty() {
                    1
                } else if (hand & led_cards).is_empty() && !(hand & ruffing).is_empty() {
                    2
                } else {
                    0
                }
            };
            let first = seen.truth & self.hidden;
            chances[state(first, self.hidden_after[0])][state(self.hidden - first, self.hidden_after[1])] += seen.oracle;
        }
        let mut entries = [0.0; 16];
        for (a, row) in chances.iter().enumerate() {
            for (b, &chance) in row.iter().enumerate() {
                // As in `later2`: the hand that could do the most, where
                // beating by following goes before ruffing, then not beating,
                // then not playing.
                let mut values = [0, 0];
                for (k, state) in [a, b].into_iter().enumerate() {
                    if !self.hidden_after[k] {
                        continue;
                    }
                    let value = [1, 2, 3][state];
                    let side = &mut values[self.hidden_partner[k] as usize];
                    let rank = |value: usize| [0, 1, 3, 2][value];
                    if rank(value) > rank(*side) {
                        *side = value;
                    }
                }
                entries[values[0] * 4 + values[1]] += chance;
            }
        }
        entries.into_iter().enumerate().filter(|&(_, chance)| chance > 0.0).collect()
    }

    /// What [`Table::SuitTricks`] says of playing `card`: each number of the
    /// suit's tricks that it gives away, 0 to 3 or more, with its chance, or
    /// 4 if it is off the suit led or the suit cannot be worked out.
    #[cfg(test)]
    fn suit_tricks(&self, card: Card) -> Vec<(usize, f64)> {
        let seen = self.beliefs.as_ref().expect("the beliefs of PLAY_SELF_LOCATIONS or PLAY_SELF_ORACLE");
        let suit = card.suit();
        let off = || vec![(4, 1.0)];
        if self.lead.is_some_and(|lead| lead != suit) || self.trick.iter().flatten().any(|c| c.suit() != suit) {
            return off();
        }
        let free: Vec<Card> = {
            let mut free: Vec<Card> = seen.free.in_suit(suit).collect();
            free.sort_by_key(|c| c.rank());
            free
        };
        // By the cards of the suit left in the hands, not by those whose place
        // is not known, so that the beliefs and the truth leave out the same
        // suits.
        if self.left.in_suit(suit).len() > SUIT_TRICKS_MOST {
            return off();
        }
        let kept = seen.suit_losses.lock().unwrap().iter().find(|(c, _)| *c == card).map(|(_, losses)| *losses);
        let losses = kept.unwrap_or_else(|| {
            let worked = self.suit_losses(seen, suit, &free);
            let mut kept = seen.suit_losses.lock().unwrap();
            kept.extend(worked.iter().copied());
            worked.iter().find(|(c, _)| *c == card).map(|(_, losses)| *losses).expect("a legal card of the suit")
        });
        losses.into_iter().enumerate().filter(|&(_, p)| p > 0.0).collect()
    }

    /// For each card of `suit` that the player can play, the chances of how
    /// many of the suit's tricks it gives away (0 to 3 or more), over the
    /// ways that the `free` cards of the suit, whose place is not known, lie.
    #[cfg(test)]
    fn suit_losses(&self, seen: &Seen, suit: Suit, free: &[Card]) -> Vec<(Card, [f64; 4])> {
        let believed = match &seen.beliefs {
            Some(beliefs) => seen.joint.get_or_init(|| beliefs.joint()).of(free),
            None => (0..1 << free.len()).map(|bits| f64::from(u8::from(bits == 0))).collect(),
        };
        let true_bits: usize =
            free.iter().enumerate().filter(|(_, c)| seen.truth.contains(**c) != seen.swapped).map(|(t, _)| 1 << t).sum();
        let legal: Vec<Card> = self.legal().in_suit(suit).collect();
        let mut losses = vec![[0.0; 4]; legal.len()];
        // Only the order of the cards of the suit still in play matters, so
        // they are numbered from 1 by it, the trick's with them, and the
        // positions of different deals are the same position.  A suit's
        // cards come lowest first.
        let in_play = self.left.in_suit(suit) + self.trick.iter().flatten().fold(Cards::EMPTY, |cards, &c| cards + Cards::singleton(c));
        let mut number = [0u8; 15];
        for (i, c) in in_play.in_suit(suit).enumerate() {
            number[c.rank() as usize] = i as u8 + 1;
        }
        let bits_of = |cards: Cards| cards.in_suit(suit).fold(0u16, |bits, c| bits | 1 << number[c.rank() as usize]);
        let place = self.place;
        let leader = (4 - place) % 4;
        let mut ranks = [0u8; 4];
        for (i, c) in self.trick.iter().flatten().enumerate() {
            ranks[(leader + i) % 4] = number[c.rank() as usize];
        }
        for (bits, &believed) in believed.iter().enumerate() {
            let chance = (1.0 - seen.oracle) * believed + seen.oracle * f64::from(u8::from(bits == true_bits));
            if chance == 0.0 {
                continue;
            }
            let mut first = seen.first;
            for (t, &c) in free.iter().enumerate() {
                if (bits & (1 << t) != 0) != seen.swapped {
                    first += Cards::singleton(c);
                }
            }
            let holdings = [self.mine, self.other, first, self.hidden - first].map(bits_of);
            let hands: [u16; 4] = std::array::from_fn(|n| holdings[self.order[n]]);
            let values: Vec<u8> = legal
                .iter()
                .map(|c| {
                    let mut after = hands;
                    let n = number[c.rank() as usize];
                    after[0] &= !(1 << n);
                    let mut ranks = ranks;
                    ranks[0] = n;
                    SUIT_SOLVER.with(|solver| solver.borrow_mut().tricks(after, leader, place + 1, ranks))
                })
                .collect();
            let best = values.iter().copied().max().unwrap_or(0);
            for (loss, &value) in losses.iter_mut().zip(&values) {
                loss[((best - value) as usize).min(3)] += chance;
            }
        }
        legal.into_iter().zip(losses).collect()
    }

    /// What [`Table::Run`] says of playing `card`: the median of
    /// [`View::run_runs`], the lower of the middle two for an even number.
    #[cfg(test)]
    fn run_median(&self, card: Card) -> usize {
        let runs = self.run_runs(card);
        runs[(runs.len() - 1) / 2].0
    }

    /// The runs of [`Table::Run`] after playing `card`, sorted: over the ways
    /// that the hidden cards could lie by the player's beliefs (or the
    /// true way, as [`ORACLE`] says), the tricks in a row that the player's
    /// side takes from this trick on, found by [`RunSearch`] in each, at most
    /// [`RUN_MOST`], each with whether the search gave up.
    #[cfg(test)]
    fn run_runs(&self, card: Card) -> Vec<(usize, bool)> {
        let seen = self.beliefs.as_ref().expect("the beliefs of PLAY_SELF_LOCATIONS or PLAY_SELF_ORACLE");
        let place = self.place;
        let leader = (4 - place) % 4;
        // The ways that the hidden cards could lie are dealt from a seed
        // that the position makes, so that the same view always gives the
        // same value, and once, for all the cards of the view.
        let deals = seen.run_deals.get_or_init(|| {
            use rand::SeedableRng;
            let seed = self.hidden.into_iter().chain(self.mine).chain(self.trick.iter().flatten().copied()).fold(0x9e3779b9u64, |h, c| {
                h.wrapping_mul(0x100000001b3).wrapping_add(c.suit() as u64 * 16 + c.rank() as u64)
            });
            let mut rng = rand::rngs::StdRng::seed_from_u64(seed);
            let samples = if seen.beliefs.is_none() { 1 } else { RUN_SAMPLES };
            (0..samples)
                .map(|i| match &seen.beliefs {
                    Some(beliefs) if (i as f64) >= seen.oracle * samples as f64 => {
                        let dealt = beliefs.deal(&mut rng);
                        let mut first = seen.first;
                        for c in seen.free {
                            if dealt.contains(c) != seen.swapped {
                                first += Cards::singleton(c);
                            }
                        }
                        first
                    }
                    _ => seen.truth & self.hidden,
                })
                .collect()
        });
        let mut runs: Vec<(usize, bool)> = Vec::with_capacity(deals.len());
        let mut search = RunSearch { trump: self.trump, nodes: 0 };
        for &first in deals {
            let holdings = [self.mine - Cards::singleton(card), self.other, first, self.hidden - first];
            let mut hands: [Cards; 4] = std::array::from_fn(|n| holdings[self.order[n]]);
            let mut played: Vec<(usize, Card)> = self.trick.iter().flatten().enumerate().map(|(i, &c)| ((i + 4 - place) % 4, c)).collect();
            played.push((0, card));
            search.nodes = 0;
            let run = search.trick(&mut hands, leader, &mut played, RUN_MOST);
            runs.push((run, search.nodes > RUN_NODES));
        }
        runs.sort_unstable();
        runs
    }

    /// The ways that each suit's cards could lie among the four hands, with
    /// their chances by the beliefs (mixed with the truth as [`ORACLE`] says),
    /// the cards of the trick in progress left out.
    #[cfg(test)]
    fn run_splits(&self, seen: &Seen) -> [Vec<SuitSplit>; 4] {
        Suit::ALL.map(|suit| {
            let mut free: Vec<Card> = seen.free.in_suit(suit).collect();
            free.sort_by_key(|c| c.rank());
            // The lowest beyond the most, which go to the hidden hands in turn.
            let low: Vec<Card> = free.drain(..free.len().saturating_sub(RUN_ODDS_MOST_FREE)).collect();
            let believed = match &seen.beliefs {
                Some(beliefs) => seen.joint.get_or_init(|| beliefs.joint()).of(&free),
                None => (0..1 << free.len()).map(|bits| f64::from(u8::from(bits == 0))).collect(),
            };
            let true_bits: usize =
                free.iter().enumerate().filter(|(_, c)| seen.truth.contains(**c) != seen.swapped).map(|(t, _)| 1 << t).sum();
            let bits_of = |cards: Cards| cards.in_suit(suit).fold(0u16, |bits, c| bits | 1 << c.rank());
            let mut base = seen.first;
            for (i, &c) in low.iter().enumerate() {
                if i % 2 == 0 {
                    base += Cards::singleton(c);
                }
            }
            let mut splits = Vec::new();
            for (bits, &believed) in believed.iter().enumerate() {
                let chance = (1.0 - seen.oracle) * believed + seen.oracle * f64::from(u8::from(bits == true_bits));
                if chance == 0.0 {
                    continue;
                }
                let mut first = base;
                for (t, &c) in free.iter().enumerate() {
                    if (bits & (1 << t) != 0) != seen.swapped {
                        first += Cards::singleton(c);
                    }
                }
                splits.push(([self.mine, self.other, first, self.hidden - first].map(bits_of), chance));
            }
            splits
        })
    }

    /// For [`Table::RunOdds`] and [`Table::RunSure`]: the chances of each run,
    /// 0 to [`RUN_MOST`] tricks in a row from this trick on, after playing
    /// `card`, and the sure run.
    #[cfg(test)]
    fn run_odds(&self, card: Card) -> ([f64; RUN_MOST + 1], usize) {
        let seen = self.beliefs.as_ref().expect("the beliefs of PLAY_SELF_LOCATIONS or PLAY_SELF_ORACLE");
        let splits = seen.run_splits.get_or_init(|| self.run_splits(seen));
        // The hands of the player's side, and the opponents': the player's
        // partner is the other hand in sight for the declarer's side, and
        // the hidden partner for a defender.
        let partner = if self.role <= 1 { 1 } else if self.hidden_partner[0] { 2 } else { 3 };
        let ours = [0, partner];
        let opponents: Vec<usize> = (1..4).filter(|&h| h != partner).collect();
        let opponents = [opponents[0], opponents[1]];
        let led = self.lead.unwrap_or(card.suit());
        let trump = self.trump;
        let rank_bit = |c: Card| 1u16 << c.rank();
        // Each hand's trumps, as a state: 0 none, 1 none above `above`, 2 one
        // above it.
        let trump_state = |bits: u16, above: u8| if bits == 0 { 0 } else if top_rank(bits) > above { 2 } else { 1 };
        // The trick so far: the top card as (trumps, rank, hand).
        let place = self.place;
        let mut top: Option<(bool, u8, usize)> = None;
        let beats = |new: (bool, u8), top: Option<(bool, u8, usize)>| match top {
            None => true,
            Some((t, r, _)) => (new.0 && !t) || (new.0 == t && new.1 > r),
        };
        for (i, c) in self.trick.iter().flatten().enumerate() {
            let hand = self.order[(i + 4 - place) % 4];
            let key = (Some(c.suit()) == trump, c.rank());
            if (c.suit() == led || key.0) && beats(key, top) {
                top = Some((key.0, key.1, hand));
            }
        }
        let mine_key = (Some(card.suit()) == trump, card.rank());
        if (card.suit() == led || mine_key.0) && beats(mine_key, top) {
            top = Some((mine_key.0, mine_key.1, 0));
        }
        let above = match top {
            Some((true, r, _)) => r,
            _ => 0,
        };
        let later: Vec<usize> = (1..4 - place).map(|n| self.order[n]).collect();
        // The ways of lying of the trump suit, as each hidden hand's trump
        // state, with the chances, and the trump suit's own runs in each.
        let trump_suit = trump.filter(|&t| t != led);
        // The chances of each suit's run but the suit led's, when the
        // hidden hands' trump states are `states` (any trump counts for a
        // ruff), the card leaving the player's hand if it is of that suit.
        let rest = |states: [u8; 2], trump_splits: Option<&[(usize, SuitSplit)]>| -> ([f64; RUN_MOST + 1], usize) {
            let mut total = [0.0; RUN_MOST + 1];
            total[0] = 1.0;
            let mut sure = 0;
            for suit in Suit::ALL {
                if suit == led {
                    continue;
                }
                let has_trump = |h: usize| match h {
                    2 | 3 => states[h - 2] > 0,
                    _ => trump.is_some_and(|t| !(if h == 0 { self.mine } else { self.other }).in_suit(t).is_empty()),
                };
                let ruffs = if trump.is_some_and(|t| t != suit) { opponents.map(has_trump) } else { [false; 2] };
                let mut odds = [0.0; RUN_MOST + 1];
                let mut fewest = RUN_MOST;
                let mut total_chance = 0.0;
                let from: Vec<(usize, SuitSplit)> = match trump_splits.filter(|_| Some(suit) == trump) {
                    Some(splits) => splits.to_vec(),
                    None => splits[suit as usize].iter().map(|&s| (0, s)).collect(),
                };
                for (_, (mut hands, chance)) in from {
                    if card.suit() == suit {
                        hands[0] &= !rank_bit(card);
                    }
                    let run = cash(hands, ours, opponents, ruffs);
                    odds[run] += chance;
                    total_chance += chance;
                    fewest = fewest.min(run);
                }
                if total_chance > 0.0 {
                    odds.iter_mut().for_each(|p| *p /= total_chance);
                    total = add_runs(&total, &odds);
                    sure = (sure + fewest).min(RUN_MOST);
                }
            }
            (total, sure)
        };
        // The ways of the trump states, when trumps are not led: from the
        // trump suit's ways of lying, grouped by the hidden hands' states.
        let groups: Vec<TrumpGroup> = match trump_suit {
            None => vec![([0, 0], 1.0, Vec::new())],
            Some(t) => {
                let mut groups: Vec<TrumpGroup> = Vec::new();
                for &(hands, chance) in &splits[t as usize] {
                    let states = [trump_state(hands[2], above), trump_state(hands[3], above)];
                    match groups.iter_mut().find(|g| g.0 == states) {
                        Some(g) => {
                            g.1 += chance;
                            g.2.push((0, (hands, chance)));
                        }
                        None => groups.push((states, chance, vec![(0, (hands, chance))])),
                    }
                }
                let total: f64 = groups.iter().map(|g| g.1).sum();
                groups.iter_mut().for_each(|g| g.1 /= total);
                groups
            }
        };
        let mut odds = [0.0; RUN_MOST + 1];
        let mut sure = RUN_MOST;
        let led_total: f64 = splits[led as usize].iter().map(|s| s.1).sum();
        for (states, group_chance, trump_splits) in &groups {
            // When trumps are led, the states come from the suit led's way.
            let mut kept: Option<([f64; RUN_MOST + 1], usize)> = None;
            for &(holdings, chance) in &splits[led as usize] {
                let mut hands = holdings;
                if card.suit() == led {
                    hands[0] &= !rank_bit(card);
                }
                let states = if trump == Some(led) { [trump_state(hands[2], above), trump_state(hands[3], above)] } else { *states };
                // Each later hand plays: to win the trick if its side is not
                // winning and it can, else its lowest of the suit led.
                let ruffs = |h: usize| {
                    trump.is_some_and(|t| t != led)
                        && match h {
                            2 | 3 => states[h - 2] == 2,
                            _ => trump_state((if h == 0 { self.mine } else { self.other }).in_suit(trump.unwrap()).fold(0u16, |b, c| b | 1 << c.rank()), above) == 2,
                        }
                };
                let (top, after, _) = play_out_trick(hands, top, &later, ours, trump == Some(led), ruffs);
                let won = top.is_some_and(|(_, _, w)| ours.contains(&w));
                let (runs, fewest) = if won {
                    let rest = *kept.get_or_insert_with(|| rest(states, Some(trump_splits)));
                    let ruffs = if trump.is_some_and(|t| t != led) {
                        opponents.map(|h| match h {
                            2 | 3 => states[h - 2] > 0,
                            _ => !(if h == 0 { self.mine } else { self.other }).in_suit(trump.unwrap()).is_empty(),
                        })
                    } else {
                        [false; 2]
                    };
                    let more = 1 + cash(after, ours, opponents, ruffs);
                    let mut runs = [0.0; RUN_MOST + 1];
                    for (k, &p) in rest.0.iter().enumerate() {
                        runs[(k + more).min(RUN_MOST)] += p;
                    }
                    (runs, (rest.1 + more).min(RUN_MOST))
                } else {
                    let mut runs = [0.0; RUN_MOST + 1];
                    runs[0] = 1.0;
                    (runs, 0)
                };
                if trump == Some(led) {
                    kept = None;
                }
                let weight = group_chance * chance / led_total;
                for (o, r) in odds.iter_mut().zip(runs) {
                    *o += weight * r;
                }
                sure = sure.min(fewest);
            }
        }
        (odds, sure)
    }

    /// For [`Table::RunPlan`]: the chances of each run, 0 to [`RUN_MOST`]
    /// tricks in a row from this trick on, after playing `card`.
    #[cfg(test)]
    fn run_plan(&self, card: Card) -> [f64; RUN_MOST + 1] {
        let seen = self.beliefs.as_ref().expect("the beliefs of PLAY_SELF_LOCATIONS or PLAY_SELF_ORACLE");
        let splits = seen.run_splits.get_or_init(|| self.run_splits(seen));
        let partner = if self.role <= 1 { 1 } else if self.hidden_partner[0] { 2 } else { 3 };
        let side = [0, partner];
        let others: Vec<usize> = (1..4).filter(|&h| h != partner).collect();
        let opponents = [others[0], others[1]];
        let led = self.lead.unwrap_or(card.suit());
        let trump = self.trump;
        let rank_bit = |c: Card| 1u16 << c.rank();
        let without_card = |suit: Suit, mut hands: [u16; 4]| {
            if card.suit() == suit {
                hands[0] &= !rank_bit(card);
            }
            hands
        };
        // The trick so far, as its top card: (trumps, rank, hand).
        let place = self.place;
        let beats = |new: (bool, u8), top: Option<(bool, u8, usize)>| match top {
            None => true,
            Some((t, r, _)) => (new.0 && !t) || (new.0 == t && new.1 > r),
        };
        let mut top: Option<(bool, u8, usize)> = None;
        for (i, c) in self.trick.iter().flatten().enumerate() {
            let key = (Some(c.suit()) == trump, c.rank());
            if (c.suit() == led || key.0) && beats(key, top) {
                top = Some((key.0, key.1, self.order[(i + 4 - place) % 4]));
            }
        }
        let key = (Some(card.suit()) == trump, card.rank());
        if (card.suit() == led || key.0) && beats(key, top) {
            top = Some((key.0, key.1, 0));
        }
        let above = match top {
            Some((true, r, _)) => r,
            _ => 0,
        };
        let later: Vec<usize> = (1..4 - place).map(|n| self.order[n]).collect();
        // The ways of lying of the trump suit, in groups that give the four
        // hands the same trumps for this trick: whether each holds one above
        // the trick's top trump, and how many it holds (at most RUN_MOST).
        let trump_key = |hands: &[u16; 4]| hands.map(|b| (top_rank(b) > above, (b.count_ones() as u8).min(RUN_MOST as u8)));
        type Group = ([(bool, u8); 4], f64, Vec<SuitSplit>);
        let groups: Vec<Group> = match trump.filter(|&t| t != led) {
            None => vec![([(false, 0); 4], 1.0, Vec::new())],
            Some(t) => {
                let mut groups: Vec<Group> = Vec::new();
                for &(hands, chance) in &splits[t as usize] {
                    let hands = without_card(t, hands);
                    let key = trump_key(&hands);
                    match groups.iter_mut().find(|g| g.0 == key) {
                        Some(g) => {
                            g.1 += chance;
                            g.2.push((hands, chance));
                        }
                        None => groups.push((key, chance, vec![(hands, chance)])),
                    }
                }
                let total: f64 = groups.iter().map(|g| g.1).sum();
                groups.iter_mut().for_each(|g| g.1 /= total);
                groups
            }
        };
        // The suits still to play after the suit led.
        let rest_mask = Suit::ALL.iter().filter(|&&s| s != led).fold(0u8, |mask, &s| mask | 1 << s as usize);
        let led_total: f64 = splits[led as usize].iter().map(|s| s.1).sum();
        // Each suit's ways of lying, their chances summing to 1, the card
        // played leaving the player's hand.
        let side_ways: [Vec<SuitSplit>; 4] = Suit::ALL.map(|suit| {
            let total: f64 = splits[suit as usize].iter().map(|s| s.1).sum();
            splits[suit as usize].iter().map(|&(h, c)| (without_card(suit, h), c / total)).collect()
        });
        let mut odds = [0.0; RUN_MOST + 1];
        for (key, group_chance, trump_splits) in &groups {
            let mut ways = side_ways.clone();
            if let Some(t) = trump.filter(|&t| t != led) {
                let total: f64 = trump_splits.iter().map(|s| s.1).sum();
                ways[t as usize] = trump_splits.iter().map(|&(h, c)| (h, c / total)).collect();
            }
            // The plan after the trick depends on the card only by the suit
            // that it leaves, if that is not the suit led.
            let memo_key = (*key, led, (card.suit() != led).then_some(card), above);
            let mut known: RunMemo = {
                let mut memos = seen.run_memos.lock().unwrap();
                match memos.iter().position(|(k, _)| *k == memo_key) {
                    Some(i) => memos.swap_remove(i).1,
                    None => RunMemo::new(),
                }
            };
            for &(holdings, chance) in &splits[led as usize] {
                let hands = without_card(led, holdings);
                let key = if trump == Some(led) { trump_key(&hands) } else { *key };
                let ruffs = |h: usize| trump.is_some_and(|t| t != led) && key[h].0;
                let (top, after, used) = play_out_trick(hands, top, &later, side, trump == Some(led), ruffs);
                let weight = group_chance * chance / led_total;
                match top {
                    Some((_, _, w)) if side.contains(&w) => {
                        let counts = |h: usize| key[h].1.saturating_sub(used[h]);
                        let start = RunState {
                            run: 1,
                            lead: u8::from(w != 0),
                            ours: side.map(counts),
                            ruffed: [used[side[0]], used[side[1]]],
                            theirs: opponents.map(counts),
                        };
                        let state = plan_step(after, start, side, opponents, trump == Some(led), trump.is_some());
                        let runs = best_rest(state, rest_mask, &ways, side, opponents, trump, &mut known);
                        for (o, r) in odds.iter_mut().zip(runs) {
                            *o += weight * r;
                        }
                    }
                    _ => odds[0] += weight,
                }
            }
            seen.run_memos.lock().unwrap().push((memo_key, known));
        }
        odds
    }

    /// The entries of the tables of `shape`, laid end to end, that playing
    /// `card` looks up, one in each table.
    /// What [`Table::SuitPlay`] says of playing `card`: 4 times how many
    /// fewer tricks at the worst split, and then at the best.
    fn suit_play(&self, card: Card) -> usize {
        usize::from(self.suit_play.get_or_init(|| self.suit_plays())[card.suit() as usize * 16 + card.rank() as usize])
    }

    /// [`View::suit_play`] of every legal card, by its offset.
    fn suit_plays(&self) -> [u8; 64] {
        use suit_play::{Hands, Search};
        let ranks = |cards: Cards, suit: Suit| cards.in_suit(suit).fold(0u16, |bits, c| bits | 1 << (c.rank() - 2));
        // Where the other hand in sight sits, counting clockwise from the
        // player, and so the two hidden hands, the first after the player
        // first.
        let other_at = [2, 2, 1, 3][self.role];
        let hidden_at: Vec<usize> = (1..4).filter(|&n| n != other_at).collect();
        // The tricks of the player's side in `suit` at the worst and the
        // best split of its hidden cards, from `start` with the hands then.
        let extremes = |suit: Suit, search: &mut Search, start: &mut dyn FnMut(&mut Search, Hands) -> u8| {
            let hidden = ranks(self.hidden, suit);
            let k = hidden.count_ones() as usize;
            let bounds: Vec<(usize, usize)> = (0..2)
                .map(|h| {
                    let (least, most) = self.shown[h].length[suit as usize];
                    (least, most.min(self.sizes[hidden_at[h]]))
                })
                .collect();
            let mut hands: Hands = [0; 4];
            hands[0] = ranks(self.mine, suit);
            hands[other_at] = ranks(self.other, suit);
            let (mut worst, mut best) = (u8::MAX, 0);
            let mut any = false;
            for fits in [true, false] {
                let mut first = hidden;
                loop {
                    let n = first.count_ones() as usize;
                    if !fits || ((bounds[0].0..=bounds[0].1).contains(&n) && (bounds[1].0..=bounds[1].1).contains(&(k - n))) {
                        hands[hidden_at[0]] = first;
                        hands[hidden_at[1]] = hidden & !first;
                        let value = start(search, hands);
                        (worst, best) = (worst.min(value), best.max(value));
                        any = true;
                    }
                    if first == 0 {
                        break;
                    }
                    first = (first - 1) & hidden;
                }
                if any {
                    break;
                }
            }
            (worst, best)
        };
        let legal: Vec<Card> = self.legal().collect();
        let mut values = [0u8; 64];
        // The tricks at the worst and best split after each card, or for a
        // discard what it costs its suit, for the cards that are compared.
        let mut after: Vec<(Card, (u8, u8))> = Vec::new();
        let mut searches: [Search; 4] = Default::default();
        for &card in &legal {
            let suit = card.suit();
            let rank = card.rank() as u16 - 2;
            let search = &mut searches[suit as usize];
            match self.lead {
                Some(led) if led != suit => {
                    if self.trump == Some(suit) {
                        continue;
                    }
                    // A discard: what the suit is worth with the card, less
                    // what it is worth without, led by the player.
                    let with = extremes(suit, search, &mut |s, hands| s.led_by(hands, 0));
                    let without = extremes(suit, search, &mut |s, mut hands| {
                        hands[0] &= !(1 << rank);
                        s.led_by(hands, 0)
                    });
                    after.push((card, (u8::MAX - (with.0 - without.0.min(with.0)), u8::MAX - (with.1 - without.1.min(with.1)))));
                }
                _ => {
                    // Played to the trick: what went before, then this card.
                    let mut played = [(0, None); 4];
                    for (i, before) in self.trick.iter().take(self.place).enumerate() {
                        let before = before.unwrap();
                        let hand = 4 - self.place + i;
                        played[i] = (hand, (before.suit() == suit).then(|| before.rank() as u16 - 2));
                    }
                    let place = self.place;
                    let leader = (4 - place) % 4;
                    let value = extremes(suit, search, &mut |s, mut hands| {
                        hands[0] &= !(1 << rank);
                        let mut played = played;
                        played[place] = (0, Some(rank));
                        s.within(hands, leader, &mut played, place + 1)
                    });
                    after.push((card, value));
                }
            }
        }
        // Each card against the best of those it is compared with: the
        // cards of its suit, or every discard.
        for &(card, (worst, best)) in &after {
            let discard = self.lead.is_some_and(|led| led != card.suit());
            let alike = after.iter().filter(|(c, _)| if discard { c.suit() != self.lead.unwrap() } else { c.suit() == card.suit() });
            let (top_worst, top_best) = alike.fold((0, 0), |(w, b), &(_, (cw, cb))| (w.max(cw), b.max(cb)));
            values[card.suit() as usize * 16 + card.rank() as usize] = 4 * (top_worst - worst).min(3) + (top_best - best).min(3);
        }
        values
    }

    pub(crate) fn entries(&self, card: Card, shape: &Shape) -> Entries {
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
            Table::Later2 => (role * PLAYS + play) * 16 + self.later2(card),
            Table::SuitPlay => (role * PLAYS + play) * 16 + self.suit_play(card),
            Table::Bids => (role * PLAYS + play) * 16 + self.bid_by[suit as usize],
            Table::Trick1 => ((role * 4 + place) * 4 + how) * 4 + trick,
            Table::Above1 => place * 125 + above3,
            Table::Below1 => place * 125 + below3,
            Table::Rank1 => ((place * 4 + how) * 6 + above) * 2 + lowest as usize,
            Table::TricksLeft1 => (place * 4 + how) * 6 + tricks,
            Table::Contract1 => ((role * 4 + place) * 4 + how) * 6 + self.contract,
            Table::FirstLength1 => (place * 4 + how) * 20 + self.shown[0].length_index(suit),
            Table::SecondLength1 => (place * 4 + how) * 20 + self.shown[1].length_index(suit),
            Table::AboveSplit | Table::BelowSplit | Table::Later3 | Table::Later3Knots | Table::SuitTricks | Table::Run | Table::RunOdds | Table::RunSure | Table::RunPlan => {
                panic!("{table:?} reads the player's beliefs, which only self-play keeps")
            }
        };
        // The entries of where the three cards just above or below lie, with
        // the hidden ones told apart, each with its chance.
        #[cfg(test)]
        let split = |above: bool| -> Vec<(usize, f64)> {
            let seen = self.beliefs.as_ref().expect("the beliefs of PLAY_SELF_LOCATIONS or PLAY_SELF_ORACLE");
            let in_suit = self.left.in_suit(suit);
            let near: Vec<Card> = if above {
                in_suit.filter(|c| c.rank() > card.rank()).take(3).collect()
            } else {
                in_suit.filter(|c| c.rank() < card.rank()).rev().take(3).collect()
            };
            let mut window: Vec<Card> = near.into_iter().filter(|&c| seen.free.contains(c)).collect();
            window.sort_by_key(|c| c.rank());
            let mut out = Vec::new();
            let believed = match &seen.beliefs {
                Some(beliefs) => seen.joint.get_or_init(|| beliefs.joint()).of(&window),
                None => (0..1 << window.len()).map(|bits| f64::from(u8::from(bits == 0))).collect(),
            };
            // The way the window's cards do lie, in the bits of the beliefs.
            let true_bits: usize =
                window.iter().enumerate().filter(|(_, c)| seen.truth.contains(**c) != seen.swapped).map(|(t, _)| 1 << t).sum();
            for (bits, &believed) in believed.iter().enumerate() {
                let chance = (1.0 - seen.oracle) * believed + seen.oracle * f64::from(u8::from(bits == true_bits));
                if chance == 0.0 {
                    continue;
                }
                let mut first = seen.first;
                for (t, &c) in window.iter().enumerate() {
                    if (bits & (1 << t) != 0) != seen.swapped {
                        first += Cards::singleton(c);
                    }
                }
                let hands = [renumbered(self.mine), renumbered(self.other), renumbered(first), renumbered(self.hidden - first)];
                let number =
                    if above { Cards::holders_above(&hands, card_now, 3) } else { Cards::holders_below(&hands, card_now, 3) };
                out.push((place * 125 + number, chance));
            }
            out
        };
        let mut offset = 0;
        let mut one = Vec::with_capacity(shape.tables.len());
        #[cfg_attr(not(test), expect(unused_mut, reason = "only self-play's tables are weighted"))]
        let mut weighted = Vec::new();
        for &table in shape.tables {
            match table {
                #[cfg(test)]
                Table::AboveSplit | Table::BelowSplit => {
                    weighted.extend(split(table == Table::AboveSplit).into_iter().map(|(e, p)| (offset + e, p)));
                }
                #[cfg(test)]
                Table::Later3 => {
                    let start = offset + (role * PLAYS + play) * 16;
                    weighted.extend(self.later3(card).into_iter().map(|(e, p)| (start + e, p)));
                }
                #[cfg(test)]
                Table::SuitTricks => {
                    let start = offset + (role * PLAYS + play) * 5;
                    weighted.extend(self.suit_tricks(card).into_iter().map(|(e, p)| (start + e, p)));
                }
                #[cfg(test)]
                Table::RunOdds => {
                    let start = offset + (role * PLAYS + play) * 5;
                    weighted.extend(self.run_odds(card).0.into_iter().enumerate().filter(|&(_, p)| p > 0.0).map(|(k, p)| (start + k, p)));
                }
                #[cfg(test)]
                Table::RunPlan => {
                    let start = offset + (role * PLAYS + play) * 5;
                    weighted.extend(self.run_plan(card).into_iter().enumerate().filter(|&(_, p)| p > 0.0).map(|(k, p)| (start + k, p)));
                }
                #[cfg(test)]
                Table::RunSure => {
                    let start = offset + (role * PLAYS + play) * 5;
                    weighted.push((start + self.run_odds(card).1, 1.0));
                }
                #[cfg(test)]
                Table::Run => {
                    let start = offset + (role * PLAYS + play) * 5;
                    weighted.push((start + self.run_median(card), 1.0));
                }
                #[cfg(test)]
                Table::Later3Knots => {
                    let start = offset + (role * PLAYS + play) * 16 * KNOTS.len();
                    for (e, p) in self.later3(card) {
                        for (j, hat) in hats(p).into_iter().enumerate() {
                            if hat > 0.0 {
                                weighted.push((start + e * KNOTS.len() + j, hat));
                            }
                        }
                    }
                }
                _ => {
                    let index = index(table);
                    debug_assert!(index < table.size(), "{table:?}");
                    one.push(offset + index);
                }
            }
            offset += table.size();
        }
        Entries { one, weighted }
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

#[cfg(test)]
/// The hands of `game` with the hidden hands of `seats` dealt `deal`, as they
/// were, or would be, once `trick` (as [`View::at`] takes it) had been
/// played to the current trick.
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

#[cfg(test)]
/// The chance that the player of `view` plays each card that they could,
/// by the play model `model`: the softmax of its scores divided by
/// `temperature`, which is fitted so that these are chances and not only an
/// order.
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

#[cfg(test)]
/// The chance that [`chances`] gives `card`, or 0 if it is not one that could
/// be played, without making the list of all.
pub(crate) fn chance_of(view: &View, model: &Model, temperature: f64, card: Card) -> f64 {
    // A hand has at most 13 cards.
    let mut scores = [0.0; 13];
    let (mut n, mut mine, mut most) = (0, None, f64::MIN);
    for c in view.legal() {
        let score = model.score(view, c) / temperature;
        if c == card {
            mine = Some(n);
        }
        scores[n] = score;
        most = most.max(score);
        n += 1;
    }
    let Some(mine) = mine else {
        return 0.0;
    };
    let sum: f64 = scores[..n].iter().map(|s| (s - most).exp()).sum();
    (scores[mine] - most).exp() / sum
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

#[cfg(test)]
/// The seat whose player chooses the cards of `seat`: the declarer chooses
/// the dummy's.
fn chooser<C>(game: &GameState<C>, seat: Seat) -> Seat {
    if Some(seat) == game.dummy() {
        seat + 2
    } else {
        seat
    }
}

#[cfg(test)]
/// How likely the cards that the players whose hands are `seats` have
/// chosen are, by the play model, if those hands were dealt `deal`: the
/// product of the chance of each card, in the view its player had when they
/// played it.  That includes the dummy's cards when the declarer is hidden,
/// since how the declarer plays the dummy shows something of their own hand.
/// It covers the tricks in `game.tricks` and the current trick: all of the
/// play, unless the game was saved before tricks were kept.  Weighting
/// deals of an ensemble by it draws what can be drawn from the play.
///
/// It gives up, with 0, as soon as the likelihood so far is below `floor`, so
/// that a deal that is hopeless is found out by the first cards that make it
/// so.
pub(crate) fn play_likelihood_above<C>(
    game: &GameState<C>,
    convention: &Convention,
    seats: [Seat; 2],
    deal: [Cards; 2],
    model: &Model,
    temperature: f64,
    floor: f64,
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
                    .map_or(1.0, |view| chance_of(&view, model, temperature, card));
                likelihood *= chance;
                if likelihood < floor {
                    return 0.0;
                }
            }
            hands[seat] -= Cards::singleton(card);
            played[seat] += Cards::singleton(card);
        }
    }
    likelihood
}

/// [`play_likelihood_above`] with no floor.
#[cfg(test)]
pub(crate) fn play_likelihood<C>(
    game: &GameState<C>,
    convention: &Convention,
    seats: [Seat; 2],
    deal: [Cards; 2],
    model: &Model,
    temperature: f64,
) -> f64 {
    play_likelihood_above(game, convention, seats, deal, model, temperature, 0.0)
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

/// A model with random weights, for tests of what uses a model.
#[cfg(test)]
pub(crate) fn random_model(seed: u64) -> Model {
    use rand::{Rng, SeedableRng};
    const SMALL: Shape = Shape { name: "small", tables: V1.tables, layers: &[4, 3] };
    let mut rng = rand::rngs::StdRng::seed_from_u64(seed);
    let mut model = Model::new(&SMALL, 3);
    for p in &mut model.network.params {
        *p = rng.gen_range(-1.0..1.0);
    }
    model
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ai::ensemble::{card_values, plays_for_the_leaders, starting_with, trump_of, Hidden, Reading, Weighted, MISS};
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

    /// A run reads the player's beliefs and never the true deal of the hidden
    /// hands: with the hidden cards of each game dealt again between the two
    /// hidden hands, in the same sizes, every card has the same value.  It
    /// sets [`LOCATIONS`] for the process, so it is run alone.
    #[test]
    #[ignore]
    fn the_run_never_reads_the_hidden_hands() {
        let _ = LOCATIONS.set((crate::ai::locations::Prior::none(), crate::ai::locations::Evidence::none()));
        let mut rng = StdRng::seed_from_u64(5);
        let (mut checked, mut changed) = (0, 0);
        for game in some_games() {
            let convention = game.system.convention();
            let Some(view) = View::new(&game, &convention).filter(|view| view.beliefs.is_some()) else {
                continue;
            };
            let seat = game.lead.unwrap() + game.played.len() % 4;
            let in_sight = if Some(seat) == game.dummy() { seat + 2 } else { game.dummy().unwrap() };
            let hidden_seats: Vec<Seat> = (1..4).map(|n| seat + n).filter(|&s| s != in_sight).collect();
            let mut cards: Vec<Card> = (game.hands[hidden_seats[0]] + game.hands[hidden_seats[1]]).into_iter().collect();
            let first_size = game.hands[hidden_seats[0]].len();
            use rand::seq::SliceRandom;
            cards.shuffle(&mut rng);
            let mut dealt = crate::ai::playout::copy_of(&game);
            dealt.hands[hidden_seats[0]] = cards[..first_size].iter().fold(Cards::EMPTY, |h, &c| h + Cards::singleton(c));
            dealt.hands[hidden_seats[1]] = cards[first_size..].iter().fold(Cards::EMPTY, |h, &c| h + Cards::singleton(c));
            // What each hand has played stays played by it.
            for &hidden_seat in &hidden_seats {
                dealt.original_hands[hidden_seat] =
                    dealt.hands[hidden_seat] + (game.original_hands[hidden_seat] - game.hands[hidden_seat]);
            }
            if dealt.hands[hidden_seats[0]] != game.hands[hidden_seats[0]] {
                changed += 1;
            }
            let other = View::new(&dealt, &convention).unwrap();
            for card in view.legal() {
                assert_eq!(view.run_median(card), other.run_median(card), "{}", serde_json::to_string(&game).unwrap());
                checked += 1;
            }
        }
        assert!(changed > 50 && checked > 100, "{changed} games dealt again, {checked} cards");
    }

    /// The tricks in a row that seats 0 and 2 take from the hands `hands` (a
    /// string for each seat, which `Cards` reads), led by seat 0, with
    /// `trump` as trumps, and how many plays the search made.
    fn run_of(hands: [&str; 4], trump: Option<Suit>) -> (usize, usize) {
        use std::str::FromStr;
        let mut hands = hands.map(|h| Cards::from_str(h).unwrap());
        let mut search = RunSearch { trump, nodes: 0 };
        let run = search.trick(&mut hands, 0, &mut Vec::new(), RUN_MOST);
        (run, search.nodes)
    }

    #[test]
    fn a_run_of_winners_is_found_without_trying_every_play() {
        // Seat 0 has the top spades, and all the tricks are theirs.
        let (run, nodes) = run_of(["SAKQ", "S432", "S765", "SJT9"], None);
        assert_eq!(run, 3);
        assert!(nodes < 100, "{nodes} plays");
        // With more cards the run is cut at the most that is counted.
    }

    #[test]
    fn an_opponent_who_can_ruff_stops_the_run() {
        // Every lead is a diamond, and seat 1 has no diamond, and trumps.
        let (run, _) = run_of(["DA2", "HKQ", "D34", "C56"], Some(Suit::Hearts));
        assert_eq!(run, 0);
        // With no trumps nothing stops the ace, and then seat 2's higher
        // diamond takes the second trick, the lead being seat 0's.
        let (run, _) = run_of(["DA2", "HKQ", "D34", "C56"], None);
        assert_eq!(run, 2);
    }

    #[test]
    fn a_cross_ruff_takes_a_trick_in_each_hand() {
        // Seat 0 leads a diamond that seat 2 ruffs, and seat 2 leads a club
        // that seat 0 ruffs.
        let (run, _) = run_of(["DAH3", "CKDK", "H2CA", "CQDQ"], Some(Suit::Hearts));
        assert_eq!(run, 2);
    }

    #[test]
    fn the_run_goes_to_the_hand_that_won_the_trick_to_lead() {
        // Neither of seat 0's cards wins on its own, but seat 2's ace takes
        // whichever is led, and then leads the other suit's ace: the run
        // crosses to the other hand to lead.
        let (run, _) = run_of(["S2H5", "S3H6", "SAHA", "S4H7"], None);
        assert_eq!(run, 2);
    }

    /// [`View::later2`] found card by card, and the soundness of what it says:
    /// when a later hidden hand really holds a card that it may play and
    /// that beats the trick so far, the table says that it could win.
    /// Of the cards that SuitPlay compares, the best at the worst split and
    /// the best at the best split are each 0 fewer, and a search of a view
    /// takes little time.
    #[test]
    fn suit_play_gives_the_best_cards_nothing_fewer() {
        let started = std::time::Instant::now();
        let mut views = 0;
        for game in some_games() {
            let convention = game.system.convention();
            let Some(view) = View::new(&game, &convention) else { continue };
            views += 1;
            let legal: Vec<Card> = view.legal().collect();
            let discard = |c: &Card| view.lead.is_some_and(|led| led != c.suit());
            for &card in &legal {
                if discard(&card) && view.trump == Some(card.suit()) {
                    assert_eq!(view.suit_play(card), 0);
                    continue;
                }
                let alike: Vec<usize> = legal
                    .iter()
                    .filter(|c| {
                        !(discard(c) && view.trump == Some(c.suit()))
                            && if discard(&card) { discard(c) } else { c.suit() == card.suit() }
                    })
                    .map(|&c| view.suit_play(c))
                    .collect();
                assert!(alike.iter().any(|v| v / 4 == 0) && alike.iter().any(|v| v % 4 == 0), "{alike:?}");
            }
        }
        println!("{views} views, {:.0} µs a view", started.elapsed().as_secs_f64() * 1e6 / views as f64);
    }

    #[test]
    fn later2_is_what_a_search_of_the_cards_finds() {
        let (mut checked, mut winners) = (0, 0);
        for game in some_games() {
            let Some(view) = View::new(&game, &game.system.convention()) else {
                continue;
            };
            let seat = game.lead.unwrap() + game.played.len() % 4;
            let in_sight = if Some(seat) == game.dummy() { seat + 2 } else { game.dummy().unwrap() };
            let hidden_seats: Vec<Seat> = (1..4).map(|n| seat + n).filter(|&s| s != in_sight).collect();
            for card in view.legal() {
                let led = view.lead.unwrap_or(card.suit());
                let top = match view.winning {
                    Some(winning) if !beats(view.trump, card, winning) => winning,
                    _ => card,
                };
                let mut expected = [0; 2];
                for k in 0..2 {
                    if !view.hidden_after[k] {
                        continue;
                    }
                    let (mut follows, mut ruffs) = (false, false);
                    for other in view.hidden {
                        let room = view.shown[k].length[other.suit() as usize].1 > 0;
                        if room && beats(view.trump, other, top) {
                            if other.suit() == led {
                                follows = true;
                            } else if view.shown[k].length[led as usize].0 == 0 {
                                ruffs = true;
                            }
                        }
                    }
                    let value = if follows {
                        2
                    } else if ruffs {
                        3
                    } else {
                        1
                    };
                    let side = view.hidden_partner[k] as usize;
                    let rank = |value: usize| [0, 1, 3, 2][value];
                    if rank(value) > rank(expected[side]) {
                        expected[side] = value;
                    }
                }
                assert_eq!(view.later2(card), expected[0] * 4 + expected[1]);
                // What the hands really hold can be done.
                for (k, &hidden_seat) in hidden_seats.iter().enumerate() {
                    let hand = game.hands[hidden_seat];
                    let legal = if hand.in_suit(led).is_empty() { hand } else { hand.in_suit(led) };
                    let side = expected[view.hidden_partner[k] as usize];
                    if view.hidden_after[k] && legal.into_iter().any(|c| beats(view.trump, c, top)) {
                        winners += 1;
                        assert!(side >= 2, "{side} for a hidden hand that can win: {}", serde_json::to_string(&game).unwrap());
                    }
                }
                checked += 1;
            }
        }
        assert!(checked > 100 && winners > 10, "{checked} cards, {winners} that a hidden hand could beat");
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
                    for shape in SHAPES.iter().filter(|shape| !shape.reads_beliefs()) {
                        let mut start = 0;
                        for (table, entry) in shape.tables.iter().zip(view.entries(card, shape).one) {
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
    /// of, whether or not they are read from the play, and count for as many
    /// deals as were asked for; with no deals asked for, the game is as it
    /// was dealt.
    #[test]
    fn playouts_deal_only_the_hands_that_the_player_cannot_see() {
        let mut rng = StdRng::seed_from_u64(5);
        let model = random_model(10);
        for game in some_games() {
            let convention = game.system.convention();
            let game = crate::ai::playout::copy_of(&game);
            let seats = Hidden::new(&game, &convention).unwrap().seats();
            let as_dealt = deals_of(&game, &convention, 0, None, &mut rng);
            assert_eq!((as_dealt[0].0.hands, as_dealt[0].1), (game.hands, 1.0));
            for reading in [None, Some((&model, 1.0))] {
                let deals = deals_of(&game, &convention, 5, reading, &mut rng);
                assert_eq!(5.0, deals.iter().map(|d| d.1).sum::<f64>());
                for (deal, _) in deals {
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
            layers: &[20, 11],
        };
        let shapes: Vec<&'static Shape> = SHAPES.iter().copied().filter(|shape| !shape.reads_beliefs()).collect();
        let mut grown: Vec<Model> = shapes.iter().filter_map(|&shape| old.grown_to(shape)).collect();
        assert_eq!(grown.len(), shapes.len() - 1, "every shape but v1 grows from v1");
        grown.push(old.grown_to(&MORE).unwrap());
        // Nor can a shape that lacks a table, or has narrower layers.
        const FEWER: Shape = Shape { name: "fewer", tables: V2.tables.split_at(1).1, layers: &[16, 8] };
        const NARROWER: Shape = Shape { name: "narrower", tables: V2.tables, layers: &[12, 8] };
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
                more.network.add_gradient_of(&more.entries(&view, card), 1.0, &mut gradient);
            }
        }
        // The output weights of the new units of the last layer come last,
        // and some of the time those units are active, so that they learn.
        let new_outputs = &gradient[gradient.len() - (MORE.layers[1] - V2.layers[1])..];
        assert!(new_outputs.iter().all(|&g| g > 0.0), "{new_outputs:?}");
    }

    /// The tables that a shape written out by [`shape_spec`] can name: every
    /// one but those of [`V1`].
    const NAMED_TABLES: [Table; 26] = [
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
        Table::History,
        Table::Trumps,
        Table::Later,
        Table::Later2,
        Table::SuitPlay,
        Table::Bids,
        Table::AboveSplit,
        Table::BelowSplit,
        Table::Later3,
        Table::Later3Knots,
        Table::SuitTricks,
        Table::Run,
        Table::RunOdds,
        Table::RunSure,
        Table::RunPlan,
    ];

    /// The shape that `spec` names: a shape's name, or tables joined by `+`
    /// that may start with a shape's name for its tables, and then, after a
    /// `/`, the sizes of the layers joined by `,` (`/0` for none; one layer
    /// of 16 if there is no `/`).  So `Trick` is the table alone with a
    /// layer of 16, `Later2/0` that table's weights alone, and
    /// `v2-trumps-history+Later2/16` the best shape and Later2 with one
    /// layer.  A shape that is not one of [`SHAPES`] lives until the program
    /// ends, and is only for fitting, since no model file can name it.
    fn shape_spec(spec: &str) -> &'static Shape {
        if let Some(shape) = Shape::named(spec) {
            return shape;
        }
        let (tables, layers) = spec.split_once('/').unwrap_or((spec, "16"));
        let layers: Vec<usize> = layers.split(',').map(|n| n.trim().parse().unwrap()).filter(|&n| n > 0).collect();
        let mut all: Vec<Table> = Vec::new();
        for (i, name) in tables.split('+').map(str::trim).enumerate() {
            if let Some(table) = NAMED_TABLES.into_iter().find(|t| format!("{t:?}") == name) {
                all.push(table);
            } else if let Some(shape) = Shape::named(name).filter(|_| i == 0) {
                all.extend(shape.tables);
            } else {
                panic!("{spec}: there is no table or first shape {name}");
            }
        }
        Box::leak(Box::new(Shape {
            name: Box::leak(spec.to_string().into_boxed_str()),
            tables: Box::leak(all.into_boxed_slice()),
            layers: Box::leak(layers.into_boxed_slice()),
        }))
    }

    #[test]
    fn a_shape_spec_names_its_tables_and_layers() {
        let shape = shape_spec("v2-trumps+History/16,8");
        assert_eq!(shape.tables, V2_TRUMPS_HISTORY.tables);
        assert_eq!(shape.layers, &[16, 8]);
        let alone = shape_spec("Later2/0");
        assert_eq!((alone.tables, alone.layers), (&[Table::Later2][..], &[][..]));
        assert_eq!(shape_spec("Trick+Rank").layers, &[16]);
        assert_eq!(shape_spec("v2"), &V2);
    }

    /// Screens shapes by a supervised fit to the data in `PLAY_DATA`: for each
    /// of the shapes `PLAY_SHAPES` (comma-separated, see [`shape_spec`]) and each of the penalties
    /// `PLAY_PENALTIES` (0.01), fits a model by `PLAY_STEPS` (300) steps, and
    /// prints the tricks a play its choices lose to the ensemble's best card
    /// on the data in `PLAY_TEST`, with the standard error, and how many more
    /// than the first fit's they are, with the standard error of that paired
    /// difference.  Writes each fit's weights to `PLAY_WEIGHTS_DIR`, if that
    /// is set, as `SHAPE-PENALTY.txt`.
    #[test]
    #[ignore]
    fn screen_play_shapes() {
        let shapes: Vec<&'static Shape> = var("PLAY_SHAPES", "v2").split(',').map(|name| shape_spec(name.trim())).collect();
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
            let split = var("PLAY_SPLIT", "0") == "1";
            for &penalty in &penalties {
                let started = std::time::Instant::now();
                // One fit of every situation, or with PLAY_SPLIT=1 one for
                // each of the groups, each tested on its own group.
                let fits: Vec<Network> = if split {
                    (0..GROUPS.len())
                        .map(|g| {
                            let choices: Vec<_> = train.iter().filter(|s| s.group == g).map(|s| &s.choice).collect();
                            fitting::fit(&choices, Model::new(shape, 1).network, penalty, steps)
                        })
                        .collect()
                } else {
                    let choices: Vec<_> = train.iter().map(|s| &s.choice).collect();
                    vec![fitting::fit(&choices, Model::new(shape, 1).network, penalty, steps)]
                };
                let network = &fits[0];
                let losses: Vec<f64> = test
                    .iter()
                    .map(|s| {
                        let network = &fits[if split { s.group } else { 0 }];
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
                    None => println!(),
                }
                for (g, name) in GROUPS.iter().enumerate() {
                    let at: Vec<usize> = (0..test.len()).filter(|&i| test[i].group == g).collect();
                    let these: Vec<f64> = at.iter().map(|&i| losses[i]).collect();
                    let (mean, spread) = error(&these);
                    print!("    {name:<17} {:>5} plays: loses {mean:.4} ({spread:.4})", at.len());
                    match &first {
                        Some(first) => {
                            let more: Vec<f64> = at.iter().map(|&i| losses[i] - first[i]).collect();
                            let (mean, spread) = error(&more);
                            println!(", {mean:+.4} ({spread:.4}) more than the first");
                        }
                        None => println!(),
                    }
                }
                if first.is_none() {
                    first = Some(losses);
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
        assert!(!model.shape.reads_beliefs(), "the tables are measured by their one entry each, which a table of the beliefs has not");
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
                        entries.one.iter().enumerate().filter(|&(t, _)| Some(t) != left_out).map(|(_, &e)| e).collect();
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
                    let weights = s.choice.iter().map(|(e, _)| network.params[e.one[t]]);
                    weights.clone().fold(f64::MIN, f64::max) - weights.fold(f64::MAX, f64::min)
                })
                .sum::<f64>()
                / n;
            let used: std::collections::HashSet<usize> =
                test.iter().flat_map(|s| s.choice.iter().map(move |(e, _)| e.one[t])).collect();
            println!(
                "{:<14} {mean:>+9.4} {error:>7.4} {spread:>8.3} {:>6} of {:<5}",
                format!("{table:?}"),
                used.len(),
                table.size()
            );
            debug_assert!(used.iter().all(|&e| (start..start + table.size()).contains(&e)));
            start += table.size();
        }
        let mut counts: Vec<Vec<usize>> = model.shape.layers.iter().map(|&width| vec![0; width]).collect();
        let mut cards = 0;
        for s in &test {
            for (entries, _) in &s.choice {
                for (layer, active) in counts.iter_mut().zip(network.active_units(&entries.one)) {
                    for (count, on) in layer.iter_mut().zip(active) {
                        *count += on as usize;
                    }
                }
                cards += 1;
            }
        }
        let share = |counts: &[usize]| {
            counts.iter().map(|&c| format!("{:.0}%", 100.0 * c as f64 / cards as f64)).collect::<Vec<_>>().join(" ")
        };
        for (layer, counts) in counts.iter().enumerate() {
            println!("how often each unit of layer {} is active: {}", layer + 1, share(counts));
        }
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
        const SMALL: Shape = Shape { name: "small", tables: V1.tables, layers: &[4, 3] };
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
                        .map(|(e, _)| weights.score_of(e) / temperature)
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

    /// Writes a corpus of situations of the play valued by playouts, in the
    /// form that [`collect_play_data`] writes and [`read_situations`] reads,
    /// to the file `PLAY_CORPUS`: `PLAY_CORPUS_DEALS` (1000) fresh deals that
    /// the robot bid, from the seed `PLAY_SEED` (1), are each played by the
    /// model `PLAY_MODEL` (see [`read_model`]) in every seat at the
    /// temperature `PLAY_CORPUS_T` (0.03), and of its choices with more than
    /// one run of touching cards a share `PLAY_CORPUS_SHARE` (0.15) is kept.
    /// Each card of a kept choice is valued by `PLAY_CORPUS_PLAYOUTS` (32)
    /// playouts on the true deal by the same model at the same temperature
    /// (see [`playout_at`]), the playouts after the different cards meeting
    /// the same luck.  A value is the tricks that the side that led the
    /// trick takes in the hand.  It runs on `PLAY_THREADS` threads (2), and
    /// a deal's situations depend only on the seed and its number.
    #[test]
    #[ignore]
    fn collect_playout_corpus() {
        let path = std::env::var("PLAY_CORPUS").expect("PLAY_CORPUS");
        let model = read_model(&std::env::var("PLAY_MODEL").expect("PLAY_MODEL"));
        let count = number("PLAY_CORPUS_DEALS", 1000);
        let seed = number("PLAY_SEED", 1) as u64;
        let real = |name: &str, default: f64| var(name, &default.to_string()).parse::<f64>().unwrap();
        let (temperature, share) = (real("PLAY_CORPUS_T", 0.03), real("PLAY_CORPUS_SHARE", 0.15));
        let playouts = number("PLAY_CORPUS_PLAYOUTS", 32);
        let mut dealer = (Generator::new(), StdRng::seed_from_u64(seed), 0);
        let deals: Vec<String> = (0..count).map(|_| fresh_deal(&mut dealer)).collect();
        let next = std::sync::atomic::AtomicUsize::new(0);
        let lines = std::sync::Mutex::new(Vec::new());
        let learners = std::slice::from_ref(&model);
        let players = [Player::Learning(0); 4];
        let started = std::time::Instant::now();
        std::thread::scope(|scope| {
            for _ in 0..number("PLAY_THREADS", 2) {
                scope.spawn(|| {
                    let mut conventions = std::collections::HashMap::new();
                    loop {
                        let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        let Some(json) = deals.get(i) else {
                            break;
                        };
                        let mut game: GameState<()> = serde_json::from_str(json).unwrap();
                        let convention = conventions
                            .entry(format!("{:?}", game.system))
                            .or_insert_with(|| game.system.convention());
                        let mut rng = StdRng::seed_from_u64(seed * 1_000_003 + i as u64);
                        let mut kept = Vec::new();
                        while !game.hand_done {
                            let Some(view) = View::new(&game, convention) else {
                                game.play(model_card(&model, &game, convention));
                                continue;
                            };
                            let legal: Vec<Card> = view.legal().collect();
                            if runs_of(&game, &legal).1 > 1 && rng.gen_bool(share) {
                                let situation = serde_json::to_string(&game).unwrap();
                                let leaders = plays_for_the_leaders(&game);
                                let player = game.lead.unwrap() + current_trick(&game).len();
                                let side = |tricks: [usize; 2]| tricks[usize::from(leaders != (player as usize).is_multiple_of(2))];
                                let mut values = vec![Vec::with_capacity(playouts); legal.len()];
                                for _ in 0..playouts {
                                    let qualities: [[f64; 4]; 13] = std::array::from_fn(|_| std::array::from_fn(|_| rng.r#gen()));
                                    let luck = rng.r#gen();
                                    for (value, &card) in values.iter_mut().zip(&legal) {
                                        let tricks =
                                            playout_at(&game, card, convention, learners, players, temperature, &qualities, luck);
                                        value.push(side(tricks) as f64);
                                    }
                                }
                                kept.push(serde_json::json!({
                                    "stage": 13 - game.tricks.len(),
                                    "situation": situation,
                                    "leaders": leaders,
                                    "values": legal.iter().map(|&c| pair(c)).zip(values).collect::<Vec<_>>(),
                                }));
                            }
                            let card = card_at_quality(&game, &view, &model, temperature, rng.r#gen());
                            game.play(card);
                        }
                        let mut lines = lines.lock().unwrap();
                        lines.extend(kept.into_iter().map(|line| (i, line.to_string())));
                        if i % 100 == 99 {
                            println!("{} deals, {} situations, {:.0} s", i + 1, lines.len(), started.elapsed().as_secs_f64());
                        }
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
        /// Which of [`GROUPS`] it is in.
        group: usize,
        choice: Vec<(Entries, f64)>,
    }

    /// The kinds of situation that [`screen_play_shapes`] reports apart: who
    /// plays, the declarer's side or the defence, and in what contract.
    const GROUPS: [&str; 4] = ["declarer, suit", "declarer, notrump", "defence, suit", "defence, notrump"];

    /// The situations in the files `paths`, comma-separated, each card with
    /// its entries in the tables of `shape`.
    ///
    /// A shape that reads the players' beliefs reads them by the weights in
    /// the file `PLAY_SELF_LOCATIONS` (see [`read_locations`]).
    fn read_situations(paths: &str, shape: &Shape) -> Vec<Situation> {
        if shape.reads_beliefs() && LOCATIONS.get().is_none() {
            let path = std::env::var("PLAY_SELF_LOCATIONS").expect("PLAY_SELF_LOCATIONS, for a shape that reads the beliefs");
            let _ = LOCATIONS.set(read_locations(&path));
        }
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
                    group: 2 * usize::from(view.role >= 2) + usize::from(view.trump.is_none()),
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
        let choices: fitting::Choices<Entries> = train.into_iter().map(|s| s.choice).collect();
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
    /// the weight of 1 over the chance that it was.  Chosen by runs (see
    /// [`Playouts::by_runs`]), it also has the run of each card, by its
    /// number, and how many runs there are.
    struct Decision {
        learner: usize,
        cards: Vec<(Entries, f64)>,
        chosen: usize,
        side: usize,
        trick: usize,
        won_before: usize,
        played_out: Option<Vec<f64>>,
        weight: f64,
        run: Vec<usize>,
        runs: usize,
        /// 1 for a choice that was also played out mirrored (see
        /// [`mirrored`]), 2 for the mirrored choice, and 0 for the others.
        mirror: u8,
    }

    /// The runs of touching cards among `legal`, the cards that the player
    /// to play in `game` could play (see
    /// [`crate::ai::playout::one_of_each_run`]): the number of each card's
    /// run, and how many runs there are.
    fn runs_of(game: &GameState<()>, legal: &[Card]) -> (Vec<usize>, usize) {
        let runs: Vec<Card> = crate::ai::playout::one_of_each_run(game).collect();
        let run = legal.iter().map(|&card| crate::ai::playout::run_of(&runs, card)).collect();
        (run, runs.len())
    }

    /// The score of each of `count` runs, and each card's share of its run,
    /// from the `scores` of the cards, which are in run `run`, at the
    /// temperature `t`: a run's score is t log of the mean of
    /// e<sup>s/t</sup> over its cards, and a card's share is its
    /// e<sup>s/t</sup> over their sum.  As t falls to 0 a run's score is that
    /// of its best card, so the likeliest run holds the card that
    /// [`model_card`] plays; equal cards give the run their score, with no
    /// bonus for its length, and equal shares, as restricted choice has it;
    /// and the slope of a run's score is shared among its cards by their
    /// shares, so every card of a run is learned.
    fn run_scores(run: &[usize], scores: &[f64], count: usize, t: f64) -> (Vec<f64>, Vec<f64>) {
        let mut most = vec![f64::MIN; count];
        for (&r, &score) in run.iter().zip(scores) {
            most[r] = most[r].max(score);
        }
        let mut sums = vec![0.0; count];
        let mut sizes = vec![0.0; count];
        for (&r, &score) in run.iter().zip(scores) {
            sums[r] += ((score - most[r]) / t).exp();
            sizes[r] += 1.0;
        }
        let runs = (0..count).map(|r| most[r] + t * (sums[r] / sizes[r]).ln()).collect();
        let shares = run.iter().zip(scores).map(|(&r, &score)| ((score - most[r]) / t).exp() / sums[r]).collect();
        (runs, shares)
    }

    /// The chance of each run, from the runs' scores (see [`run_scores`]):
    /// their softmax at `temperature`, in tricks.  The entropy is counted
    /// over the runs, and each card has its share of its run's chance.
    fn run_chances(scores: &[f64], temperature: f64) -> Vec<f64> {
        let most = scores.iter().copied().fold(f64::MIN, f64::max);
        let odds: Vec<f64> = scores.iter().map(|s| ((s - most) / temperature).exp()).collect();
        let sum: f64 = odds.iter().sum();
        odds.into_iter().map(|o| o / sum).collect()
    }

    /// The slope of the free energy Σ p v + T H(p) of a choice with the
    /// `scores` of its runs, whose worth is `worth`, at the temperature `t`,
    /// with p the chances of [`run_chances`]: p<sub>i</sub>(v<sub>i</sub> −
    /// Σ p v − t(log p<sub>i</sub> − Σ p log p))/t for run i.  It is zero
    /// when the scores are the worth, up to one constant.
    fn free_energy_slope(scores: &[f64], worth: &[f64], t: f64) -> Vec<f64> {
        let p = run_chances(scores, t);
        let most = scores.iter().copied().fold(f64::MIN, f64::max);
        let sum: f64 = scores.iter().map(|s| ((s - most) / t).exp()).sum();
        let log_p: Vec<f64> = scores.iter().map(|s| (s - most) / t - sum.ln()).collect();
        let mean_worth: f64 = p.iter().zip(worth).map(|(p, v)| p * v).sum();
        let mean_log: f64 = p.iter().zip(&log_p).map(|(p, l)| p * l).sum();
        (0..scores.len())
            .map(|r| p[r] * (worth[r] - mean_worth - t * (log_p[r] - mean_log)) / t)
            .collect()
    }

    /// The free energy Σ p v + t H(p) of a choice whose cards, in run `run`
    /// of `count`, have the `scores`, and whose runs are worth `worth`, with
    /// the chances of the runs at `t` (see [`run_scores`]).
    fn free_energy_of(run: &[usize], scores: &[f64], count: usize, worth: &[f64], t: f64) -> f64 {
        let (runs, _) = run_scores(run, scores, count, t);
        let p = run_chances(&runs, t);
        let entropy: f64 = p.iter().filter(|&&p| p > 0.0).map(|p| -p * p.ln()).sum();
        p.iter().zip(worth).map(|(p, v)| p * v).sum::<f64>() + t * entropy
    }

    /// A choice played out, for fitting a learner's scale: its cards'
    /// scores, their runs, how many, the runs' worth and its weight.
    type Gathered = (Vec<f64>, Vec<usize>, usize, Vec<f64>, f64);

    /// The factor for the scores of `choices` (each its cards' scores, their
    /// runs, how many, the runs' worth and its weight) that makes the sum of
    /// their free energies at `t` highest, by golden section on its log
    /// between 10⁻⁴ and 10.
    fn best_scale(choices: &[&Gathered], t: f64) -> f64 {
        let free = |log_scale: f64| -> f64 {
            let scale = log_scale.exp();
            choices
                .iter()
                .map(|(scores, run, count, worth, weight)| {
                    let scaled: Vec<f64> = scores.iter().map(|s| s * scale).collect();
                    weight * free_energy_of(run, &scaled, *count, worth, t)
                })
                .sum()
        };
        let golden = (5f64.sqrt() - 1.0) / 2.0;
        let (mut low, mut high) = (1e-4f64.ln(), 10f64.ln());
        for _ in 0..60 {
            let (a, b) = (high - golden * (high - low), low + golden * (high - low));
            if free(a) < free(b) {
                low = a;
            } else {
                high = b;
            }
        }
        ((low + high) / 2.0).exp()
    }

    /// Scores that are the worth of their runs have their free energy
    /// highest as they are, at any temperature.
    #[test]
    fn the_best_scale_of_scores_that_are_the_worth_is_one() {
        let choice = (vec![7.0, 6.5, 6.9, 4.0, 6.0], vec![0, 1, 2, 3, 4], 5, vec![7.0, 6.5, 6.9, 4.0, 6.0], 1.0);
        let twice = (choice.0.iter().map(|s| s * 2.0).collect(), choice.1.clone(), 5, choice.3.clone(), 1.0);
        for t in [1.0, 0.3, 0.1] {
            assert!((best_scale(&[&choice], t) - 1.0).abs() < 1e-6);
            assert!((best_scale(&[&twice], t) - 0.5).abs() < 1e-6);
        }
    }

    /// The slope of the free energy is zero where the scores are the worth
    /// of the runs plus a constant, at any temperature, and it pushes the
    /// better run up elsewhere; a little way along it the free energy rises.
    #[test]
    fn the_free_energy_is_flat_where_the_scores_are_the_worth() {
        let worth = [7.0, 6.5, 6.9, 4.0];
        for t in [0.1, 0.3, 1.0] {
            let at: Vec<f64> = worth.iter().map(|v| v + 3.0).collect();
            for g in free_energy_slope(&at, &worth, t) {
                assert!(g.abs() < 1e-12, "{g} at {t}");
            }
            let flat = [0.0; 4];
            let slope = free_energy_slope(&flat, &worth, t);
            assert!(slope[0] > 0.0 && slope[3] < 0.0, "{slope:?}");
            let free = |scores: &[f64]| {
                let p = run_chances(scores, t);
                let entropy: f64 = p.iter().map(|p| -p * p.ln()).sum();
                p.iter().zip(&worth).map(|(p, v)| p * v).sum::<f64>() + t * entropy
            };
            let stepped: Vec<f64> = flat.iter().zip(&slope).map(|(s, g)| s + 1e-3 * g).collect();
            assert!(free(&stepped) > free(&flat));
        }
    }

    /// A draw of 0 plays the likeliest run, cold that is the run of the card
    /// that the model plays, and each card is drawn as often as its run's
    /// chance times its share.
    #[test]
    fn a_draw_of_quality_picks_the_runs_in_order_of_their_chances() {
        let model = random_model(5);
        for game in some_games().iter().take(20) {
            let Some(view) = View::new(game, &game.system.convention()) else {
                continue;
            };
            let legal: Vec<Card> = view.legal().collect();
            let (run, count) = runs_of(game, &legal);
            let all: Vec<f64> = legal.iter().map(|&card| model.score(&view, card)).collect();
            let (scores, shares) = run_scores(&run, &all, count, 0.3);
            let chances = run_chances(&scores, 0.3);
            // Cold, the likeliest run is that of the card that the model plays.
            let cold = run_chances(&run_scores(&run, &all, count, 1e-6).0, 1e-6);
            let played = model_card(&model, game, &game.system.convention());
            let coldest = (0..count).max_by(|&a, &b| cold[a].total_cmp(&cold[b])).unwrap();
            assert_eq!(run[legal.iter().position(|&c| c == played).unwrap()], coldest);
            assert!((chances.iter().sum::<f64>() - 1.0).abs() < 1e-9);
            let likeliest = (0..chances.len()).max_by(|&a, &b| chances[a].total_cmp(&chances[b])).unwrap();
            let first = card_at_quality(game, &view, &model, 0.3, 0.0);
            assert_eq!(run[legal.iter().position(|&c| c == first).unwrap()], likeliest);
            // Many draws play each card about as often as its share.
            let n = 4000;
            let mut played = vec![0usize; legal.len()];
            for k in 0..n {
                let card = card_at_quality(game, &view, &model, 0.3, (k as f64 + 0.5) / n as f64);
                played[legal.iter().position(|&c| c == card).unwrap()] += 1;
            }
            for (j, &count) in played.iter().enumerate() {
                let expected = chances[run[j]] * shares[j];
                assert!((count as f64 / n as f64 - expected).abs() < 2.0 / n as f64 + 1e-9, "{count} of {n} for {expected}");
            }
        }
    }

    /// The card that `model` plays in `view`, the view of the player to play
    /// in `game`, at `temperature`, when the draw is `quality`, from 0 to 1:
    /// the runs are put in order of their chances, the likeliest first (and
    /// by their cards when the chances are equal), and the card is that of
    /// the run at `quality` in the sum of their chances, and within the run
    /// at the rest of `quality`, so that a draw of 0 is the likeliest play
    /// and the same draw is the same degree of error wherever it is used.
    fn card_at_quality(game: &GameState<()>, view: &View, model: &Model, temperature: f64, quality: f64) -> Card {
        let legal: Vec<Card> = view.legal().collect();
        let (run, count) = runs_of(game, &legal);
        let all: Vec<f64> = legal.iter().map(|&card| model.score(view, card)).collect();
        let (scores, shares) = run_scores(&run, &all, count, temperature);
        let chances = run_chances(&scores, temperature);
        // Each run's lowest card, to put runs of equal chances in order.
        let lowest: Vec<Card> = (0..count).map(|r| legal[run.iter().position(|&x| x == r).unwrap()]).collect();
        let mut order: Vec<usize> = (0..count).collect();
        order.sort_by(|&a, &b| chances[b].total_cmp(&chances[a]).then(lowest[a].cmp(&lowest[b])));
        let mut before = 0.0;
        let mut pick = order[order.len() - 1];
        for &r in &order {
            if quality < before + chances[r] || r == pick {
                pick = r;
                break;
            }
            before += chances[r];
        }
        // Within the run, the card at the rest of the draw in the sum of
        // their shares.
        let rest = ((quality - before) / chances[pick]).clamp(0.0, 1.0);
        let members: Vec<usize> = (0..legal.len()).filter(|&i| run[i] == pick).collect();
        let mut within = 0.0;
        for &i in &members {
            within += shares[i];
            if rest < within {
                return legal[i];
            }
        }
        legal[members[members.len() - 1]]
    }

    /// Plays `card` in `game` and the rest of the game after it, as
    /// [`playout`] does, but with every later card, by either side, played by
    /// the model of its seat at `temperature` (see [`card_at_quality`]), the
    /// draw for each trick and seat taken from `qualities`, so that the
    /// playouts after the different cards of one choice, on one deal, meet
    /// the same luck.  The players of any card draw from `seed`.
    #[expect(clippy::too_many_arguments, reason = "a playout's settings, all at hand where it is called")]
    fn playout_at(
        game: &GameState<()>,
        card: Card,
        convention: &Convention,
        learners: &[Model],
        players: [Player; 4],
        temperature: f64,
        qualities: &[[f64; 4]; 13],
        seed: u64,
    ) -> [usize; 2] {
        let mut game = crate::ai::playout::copy_of(game);
        let mut rng = StdRng::seed_from_u64(seed);
        game.play(card);
        while !game.hand_done {
            let seat = game.lead.unwrap() + current_trick(&game).len();
            let model = match players[chooser(&game, seat) as usize] {
                Player::Learning(learner) => &learners[learner],
                Player::Past(model, _) => model,
                Player::Random => {
                    let legal: Vec<Card> = game.could_be_played().collect();
                    game.play(legal[rng.gen_range(0..legal.len())]);
                    continue;
                }
            };
            let Some(view) = View::new(&game, convention) else {
                break;
            };
            let quality = qualities[game.tricks.len().min(12)][seat as usize];
            game.play(card_at_quality(&game, &view, model, temperature, quality));
        }
        [game.ns_tricks, game.ew_tricks]
    }

    /// The seat that `seat` becomes when the table is reflected through the
    /// declarer of `game` and the dummy: the declarer's two opponents trade
    /// places.
    fn reflected(game: &GameState<()>, seat: Seat) -> Option<Seat> {
        let declarer = game.dummy()? + 2;
        Some(declarer + (4 - (seat as usize + 4 - declarer as usize) % 4) % 4)
    }

    /// `game` at the start of a trick with the declarer's opponents' hands
    /// trading places and the play so far turned round: each opponent keeps
    /// its own cards, has played the same cards to the same tricks, and sits
    /// on the other side of the declarer, so that each trick so far went the
    /// other way round the table, and the calls that showed each hand still
    /// show it (see [`GameState::hand_called_by`]).  Reflecting the table and
    /// reversing the direction of play leaves the game as it was, so this is
    /// `game` played clockwise to here and the other way from here on: the
    /// cards that are gone, the suits shown out of and the tricks won are the
    /// same, and each opponent's cards to come lie on the other side of the
    /// declarer's.  `None` in the middle of a trick or before the opening lead.
    fn mirrored(game: &GameState<()>) -> Option<GameState<()>> {
        if !current_trick(game).is_empty() || game.tricks.is_empty() || game.hand_done {
            return None;
        }
        let m = |seat: Seat| reflected(game, seat).unwrap();
        reflected(game, Seat::South)?;
        let mut twin = crate::ai::playout::copy_of(game);
        let mut callers = Seated::new([Seat::South; 4]);
        for seat in (0..4).map(Seat::from) {
            twin.hands[m(seat)] = game.hands[seat];
            twin.original_hands[m(seat)] = game.original_hands[seat];
            twin.shown_out[m(seat)] = game.shown_out[seat];
            callers[m(seat)] = game.hand_called_by(seat);
        }
        twin.hands_called_by = Some(callers);
        let turned = |cards: &[Card]| [cards[0], cards[3], cards[2], cards[1]];
        twin.tricks = game.tricks.iter().map(|(leader, cards)| (m(*leader), turned(cards))).collect();
        if game.played.len() == 4 {
            twin.played = turned(&game.played).to_vec();
        }
        twin.lead = game.lead.map(m);
        Some(twin)
    }

    #[test]
    fn mirrored_games_are_the_same_play_turned_round() {
        let mut rng = StdRng::seed_from_u64(5);
        let mut twins = 0;
        for game in crate::ai::opening_lead::candidates::opening_leads(23, 40) {
            let convention = game.system.convention();
            let mut game = crate::ai::playout::copy_of(&game);
            while !game.hand_done {
                if let Some(twin) = mirrored(&game) {
                    twins += 1;
                    let m = |seat: Seat| reflected(&game, seat).unwrap();
                    // Mirrored again, it is the game as it was.
                    let back = mirrored(&twin).unwrap();
                    assert_eq!(serde_json::to_string(&back.tricks).unwrap(), serde_json::to_string(&game.tricks).unwrap());
                    assert_eq!(back.hands, game.hands);
                    assert_eq!(back.lead, game.lead);
                    assert!((0..4).map(Seat::from).all(|s| back.hand_called_by(s) == s));
                    // Each card of each trick so far was played by the seat
                    // its hand now sits in, and each trick was won by the
                    // seat that leads the next.
                    let trump = super::super::ensemble::trump_of(&game);
                    for (k, ((leader, cards), (twin_leader, twin_cards))) in game.tricks.iter().zip(&twin.tricks).enumerate() {
                        for (i, &card) in cards.iter().enumerate() {
                            let j = twin_cards.iter().position(|&c| c == card).unwrap();
                            assert_eq!(*twin_leader + j, m(*leader + i));
                            assert!(twin.original_hands[*twin_leader + j].contains(card));
                        }
                        let mut winner = 0;
                        for i in 1..4 {
                            if beats(trump, twin_cards[i], twin_cards[winner]) {
                                winner = i;
                            }
                        }
                        let next = twin.tricks.get(k + 1).map(|t| t.0).or(twin.lead).unwrap();
                        assert_eq!(*twin_leader + winner, next);
                    }
                    // Each hand that cannot be seen shows the same in both,
                    // and so do the cards in sight.
                    if let (Some(view), Some(twin_view)) = (View::new(&game, &convention), View::new(&twin, &convention)) {
                        assert_eq!(view.mine, twin_view.mine);
                        assert_eq!(view.other, twin_view.other);
                        assert_eq!(view.hidden, twin_view.hidden);
                        assert_eq!(view.contract, twin_view.contract);
                        for (a, b) in view.shown.iter().zip(twin_view.shown.iter().rev()) {
                            assert_eq!((a.length, a.points), (b.length, b.points));
                        }
                    }
                    // The twin plays on to the end.
                    let mut twin = twin;
                    while !twin.hand_done {
                        let legal: Vec<Card> = twin.could_be_played().collect();
                        twin.play(legal[rng.gen_range(0..legal.len())]);
                    }
                    assert_eq!(twin.ns_tricks + twin.ew_tricks, 13);
                }
                let legal: Vec<Card> = game.could_be_played().collect();
                game.play(legal[rng.gen_range(0..legal.len())]);
            }
        }
        assert!(twins > 100, "only {twins} twins");
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
    /// that the player cannot see (see [`deals_of`]), read from the play at
    /// the temperature `reading` if that is set, and fitted to the
    /// `level` and the `apart` accuracies, in tricks (see
    /// [`learn_from_playouts`]): an infinite `level` leaves the level of the
    /// scores out, and the tricks are not put at a level.  With `by_runs`,
    /// the models choose by the runs of touching cards (see [`run_chances`]),
    /// in the games and in the playouts, where every later card is drawn at
    /// the temperature of the games (see [`playout_at`]).
    #[derive(Clone, Copy)]
    struct Playouts {
        share: f64,
        most: usize,
        per_game: usize,
        deals: usize,
        reading: Option<f64>,
        level: f64,
        apart: f64,
        by_runs: bool,
        /// Whether each choice played out on the true deal, when the
        /// declarer's side is on lead, is also played out in the game
        /// [`mirrored`], as a choice of its own.
        mirror: bool,
    }

    /// About `count` deals of the hands that the player to play in `game`
    /// cannot see, each as the game would be with them and with how much it
    /// counts for, or none if there is no one to play; or, with `count` 0,
    /// the game as it was dealt.  With `reading`, a play model and a
    /// temperature, they are the deals that the bidding system and the play
    /// so far make likely, the play read by that model as though it had
    /// played every card (see [`Hidden::read_the_play`], with what the
    /// playout yardstick uses: ten proposals a deal, walked when they are
    /// worth under half the deals, and a chance of [`MISS`] for each call
    /// that the robot would not have made); without, deals that fit only the
    /// sizes of the hands and the suits that they have shown out of (see
    /// [`Hidden::ensemble`]).
    fn deals_of(
        game: &GameState<()>,
        convention: &Convention,
        count: usize,
        reading: Option<(&Model, f64)>,
        rng: &mut StdRng,
    ) -> Vec<(GameState<()>, f64)> {
        if count == 0 {
            return vec![(crate::ai::playout::copy_of(game), 1.0)];
        }
        let Some(hidden) = Hidden::new(game, convention) else {
            return Vec::new();
        };
        let dealt: Vec<Weighted> = match reading {
            Some((model, temperature)) => {
                let reading = Reading {
                    model,
                    temperature,
                    miss: Some(MISS),
                    proposals: 10 * count,
                    at_most: 10 * count,
                    worth: (count / 2) as f64,
                    steps: 0,
                };
                hidden.read_the_play(rng, game, convention, &reading, count)
            }
            None => hidden.ensemble(rng, count, None).into_iter().map(|deal| (deal, 1.0)).collect(),
        };
        dealt
            .iter()
            .map(|(deal, weight)| (crate::ai::playout::dealt_as(game, hidden.seats(), deal), *weight))
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
    /// Learning the free energy, `short` is the sum of how far each choice's
    /// free energy at the temperature of the games falls short of the best
    /// card's tricks.
    #[derive(Clone, Copy, Default)]
    struct Tally {
        lost: f64,
        short: f64,
        choices: usize,
        off: f64,
        off_apart: f64,
        chi_squared: f64,
        across: f64,
        squares: f64,
        weights: f64,
        scores: f64,
        tricks: f64,
        /// What the likeliest cards lose, and the number of choices, of the
        /// choices also played out mirrored and of their mirrored twins.
        paired_lost: [f64; 2],
        paired: [usize; 2],
    }

    impl Tally {
        fn add(&mut self, other: &Tally) {
            self.lost += other.lost;
            self.short += other.short;
            self.choices += other.choices;
            self.off += other.off;
            self.off_apart += other.off_apart;
            self.chi_squared += other.chi_squared;
            self.across += other.across;
            self.squares += other.squares;
            self.weights += other.weights;
            self.scores += other.scores;
            self.tricks += other.tricks;
            for k in 0..2 {
                self.paired_lost[k] += other.paired_lost[k];
                self.paired[k] += other.paired[k];
            }
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
        /// Up the slope of the free energy of each choice played out, at a
        /// temperature that is lowered as it learns.
        FreeEnergy,
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
                let (run, count) = if playouts.by_runs { runs_of(&game, &legal) } else { (Vec::new(), 0) };
                let cards: Vec<(Entries, f64)> = if playouts.by_runs {
                    // Each run's chance, shared among its cards.
                    let entries: Vec<Entries> = legal.iter().map(|&card| weights.entries(&view, card)).collect();
                    let all: Vec<f64> = entries.iter().map(|e| weights.network.score_of(e)).collect();
                    let (scores, shares) = run_scores(&run, &all, count, temperature);
                    let chances = run_chances(&scores, temperature);
                    entries.into_iter().zip(&run).zip(&shares).map(|((e, &r), share)| (e, chances[r] * share)).collect()
                } else {
                    chances(&view, weights, temperature)
                        .into_iter()
                        .map(|(card, chance)| (weights.entries(&view, card), chance))
                        .collect()
                };
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
                        run,
                        runs: count,
                        mirror: 0,
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
        let mut twins = Vec::new();
        if !later.is_empty() && !any_card {
            let chosen = playouts.per_game.min(later.len());
            let weight = later.len() as f64 / chosen as f64;
            for i in rand::seq::index::sample(rng, later.len(), chosen) {
                let (number, game, runs, legal, side): &(usize, GameState<()>, Vec<Card>, Vec<Card>, usize) = &later[i];
                // Each deal is played out after each card, so that the cards
                // are compared on the same deals.
                let reading = playouts.reading.map(|t| (&learners[decisions[*number].learner], t));
                // With each deal, the draws of the later plays of each trick
                // and seat, shared by the playouts after every card.
                type Dealt = (GameState<()>, f64, u64, [[f64; 4]; 13]);
                let deals: Vec<Dealt> = deals_of(game, convention, playouts.deals, reading, rng)
                    .into_iter()
                    .map(|(deal, weight)| {
                        let seed = rng.gen::<u64>();
                        // Drawn only when used, so that the other ways of
                        // learning draw as they did.
                        let qualities = if playouts.by_runs {
                            [(); 13].map(|()| [(); 4].map(|()| rng.gen_range(0.0..1.0)))
                        } else {
                            [[0.0; 4]; 13]
                        };
                        (deal, weight, seed, qualities)
                    })
                    .collect();
                let counted: f64 = deals.iter().map(|d| d.1).sum();
                if deals.is_empty() || counted <= 0.0 {
                    continue;
                }
                let mean = |card: Card, players: [Player; 4]| -> f64 {
                    deals
                        .iter()
                        .map(|(deal, w, seed, qualities)| {
                            let won = if playouts.by_runs {
                                playout_at(deal, card, convention, learners, players, temperature, qualities, *seed)
                            } else {
                                playout(deal, card, convention, learners, players, *seed)
                            };
                            w * won[*side] as f64
                        })
                        .sum::<f64>()
                        / counted
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
                // The same choice with the opponents' cards to come on the
                // other side of the declarer's, played out on its own draws
                // turned round with the table, as a choice of its own.
                let declarers = game.lead.is_some_and(|lead| chooser(game, lead) as usize % 2 == *side)
                    && game.dummy().is_some_and(|dummy| dummy as usize % 2 == *side);
                if playouts.mirror && playouts.by_runs && playouts.deals == 0 && declarers {
                    if let Some(twin) = mirrored(game) {
                        let learner = decision.learner;
                        let model = &learners[learner];
                        let m = |seat: Seat| reflected(game, seat).unwrap();
                        let (_, _, seed, qualities) = &deals[0];
                        let mut turned_players = players;
                        let mut turned_qualities = *qualities;
                        for seat in (0..4).map(Seat::from) {
                            turned_players[m(seat) as usize] = players[seat as usize];
                            for t in 0..13 {
                                turned_qualities[t][m(seat) as usize] = qualities[t][seat as usize];
                            }
                        }
                        if let Some(view) = View::new(&twin, convention) {
                            let legal: Vec<Card> = view.legal().collect();
                            let (run, count) = runs_of(&twin, &legal);
                            let entries: Vec<Entries> = legal.iter().map(|&card| model.entries(&view, card)).collect();
                            let all: Vec<f64> = entries.iter().map(|e| model.network.score_of(e)).collect();
                            let (scores, shares) = run_scores(&run, &all, count, temperature);
                            let chances = run_chances(&scores, temperature);
                            let cards: Vec<(Entries, f64)> =
                                entries.into_iter().zip(&run).zip(&shares).map(|((e, &r), share)| (e, chances[r] * share)).collect();
                            let twin_runs: Vec<Card> = crate::ai::playout::one_of_each_run(&twin).collect();
                            let values: Vec<f64> = twin_runs
                                .iter()
                                .map(|&card| {
                                    let won = playout_at(&twin, card, convention, learners, turned_players, temperature, &turned_qualities, *seed);
                                    won[*side] as f64
                                })
                                .collect();
                            twins.push(Decision {
                                learner,
                                cards,
                                chosen: 0,
                                side: *side,
                                trick: twin.tricks.len(),
                                won_before: if *side == 0 { twin.ns_tricks } else { twin.ew_tricks },
                                played_out: Some(legal.iter().map(|&card| values[crate::ai::playout::run_of(&twin_runs, card)]).collect()),
                                weight,
                                run,
                                runs: count,
                                mirror: 2,
                            });
                            decision.mirror = 1;
                        }
                    }
                }
            }
        }
        decisions.extend(twins);
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
    /// games keep calling for them.  With `PLAY_SELF_COOLING` (0) set to a
    /// number of rounds t0, the rate cools as rate·√(t0/(t0 + n)), n rounds
    /// after round `PLAY_SELF_COOLING_START` (the first of the run), so that a
    /// run started again there warms up again.  That is Adagrad's cooling:
    /// Adagrad divides by the root of the sum of the squared slopes, which
    /// grows as n times their mean square, where Adam divides by the root of
    /// their mean square.  Adam's momentum does not change the drift or the
    /// noise over more than ten rounds or so, as its weights sum to one.
    /// The shapes that read the players' beliefs read them as much the truth
    /// as `PLAY_SELF_ORACLE` (0) says (see [`ORACLE`]); with
    /// `PLAY_SELF_ORACLE_ROUNDS` (0) set to n, that fades to 0 in a straight
    /// line over the n rounds from `PLAY_SELF_ORACLE_START` (the first of the
    /// run), so that a player that learned with the true deal in sight learns
    /// to do without it.
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
    /// `PLAY_THREADS` threads, or else every core, and its random numbers,
    /// and with it the fresh deals, are seeded by `PLAY_SELF_SEED` (10 more
    /// than its first round, and the deals by the first round alone).
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
    /// player cannot see, as the playout yardstick values cards (see
    /// [`score_on_the_playout_yardstick`]), or with 0 on the deal as it was
    /// dealt, and the tricks after each card are their mean over the deals.
    /// With `PLAY_SELF_DEALER` `play` (the default), the deals are those that
    /// the bidding system and the play so far make likely, the play read by
    /// the learner's own model at `PLAY_SELF_DEALER_TEMPERATURE` (1), as
    /// though it had played every card; with `ensemble`, any that fit the
    /// sizes of the hands and the suits that they have shown out of (see
    /// [`deals_of`]).  Each learner's step is then down the slope
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

    /// Self-play (see [`self_play`], and its settings) in which the learners
    /// climb the free energy of their choices, as `robot/PLAYER_TRAINING.md`
    /// plans.  A model's scores are tricks, and it chooses among the runs of
    /// touching cards with chances e<sup>s/T</sup> over their sum, each card
    /// with its share (see [`run_scores`]): a run's score is T log of the mean
    /// of e<sup>s/T</sup> over its cards, which cold is its best card's, so
    /// that cold it plays as [`model_card`] does, and the slope of a run is
    /// shared among its cards, so that all are learned.  Every card of the
    /// games, by every player, is chosen at `PLAY_SELF_TEMPERATURE` (0.3
    /// tricks), T<sub>opp</sub>.  In each game with no player of any card,
    /// `PLAY_SELF_PLAYOUTS` (1) of the learners' choices of more than one
    /// run, with up to `PLAY_SELF_PLAYOUT_TRICKS` (13) tricks left, are
    /// chosen at random once the game is over, each weighted by the number
    /// of such choices over the number chosen, and played out after each run
    /// on the same `PLAY_SELF_PLAYOUT_DEALS` (0: the deal as it was dealt)
    /// deals of the hands that the player cannot see, dealt by
    /// `PLAY_SELF_DEALER` as [`learn_from_playouts`] says, the play read at
    /// `PLAY_SELF_DEALER_TEMPERATURE` (T<sub>opp</sub>).  Every later card of
    /// a playout, by either side, is chosen at T<sub>opp</sub> with draws
    /// shared by the playouts after every run (see [`playout_at`]), and the
    /// worth v of each run is the mean of its side's tricks.
    ///
    /// Each learner's step is up the slope of the free energy Σ p v + T H(p)
    /// of each choice played out, over its runs, with p the chances at T:
    /// the score of run i rises by p<sub>i</sub>(v<sub>i</sub> − Σ p v −
    /// T(log p<sub>i</sub> − Σ p log p))/T.  That is linear in v, so the luck
    /// of the playouts averages out, and it is zero when the scores are the
    /// worth of the runs, up to a constant for the choice, at every T.  T is
    /// annealed from `PLAY_SELF_ANNEAL_FROM` (1 trick) down to
    /// T<sub>opp</sub>, geometrically over `PLAY_SELF_ANNEAL_ROUNDS` (1000)
    /// rounds counted from round `PLAY_SELF_ANNEAL_START` (the first of this
    /// run; give it again to a run that resumes), and is T<sub>opp</sub>
    /// after; with 0 rounds it is T<sub>opp</sub> from the start, which suits
    /// a start that is already a trained, cold model, whose order a hot
    /// slope would melt.  Neither `PLAY_SELF_PLAYOUT_SHARE` nor the tricks after each
    /// choice are used, and there is no level and no entropy target.
    ///
    /// A learner that starts from a model of self-play, whose scores are not
    /// tricks, first has them put in tricks as `PLAY_SELF_UNITS` says: with
    /// `fitted` (the default), multiplied by the factor that makes the free
    /// energy of the first round's choices played out highest, by this
    /// objective itself, at `PLAY_SELF_ANNEAL_FROM` whether or not the run
    /// anneals (at a cold T<sub>opp</sub> the free energy is nearly flat in
    /// the scale, so a fit there lands anywhere), in place of that round's
    /// step;
    /// with `cautious`, by T<sub>opp</sub>, which keeps its play in the games
    /// as it was at temperature 1; with `honest`, by the factor that brings
    /// the differences of its scores nearest those of the tricks of the
    /// first round's playouts by least squares, in place of that round's
    /// step.  A scale is one number, which Adam's steps, each about the same
    /// size whatever the size of its weight, cannot change uniformly.  After
    /// it, each step is taken in the units that the weights had before (the
    /// weights that [`Network::scale_scores`] multiplies are divided by the
    /// scale for the step and multiplied again after), so that the rate means
    /// what it did for the model that the learner started from; the scales
    /// are kept beside the checkpoint, as `.scales`, for a run that resumes.  The
    /// report gives the temperature, how many tricks a play the likeliest
    /// cards lose to the best played out, and how far the free energy at
    /// T<sub>opp</sub> falls short of the best card's tricks.
    #[test]
    #[ignore]
    fn learn_the_free_energy() {
        run_self_play(Learning::FreeEnergy);
    }

    /// Self-play, as [`self_play`] and [`learn_from_playouts`] say, the
    /// learners learning as `learning` says.
    fn run_self_play(learning: Learning) {
        let float = |name: &str, default: f64| -> f64 { var(name, &default.to_string()).parse().unwrap() };
        let rounds = number("PLAY_SELF_ROUNDS", 100);
        let batch = number("PLAY_SELF_DEALS", 256);
        let group = number("PLAY_SELF_GROUP", 8);
        let report = number("PLAY_SELF_REPORT", 10);
        let temperature = float(
            "PLAY_SELF_TEMPERATURE",
            match learning {
                Learning::Outcomes => 1.0,
                Learning::Playouts => 0.1,
                Learning::FreeEnergy => 0.3,
            },
        );
        let rate = float("PLAY_SELF_RATE", 0.01);
        // The players' beliefs about where the hidden cards lie, for shapes
        // that read them: the prior's weights and then the evidence's, one a
        // line, as `learn_card_locations` writes them.  They do not learn.
        if let Ok(path) = std::env::var("PLAY_SELF_LOCATIONS") {
            let _ = LOCATIONS.set(read_locations(&path));
            println!("the players' beliefs come from {path}");
        }
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
                reading: None,
                level: f64::INFINITY,
                apart: f64::INFINITY,
                by_runs: false,
                mirror: false,
            },
            Learning::FreeEnergy => Playouts {
                share: 0.0,
                most: number("PLAY_SELF_PLAYOUT_TRICKS", 13),
                per_game: number("PLAY_SELF_PLAYOUTS", 1),
                deals: number("PLAY_SELF_PLAYOUT_DEALS", 0),
                reading: match var("PLAY_SELF_DEALER", "play").as_str() {
                    "play" => Some(float("PLAY_SELF_DEALER_TEMPERATURE", temperature)),
                    "ensemble" => None,
                    other => panic!("PLAY_SELF_DEALER is play or ensemble, not {other}"),
                },
                level: f64::INFINITY,
                apart: f64::INFINITY,
                by_runs: true,
                mirror: number("PLAY_SELF_MIRROR", 0) == 1,
            },
            Learning::Playouts => Playouts {
                share: 0.0,
                most: number("PLAY_SELF_PLAYOUT_TRICKS", 13),
                per_game: number("PLAY_SELF_PLAYOUTS", 1),
                deals: number("PLAY_SELF_PLAYOUT_DEALS", 100),
                reading: match var("PLAY_SELF_DEALER", "play").as_str() {
                    "play" => Some(float("PLAY_SELF_DEALER_TEMPERATURE", 1.0)),
                    "ensemble" => None,
                    other => panic!("PLAY_SELF_DEALER is play or ensemble, not {other}"),
                },
                level: float("PLAY_SELF_LEVEL_ACCURACY", 0.5),
                apart: float("PLAY_SELF_CHOICE_ACCURACY", 0.05),
                by_runs: false,
                mirror: false,
            },
        };
        let anneal_from = float("PLAY_SELF_ANNEAL_FROM", 1.0);
        let anneal_rounds = number("PLAY_SELF_ANNEAL_ROUNDS", 1000);
        #[derive(PartialEq)]
        enum Units {
            Cautious,
            Honest,
            Fitted,
        }
        let units = match var("PLAY_SELF_UNITS", "fitted").as_str() {
            "cautious" => Units::Cautious,
            "honest" => Units::Honest,
            "fitted" => Units::Fitted,
            other => panic!("PLAY_SELF_UNITS is fitted, cautious or honest, not {other}"),
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
        // How much each learner's scores have been multiplied by since it
        // started from a model whose scores were not tricks, to take its
        // steps in the units that its weights had then (see
        // `Network::scale_scores`): Adam moves each weight by about the rate
        // whatever its size, so after the scores are multiplied by 0.01, a
        // step in the weights that were multiplied would be a hundred times
        // as large beside them.  A run that resumes reads them from beside
        // its file, as `.scales`.
        let mut scales = vec![1.0; learners.len()];
        if let Ok(path) = std::env::var("PLAY_SELF_RESUME") {
            if let Ok(text) = std::fs::read_to_string(format!("{path}.scales")) {
                for (scale, word) in scales.iter_mut().zip(text.split_whitespace()) {
                    *scale = word.parse().expect("a learner's scale");
                }
                println!("the learners' scores have been multiplied by {scales:?}");
            }
        }
        // Learning the free energy, the cautious start puts the scores in
        // tricks so that the learners play at the temperature of the games
        // as they did at 1.
        if learning == Learning::FreeEnergy && !resumed && units == Units::Cautious {
            for (learner, scale) in learners.iter_mut().zip(&mut scales) {
                learner.network.scale_scores(temperature);
                *scale *= temperature;
            }
            println!("the learners' scores are multiplied by {temperature}, to play as they did at temperature 1");
        }
        let anneal_start = number("PLAY_SELF_ANNEAL_START", first_round);
        // With `PLAY_SELF_NOISE` 1, the sums of each learner's slopes and of
        // their squares, weight by weight, and the rounds summed, for how
        // much of each round's slope is noise (with `PLAY_SELF_RATE` 0 the
        // learners stay as they are, and every round's slope is a draw of
        // the same thing).
        let noise_on = number("PLAY_SELF_NOISE", 0) == 1;
        let mut noise: Vec<(Vec<f64>, Vec<f64>, usize)> = Vec::new();
        let cooling = number("PLAY_SELF_COOLING", 0) as f64;
        let cooling_start = number("PLAY_SELF_COOLING_START", first_round);
        let cooled = |round: usize| {
            if cooling > 0.0 {
                (cooling / (cooling + round.saturating_sub(cooling_start) as f64)).sqrt()
            } else {
                1.0
            }
        };
        if cooling > 0.0 {
            println!(
                "the rate cools from {} at round {cooling_start} as the root of {cooling}/({cooling} + rounds since), {} at round {first_round}",
                rate,
                rate * cooled(first_round)
            );
        }
        let oracle_from = float("PLAY_SELF_ORACLE", 0.0);
        let oracle_rounds = number("PLAY_SELF_ORACLE_ROUNDS", 0);
        let oracle_start = number("PLAY_SELF_ORACLE_START", first_round);
        let oracle_at = |round: usize| {
            if oracle_rounds == 0 {
                oracle_from
            } else {
                oracle_from * (1.0 - round.saturating_sub(oracle_start) as f64 / oracle_rounds as f64).max(0.0)
            }
        };
        if oracle_from > 0.0 {
            println!(
                "the beliefs are {oracle_from} the truth{}, {} at round {first_round}",
                if oracle_rounds == 0 { String::new() } else { format!(", fading to none over {oracle_rounds} rounds from {oracle_start}") },
                oracle_at(first_round)
            );
        }
        let mut playout_losses = vec![Tally::default(); learners.len()];
        if snapshots.is_empty() {
            snapshots = learners.iter().map(|l| (first_round - 1, l.clone())).collect();
        }
        let last_round = first_round + rounds - 1;
        let mut best = 0;
        let mut watched: Vec<serde_json::Value> = Vec::new();
        let samples = number("PLAY_SELF_SAMPLES", 2);
        // `PLAY_SELF_SEED` gives a run other random numbers, for a replicate.
        let mut rng = StdRng::seed_from_u64(number("PLAY_SELF_SEED", 10 + first_round) as u64);
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
            // The deals too follow `PLAY_SELF_SEED`, so that a replicate meets
            // other deals; without it, as they always were.
            let dealing = std::env::var("PLAY_SELF_SEED").ok().map_or(0, |seed| {
                seed.parse::<u64>().expect("PLAY_SELF_SEED").wrapping_mul(1_000_003)
            });
            let mut dealer = (Generator::new(), StdRng::seed_from_u64(1000 + dealing + first_round as u64), 10);
            std::thread::spawn(move || while dealt.send(fresh_deal(&mut dealer)).is_ok() {});
        }
        // The same threads play every round.
        let workers = rayon::ThreadPoolBuilder::new().num_threads(threads).build().unwrap();
        let started = std::time::Instant::now();
        for round in first_round..=last_round {
            set_oracle(oracle_at(round));
            // The temperature of the slope, annealed geometrically down to
            // that of the games.
            let annealed = if learning == Learning::FreeEnergy {
                let done = if anneal_rounds == 0 {
                    1.0
                } else {
                    (round.saturating_sub(anneal_start) as f64 / anneal_rounds as f64).min(1.0)
                };
                anneal_from * (temperature / anneal_from).powf(done)
            } else {
                temperature
            };
            let deals: Vec<usize> = if pool.len() == batch {
                (0..batch).collect()
            } else {
                (0..batch).map(|_| rng.gen_range(0..pool.len())).collect()
            };
            let seed = rng.gen::<u64>();
            let chunk = batch.div_ceil(threads);
            let (learners_now, pool_now, past, bonus_now) = (&learners, &pool, &snapshots, bonus);
            let watching = round % report == 0 || round == last_round;
            type Results = (Vec<Vec<f64>>, Vec<usize>, f64, Vec<serde_json::Value>, Vec<Tally>, Vec<Vec<Gathered>>);
            let gathering = learning == Learning::FreeEnergy
                && ((units == Units::Fitted && round == first_round && !resumed) || watching);
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
                            let mut gathered: Vec<Vec<Gathered>> = vec![Vec::new(); learners_now.len()];
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
                                        if learning == Learning::FreeEnergy {
                                            if let Some(values) = &d.played_out {
                                                // The slope of the free energy
                                                // at the annealed temperature,
                                                // over the runs, each run's
                                                // slope shared among its cards.
                                                let all: Vec<f64> = d.cards.iter().map(|(e, _)| model.network.score_of(e)).collect();
                                                let mut worth = vec![0.0; d.runs];
                                                for (&r, &v) in d.run.iter().zip(values) {
                                                    worth[r] = v;
                                                }
                                                let (scores, shares) = run_scores(&d.run, &all, d.runs, annealed);
                                                let slope = free_energy_slope(&scores, &worth, annealed);
                                                for (((entries, _), &r), share) in d.cards.iter().zip(&d.run).zip(&shares) {
                                                    model.network.add_gradient_of(entries, d.weight * slope[r] * share, &mut gradients[d.learner]);
                                                }
                                                // How far the free energy at
                                                // the games' temperature falls
                                                // short of the best run.
                                                let best = worth.iter().copied().fold(f64::MIN, f64::max);
                                                let free = free_energy_of(&d.run, &all, d.runs, &worth, temperature);
                                                let (scores, _) = run_scores(&d.run, &all, d.runs, temperature);
                                                let n = scores.len() as f64;
                                                let (score_mean, worth_mean) = (scores.iter().sum::<f64>() / n, worth.iter().sum::<f64>() / n);
                                                let tally = &mut lost[d.learner];
                                                tally.short += best - free;
                                                for (s, v) in scores.iter().zip(&worth) {
                                                    tally.across += d.weight * (s - score_mean) * (v - worth_mean);
                                                    tally.squares += d.weight * (s - score_mean) * (s - score_mean);
                                                }
                                                if gathering && d.mirror != 2 {
                                                    gathered[d.learner].push((all, d.run.clone(), d.runs, worth, d.weight));
                                                }
                                            }
                                        } else if learning == Learning::Playouts {
                                            if let Some(values) = &d.played_out {
                                                // Fits the scores to the
                                                // tricks, and their
                                                // differences to those of the
                                                // tricks, by least squares,
                                                // each square divided by
                                                // that of its accuracy.
                                                let scores: Vec<f64> =
                                                    d.cards.iter().map(|(entries, _)| model.network.score_of(entries)).collect();
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
                                                    model.network.add_gradient_of(entries, -w * off, &mut gradients[d.learner]);
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
                                                learners_now[d.learner].network.add_gradient_of(entries, g, &mut gradients[d.learner]);
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
                                                        learners_now[d.learner].network.add_gradient_of(entries, g, &mut gradients[d.learner]);
                                                    }
                                                }
                                            }
                                        }
                                        if let Some(values) = &d.played_out {
                                            // Chosen by runs, the likeliest
                                            // run.
                                            let chance = |a: usize| -> f64 {
                                                if d.run.is_empty() {
                                                    d.cards[a].1
                                                } else {
                                                    d.run.iter().zip(&d.cards).filter(|(&r, _)| r == d.run[a]).map(|(_, c)| c.1).sum()
                                                }
                                            };
                                            let likeliest =
                                                (0..values.len()).max_by(|&a, &b| chance(a).total_cmp(&chance(b))).unwrap();
                                            lost[d.learner].lost += values.iter().copied().fold(f64::MIN, f64::max) - values[likeliest];
                                            lost[d.learner].choices += 1;
                                            if d.mirror > 0 {
                                                let tally = &mut lost[d.learner];
                                                tally.paired_lost[d.mirror as usize - 1] +=
                                                    values.iter().copied().fold(f64::MIN, f64::max) - values[likeliest];
                                                tally.paired[d.mirror as usize - 1] += 1;
                                            }
                                        }
                                        counts[d.learner] += 1;
                                    }
                                }
                            }
                            (gradients, counts, entropy, sampled, lost, gathered)
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
            let calibrating = (learning == Learning::Playouts || (learning == Learning::FreeEnergy && units != Units::Cautious))
                && round == first_round
                && !resumed;
            if calibrating && units == Units::Fitted && learning == Learning::FreeEnergy {
                // The scale of each learner's scores that makes the free
                // energy at the games' temperature of this round's choices
                // played out highest, found by golden section on its log.
                for (i, learner) in learners.iter_mut().enumerate() {
                    let choices: Vec<&Gathered> = results.iter().flat_map(|r| &r.5[i]).collect();
                    // At the temperature that the anneal starts from (1 trick
                    // by default), whether or not it anneals, where the free
                    // energy is curved in the scale: at a cold T_opp any
                    // scale that makes the play nearly greedy does as well.
                    // The scores' fixed point is the worth at every T, so the
                    // scale found at one T serves at all.
                    let scale = best_scale(&choices, anneal_from);
                    learner.network.scale_scores(scale);
                    scales[i] *= scale;
                    println!(
                        "learner {}'s scores are multiplied by {scale:.4}, which makes the free energy at {anneal_from} of {} choices highest",
                        i + 1,
                        choices.len()
                    );
                }
            }
            // As a reading of how well the scores are calibrated in tricks,
            // the scale that would make the free energy highest at three
            // temperatures, which is 1 at each for scores that are the worth.
            let calibration: Option<Vec<[f64; 3]>> = (learning == Learning::FreeEnergy && watching && !calibrating).then(|| {
                (0..learners.len())
                    .map(|i| {
                        let choices: Vec<&Gathered> = results.iter().flat_map(|r| &r.5[i]).collect();
                        [1.0, 0.3, 0.1].map(|t| best_scale(&choices, t))
                    })
                    .collect()
            });
            if calibrating && !(units == Units::Fitted && learning == Learning::FreeEnergy) {
                for (i, learner) in learners.iter_mut().enumerate() {
                    let mut tally = Tally::default();
                    for result in &results {
                        tally.add(&result.4[i]);
                    }
                    let (factor, shift) = tally.fit();
                    if factor.is_finite() && factor > 0.0 {
                        learner.network.scale_scores(factor);
                        scales[i] *= factor;
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
                // The step in the units of the weights before the scores were
                // multiplied, which Adam does not see in the slope, as its
                // steps do not depend on the slope's size.
                let scale = scales[i];
                if scale != 1.0 {
                    learner.network.scale_scores(1.0 / scale);
                }
                if noise_on {
                    if noise.len() <= i {
                        noise.push((vec![0.0; gradient.len()], vec![0.0; gradient.len()], 0));
                    }
                    let (sums, squares, rounds) = &mut noise[i];
                    for ((sum, square), g) in sums.iter_mut().zip(squares.iter_mut()).zip(&gradient) {
                        *sum += g;
                        *square += g * g;
                    }
                    *rounds += 1;
                }
                let rate = rate * cooled(round);
                for p in &mut learner.network.params {
                    *p *= 1.0 - rate * ramp * decay;
                }
                adams[i].step(&mut learner.network.params, &gradient, rate * ramp);
                if scale != 1.0 {
                    learner.network.scale_scores(scale);
                }
            }
            if noise_on && watching && noise.first().is_some_and(|n| n.2 > 1) {
                // For each learner, the mean slope's square, less what its
                // noise adds to it, against the variance of one round's
                // slope: summed over the weights, and over the weights each
                // in units of its own noise.
                let each: Vec<String> = noise
                    .iter()
                    .map(|(sums, squares, rounds)| {
                        let r = *rounds as f64;
                        let (mut signal, mut spread, mut whitened) = (0.0, 0.0, 0.0);
                        for (sum, square) in sums.iter().zip(squares) {
                            let mean = sum / r;
                            let variance = (square / r - mean * mean) * r / (r - 1.0);
                            let squared = mean * mean - variance / r;
                            signal += squared;
                            spread += variance;
                            if variance > 0.0 {
                                whitened += squared / variance;
                            }
                        }
                        format!("{signal:.3e}/{spread:.3e} = {:.4}, whitened {whitened:.2}", signal / spread)
                    })
                    .collect();
                println!("round {round}: over {} rounds, each learner's slope squared against the variance of a round's: {}", noise[0].2, each.join("; "));
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
                    if playouts.mirror {
                        let paired: Vec<String> = playout_losses
                            .iter()
                            .map(|t| {
                                let [a, b] = [0, 1].map(|k| t.paired_lost[k] / t.paired[k].max(1) as f64);
                                format!("{a:.4} and {b:.4} ({})", t.paired[1])
                            })
                            .collect();
                        print!(", of those also played out mirrored and of their twins {}", paired.join(", "));
                    }
                    if learning == Learning::FreeEnergy {
                        let short: Vec<String> = playout_losses
                            .iter()
                            .map(|t| format!("{:.4}", t.short / t.choices.max(1) as f64))
                            .collect();
                        print!(
                            ", the free energy at {temperature} falls short of the best by {} tricks a play, at the temperature {annealed:.4}",
                            short.join(", ")
                        );
                        // How consistent each learner's steps are: the mean
                        // over its weights that have moved of Adam's |m|/√v,
                        // near 1 when each step goes the same way and far
                        // below it when the steps are mostly noise.
                        let consistency: Vec<String> = adams
                            .iter()
                            .map(|adam| {
                                let (first, second, steps) = adam.parts();
                                // With Adam's corrections for its start.
                                let t = steps.max(1) as f64;
                                let corrected = (1.0 - 0.999f64.powf(t)).sqrt() / (1.0 - 0.9f64.powf(t));
                                let moved: Vec<f64> = first
                                    .iter()
                                    .zip(second)
                                    .filter(|(_, &v)| v > 0.0)
                                    .map(|(m, v)| corrected * m.abs() / v.sqrt())
                                    .collect();
                                format!("{:.3}", moved.iter().sum::<f64>() / moved.len().max(1) as f64)
                            })
                            .collect();
                        print!(", the steps' |m|/√v is {}", consistency.join(", "));
                        if let Some(calibration) = &calibration {
                            let scales: Vec<String> = calibration
                                .iter()
                                .map(|c| format!("{:.2}/{:.2}/{:.2}", c[0], c[1], c[2]))
                                .collect();
                            print!(", and this round's free energy is highest with the scores times {} at 1/0.3/0.1", scales.join(", "));
                        }
                    }
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
                    let words: Vec<String> = scales.iter().map(|s| s.to_string()).collect();
                    std::fs::write(format!("{path}.scales"), words.join(" ") + "\n").unwrap();
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
            let network = Network::read(path, shape.entries(), shape.layers);
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
            Model { shape, network: Network::read(path, shape.entries(), shape.layers) }
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
        let dealer = var("PLAY_YARD_DEALER", "play");
        let temperature: f64 = var("PLAY_YARD_TEMPERATURE", "1.0").parse().unwrap();
        let proposals = number("PLAY_YARD_PROPOSALS", count * 10);
        let steps = number("PLAY_YARD_STEPS", 0);
        let miss = chance_of_a_miss(&var("PLAY_YARD_MISS", "0.1"));
        let at_most = number("PLAY_YARD_AT_MOST", proposals);
        let worth: f64 = var("PLAY_YARD_WORTH", &(count / 2).to_string()).parse().unwrap();
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
                                let dealt: Vec<Weighted> = match dealer.as_str() {
                                    "ensemble" => hidden.ensemble(&mut rng, count, None).into_iter().map(|d| (d, 1.0)).collect(),
                                    "bounds" => hidden.within_bounds(&mut rng, count).deals.into_iter().map(|d| (d, 1.0)).collect(),
                                    "play" => hidden.read_the_play(&mut rng, &game, convention, &Reading { model: &competitor, temperature, miss, proposals, at_most, worth, steps }, count),
                                    other => panic!("PLAY_YARD_DEALER is ensemble, bounds or play, not {other}"),
                                };
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
                        crate::ai::playout::weighted_card_values(game, runs, dealt, both, convention).unwrap()
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
    type Position = (GameState<()>, Vec<Card>, Vec<Weighted>);

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
                for entry in model.entries(&view, card).one {
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

    /// How much it costs to make the ensembles of the playout yardstick by
    /// the plays of the competitor (see [`Hidden::read_the_play`]), and how
    /// much closer to the hands that were dealt they come than deals that
    /// are only within the bounds that the calls showed.  The positions are
    /// those of [`score_on_the_playout_yardstick`] (`PLAY_YARD_COMPETITOR`,
    /// `PLAY_YARD_GAMES` (200), `PLAY_YARD_SHARE` (0.25), `PLAY_YARD_DEALS`
    /// (100)), each with `PLAY_YARD_PROPOSALS` (ten times the deals) deals
    /// proposed.  For each of the temperatures `PLAY_YARD_TEMPERATURES`
    /// (comma-separated) it prints the time to propose and to weigh, how
    /// many deals of weight 1 the ensemble is worth (see
    /// [`effective_size`]) and how many different deals it holds, and the
    /// mean error, over positions, of the mean of the high card points and
    /// the lengths of the suits of the hidden hands, as the proposals alone
    /// give it and as the weighted ensemble does.  One thread, so that the
    /// times mean something.
    #[test]
    #[ignore]
    fn report_the_play_weighted_dealer() {
        use crate::ai::ensemble::effective_size;
        use crate::ai::playout::{copy_of, one_of_each_run};
        use std::time::{Duration, Instant};
        let competitor = read_model(&std::env::var("PLAY_YARD_COMPETITOR").expect("PLAY_YARD_COMPETITOR"));
        let games = number("PLAY_YARD_GAMES", 200);
        let share: f64 = var("PLAY_YARD_SHARE", "0.25").parse().unwrap();
        let count = number("PLAY_YARD_DEALS", 100);
        let proposals = number("PLAY_YARD_PROPOSALS", count * 10);
        let temperatures: Vec<f64> = var("PLAY_YARD_TEMPERATURES", "0.5,1,2,4").split(',').map(|t| t.trim().parse().unwrap()).collect();
        let mut positions: Vec<GameState<()>> = Vec::new();
        for (i, game) in crate::ai::opening_lead::candidates::opening_leads(23, games).iter().enumerate() {
            let convention: &Convention = game.system.table();
            let mut rng = StdRng::seed_from_u64(i as u64);
            let mut game = copy_of(game);
            while !game.hand_done {
                if one_of_each_run(&game).len() > 1 && View::new(&game, convention).is_some() && rng.gen_range(0.0..1.0) < share {
                    positions.push(copy_of(&game));
                }
                let card = model_card(&competitor, &game, convention);
                game.play(card);
            }
        }
        // What is compared of a hand: its high card points and the lengths
        // of its suits.
        let features = |hand: Cards| -> [f64; 5] {
            let length = |suit: Suit| hand.in_suit(suit).len() as f64;
            [
                hand.high_card_points() as f64,
                length(Suit::Clubs),
                length(Suit::Diamonds),
                length(Suit::Hearts),
                length(Suit::Spades),
            ]
        };
        let mean_features = |deals: &[Weighted], i: usize| -> [f64; 5] {
            let weight: f64 = deals.iter().map(|d| d.1).sum();
            let mut mean = [0.0; 5];
            for &(deal, w) in deals {
                for (m, f) in mean.iter_mut().zip(features(deal[i])) {
                    *m += w * f / weight;
                }
            }
            mean
        };
        // Each variant is the deals proposed at a time, the steps of the walk
        // after, the most deals proposed and the worth to stop at, as
        // `proposals:steps:at_most:worth`, the last two being optional.
        let variants: Vec<(usize, usize, usize, f64)> = var("PLAY_YARD_VARIANTS", &format!("{proposals}:0"))
            .split(',')
            .map(|v| {
                let parts: Vec<&str> = v.trim().split(':').collect();
                let proposals = parts[0].parse().unwrap();
                (proposals, parts[1].parse().unwrap(), parts.get(2).map_or(proposals, |p| p.parse().unwrap()), parts.get(3).map_or(0.0, |p| p.parse().unwrap()))
            })
            .collect();
        println!("{} positions from {games} games; {count} deals wanted", positions.len());
        let misses: Vec<Option<f64>> = var("PLAY_YARD_MISSES", "none,0.1")
            .split(',')
            .map(chance_of_a_miss)
            .collect();
        let gold_deals = number("PLAY_YARD_GOLD", 5000);
        for (&temperature, &miss) in temperatures.iter().flat_map(|t| misses.iter().map(move |m| (t, m))) {
            // What the ensemble should say of each hidden hand, from many
            // more proposals than any variant makes.
            let gold_reading = Reading { model: &competitor, temperature, miss, proposals: gold_deals, at_most: gold_deals, worth: 0.0, steps: 0 };
            let mut gold_rng = StdRng::seed_from_u64(99);
            let gold: Vec<Option<[[f64; 5]; 2]>> = positions
                .iter()
                .map(|game| {
                    let convention: &Convention = game.system.table();
                    let hidden = Hidden::new(game, convention)?;
                    let gold_auction = miss.map(|m| (hidden.replay(convention), m));
                    let (deals, likelihoods) = hidden.weighed_proposals(&mut gold_rng, game, convention, &gold_reading, gold_auction.as_ref());
                    let weighted: Vec<Weighted> = deals.into_iter().zip(likelihoods).collect();
                    (!weighted.is_empty() && weighted.iter().any(|d| d.1 > 0.0)).then(|| [mean_features(&weighted, 0), mean_features(&weighted, 1)])
                })
                .collect();
            for &(proposals, steps, at_most, worth) in &variants {
                let reading = Reading { model: &competitor, temperature, miss, proposals, at_most, worth, steps };
                let mut rng = StdRng::seed_from_u64(7);
                let (mut propose, mut weigh, mut walk) = (Duration::ZERO, Duration::ZERO, Duration::ZERO);
                let (mut raw, mut ensemble_worth, mut different, mut short, mut n) = (Vec::new(), 0.0, 0.0, 0, 0);
                let (mut proposed, mut accepted, mut tried) = ([0usize; 4], [0usize; 4], 0usize);
                // The different deals of the ensemble, in the positions whose
                // proposals are worth under a twentieth of themselves, and in
                // the rest.
                let (mut tight, mut loose) = (Vec::new(), Vec::new());
                // The errors with the proposals alone and with the weights, for
                // each feature, over every hidden hand of every position.
                let mut errors: [Vec<f64>; 5] = Default::default();
                let mut weighted_errors: [Vec<f64>; 5] = Default::default();
                let (mut sampling_tight, mut sampling_loose): ([Vec<f64>; 2], [Vec<f64>; 2]) = Default::default();
                for (position, game) in positions.iter().enumerate() {
                    let convention: &Convention = game.system.table();
                    let Some(hidden) = Hidden::new(game, convention) else { continue };
                    let started = Instant::now();
                    let auction = miss.map(|m| (hidden.replay(convention), m));
                    let (proposed_deals, likelihoods) = hidden.weighed_proposals(&mut rng, game, convention, &reading, auction.as_ref());
                    propose += started.elapsed();
                    if proposed_deals.len() < count {
                        short += 1;
                    }
                    if proposed_deals.is_empty() {
                        continue;
                    }
                    tried += proposed_deals.len();
                    let started = Instant::now();
                    let chosen = crate::ai::ensemble::resample(&mut rng, &proposed_deals, &likelihoods, count);
                    weigh += started.elapsed();
                    let all: Vec<Weighted> = proposed_deals.iter().zip(&likelihoods).map(|(&d, &l)| (d, l)).collect();
                    raw.push(effective_size(&all) / all.len() as f64);
                    let ensemble: Vec<Weighted> = if steps == 0 {
                        chosen
                    } else {
                        let started = Instant::now();
                        let each: Vec<[Cards; 2]> = chosen
                            .iter()
                            .flat_map(|&(deal, times)| std::iter::repeat_n(deal, times as usize))
                            .collect();
                        let walked = hidden.walk(&mut rng, game, convention, &reading, auction.as_ref(), &each, steps);
                        walk += started.elapsed();
                        for k in 0..4 {
                            proposed[k] += walked.proposed[k];
                            accepted[k] += walked.accepted[k];
                        }
                        walked.deals.into_iter().map(|d| (d, 1.0)).collect()
                    };
                    n += 1;
                    ensemble_worth += effective_size(&ensemble);
                    let mut once: Vec<[Cards; 2]> = ensemble.iter().map(|d| d.0).collect();
                    once.sort_by_key(|d| format!("{d:?}"));
                    once.dedup();
                    different += once.len() as f64;
                    if *raw.last().unwrap() < 0.05 { &mut tight } else { &mut loose }.push(once.len() as f64);
                    let plain: Vec<Weighted> = proposed_deals.iter().map(|&d| (d, 1.0)).collect();
                    for (i, &seat) in hidden.seats().iter().enumerate() {
                        let real = features(game.hands[seat]);
                        let (plain, weighted) = (mean_features(&plain, i), mean_features(&ensemble, i));
                        if let Some(gold) = &gold[position] {
                            let group = if *raw.last().unwrap() < 0.05 { &mut sampling_tight } else { &mut sampling_loose };
                            group[0].push((weighted[0] - gold[i][0]).abs());
                            group[1].push((1..5).map(|f| (weighted[f] - gold[i][f]).abs()).sum::<f64>() / 4.0);
                        }
                        for f in 0..5 {
                            errors[f].push((plain[f] - real[f]).abs());
                            weighted_errors[f].push((weighted[f] - real[f]).abs());
                        }
                    }
                }
                raw.sort_by(f64::total_cmp);
                let each = |total: Duration| total.as_secs_f64() * 1e3 / n as f64;
                println!(
                    "temperature {temperature}, {} calls, {proposals} proposals (to {at_most}, worth {worth}), {steps} steps: {:.0} proposals, {:.2} ms to propose and weigh, {:.2} to resample and {:.2} to walk a position; the proposals are worth {:.0}% of themselves (a tenth of positions under {:.0}%); the ensemble is worth {:.1} deals of {:.1} different; {short} positions of {n} with fewer than {count} proposals",
                    miss.map_or("bounds only".to_string(), |m| format!("miss {m}")),
                    tried as f64 / n as f64,
                    each(propose),
                    each(weigh),
                    each(walk),
                    100.0 * raw.iter().sum::<f64>() / raw.len() as f64,
                    100.0 * raw[raw.len() / 10],
                    ensemble_worth / n as f64,
                    different / n as f64
                );
                let mean = |v: &[f64]| v.iter().sum::<f64>() / v.len().max(1) as f64;
                println!(
                    "  sampling error against {} proposals: points {:.3} tight, {:.3} others; suit lengths {:.3} tight, {:.3} others",
                    gold_reading.at_most, mean(&sampling_tight[0]), mean(&sampling_loose[0]), mean(&sampling_tight[1]), mean(&sampling_loose[1])
                );
                println!(
                    "  {} tight positions with {:.1} different deals, {} others with {:.1}; steps made of 1 to 4 cards: {:.1}% {:.1}% {:.1}% {:.1}%",
                    tight.len(), mean(&tight), loose.len(), mean(&loose),
                    100.0 * accepted[0] as f64 / proposed[0].max(1) as f64,
                    100.0 * accepted[1] as f64 / proposed[1].max(1) as f64,
                    100.0 * accepted[2] as f64 / proposed[2].max(1) as f64,
                    100.0 * accepted[3] as f64 / proposed[3].max(1) as f64,
                );
                for (f, name) in ["points", "clubs", "diamonds", "hearts", "spades"].into_iter().enumerate() {
                    let (plain, _) = mean_and_error(&errors[f]);
                    let (weighted, _) = mean_and_error(&weighted_errors[f]);
                    let gains: Vec<f64> = errors[f].iter().zip(&weighted_errors[f]).map(|(a, b)| a - b).collect();
                    let (gain, gain_error) = mean_and_error(&gains);
                    println!("  {name:8}: error {plain:.3} with the proposals, {weighted:.3} weighted: {gain:+.3} ({gain_error:.3}) better");
                }
            }
        }
    }

    /// How fast [`Hidden::draw`] is, in the positions of
    /// [`report_the_play_weighted_dealer`]: the deals tried for 1000 drawn,
    /// and the time of a try, with `PLAY_YARD_MISS` (0.1) for the chance of
    /// a miss.
    #[test]
    #[ignore]
    fn report_the_speed_of_drawing() {
        use crate::ai::playout::{copy_of, one_of_each_run};
        let competitor = read_model(&std::env::var("PLAY_YARD_COMPETITOR").expect("PLAY_YARD_COMPETITOR"));
        let miss = chance_of_a_miss(&var("PLAY_YARD_MISS", "0.1")).expect("a chance of a miss, not none");
        let games = number("PLAY_YARD_GAMES", 30);
        let mut rng = StdRng::seed_from_u64(3);
        let (mut tried, mut drawn, mut compile, mut draw, mut n) = (0, 0, std::time::Duration::ZERO, std::time::Duration::ZERO, 0);
        for (i, game) in crate::ai::opening_lead::candidates::opening_leads(23, games).iter().enumerate() {
            let convention: &Convention = game.system.table();
            let mut game = copy_of(game);
            let mut coin = StdRng::seed_from_u64(i as u64);
            while !game.hand_done {
                if one_of_each_run(&game).len() > 1 && View::new(&game, convention).is_some() && coin.gen_range(0.0..1.0) < 0.25 {
                    if let Some(hidden) = Hidden::new(&game, convention) {
                        let started = std::time::Instant::now();
                        let replay = hidden.replay(convention);
                        compile += started.elapsed();
                        let started = std::time::Instant::now();
                        let dealt = hidden.draw(&mut rng, 1000, &replay, miss, 200_000);
                        draw += started.elapsed();
                        tried += dealt.tried;
                        drawn += dealt.deals.len();
                        n += 1;
                    }
                }
                let card = model_card(&competitor, &game, convention);
                game.play(card);
            }
        }
        println!(
            "{n} positions: {:.2} ms to compile the replay; {} deals tried and {} drawn a position, {:.2} us a try",
            compile.as_secs_f64() * 1e3 / n as f64,
            tried / n,
            drawn / n,
            draw.as_secs_f64() * 1e6 / tried as f64
        );
    }

    /// The chance of a miss that `text` says (see [`Hidden::draw`]): a chance
    /// from 0 to 1, or "none" for no use of the calls beyond their bounds.
    fn chance_of_a_miss(text: &str) -> Option<f64> {
        if text.trim() == "none" {
            return None;
        }
        let chance: f64 = text.trim().parse().unwrap_or_else(|_| panic!("a chance of a miss, or none, not {text}"));
        assert!((0.0..=1.0).contains(&chance), "a chance of a miss from 0 to 1, not {text}");
        Some(chance)
    }

    /// One ensemble for the playout yardstick, as `spec` says: "ensemble"
    /// (deals that fit what the player has seen), "bounds" (also within the
    /// bounds that the calls showed), or "play:T:P:S", the deals that the play
    /// of `competitor` makes likely, at temperature T, from P proposals and
    /// with S steps of the walk (see [`Hidden::read_the_play`]).  `miss` is
    /// the chance of a call that the robot would not have made.
    fn ensemble_for_yardstick(
        spec: &str,
        hidden: &Hidden,
        game: &GameState<()>,
        competitor: &Model,
        miss: Option<f64>,
        count: usize,
        rng: &mut StdRng,
    ) -> Vec<Weighted> {
        let convention: &Convention = game.system.table();
        let parts: Vec<&str> = spec.split(':').collect();
        match parts[0] {
            "ensemble" => hidden.ensemble(rng, count, None).into_iter().map(|d| (d, 1.0)).collect(),
            "bounds" => hidden.within_bounds(rng, count).deals.into_iter().map(|d| (d, 1.0)).collect(),
            "play" => {
                let proposals = parts[2].parse().unwrap();
                let reading = Reading {
                    model: competitor,
                    temperature: parts[1].parse().unwrap(),
                    miss,
                    proposals,
                    at_most: proposals,
                    worth: 0.0,
                    steps: parts[3].parse().unwrap(),
                };
                hidden.read_the_play(rng, game, convention, &reading, count)
            }
            other => panic!("no ensemble called {other}"),
        }
    }

    /// Whether the loss of the playout yardstick is partly luck of the
    /// choice of the best card, and how much.  The loss of a play is the mean
    /// over the deals of the tricks of the best card, less those of the card
    /// played; but the best of several cards on a few deals is the one that
    /// those deals favour, so its mean is too high, and the loss is too big,
    /// the more so the fewer different deals there are.  Here each position
    /// has two ensembles that are dealt separately, and the loss is also
    /// worked out by choosing the best card on one and valuing it on the
    /// other (and the other way round, and the mean of the two), which has no
    /// such bias, but is a little low where the best card is not clear.  For
    /// each of the ensembles of `PLAY_YARD_ENSEMBLES` (comma-separated, see
    /// [`ensemble_for_yardstick`]; `ensemble,bounds,play:0.5:1000:0` by
    /// default) it prints the two losses, with the standard errors, and how
    /// many different deals an ensemble has.  Positions as in
    /// [`score_on_the_playout_yardstick`] (`PLAY_YARD_COMPETITOR`,
    /// `PLAY_YARD_GAMES` (40), `PLAY_YARD_SHARE` (0.25), `PLAY_YARD_DEALS`
    /// (100), `PLAY_YARD_MISS` (0.1), `PLAY_THREADS`).
    #[test]
    #[ignore]
    fn check_the_loss_for_the_winners_curse() {
        use crate::ai::playout::{copy_of, one_of_each_run, run_of, weighted_card_values};
        let competitor = read_model(&std::env::var("PLAY_YARD_COMPETITOR").expect("PLAY_YARD_COMPETITOR"));
        let games = number("PLAY_YARD_GAMES", 40);
        let share: f64 = var("PLAY_YARD_SHARE", "0.25").parse().unwrap();
        let count = number("PLAY_YARD_DEALS", 100);
        let miss = chance_of_a_miss(&var("PLAY_YARD_MISS", "0.1"));
        let specs: Vec<String> =
            var("PLAY_YARD_ENSEMBLES", "ensemble,bounds,play:0.5:1000:0").split(',').map(|s| s.trim().to_string()).collect();
        let threads = number("PLAY_THREADS", std::thread::available_parallelism().map_or(4, |n| n.get()));
        let workers = rayon::ThreadPoolBuilder::new().num_threads(threads).build().unwrap();
        let deals = crate::ai::opening_lead::candidates::opening_leads(23, games);
        let positions: Vec<(GameState<()>, Vec<Card>)> = workers.install(|| {
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
                            kept.push((copy_of(&game), runs));
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
        println!("{} positions from {games} games, {count} deals in each of two ensembles", positions.len());
        for spec in &specs {
            let started = std::time::Instant::now();
            // For each position: the naive loss, the loss when the best card
            // is chosen on one ensemble and valued on the other, and how
            // many different deals the first has.
            let rows: Vec<Option<(f64, f64, f64)>> = workers.install(|| {
                positions
                    .par_iter()
                    .enumerate()
                    .map(|(i, (game, runs))| {
                        let convention: &Convention = game.system.table();
                        let hidden = Hidden::new(game, convention)?;
                        let mut rng = StdRng::seed_from_u64(1000 + i as u64);
                        let a = ensemble_for_yardstick(spec, &hidden, game, &competitor, miss, count, &mut rng);
                        let b = ensemble_for_yardstick(spec, &hidden, game, &competitor, miss, count, &mut rng);
                        let both = [&competitor, &competitor];
                        let on_a = weighted_card_values(game, runs, &a, both, convention)?;
                        let on_b = weighted_card_values(game, runs, &b, both, convention)?;
                        let played = run_of(runs, model_card(&competitor, game, convention));
                        let best = |values: &[f64]| (0..values.len()).fold(0, |best, i| if values[i] > values[best] { i } else { best });
                        let naive = 0.5 * (on_a[best(&on_a)] - on_a[played] + on_b[best(&on_b)] - on_b[played]);
                        // Chosen on one, valued on the other.
                        let split = 0.5 * (on_b[best(&on_a)] - on_b[played] + on_a[best(&on_b)] - on_a[played]);
                        let mut once: Vec<[Cards; 2]> = a.iter().map(|d| d.0).collect();
                        once.sort_by_key(|d| format!("{d:?}"));
                        once.dedup();
                        Some((naive, split, once.len() as f64))
                    })
                    .collect()
            });
            let rows: Vec<(f64, f64, f64)> = rows.into_iter().flatten().collect();
            let naive: Vec<f64> = rows.iter().map(|r| r.0).collect();
            let split: Vec<f64> = rows.iter().map(|r| r.1).collect();
            let different: Vec<f64> = rows.iter().map(|r| r.2).collect();
            let (naive, naive_error) = mean_and_error(&naive);
            let (split, split_error) = mean_and_error(&split);
            println!(
                "{spec}: {} positions, {:.1} different deals; loss {naive:.4} ({naive_error:.4}), chosen on one ensemble and valued on the other {split:.4} ({split_error:.4}) ({:.0} s)",
                rows.len(),
                different.iter().sum::<f64>() / different.len() as f64,
                started.elapsed().as_secs_f64()
            );
        }
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

    /// How long views and entries take with the players' beliefs and
    /// without.
    #[test]
    #[ignore]
    fn time_views_with_beliefs() {
        let old = Model::new(&V2_TRUMPS, 3);
        let new = old.grown_to(&V2_TRUMPS_BELIEFS).unwrap();
        let mut rng = StdRng::seed_from_u64(14);
        let games: Vec<(GameState<()>, std::sync::Arc<Convention>)> = crate::ai::opening_lead::candidates::opening_leads(45, 30)
            .iter()
            .map(|game| {
                let convention = game.system.convention();
                let mut game = crate::ai::playout::copy_of(game);
                let leader = game.turn().unwrap();
                let legal: Vec<Card> = game.hands[leader].collect();
                game.play(legal[0]);
                for _ in 0..rng.gen_range(0..40) {
                    let legal: Vec<Card> = game.could_be_played().collect();
                    game.play(legal[rng.gen_range(0..legal.len())]);
                }
                (game, convention)
            })
            .collect();
        let time = |what: &str, f: &dyn Fn(&GameState<()>, &Convention) -> f64| {
            let started = std::time::Instant::now();
            let mut sum = 0.0;
            let n = number("PLAY_TIME_REPEATS", 50) as u32;
            for _ in 0..n {
                for (game, convention) in &games {
                    sum += f(game, convention);
                }
            }
            println!("{what}: {:?} a view ({sum})", started.elapsed() / (n * games.len() as u32));
        };
        let scores = |model: &Model, game: &GameState<()>, convention: &Convention| -> f64 {
            View::new(game, convention).map_or(0.0, |view| view.legal().map(|card| model.score(&view, card)).sum())
        };
        time("a view without beliefs", &|game, convention| View::new(game, convention).map_or(0.0, |v| v.mine.len() as f64));
        time("scores without beliefs", &|game, convention| scores(&old, game, convention));
        let _ = LOCATIONS.set((crate::ai::locations::Prior::walls(10.0), crate::ai::locations::Evidence::none()));
        time("a view with beliefs", &|game, convention| View::new(game, convention).map_or(0.0, |v| v.mine.len() as f64));
        time("scores of the old shape with beliefs", &|game, convention| scores(&old, game, convention));
        time("scores of the new shape with beliefs", &|game, convention| scores(&new, game, convention));
        let later2 = Model::new(&V2_TRUMPS_HISTORY_LATER2, 3);
        let later3 = later2.grown_to(&V2_TRUMPS_HISTORY_LATER3).unwrap();
        time("scores with Later2", &|game, convention| scores(&later2, game, convention));
        time("scores with Later3", &|game, convention| scores(&later3, game, convention));
        let knots = later2.grown_to(&V2_TRUMPS_HISTORY_LATER3_KNOTS).unwrap();
        time("scores with Later3Knots", &|game, convention| scores(&knots, game, convention));
        let suit_tricks = Model::new(shape_spec("Trick+SuitTricks/16"), 3);
        time("scores with Trick+SuitTricks", &|game, convention| scores(&suit_tricks, game, convention));
    }

    #[test]
    fn a_model_grown_to_read_beliefs_scores_as_it_did() {
        let _ = LOCATIONS.set((crate::ai::locations::Prior::walls(10.0), crate::ai::locations::Evidence::none()));
        let old = Model::new(&V2_TRUMPS, 3);
        let new = old.grown_to(&V2_TRUMPS_BELIEFS).unwrap();
        let mut rng = StdRng::seed_from_u64(12);
        let mut weighted = 0;
        for game in crate::ai::opening_lead::candidates::opening_leads(41, 20) {
            let convention = game.system.convention();
            let mut game = crate::ai::playout::copy_of(&game);
            while !game.hand_done {
                if let Some(view) = View::new(&game, &convention) {
                    for card in view.legal() {
                        let entries = new.entries(&view, card);
                        let total: f64 = entries.weighted.iter().map(|w| w.1).sum();
                        assert!((total - 2.0).abs() < 1e-9, "the chances of each table sum to {total}");
                        weighted += entries.weighted.len();
                        let (a, b) = (old.score(&view, card), new.score(&view, card));
                        assert!((a - b).abs() < 1e-9, "{a} against {b}");
                    }
                }
                let legal: Vec<Card> = game.could_be_played().collect();
                game.play(legal[rng.gen_range(0..legal.len())]);
            }
        }
        assert!(weighted > 2000, "only {weighted} weighted entries");
    }

    #[test]
    fn beliefs_mixed_with_the_truth_are_mixed_entry_by_entry() {
        let _ = LOCATIONS.set((crate::ai::locations::Prior::walls(10.0), crate::ai::locations::Evidence::none()));
        let models = [
            Model::new(&V2_TRUMPS, 3).grown_to(&V2_TRUMPS_BELIEFS).unwrap(),
            Model::new(&V2_TRUMPS_HISTORY_LATER2, 3).grown_to(&V2_TRUMPS_HISTORY_LATER3).unwrap(),
            Model::new(shape_spec("Trick+SuitTricks/16"), 3),
        ];
        let mut rng = StdRng::seed_from_u64(14);
        let mut compared = 0;
        // The chances of the weighted entries of `model` in the views of
        // `game` with the beliefs `lambda` the truth, each table's summing
        // to 1.
        let chances = |model: &Model, game: &GameState<()>, convention: &Convention, lambda: f64| {
            set_oracle(lambda);
            let view = View::new(game, convention);
            set_oracle(0.0);
            let view = view?;
            let tables =
                model.shape.tables.iter().filter(|t| matches!(t, Table::AboveSplit | Table::BelowSplit | Table::Later3 | Table::SuitTricks | Table::Run | Table::RunOdds | Table::RunSure | Table::RunPlan)).count();
            let mut chances = std::collections::BTreeMap::<(Card, usize), f64>::new();
            for card in view.legal() {
                let weighted = model.entries(&view, card).weighted;
                let total: f64 = weighted.iter().map(|w| w.1).sum();
                assert!((total - tables as f64).abs() < 1e-9, "the chances of {tables} tables sum to {total}");
                for (entry, chance) in weighted {
                    *chances.entry((card, entry)).or_default() += chance;
                }
            }
            Some(chances)
        };
        for game in crate::ai::opening_lead::candidates::opening_leads(47, 12) {
            let convention = game.system.convention();
            let mut game = crate::ai::playout::copy_of(&game);
            while !game.hand_done {
                for model in &models {
                    let (Some(believed), Some(truth), Some(half)) = (
                        chances(model, &game, &convention, 0.0),
                        chances(model, &game, &convention, 1.0),
                        chances(model, &game, &convention, 0.5),
                    ) else {
                        continue;
                    };
                    for (key, &chance) in &truth {
                        assert_eq!(chance, 1.0, "the truth is one way the cards lie");
                        assert!(believed.get(key).is_some_and(|&c| c > 0.0), "the beliefs rule out the truth");
                    }
                    for (key, &chance) in &half {
                        let mixed = 0.5 * believed.get(key).copied().unwrap_or(0.0) + 0.5 * truth.get(key).copied().unwrap_or(0.0);
                        assert!((chance - mixed).abs() < 1e-12, "{chance} against {mixed}");
                        compared += 1;
                    }
                }
                let legal: Vec<Card> = game.could_be_played().collect();
                game.play(legal[rng.gen_range(0..legal.len())]);
            }
        }
        assert!(compared > 2000, "only {compared} compared");
    }

    #[test]
    fn a_suit_is_cashed_from_the_top() {
        let bits = |ranks: &[u8]| ranks.iter().fold(0u16, |b, &r| b | 1 << r);
        let (ours, opponents) = ([0, 2], [1, 3]);
        // A K Q against small cards: three tricks.
        let hands = [bits(&[14, 13, 12]), bits(&[5, 4, 3]), bits(&[2]), bits(&[9, 8])];
        assert_eq!(cash(hands, ours, opponents, [false; 2]), 3);
        // A Q against the king: one trick, then the king wins.
        let hands = [bits(&[14, 12]), bits(&[13, 3]), bits(&[2]), bits(&[9, 8])];
        assert_eq!(cash(hands, ours, opponents, [false; 2]), 1);
        // A K J against a doubleton queen: the queen drops under the king,
        // and the jack is the best card left.
        let hands = [bits(&[14, 13, 11]), bits(&[12, 3]), bits(&[2, 4]), bits(&[9, 8, 7])];
        assert_eq!(cash(hands, ours, opponents, [false; 2]), 3);
        // A K Q with a void opponent who can ruff: nothing.
        let hands = [bits(&[14, 13, 12]), 0, bits(&[2]), bits(&[9, 8])];
        assert_eq!(cash(hands, ours, opponents, [true, false]), 0);
        // Partner's long suit runs once the opponents are out of it, at most
        // the most that is counted.
        let hands = [bits(&[14]), bits(&[3]), bits(&[13, 12, 11, 10, 9, 8]), bits(&[2])];
        assert_eq!(cash(hands, ours, opponents, [false; 2]), RUN_MOST);
        // The run odds add up as independent parts do.
        let a = [0.5, 0.5, 0.0, 0.0, 0.0];
        assert_eq!(add_runs(&a, &a), [0.25, 0.5, 0.25, 0.0, 0.0]);
        let b = [0.0, 0.0, 0.0, 1.0, 0.0];
        assert_eq!(add_runs(&b, &b), [0.0, 0.0, 0.0, 0.0, 1.0]);
    }

    #[test]
    fn a_planned_suit_keeps_its_transport_and_ruffs() {
        let bits = |ranks: &[u8]| ranks.iter().fold(0u16, |b, &r| b | 1 << r);
        let (side, opponents) = ([0, 2], [1, 3]);
        let state = |lead: u8, ours: [u8; 2], theirs: [u8; 2]| RunState { run: 0, lead, ours, ruffed: [0; 2], theirs };
        // The ace alone opposite K Q J: cashed from the player's hand, the
        // lead stays there and the suit is blocked after one trick, where
        // cashing with no transport takes all of them.
        let hands = [bits(&[14]), bits(&[5, 4, 3]), bits(&[13, 12, 11]), bits(&[9, 8, 7])];
        assert_eq!(plan_step(hands, state(0, [0; 2], [0; 2]), side, opponents, false, false).run, 1);
        assert_eq!(cash(hands, side, opponents, [false; 2]), 3);
        // Small cards opposite a void with two trumps, the opponents holding
        // the top cards: two ruffs, and then the lead is in the void hand.
        let hands = [bits(&[5, 4, 3]), bits(&[14, 13]), 0, bits(&[12, 11, 2])];
        let after = plan_step(hands, state(0, [0, 2], [0, 0]), side, opponents, false, true);
        assert_eq!((after.run, after.lead, after.ours, after.ruffed), (1, 1, [0, 1], [0, 1]));
        // ...the void hand on lead has none of the suit, so it stops after one;
        // a hand on lead with the small cards ruffs again only by coming back.
        // An opponent with none of the suit and a trump overruffs: no ruff.
        let hands = [bits(&[5, 4, 3]), 0, 0, bits(&[12, 11, 2])];
        assert_eq!(plan_step(hands, state(0, [0, 2], [3, 0]), side, opponents, false, true).run, 0);
        // Cashing trumps draws the opponents': A K Q J against 3 2 and 5 4.
        let hands = [bits(&[14, 13, 12, 11]), bits(&[3, 2]), bits(&[6]), bits(&[5, 4])];
        let after = plan_step(hands, state(0, [4, 1], [2, 2]), side, opponents, true, true);
        assert_eq!((after.run, after.theirs), (4, [0, 0]));
    }

    #[test]
    fn the_run_odds_add_up_and_the_sure_run_is_the_least() {
        let _ = LOCATIONS.set((crate::ai::locations::Prior::walls(10.0), crate::ai::locations::Evidence::none()));
        let mut rng = StdRng::seed_from_u64(21);
        let mut checked = 0;
        let mut runs = [0usize; RUN_MOST + 1];
        for game in crate::ai::opening_lead::candidates::opening_leads(53, 16) {
            let convention = game.system.convention();
            let mut game = crate::ai::playout::copy_of(&game);
            while !game.hand_done {
                if let Some(view) = View::new(&game, &convention).filter(|v| v.beliefs.is_some()) {
                    for card in view.legal() {
                        let (odds, sure) = view.run_odds(card);
                        let total: f64 = odds.iter().sum();
                        assert!((total - 1.0).abs() < 1e-9, "the odds sum to {total}");
                        let planned: f64 = view.run_plan(card).iter().sum();
                        assert!((planned - 1.0).abs() < 1e-9, "the planned odds sum to {planned}");
                        let least = odds.iter().position(|&p| p > 1e-12).unwrap();
                        assert_eq!(least, sure, "{odds:?}");
                        runs[sure] += 1;
                        checked += 1;
                    }
                }
                let legal: Vec<Card> = game.could_be_played().collect();
                game.play(legal[rng.gen_range(0..legal.len())]);
            }
        }
        assert!(checked > 1000, "only {checked}");
        // Some cards of every kind of run.
        assert!(runs.iter().all(|&n| n > 0), "{runs:?}");
    }

    /// How often [`Table::RunOdds`]'s median run is [`Table::Run`]'s, and how
    /// long each takes, over the views of random games.
    #[test]
    #[ignore]
    fn compare_the_run_odds_with_the_run() {
        let _ = LOCATIONS.set(match std::env::var("PLAY_SELF_LOCATIONS") {
            Ok(path) => read_locations(&path),
            Err(_) => (crate::ai::locations::Prior::walls(10.0), crate::ai::locations::Evidence::none()),
        });
        let mut rng = StdRng::seed_from_u64(22);
        let (mut same, mut within_one, mut cards) = (0, 0, 0);
        let (mut plan_same, mut plan_within_one) = (0, 0);
        let (mut odds_time, mut run_time, mut plan_time) = (std::time::Duration::ZERO, std::time::Duration::ZERO, std::time::Duration::ZERO);
        let median = |odds: [f64; RUN_MOST + 1]| {
            let mut sum = 0.0;
            odds.iter()
                .position(|&p| {
                    sum += p;
                    sum >= 0.5
                })
                .unwrap()
        };
        let mut table = [[0usize; RUN_MOST + 1]; RUN_MOST + 1];
        for game in crate::ai::opening_lead::candidates::opening_leads(61, number("PLAY_GAMES", 40)) {
            let convention = game.system.convention();
            let mut game = crate::ai::playout::copy_of(&game);
            while !game.hand_done {
                // Two views of each moment, so that each table works its own out.
                if let (Some(a), Some(b)) = (View::new(&game, &convention), View::new(&game, &convention)) {
                    if a.beliefs.is_some() {
                        let started = std::time::Instant::now();
                        let medians: Vec<usize> = a
                            .legal()
                            .map(|card| {
                                let (odds, _) = a.run_odds(card);
                                let mut sum = 0.0;
                                odds.iter().position(|&p| {
                                    sum += p;
                                    sum >= 0.5
                                })
                                .unwrap()
                            })
                            .collect();
                        odds_time += started.elapsed();
                        let started = std::time::Instant::now();
                        let theirs: Vec<usize> = b.legal().map(|card| b.run_median(card)).collect();
                        run_time += started.elapsed();
                        let started = std::time::Instant::now();
                        let planned: Vec<usize> = a.legal().map(|card| median(a.run_plan(card))).collect();
                        plan_time += started.elapsed();
                        for ((&m, &r), &q) in medians.iter().zip(&theirs).zip(&planned) {
                            same += usize::from(m == r);
                            within_one += usize::from(m.abs_diff(r) <= 1);
                            plan_same += usize::from(q == r);
                            plan_within_one += usize::from(q.abs_diff(r) <= 1);
                            table[r][m] += 1;
                            cards += 1;
                        }
                    }
                }
                let legal: Vec<Card> = game.could_be_played().collect();
                game.play(legal[rng.gen_range(0..legal.len())]);
            }
        }
        println!(
            "{cards} cards: the medians agree for {:.1}%, within one for {:.1}%; RunOdds {:?} a card, Run {:?} a card",
            100.0 * same as f64 / cards as f64,
            100.0 * within_one as f64 / cards as f64,
            odds_time / cards as u32,
            run_time / cards as u32
        );
        println!(
            "RunPlan's medians agree with Run's for {:.1}%, within one for {:.1}%, at {:?} a card",
            100.0 * plan_same as f64 / cards as f64,
            100.0 * plan_within_one as f64 / cards as f64,
            plan_time / cards as u32
        );
        println!("rows Run's median, columns RunOdds': {table:?}");
    }

    /// Whether [`View::run_splits`], the beliefs' chances of each card and
    /// the deals that [`Table::Run`] makes agree on where each free card is.
    #[test]
    #[ignore]
    fn the_splits_and_the_deals_put_the_cards_alike() {
        let _ = LOCATIONS.set(match std::env::var("PLAY_SELF_LOCATIONS") {
            Ok(path) => read_locations(&path),
            Err(_) => (crate::ai::locations::Prior::walls(10.0), crate::ai::locations::Evidence::none()),
        });
        let mut rng = StdRng::seed_from_u64(24);
        let (mut worst_splits, mut worst_deals, mut views) = (0.0f64, 0.0f64, 0);
        for game in crate::ai::opening_lead::candidates::opening_leads(61, 10) {
            let convention = game.system.convention();
            let mut game = crate::ai::playout::copy_of(&game);
            while !game.hand_done {
                if let Some(view) = View::new(&game, &convention).filter(|v| v.beliefs.is_some()) {
                    let seen = view.beliefs.as_ref().unwrap();
                    let beliefs = seen.beliefs.as_ref().unwrap();
                    let chances = beliefs.chances();
                    let splits = view.run_splits(seen);
                    let n = 400;
                    let deals: Vec<Cards> = (0..n).map(|_| beliefs.deal(&mut rng)).collect();
                    for c in seen.free {
                        // The chance of the card in the player's first hidden hand, three ways.
                        let by_chances = if seen.swapped { 1.0 - chances.card[crate::ai::locations::index(c)] } else { chances.card[crate::ai::locations::index(c)] };
                        let suit = &splits[c.suit() as usize];
                        let total: f64 = suit.iter().map(|s| s.1).sum();
                        let by_splits = suit.iter().filter(|s| s.0[2] & 1 << c.rank() != 0).map(|s| s.1).sum::<f64>() / total;
                        let by_deals = deals.iter().filter(|d| d.contains(c) != seen.swapped).count() as f64 / n as f64;
                        let free_in_suit = seen.free.in_suit(c.suit()).len();
                        if free_in_suit <= RUN_ODDS_MOST_FREE {
                            worst_splits = worst_splits.max((by_splits - by_chances).abs());
                        }
                        worst_deals = worst_deals.max((by_deals - by_chances).abs());
                        if (by_deals - by_chances).abs() > 0.15 || (free_in_suit <= RUN_ODDS_MOST_FREE && (by_splits - by_chances).abs() > 0.02) {
                            println!("{c} (of {free_in_suit} free in its suit), swapped {}: chances {by_chances:.3}, splits {by_splits:.3}, deals {by_deals:.3}", seen.swapped);
                        }
                    }
                    views += 1;
                }
                let legal: Vec<Card> = game.could_be_played().collect();
                game.play(legal[rng.gen_range(0..legal.len())]);
            }
        }
        println!("{views} views: the splits differ from the chances by at most {worst_splits:.4}, the deals by at most {worst_deals:.4}");
    }

    /// Prints cards where the median of [`Table::RunPlan`] and that of
    /// [`Table::Run`] differ, `PLAY_SHOW` (6) of each kind (RunPlan longer, Run
    /// longer), at most one a game, with the hands (the hidden ones as they
    /// truly are, which neither reads), Run's runs and RunPlan's and RunOdds'
    /// chances, and how often each kind happens.
    #[test]
    #[ignore]
    fn show_run_disagreements() {
        let _ = LOCATIONS.set(match std::env::var("PLAY_SELF_LOCATIONS") {
            Ok(path) => read_locations(&path),
            Err(_) => (crate::ai::locations::Prior::walls(10.0), crate::ai::locations::Evidence::none()),
        });
        let show = number("PLAY_SHOW", 6);
        let mut rng = StdRng::seed_from_u64(23);
        let (mut plan_longer, mut run_longer, mut cards) = (0, 0, 0);
        let mut by_gap = [[0usize; RUN_MOST + 1]; 2];
        let (mut shown_plan, mut shown_run) = (0, 0);
        let median = |odds: &[f64; RUN_MOST + 1]| {
            let mut sum = 0.0;
            odds.iter()
                .position(|&p| {
                    sum += p;
                    sum >= 0.5
                })
                .unwrap()
        };
        let odds_text = |odds: &[f64; RUN_MOST + 1]| odds.iter().map(|p| format!("{p:.2}")).collect::<Vec<_>>().join(" ");
        for game in crate::ai::opening_lead::candidates::opening_leads(61, number("PLAY_GAMES", 40)) {
            let convention = game.system.convention();
            let mut game = crate::ai::playout::copy_of(&game);
            let mut shown_this_game = false;
            while !game.hand_done {
                if let (Some(a), Some(b)) = (View::new(&game, &convention), View::new(&game, &convention)) {
                    if a.beliefs.is_some() {
                        for card in a.legal() {
                            let plan = a.run_plan(card);
                            let runs = b.run_runs(card);
                            let (p, r) = (median(&plan), runs[(runs.len() - 1) / 2].0);
                            cards += 1;
                            if p == r {
                                continue;
                            }
                            let plan_is_longer = p > r;
                            if plan_is_longer {
                                plan_longer += 1;
                            } else {
                                run_longer += 1;
                            }
                            by_gap[usize::from(!plan_is_longer)][p.abs_diff(r)] += 1;
                            let shown = if plan_is_longer { &mut shown_plan } else { &mut shown_run };
                            if *shown >= show || shown_this_game {
                                continue;
                            }
                            *shown += 1;
                            shown_this_game = true;
                            let seat = game.hand_playing().unwrap();
                            let names = ["me", "LHO", "partner", "RHO"];
                            let dummy = game.dummy().unwrap();
                            println!(
                                "\n{} wins: RunPlan {p}, Run {r}.  {} {:?}, the player is {} ({seat:?}), dummy {dummy:?}, tricks done {}, trick so far {:?}, plays {card}",
                                if plan_is_longer { "RunPlan" } else { "Run" },
                                game.highest_contract_bid().map_or("?".to_string(), |b| format!("{b:?}")),
                                a.trump,
                                ["declarer", "dummy", "defender (LHO of declarer)", "defender (RHO of declarer)"][a.role],
                                game.tricks.len(),
                                a.trick.iter().flatten().map(|c| c.to_string()).collect::<Vec<_>>(),
                            );
                            for (n, name) in names.iter().enumerate() {
                                let s = seat + n;
                                let hidden = n != 0 && s != dummy && !(seat == dummy && n == 2);
                                println!("    {name:<8}{} {}", if hidden { "(hidden)" } else { "        " }, game.hands[s]);
                            }
                            let gave_up = runs.iter().filter(|r| r.1).count();
                            println!(
                                "    Run's runs: {:?}{}",
                                runs.iter().map(|r| r.0).collect::<Vec<_>>(),
                                if gave_up > 0 { format!(" ({gave_up} searches gave up)") } else { String::new() }
                            );
                            println!("    RunPlan's chances of 0..4+: {}", odds_text(&plan));
                            println!("    RunOdds' chances of 0..4+: {}", odds_text(&a.run_odds(card).0));
                        }
                    }
                }
                let legal: Vec<Card> = game.could_be_played().collect();
                game.play(legal[rng.gen_range(0..legal.len())]);
            }
        }
        println!(
            "\n{cards} cards: RunPlan longer for {plan_longer} ({:.1}%), Run longer for {run_longer} ({:.1}%)",
            100.0 * plan_longer as f64 / cards as f64,
            100.0 * run_longer as f64 / cards as f64
        );
        println!("by how much longer (1, 2, 3, 4): RunPlan {:?}, Run {:?}", &by_gap[0][1..], &by_gap[1][1..]);
    }

    #[test]
    fn a_suit_played_out_alone_takes_its_tricks() {
        let bits = |ranks: &[u8]| ranks.iter().fold(0u16, |b, &r| b | 1 << r);
        // The player holds A Q, the king is on the right (hand 3), and
        // partner (hand 2) leads through it: both tricks.  Led from the
        // player's hand, the queen loses to the king.
        let hands = [bits(&[14, 12]), bits(&[6, 5]), bits(&[4, 3]), bits(&[13, 2])];
        assert_eq!(SuitSolver::default().tricks(hands, 2, 0, [0; 4]), 2);
        assert_eq!(SuitSolver::default().tricks(hands, 0, 0, [0; 4]), 1);
        // With the king on the left, partner's lead does not help.
        let hands = [bits(&[14, 12]), bits(&[13, 2]), bits(&[4, 3]), bits(&[6, 5])];
        assert_eq!(SuitSolver::default().tricks(hands, 2, 0, [0; 4]), 1);
        // A K Q J opposite nothing take four however the rest lie.
        let hands = [bits(&[14, 13, 12, 11]), bits(&[10, 9, 8]), 0, bits(&[7, 6, 5, 4])];
        assert_eq!(SuitSolver::default().tricks(hands, 0, 0, [0; 4]), 4);
        // Opponents' A K Q opposite our J 10 9 8 7: two tricks, the last two.
        let hands = [bits(&[11, 10, 9, 8, 7]), bits(&[14, 13, 12]), bits(&[2]), bits(&[3, 4])];
        assert_eq!(SuitSolver::default().tricks(hands, 0, 0, [0; 4]), 2);
        // In the middle of a trick: the right has played the king to the
        // player's lead of the 2 from partner's hand, and the player can
        // take it with the ace, but then the 3 loses to the 6.
        let hands = [bits(&[14, 3]), bits(&[5]), bits(&[4]), bits(&[6])];
        assert_eq!(SuitSolver::default().tricks(hands, 2, 2, [0, 0, 2, 13]), 1);
    }

    #[test]
    fn the_hats_add_up_to_the_chance() {
        for i in 0..=100 {
            let p = i as f64 / 100.0;
            let sum: f64 = hats(p).iter().zip(KNOTS).map(|(h, k)| h * k).sum();
            assert!((sum - p).abs() < 1e-12, "{p}: {sum}");
        }
    }

    #[test]
    fn a_model_grown_to_later3_knots_scores_as_it_did() {
        let _ = LOCATIONS.set((crate::ai::locations::Prior::walls(10.0), crate::ai::locations::Evidence::none()));
        let mut rng = StdRng::seed_from_u64(16);
        let mut later2 = Model::new(&V2_TRUMPS_HISTORY_LATER2, 5);
        later2.network.params.iter_mut().for_each(|p| *p = rng.gen_range(-0.5..0.5));
        let later3 = later2.grown_to(&V2_TRUMPS_HISTORY_LATER3).unwrap();
        let knots = [later2.grown_to(&V2_TRUMPS_HISTORY_LATER3_KNOTS).unwrap(), later3.grown_to(&V2_TRUMPS_HISTORY_LATER3_KNOTS).unwrap()];
        let mut compared = 0;
        for game in crate::ai::opening_lead::candidates::opening_leads(49, 10) {
            let convention = game.system.convention();
            let mut game = crate::ai::playout::copy_of(&game);
            while !game.hand_done {
                if let Some(view) = View::new(&game, &convention) {
                    for card in view.legal() {
                        let a = later3.score(&view, card);
                        for model in &knots {
                            let b = model.score(&view, card);
                            assert!((a - b).abs() < 1e-9, "{a} against {b}");
                        }
                        compared += 1;
                    }
                }
                let legal: Vec<Card> = game.could_be_played().collect();
                game.play(legal[rng.gen_range(0..legal.len())]);
            }
        }
        assert!(compared > 1000, "only {compared}");
    }

    #[test]
    fn a_model_grown_from_later2_to_later3_keeps_its_weights() {
        let old = Model::new(&V2_TRUMPS_HISTORY_LATER2, 5);
        let mut old = old;
        let mut rng = StdRng::seed_from_u64(15);
        old.network.params.iter_mut().for_each(|p| *p = rng.gen_range(-1.0..1.0));
        let new = old.grown_to(&V2_TRUMPS_HISTORY_LATER3).unwrap();
        assert_eq!(old.network.params, new.network.params, "the same tables and entries, so the same weights");
    }

    /// The prior and the evidence of the players' beliefs in the file at
    /// `path`: the prior's weights and then the evidence's, one a line, as
    /// `learn_card_locations` writes them (see [`Evidence::from_weights`]).
    ///
    /// [`Evidence::from_weights`]: crate::ai::locations::Evidence::from_weights
    fn read_locations(path: &str) -> (crate::ai::locations::Prior, crate::ai::locations::Evidence) {
        let weights: Vec<f64> = std::fs::read_to_string(path).unwrap().lines().map(|l| l.parse().expect("a weight")).collect();
        let mut prior = crate::ai::locations::Prior::none();
        let (a, b) = weights.split_at(prior.weights.len());
        prior.weights.copy_from_slice(a);
        (prior, crate::ai::locations::Evidence::from_weights(b))
    }

    /// A tracker's beliefs after a play, what the play added to them if it
    /// showed something, the first hidden hand then, the tricks done before
    /// the play, and whether the tracker is a defender's.
    type Tracked = (crate::ai::locations::Beliefs, Option<crate::ai::locations::Update>, Cards, usize, bool);

    /// A tracker's beliefs before the opening lead, the entries of the prior
    /// they started from, and the first and second hidden hands.
    type Started = (crate::ai::locations::Beliefs, crate::ai::locations::Opening, Cards, Cards);

    /// The beliefs of the declarer and both defenders before the opening
    /// lead, from `prior`, and after each play, with
    /// `evidence`, in `json` played out by `model` at every seat (each card
    /// the one it scores highest, the opening lead by
    /// `likely_opening_leads`), with what each update added, the first
    /// hidden hand as it then was, and the tricks done before the play.
    fn tracked_beliefs(
        json: &str,
        convention: &Convention,
        model: &Model,
        prior: &crate::ai::locations::Prior,
        evidence: &crate::ai::locations::Evidence,
    ) -> (Vec<Started>, Vec<Tracked>) {
        use crate::ai::locations::Tracker;
        use crate::ai::opening_lead::{likely_opening_leads, Auction};
        let mut game: GameState<()> = serde_json::from_str(json).unwrap();
        let Some(dummy) = game.dummy() else {
            return (Vec::new(), Vec::new());
        };
        let declarer = dummy + 2;
        let mut trackers: Vec<Tracker> =
            [declarer, declarer + 1, declarer + 3].iter().filter_map(|&viewer| Tracker::new(&game, viewer)).collect();
        // `PLAY_LOC_POINTS` 0 leaves the hands' points out of the beliefs.
        let points = std::env::var("PLAY_LOC_POINTS").map_or(true, |v| v != "0");
        let started: Vec<Started> = trackers
            .iter_mut()
            .map(|tracker| {
                tracker.beliefs.points = points;
                let opening = tracker.start(&game, convention, prior);
                (tracker.beliefs.clone(), opening, game.hands[tracker.first], game.hands[tracker.second])
            })
            .collect();
        let mut seen = Vec::new();
        while !game.hand_done {
            let card = if game.tricks.is_empty() && game.played.is_empty() {
                let leader = game.turn().unwrap();
                likely_opening_leads(&Auction::new(&game).unwrap(), game.hands[leader])[0]
            } else {
                model_card(model, &game, convention)
            };
            let done = game.tricks.len();
            let updates: Vec<_> = trackers.iter_mut().map(|t| t.observe(&game, card, evidence)).collect();
            game.play(card);
            // The declarer's tracker comes first, and then the defenders'.
            for (i, (tracker, update)) in trackers.iter().zip(updates).enumerate() {
                if !tracker.beliefs.unseen.is_empty() {
                    seen.push((tracker.beliefs.clone(), update, game.hands[tracker.first], done, i > 0));
                }
            }
        }
        (started, seen)
    }

    /// Learns what the auction shows of the hidden hands and what their
    /// plays show (see [`crate::ai::locations::Tracker`]) by log loss on
    /// the true deal, from games of the play model `PLAY_MODEL` (see
    /// [`read_model`]) at every seat: `PLAY_LOC_ROUNDS` (200) rounds of
    /// `PLAY_LOC_GAMES` (128) fresh deals, each giving the beliefs of each
    /// player before the opening lead to the prior and `PLAY_LOC_EXAMPLES`
    /// (8) of its plays at random to the evidence, with a step of Adam of
    /// size `PLAY_LOC_RATE` (0.01) a round for each, the beliefs before each
    /// play taken as they are, with no slope through them.  The prior starts
    /// as `PLAY_LOC_PRIOR` (walls, or none): walls make each length and
    /// number of points outside what the calls showed cost `PLAY_LOC_WALL`
    /// (10).  Every `PLAY_LOC_REPORT` (20) rounds it prints the log loss a
    /// play on `PLAY_LOC_HELD` (200) other deals, before the opening lead
    /// and by the tricks done, against vacant places and the walls (each
    /// with the exact bookkeeping and no evidence), how often a hand is
    /// outside what its calls showed, and the calibration of the cards'
    /// chances.  It writes the prior's and the evidence's weights to
    /// `PLAY_LOC_OUT` if that is set, and uses `PLAY_THREADS` (1) threads.
    #[test]
    #[ignore]
    fn learn_card_locations() {
        use crate::ai::locations::{Evidence, Prior};
        let model = read_model(&std::env::var("PLAY_MODEL").expect("PLAY_MODEL"));
        let rounds = number("PLAY_LOC_ROUNDS", 200);
        let games = number("PLAY_LOC_GAMES", 128);
        let examples = number("PLAY_LOC_EXAMPLES", 8);
        let float = |name: &str, default: f64| -> f64 { std::env::var(name).map_or(default, |v| v.parse().expect(name)) };
        let rate = float("PLAY_LOC_RATE", 0.01);
        let wall = float("PLAY_LOC_WALL", 10.0);
        let report = number("PLAY_LOC_REPORT", 20);
        let workers = rayon::ThreadPoolBuilder::new().num_threads(number("PLAY_THREADS", 1)).build().unwrap();
        let deals = |seed: u64, count: usize| -> Vec<String> {
            crate::ai::opening_lead::candidates::opening_leads(seed, count)
                .iter()
                .map(|game| serde_json::to_string(game).unwrap())
                .collect()
        };
        let held = deals(77, number("PLAY_LOC_HELD", 200));
        let convention_of = |json: &str| serde_json::from_str::<GameState<()>>(json).unwrap().system.convention();
        // Before the opening lead, then by the tricks done.
        let stage = |done: usize| match done {
            0 => 1,
            1..=4 => 2,
            5..=8 => 3,
            _ => 4,
        };
        // For the declarer's beliefs and the defenders', the log likelihood
        // summed and the beliefs by stage, the hands outside what their calls
        // showed, and with `calibrate`, for each tenth of the chances, the
        // chances and the cards there summed and their number.
        type Judged = ([[(f64, usize); 5]; 2], usize, [(f64, f64, usize); 10]);
        let judge = |prior: &Prior, evidence: &Evidence, calibrate: bool| -> Judged {
            let results: Vec<Judged> = workers.install(|| {
                held.par_iter()
                    .map(|json| {
                        let mut by_stage = [[(0.0, 0); 5]; 2];
                        let mut outside = 0;
                        let mut bins = [(0.0, 0.0, 0); 10];
                        let (started, seen) = tracked_beliefs(json, &convention_of(json), &model, prior, evidence);
                        for (i, (beliefs, opening, first, second)) in started.iter().enumerate() {
                            let s = &mut by_stage[usize::from(i > 0)][0];
                            s.0 += beliefs.log_likelihood(*first);
                            s.1 += 1;
                            outside += usize::from(opening.outside(*first, *second));
                        }
                        for (i, (beliefs, _, first, done, defender)) in seen.iter().enumerate() {
                            let s = &mut by_stage[usize::from(*defender)][stage(*done)];
                            s.0 += beliefs.log_likelihood(*first);
                            s.1 += 1;
                            if calibrate && i % 4 == 0 {
                                let chances = beliefs.chances();
                                for card in beliefs.unseen - beliefs.in_first - beliefs.in_second {
                                    let p = chances.card[crate::ai::locations::index(card)];
                                    let bin = &mut bins[((p * 10.0) as usize).min(9)];
                                    bin.0 += p;
                                    bin.1 += f64::from(u8::from(first.contains(card)));
                                    bin.2 += 1;
                                }
                            }
                        }
                        (by_stage, outside, bins)
                    })
                    .collect()
            });
            let mut total: Judged = ([[(0.0, 0); 5]; 2], 0, [(0.0, 0.0, 0); 10]);
            for (s, o, b) in results {
                for (sum, part) in total.0.iter_mut().flatten().zip(s.iter().flatten()) {
                    sum.0 += part.0;
                    sum.1 += part.1;
                }
                total.1 += o;
                for (sum, part) in total.2.iter_mut().zip(b) {
                    sum.0 += part.0;
                    sum.1 += part.1;
                    sum.2 += part.2;
                }
            }
            total
        };
        // For each kind of player, the nats lost a play, from the opening
        // lead on, and by stage.
        let show = |kinds: &[[(f64, usize); 5]; 2]| -> String {
            let one = |stages: &[(f64, usize); 5]| -> String {
                let total: f64 = stages[1..].iter().map(|s| s.0).sum();
                let count: usize = stages[1..].iter().map(|s| s.1).sum();
                let each: Vec<String> = stages.iter().map(|s| format!("{:.4}", -s.0 / s.1.max(1) as f64)).collect();
                format!("{:.4} ({})", -total / count.max(1) as f64, each.join(", "))
            };
            format!(
                "declarer {}, defenders {} a play (before the lead, then by tricks done 0, 1–4, 5–8, 9–12)",
                one(&kinds[0]),
                one(&kinds[1])
            )
        };
        let (vacant, _, _) = judge(&Prior::none(), &Evidence::none(), false);
        let walls = Prior::walls(wall);
        let (walled, outside, _) = judge(&walls, &Evidence::none(), false);
        println!("vacant places lose {} nats", show(&vacant));
        println!(
            "the walls at {wall} lose {} nats, with {outside} of the {} players' views having a hidden hand outside what its calls showed",
            show(&walled),
            walled[0][0].1 + walled[1][0].1
        );
        if let Ok(path) = std::env::var("PLAY_LOC_BEFORE") {
            let (prior, evidence) = read_locations(&path);
            let (before, _, _) = judge(&prior, &evidence, false);
            println!("the beliefs of {path} lose {} nats", show(&before));
        }
        let mut prior = match std::env::var("PLAY_LOC_PRIOR").as_deref() {
            Ok("none") => Prior::none(),
            _ => walls.clone(),
        };
        let mut evidence = Evidence::none();
        let mut prior_adam = fitting::Adam::new(prior.weights.len());
        let mut evidence_adam = fitting::Adam::new(evidence.weights.len());
        let started = std::time::Instant::now();
        for round in 1..=rounds {
            let dealt = deals(1000 + round as u64, games);
            let parts: Vec<(Vec<f64>, usize, Vec<f64>, usize)> = workers.install(|| {
                dealt
                    .par_iter()
                    .enumerate()
                    .map(|(g, json)| {
                        let mut rng = StdRng::seed_from_u64(round as u64 * 100_003 + g as u64);
                        let (begun, seen) = tracked_beliefs(json, &convention_of(json), &model, &prior, &evidence);
                        let mut prior_slope = vec![0.0; prior.weights.len()];
                        for (beliefs, opening, first, _) in &begun {
                            opening.add_slope(beliefs, *first, &mut prior_slope);
                        }
                        let mut slope = vec![0.0; evidence.weights.len()];
                        let updated: Vec<_> = seen.iter().filter_map(|(b, u, f, _, _)| Some((b, u.as_ref()?, f))).collect();
                        let chosen = examples.min(updated.len());
                        for i in rand::seq::index::sample(&mut rng, updated.len(), chosen) {
                            let (beliefs, update, first) = updated[i];
                            update.add_slope(beliefs, *first, &mut slope);
                        }
                        (prior_slope, begun.len(), slope, chosen)
                    })
                    .collect()
            });
            let mean = |slopes: Vec<(&Vec<f64>, usize)>, len: usize| -> Vec<f64> {
                let count: usize = slopes.iter().map(|s| s.1).sum();
                let mut gradient = vec![0.0; len];
                for (slope, _) in slopes {
                    for (g, s) in gradient.iter_mut().zip(slope) {
                        *g += s / count.max(1) as f64;
                    }
                }
                gradient
            };
            let prior_gradient = mean(parts.iter().map(|p| (&p.0, p.1)).collect(), prior.weights.len());
            let gradient = mean(parts.iter().map(|p| (&p.2, p.3)).collect(), evidence.weights.len());
            prior_adam.step(&mut prior.weights, &prior_gradient, rate);
            evidence_adam.step(&mut evidence.weights, &gradient, rate);
            if round % report == 0 || round == rounds {
                let (learned, _, bins) = judge(&prior, &evidence, true);
                let calibration: Vec<String> = bins
                    .iter()
                    .filter(|b| b.2 > 0)
                    .map(|b| format!("{:.3}/{:.3} ({})", b.0 / b.2 as f64, b.1 / b.2 as f64, b.2))
                    .collect();
                let (blind, _, _) = judge(&prior, &evidence.without_the_dummy(), false);
                println!(
                    "round {round} at {:.0} s: the beliefs lose {} nats, and {} with the dummy's plays showing nothing; predicted/observed {}",
                    started.elapsed().as_secs_f64(),
                    show(&learned),
                    show(&blind),
                    calibration.join(", ")
                );
                if let Ok(path) = std::env::var("PLAY_LOC_OUT") {
                    let text: Vec<String> = prior.weights.iter().chain(&evidence.weights).map(|w| w.to_string()).collect();
                    std::fs::write(&path, text.join("\n") + "\n").unwrap();
                }
            }
        }
    }

    /// Averages the weights of learner `PLAY_LEARNER` (1) over its snapshots
    /// in the directory `PLAY_SELF_SNAPSHOTS` from round `PLAY_FROM` to
    /// `PLAY_TO`, and plays the average, and each of those snapshots, against
    /// the model of `PLAY_SELF_REFERENCE` in two-table matches on the deals of
    /// self-play's tournament (`PLAY_SELF_TOURNAMENT`, 200).  At a fixed rate
    /// the weights hover about a center, and the average estimates it, as
    /// Polyak and Ruppert's does.  It prints the tricks a deal more than the
    /// reference of the average and of the snapshots on average, and the
    /// difference of the two deal by deal, with its standard error.  It
    /// writes the average to `PLAY_AVERAGE` if that is set.  It uses
    /// `PLAY_THREADS` (1) threads.
    #[test]
    #[ignore]
    fn averaged_snapshots() {
        let directory = std::env::var("PLAY_SELF_SNAPSHOTS").expect("PLAY_SELF_SNAPSHOTS");
        // The players' beliefs, for learners of the shapes that read them.
        if let Ok(path) = std::env::var("PLAY_SELF_LOCATIONS") {
            let _ = LOCATIONS.set(read_locations(&path));
        }
        // How much the beliefs are the truth, as the learner learned with.
        set_oracle(std::env::var("PLAY_SELF_ORACLE").map_or(0.0, |v| v.parse().expect("PLAY_SELF_ORACLE")));
        let learner = number("PLAY_LEARNER", 1);
        let (from, to) = (number("PLAY_FROM", 0), number("PLAY_TO", usize::MAX));
        let mut paths: Vec<(usize, String)> = std::fs::read_dir(&directory)
            .unwrap()
            .filter_map(|entry| {
                let path = entry.unwrap().path().to_str()?.to_string();
                let name = path.rsplit('/').next()?;
                let digits: String = name.strip_prefix("round-")?.chars().take_while(char::is_ascii_digit).collect();
                let round: usize = digits.parse().ok()?;
                let rest = name.strip_prefix(&format!("round-{digits}-learner-{learner}-"))?;
                (rest.ends_with(".txt") && (from..=to).contains(&round)).then_some((round, path))
            })
            .collect();
        paths.sort();
        assert!(!paths.is_empty(), "no snapshots of learner {learner} from round {from} to {to}");
        let snapshots: Vec<Model> = paths.iter().map(|(_, path)| read_model(path)).collect();
        let mut average = snapshots[0].clone();
        for (j, p) in average.network.params.iter_mut().enumerate() {
            *p = snapshots.iter().map(|s| s.network.params[j]).sum::<f64>() / snapshots.len() as f64;
        }
        if let Ok(path) = std::env::var("PLAY_AVERAGE") {
            average.network.write(&path);
        }
        let reference = read_model(&std::env::var("PLAY_SELF_REFERENCE").expect("PLAY_SELF_REFERENCE"));
        let deals: Vec<String> = crate::ai::opening_lead::candidates::opening_leads(17, number("PLAY_SELF_TOURNAMENT", 200))
            .iter()
            .map(|game| serde_json::to_string(game).unwrap())
            .collect();
        let workers = rayon::ThreadPoolBuilder::new().num_threads(number("PLAY_THREADS", 1)).build().unwrap();
        let gains = |model: &Model| -> Vec<f64> {
            workers.install(|| {
                deals
                    .par_iter()
                    .map(|json| {
                        let game: GameState<()> = serde_json::from_str(json).unwrap();
                        let convention = game.system.convention();
                        play_at_a_table(json, &convention, model, 0, Some(&reference)) as f64
                            - play_at_a_table(json, &convention, model, 1, Some(&reference)) as f64
                    })
                    .collect()
            })
        };
        let mean_and_error = |values: &[f64]| {
            let n = values.len() as f64;
            let mean = values.iter().sum::<f64>() / n;
            (mean, (values.iter().map(|v| (v - mean).powi(2)).sum::<f64>() / (n - 1.0) / n).sqrt())
        };
        let of_average = gains(&average);
        let mut of_snapshots = vec![0.0; deals.len()];
        for snapshot in &snapshots {
            for (total, gain) in of_snapshots.iter_mut().zip(gains(snapshot)) {
                *total += gain / snapshots.len() as f64;
            }
        }
        let differences: Vec<f64> = of_average.iter().zip(&of_snapshots).map(|(a, s)| a - s).collect();
        let (average_mean, average_error) = mean_and_error(&of_average);
        let (snapshots_mean, _) = mean_and_error(&of_snapshots);
        let (difference, difference_error) = mean_and_error(&differences);
        println!(
            "learner {learner}, {} snapshots from round {} to {}: against the reference, the average takes {average_mean:+.3} ({average_error:.3}) tricks a deal more, the snapshots {snapshots_mean:+.3} on average, so the average gains {difference:+.3} ({difference_error:.3})",
            snapshots.len(),
            paths[0].0,
            paths[paths.len() - 1].0,
        );
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
