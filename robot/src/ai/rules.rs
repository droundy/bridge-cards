//! The contract rules, which give natural bids their meaning from the
//! contracts within reach instead of from table entries, in auctions where
//! the opponents have only passed or doubled.
//!
//! A contract needs points between the two hands: 25 for game in notrump or
//! a major, 28 in a minor, 33 for a small slam and 37 for a grand slam, and
//! eight trumps in a suit.  Partner has promised a range of points, `lo` to
//! `hi`, and the rules weigh the contract against it:
//!
//! 1. Bid the contract when it makes opposite `lo`.
//! 2. Invite it when it makes opposite `lo` + 2 (but no more than `hi`).
//! 3. With no bid left to invite with, as when partner has just invited,
//!    bid it when it makes opposite `lo` + 1 (but no more than `hi`).  This
//!    weighs about half partner's range, so it needs a range with a top:
//!    opposite an unlimited bid, such as a strong jump shift, only rule 1
//!    bids.
//!
//! An invitation is the lowest bid in the strain, below the contract it
//! invites, that no table entry describes.  A suit is invited in only when
//! one of the partners has shown four cards in it, so that a first bid of a
//! suit, such as a jump shift, is no invitation.  The rules only bid games and
//! slams.  They invite games in notrump and the majors, and slams only in
//! notrump (the quantitative 4NT), so slams in a suit are bid only when
//! they are sure, and then after asking for aces with Blackwood or Gerber
//! (see `slam`).  Notrump is a game only at 3NT: 4NT is the quantitative
//! invitation to a slam when no table entry describes it, and no higher bid in
//! notrump places or invites anything, however far exploring has gone.
//! Part-scores come from the table, or a pass.
//!
//! Each hand counts the points that suit its part: high card points in
//! notrump, dummy points (`shcp`) in a suit partner has shown, and long
//! points (`lhcp`) in a suit of its own.  Partner counts whichever they
//! promised.  When the rules would bid in more than one strain, the hand
//! picks the one with the most points to spare, then the one whose points
//! are all of the proper kind, then a major, notrump and a minor in turn.
//!
//! The rules come after the table entries that describe a hand and before
//! those that explore, such as new suits, so that a hand explores only when
//! the rules do not know the contract: when a fit that might yet be found
//! could be better than notrump, or a slam in a suit might make opposite
//! partner's most (`reach`).  If nothing explores, the hand bids what it
//! knows.
//!
//! In a game force the rules explore themselves, with the cheapest natural
//! bid below game that is true of the hand (see `Exploration`): a raise of
//! partner's suit with the cards that make eight, a suit of one's own with
//! one more card than shown, or a new suit with four.  Bidding the cheapest
//! saves room, and makes skipping a bid a statement: a hand that bids 3S
//! over 1H-3C denies six hearts and four diamonds.  Only a raise goes past
//! 3NT, which leaves the agreed suit to play in, and only when a slam is
//! possible, opposite the most partner may have (or their minimum and two
//! more, if they have no most).  When a slam is out of reach, the hand bids
//! the game it knows instead, and explores only for a fit.  A game force
//! needs no points for game and invites none, and leaves rule 3 to
//! answering partner's invitations.  It must reach some game, so a suit of
//! six cards, or as many as the hand has shown, may be played as the game
//! with no eight-card fit known.  That is a last resort: the hand explores
//! for a fit first, if a bid that explores fits.  When none does and it bids
//! what it knows, a suit it has not yet shown comes after any other game,
//! such as 3NT, since partner has had no chance to say what they have there;
//! a suit it has shown, it may insist on.
//!
//! A bid opposite a limit bid places the contract: its bidder is captain,
//! so it shows nothing more.  A limit bid is one that does not force and
//! whose points span at most four values.  A table entry can be marked as
//! a limit bid (`Fit::limit`), and then its most points are one fewer than
//! the fewest with which the system makes a stronger bid instead.  Each bid
//! the rules make carries a `Reason`, which explains it in words.
use super::{bids_string, earlier_auctions, legal_bids, Convention, Explanation, Fit, Forcing};
use crate::Bid;
use bridge_deck::{HandValuation, Suit};
use wording::{numbered, Number, Text, Word};

/// The fewest cards that show a suit, so that a game in it can be invited.
const A_SUIT: u8 = 4;

/// The trumps a suit contract needs between the two hands.
const TRUMPS: u8 = 8;
/// The cards in a suit of its own that a hand may play as the game in a game
/// force, when no fit is known (or as many as it has shown).
const OWN_SUIT: u8 = 6;
/// How many points more than the fewest partner promised an invitation
/// counts partner for.
const INVITING: u8 = 2;
/// The same for a contract bid when no bid is left to invite it with.
const LAST_CHANCE: u8 = 1;
/// The highest bid in notrump that invites a slam, the quantitative 4NT.
const QUANTITATIVE: Bid = Bid::NT(4);
/// The most values that the points of a limit bid span.
const LIMIT_SPAN: u8 = 4;

/// The rules in words, for a table that bids by them to say of itself (see
/// `Convention::explain`), with the numbers the rules use.
pub(crate) fn explanation() -> Explanation {
    let needs = |bid| contract_points(bid).expect("a game or a slam");
    let game = needs(Bid::NT(3));
    let minor = needs(Bid::Suit(5, Suit::Clubs));
    let small = needs(Bid::NT(6));
    let grand = needs(Bid::NT(7));
    Explanation::new(
        "When the opponents have only passed",
        [
            format!(
                "A bid that no entry describes is about the contract your side can make. \
                 Between the two hands, a game in notrump or a major needs {game} points, a \
                 game in a minor {minor}, a small slam {small} and a grand slam {grand}, and \
                 a suit contract needs {TRUMPS} trumps as well."
            ),
            "Count high card points for notrump, short high card points for a suit that \
             partner has shown, and long high card points for a suit of your own. Partner \
             has shown a range of points, and what you bid goes by the fewest of them."
                .to_string(),
            "Bid the contract when your side has the points even if partner has the fewest \
             they have shown."
                .to_string(),
            format!(
                "Invite it when your side has the points if partner has {INVITING} more than \
                 that. The invitation is the lowest bid in the same strain, below the \
                 contract, that no entry describes. Games in notrump and the majors are \
                 invited, a major only when one of you has shown {A_SUIT} cards in it, \
                 and a slam only in notrump, with 4NT, if no entry describes it. Notrump is a \
                 game only at 3NT: no higher bid in notrump places or invites anything."
            ),
            format!(
                "With no bid left to invite with, as when partner has just invited, bid the \
                 contract when your side has the points if partner has {LAST_CHANCE} more \
                 than the fewest."
            ),
            "A slam in a suit is bid only when it is sure. A hand that would bid one asks \
             for aces first, whether or not it has an ace, with Blackwood's 4NT, or \
             Gerber's 4C directly over partner's notrump. The first step of the answer \
             shows none or all four, and your own aces tell which: with one you know \
             partner has none, and with none you know partner has all four. The slam is \
             bid only if no more than one ace is missing, and a grand slam only after \
             asking for kings as well, which needs a king. Blackwood's answers go up to \
             5S, so a hand asks only if it can stop at the five level in its suit after any \
             answer that leaves two aces missing: with clubs it needs two aces, with \
             diamonds one, and with a major none. A hand that cannot ask bids the game \
             instead. A slam in notrump is different, since its points are high card \
             points: a hand without an ace bids the small slam without asking."
                .to_string(),
            format!(
                "When your side has forced to game, nothing is invited, and the cheapest bid \
                 that is true explores, with a raise of partner's suit that makes {TRUMPS} \
                 trumps, a suit of your own one longer than shown, or a new suit of {A_SUIT} \
                 cards, so that skipping a bid denies it. A suit of {OWN_SUIT} cards, or as \
                 many as you have shown, may be played as the game without a fit, but you \
                 explore first if you can. When you cannot, a suit you have shown you may \
                 insist on, and one you have not shown comes after any other game, such \
                 as 3NT."
            ),
            format!(
                "Nothing is bid for a \
                 part-score: that is a call an entry describes, or a pass. When partner's \
                 last bid was a limit bid, one that is not forcing and whose points span at \
                 most {LIMIT_SPAN} values, you are the captain: placing the contract shows \
                 nothing more about your hand."
            ),
        ],
    )
}

mod slam;
use slam::aces_to_ask;
use slam::Asked;
pub(crate) use slam::{asking_fits, Asking};

/// The ways of counting a hand's points.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Points {
    /// High card points, for notrump.
    Hcp,
    /// High card points and long points, for a suit of one's own.
    Lhcp,
    /// Dummy points, for supporting partner's suit.
    Shcp,
}

impl Points {
    const ALL: [Points; 3] = [Points::Hcp, Points::Lhcp, Points::Shcp];
    pub(crate) fn of(self, v: &HandValuation) -> u8 {
        match self {
            Points::Hcp => v.hcp,
            Points::Lhcp => v.lhcp,
            Points::Shcp => v.shcp,
        }
    }
    fn set(self, v: &mut HandValuation, points: u8) {
        match self {
            Points::Hcp => v.hcp = points,
            Points::Lhcp => v.lhcp = points,
            Points::Shcp => v.shcp = points,
        }
    }
    /// The points a range describes: the way of counting it bounds most
    /// narrowly, if it bounds any.
    fn described(min: &HandValuation, max: &HandValuation) -> Option<Points> {
        Points::ALL
            .iter()
            .copied()
            .filter(|&p| p.of(min) > 0 || p.of(max) < p.of(&HandValuation::MAX))
            .min_by_key(|&p| p.of(max).saturating_sub(p.of(min)))
    }
    fn name(self) -> &'static str {
        match self {
            Points::Hcp => "high card points",
            Points::Lhcp => "points",
            Points::Shcp => "dummy points",
        }
    }
}

/// The kinds of contract, from the smallest.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Kind {
    PartScore,
    Game,
    SmallSlam,
    GrandSlam,
}

impl Kind {
    pub(crate) fn of(bid: Bid) -> Self {
        match bid {
            Bid::Suit(7, _) | Bid::NT(7) => Kind::GrandSlam,
            Bid::Suit(6, _) | Bid::NT(6) => Kind::SmallSlam,
            Bid::NT(level) if level >= 3 => Kind::Game,
            Bid::Suit(level, Suit::Hearts | Suit::Spades) if level >= 4 => Kind::Game,
            Bid::Suit(level, _) if level >= 5 => Kind::Game,
            _ => Kind::PartScore,
        }
    }
    pub(crate) fn name(self) -> &'static str {
        match self {
            Kind::PartScore => "part-score",
            Kind::Game => "game",
            Kind::SmallSlam => "small slam",
            Kind::GrandSlam => "grand slam",
        }
    }
    fn above(self) -> Option<Kind> {
        match self {
            Kind::PartScore => Some(Kind::Game),
            Kind::Game => Some(Kind::SmallSlam),
            Kind::SmallSlam => Some(Kind::GrandSlam),
            Kind::GrandSlam => None,
        }
    }
    fn below(self) -> Option<Kind> {
        match self {
            Kind::PartScore => None,
            Kind::Game => Some(Kind::PartScore),
            Kind::SmallSlam => Some(Kind::Game),
            Kind::GrandSlam => Some(Kind::SmallSlam),
        }
    }
}

/// The points a partnership needs between them for a game or slam.
pub(crate) fn contract_points(bid: Bid) -> Option<u8> {
    match (Kind::of(bid), bid) {
        (Kind::PartScore, _) => None,
        (Kind::Game, Bid::Suit(_, Suit::Clubs | Suit::Diamonds)) => Some(28),
        (Kind::Game, _) => Some(25),
        (Kind::SmallSlam, _) => Some(33),
        (Kind::GrandSlam, _) => Some(37),
    }
}

/// What a partnership needs between them for a game or slam, with the
/// points counted as high card points.
pub(crate) fn contract_needs(bid: Bid) -> Option<HandValuation> {
    let mut needs = HandValuation::MIN.with_hcp(contract_points(bid)?);
    if let Some(suit) = trump(bid) {
        needs.length[suit] = TRUMPS;
    }
    Some(needs)
}

/// The trump suit of a bid, which is `None` for notrump.
fn trump(bid: Bid) -> Option<Suit> {
    match bid {
        Bid::Suit(_, suit) => Some(suit),
        _ => None,
    }
}

/// Have the opponents of the player making the last bid of `auction` (as
/// `bids_string` writes it) bid nothing, only passing or doubling?  A
/// double takes no room, and where the booklet gives a bid another meaning
/// after one, such as 2NT as a limit raise, the table says so.
pub(crate) fn uncontested(auction: &str) -> bool {
    auction
        .split(' ')
        .rev()
        .skip(1)
        .step_by(2)
        .all(|call| matches!(call, "P" | "X" | "XX"))
}

/// Which of the three rules makes a bid.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Rule {
    /// The contract makes opposite the least partner has promised.
    Sure,
    /// The contract makes opposite two more than that, so invite it.
    Invite,
    /// No bid is left to invite with, and the contract makes opposite one
    /// more than partner promised.
    LastChance,
    /// The side has forced to game, which has promised the points for it.
    Forced,
}

/// Why the contract rules make a bid, to explain it.
#[derive(Clone, Debug)]
pub struct Reason {
    pub(crate) rule: Rule,
    /// The contract the bid places, or invites.
    pub(crate) contract: Bid,
    /// The points that contract needs between the two hands.
    pub(crate) needs: u8,
    /// The points partner promised, and the most.
    pub(crate) promised: (u8, u8),
    /// The points the rule counts partner for.
    pub(crate) partner: u8,
    /// How this hand's points are counted.
    pub(crate) points: Points,
    /// Is partner's last bid a limit bid, so that this places the contract
    /// as captain and shows nothing?
    pub(crate) captain: bool,
}

impl Reason {
    /// What the bid is for, in words that are the same in every auction, as
    /// in "Invites a game, which needs 25 dummy points between you".
    pub(crate) fn aim(&self) -> Text {
        let does = match self.rule {
            Rule::Invite => "Invites a",
            Rule::Sure | Rule::LastChance | Rule::Forced => "To play: a",
        };
        let kind = Kind::of(self.contract).name();
        let points = self.points.name();
        Text::from(format!(
            "{does} {kind}, which needs {} {points} between you",
            self.needs
        ))
    }
    /// Does the bid show partner nothing about the hand?
    pub(crate) fn shows_nothing(&self) -> bool {
        self.captain && self.rule != Rule::Invite
    }
    /// The reason in words, as in "3NT needs 25 high card points between
    /// you, which you have even if partner has only the 15 they promised."
    pub(crate) fn text(&self) -> Text {
        let partner = numbered("partner", Number::Singular);
        let you = numbered("you", Number::Plural);
        let (lo, hi) = self.promised;
        let needs = Text::from(self.contract)
            + Kind::of(self.contract).name()
            + "needs"
            + self.needs
            + self.points.name()
            + "between you,";
        let opposite = match self.rule {
            Rule::Sure => {
                Text::from("which")
                    + (you + Word::HAS + "even if")
                    + (partner + Word::HAS + "only the")
                    + lo
                    + "they promised."
            }
            Rule::Invite => {
                Text::from("which")
                    + (you + Word::HAS + "if")
                    + (partner + Word::HAS)
                    + self.partner
                    + format!(", {INVITING} more than they promised, so invite it.")
            }
            Rule::LastChance => {
                Text::from("which")
                    + (you + Word::HAS + "if")
                    + (partner + Word::HAS)
                    + self.partner
                    + "of the"
                    + Text::from(format!("{lo}–{hi}"))
                    + "they promised, and there is no room to invite."
            }
            Rule::Forced => Text::from("which you have, since you have forced to game."),
        };
        let mut text = needs + opposite;
        if self.shows_nothing() {
            text += "Partner made a limit bid, so you are captain: this places the contract and shows nothing more.";
        }
        text
    }
}

/// How the rules choose a bid for a hand: the strains it could play the
/// same kind of contract in, and what would leave the contract unknown.
#[derive(Clone, Debug)]
pub struct Choice {
    /// This bid's strain first, then the others.
    rivals: Vec<Rival>,
    /// For a game in a suit, or in notrump in a game force, the points a
    /// slam needs, which the rules cannot invite, so that a hand that might
    /// make one explores instead.
    slam: Option<u8>,
    /// Is this a slam that waits for an ask for aces (`Some(false)`) or for
    /// kings (`Some(true)`)?  See `slam`.  A slam in a suit always waits.  In
    /// notrump a hand without an ace cannot ask, so it bids a small slam on
    /// its points, but not a grand slam.
    ask_first: Option<bool>,
    /// What a hand needs to ask with before the slam this bid is, or the
    /// slam it is the game for, if it can be asked about.  See `Asks`.
    asks: Option<Asks>,
    /// For a slam bid once the aces are known, with the first step of the
    /// answer ("none or four") given: the aces a hand needs, unless it has
    /// none at all.  A hand with an ace knows that step means none, and a
    /// hand with none knows that partner has all four, so only a hand in
    /// between lacks the aces for the slam.
    aces_or_none: Option<u8>,
    /// For a game in a game force that is allowed with fewer trumps than
    /// make eight, because it must reach some game: the suit, the trumps
    /// that would make a fit, and whether the hand has shown the suit.  A
    /// hand with fewer trumps explores first if it can (see `last_resort`),
    /// and in a suit it has not shown it also prefers any other game (see
    /// `comes_last`).
    unsupported: Option<(Suit, u8, bool)>,
}

/// The ask that comes before a slam, and what a hand without the cards to ask
/// with does instead.
#[derive(Clone, Debug)]
struct Asks {
    /// Does it ask for kings (`true`) or aces (`false`)?
    kings: bool,
    /// The kings, or aces, a hand needs to ask.
    needed: u8,
    /// The most the bid takes of a hand without them, which is not capped by
    /// the points with which it would ask: for a small slam that might be a
    /// grand slam, or a game that might be a slam.  Such a hand bids the
    /// small slam in notrump, or the game in a suit, instead.
    uncapped: HandValuation,
}

impl Asks {
    /// Does `hand` lack the cards to ask with?
    fn lacked_by(&self, hand: &HandValuation) -> bool {
        let held = if self.kings { hand.kings } else { hand.aces };
        held < self.needed
    }
}

/// A strain the hand might bid in instead, to choose among them.
#[derive(Clone, Debug)]
pub struct Rival {
    /// The trump suit, if any, with the fewest and most of them partner has
    /// promised.
    trumps: Option<(Suit, u8, u8)>,
    /// Can the rules bid in this strain, rather than a table entry?
    available: bool,
    /// The points the contract needs.
    needs: u8,
    /// How this hand's points are counted for it.
    points: Points,
    /// Are partner's promised points of the kind the contract counts?
    proper: bool,
    /// The order of preference when all else is equal: a major, notrump, a
    /// minor.
    order: u8,
}

impl Rival {
    /// How the hand rates bidding in this strain, best highest.
    fn rating(&self, hand: &HandValuation, partner: u8) -> (i16, bool, u8) {
        // A fit in a suit partner has not shown, with three cards or fewer
        // here, would be partner's long suit, where this hand's long suits
        // make no tricks, so it counts only its high cards.
        let points = match self.trumps {
            Some((suit, ..)) if self.points == Points::Lhcp && hand.length[suit] < 4 => Points::Hcp,
            _ => self.points,
        };
        let spare = points.of(hand) as i16 + partner as i16 - self.needs as i16;
        (spare, self.proper, self.order)
    }
    /// Is there a fit in this strain, as far as partner has promised, and
    /// might there be one, if partner has the most they could?
    fn fit(&self, hand: &HandValuation) -> (bool, bool) {
        match self.trumps {
            Some((suit, least, most)) => (
                hand.length[suit] + least >= TRUMPS,
                hand.length[suit] + most >= TRUMPS,
            ),
            None => (true, true),
        }
    }
}

/// Does a hand fit what the contract rules make of a bid?  It must lie
/// within the bounds, and no other strain where the rules can bid may rate
/// better.  If `unsure_explores`, the rules must also know the contract: no
/// strain where a fit might yet be found rates as well, no slam that
/// the rules cannot invite is within `reach`, and a game with no fit in this
/// hand is not a last resort.  Otherwise the hand explores,
/// with the rules' own bids in a game force or the table entries that come
/// after the rules, and only when none of them fits does it bid what it
/// knows.
pub(crate) fn contracted_fits(
    min: &HandValuation,
    max: &HandValuation,
    reason: &Reason,
    choice: &Choice,
    hand: &HandValuation,
    unsure_explores: bool,
) -> bool {
    // A slam in a suit waits for the ask for aces, whether or not the hand
    // has an ace to ask with.  A slam in notrump waits only for a hand with an
    // ace: one without bids a small slam on its points, but a grand slam
    // waits for the ask even then, since it is never bid unasked.
    let grand = Kind::of(reason.contract) == Kind::GrandSlam;
    let in_suit = trump(reason.contract).is_some();
    let waits = match choice.ask_first {
        Some(false) => in_suit || hand.aces > 0 || grand,
        Some(true) => true,
        None => false,
    };
    // A hand without the cards to ask with is not held back by the points
    // with which it would have asked.  It still bids a small slam on those
    // points, rather than explore for a grand slam.  But it may explore for
    // a slam instead of bidding a game, and does if a bid of the table fits.
    let lacking = choice.asks.as_ref().filter(|asks| asks.lacked_by(hand));
    let max = lacking.map_or(max, |asks| &asks.uncapped);
    let can_ask = lacking.is_none() || Kind::of(reason.contract) < Kind::SmallSlam;
    let short_of_aces = choice
        .aces_or_none
        .is_some_and(|aces| hand.aces != 0 && hand.aces < aces);
    // A game with no fit known, and none in this hand, is bid only when
    // nothing else fits, not instead of exploring.
    let explores_first = unsure_explores && last_resort(choice, hand);
    if waits || short_of_aces || explores_first || hand.exceeds(*max) || min.exceeds(*hand) {
        return false;
    }
    let (lo, hi) = reason.promised;
    let (ours, rivals) = choice.rivals.split_first().unwrap();
    if unsure_explores
        && can_ask
        && choice
            .slam
            .is_some_and(|slam| ours.points.of(hand) + reach(lo, hi) >= slam)
    {
        return false;
    }
    // A fit that might yet be found matters only on the way to notrump: one
    // that is known, as in partner's suit, is not given up to look for it.
    let looking = unsure_explores && ours.trumps.is_none();
    // A known fit rules out a slam in notrump even though the rules cannot
    // bid a slam in the suit, which waits for the table or a sure slam.
    let notrump_slam = ours.trumps.is_none() && Kind::of(reason.contract) > Kind::Game;
    let rating = ours.rating(hand, lo);
    rivals.iter().all(|r| match r.fit(hand) {
        (true, _) if r.available || notrump_slam => r.rating(hand, lo) < rating,
        (false, true) if looking => r.rating(hand, lo) < rating,
        _ => true,
    })
}

/// Is the bid a last resort for `hand`: a game in a game force that has no
/// eight-card fit known, played in a suit that the hand has only the cards
/// for that the force allows?  The hand explores instead, if a bid that
/// explores fits.
fn last_resort(choice: &Choice, hand: &HandValuation) -> bool {
    choice
        .unsupported
        .is_some_and(|(suit, trumps, _)| hand.length[suit] < trumps)
}

/// When nothing explores and `hand` bids what it knows, does this bid come
/// after every other contract that fits?  That is a last resort in a suit the
/// hand has not shown, where partner has had no chance to say what they have.
/// A suit it has shown, the hand may insist on.
pub(crate) fn comes_last(choice: &Choice, hand: &HandValuation) -> bool {
    last_resort(choice, hand) && choice.unsupported.is_some_and(|(_, _, shown)| !shown)
}

/// The most points partner might have, for a slam: the most they promised,
/// or their minimum and two more if they have no most, as after a strong
/// jump shift.
fn reach(lo: u8, hi: u8) -> u8 {
    if hi < HandValuation::MAX.hcp {
        hi
    } else {
        lo + INVITING
    }
}

/// What a bid that explores says about its suit.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Explore {
    /// Partner's suit, with the cards that make eight.
    Raise,
    /// A suit of one's own, with one more card than shown.
    Rebid,
    /// A suit neither has shown, with four cards.
    NewSuit,
}

/// Why the rules explore with a bid in a game force, and what decides it
/// for a hand.  The robot explores only when the rules do not know the
/// contract, with the cheapest bid that is true of its hand, so a bid
/// denies the cheaper ones (in its bounds).
#[derive(Clone, Debug)]
pub struct Exploration {
    explore: Explore,
    suit: Suit,
    /// The cards in `suit` that the bid shows.
    length: u8,
    /// Does the bid go past 3NT, as only a raise does, when a slam in its
    /// suit is possible?
    past_notrump: bool,
    /// The cheaper bids past 3NT, each suit with the cards it would show
    /// and how its points count, which a hand that might make a slam would
    /// bid instead.
    cheaper_past_notrump: Vec<(Suit, u8, Points)>,
    /// The points with which a slam is possible, within `reach`, counted as
    /// dummy points for a raise and long points for a suit of one's own.
    slam: u8,
    /// What the rules would place instead, which a hand that fits them
    /// bids rather than explore.
    placements: Vec<Convention>,
}

impl Explore {
    /// How a hand counts its points for a slam in the suit.
    fn points(self) -> Points {
        match self {
            Explore::Raise => Points::Shcp,
            _ => Points::Lhcp,
        }
    }
}

impl Exploration {
    /// The exploration in words.
    pub(crate) fn text(&self) -> Text {
        let mut text =
            Text::from("In a game force, the cheapest bid that is true explores, so this shows")
                + Text::from(format!("{}+", self.length))
                + self.suit
                + match self.explore {
                    Explore::Raise => ", a fit with partner.",
                    Explore::Rebid => ", one more than shown.",
                    Explore::NewSuit => ".",
                };
        if self.past_notrump {
            text += "It goes past 3NT, so a slam is possible.";
        }
        text + "It denies the cheaper bids."
    }
}

/// Does a hand fit a bid that explores?  It must lie within the bounds,
/// which deny the cheaper bids, and the rules must not know the contract.
/// A raise past 3NT needs a slam to be possible, and with one possible,
/// the hand makes a cheaper raise past 3NT if it can.
pub(crate) fn explored_fits(
    min: &HandValuation,
    max: &HandValuation,
    exploration: &Exploration,
    hand: &HandValuation,
) -> bool {
    if hand.exceeds(*max) || min.exceeds(*hand) {
        return false;
    }
    let slam = |points: Points| points.of(hand) >= exploration.slam;
    if exploration.past_notrump && !slam(exploration.explore.points()) {
        return false;
    }
    let cheaper =
        |&(suit, length, points): &(Suit, u8, Points)| hand.length[suit] >= length && slam(points);
    if exploration.cheaper_past_notrump.iter().any(cheaper) {
        return false;
    }
    exploration.placements.iter().all(|p| !p.fits(hand))
}

/// Is `meaning` a limit bid: one that does not force, and whose points span
/// at most four values?
fn is_limit_bid(meaning: &Convention) -> bool {
    if meaning.is_forcing() != Forcing::Passable {
        return false;
    }
    let (min, max) = meaning.bounds();
    Points::described(&min, &max).is_some_and(|p| p.of(&max) < p.of(&min) + LIMIT_SPAN)
}

/// What the player has promised, as bounds combined from the meanings of
/// their calls.
struct Promise {
    min: HandValuation,
    max: HandValuation,
}

impl Promise {
    /// The least and most points promised, counting whichever way the calls
    /// counted them.
    fn points(&self) -> (u8, u8) {
        let lo = Points::ALL.iter().map(|p| p.of(&self.min)).max().unwrap();
        let hi = Points::ALL.iter().map(|p| p.of(&self.max)).min().unwrap();
        (lo, hi.max(lo))
    }
}

/// The contract this side already plays after `history`, if it does.
fn ours(history: &[Bid]) -> Option<Bid> {
    history
        .iter()
        .rposition(|b| b.is_contract())
        .and_then(|i| (history.len() - i).is_multiple_of(2).then(|| history[i]))
}

/// Where the rules stand in one strain after `history`.
struct Strain<'a> {
    convention: &'a Convention,
    history: &'a [Bid],
    strain: Option<Suit>,
    /// A contract partner has just invited.
    invited: Option<Bid>,
    /// The contract this side already plays, if it does.
    ours: Option<Bid>,
    /// Has this side forced to game, so that no game is invited?
    game_forced: bool,
    /// The most cards that either partner has promised in the strain's
    /// suit.
    shown: u8,
}

impl Strain<'_> {
    fn legal(&self) -> impl Iterator<Item = Bid> + '_ {
        legal_bids(self.history)
            .into_iter()
            .filter(move |b| b.is_contract() && trump(*b) == self.strain)
    }
    /// The contract of `kind` in this strain that is played, if it is still
    /// legal: 3NT, four of a major or five of a minor for game, and six and
    /// seven for slams.  Other bids of the kind, such as 4NT, are not.
    fn contract(&self, kind: Kind) -> Option<Bid> {
        let level = match (kind, self.strain) {
            (Kind::PartScore, _) => return None,
            (Kind::Game, None) => 3,
            (Kind::Game, Some(Suit::Hearts | Suit::Spades)) => 4,
            (Kind::Game, Some(_)) => 5,
            (Kind::SmallSlam, _) => 6,
            (Kind::GrandSlam, _) => 7,
        };
        let bid = match self.strain {
            Some(suit) => Bid::Suit(level, suit),
            None => Bid::NT(level),
        };
        self.legal().any(|b| b == bid).then_some(bid)
    }
    /// The bid that places a contract of `kind`, unless this side already
    /// plays one in this strain, when passing places it.
    fn placing(&self, kind: Kind) -> Option<Bid> {
        let played = self
            .ours
            .is_some_and(|c| trump(c) == self.strain && Kind::of(c) >= kind);
        self.contract(kind).filter(|_| !played)
    }
    /// Does a table entry describe `bid` here?
    fn conventional(&self, bid: Bid) -> bool {
        let mut bids = self.history.to_vec();
        bids.push(bid);
        self.convention.conventional(&bids)
    }
    /// The bid that invites `contract`: the lowest of the kind below it that
    /// no table entry describes, other than one that places a game.  As in
    /// the booklet, a game is invited only in notrump or a major, and a slam
    /// only in notrump once this side has bid notrump (as with the
    /// quantitative 4NT, also after Stayman or a transfer), since 4NT is
    /// otherwise Blackwood, and then only with 4NT itself.  Partner's own invitation is answered, not
    /// invited again, and in a game force no game is invited.
    fn invitation(&self, contract: Bid) -> Option<Bid> {
        let kind = Kind::of(contract);
        let in_notrump = self
            .history
            .iter()
            .rev()
            .skip(1)
            .step_by(2)
            .any(|b| matches!(b, Bid::NT(_)));
        // Nothing is invited that this side has already reached.
        let reached = self.ours.is_some_and(|c| Kind::of(c) >= kind);
        let minor = matches!(self.strain, Some(Suit::Clubs | Suit::Diamonds));
        // Nor in a suit that neither partner has shown: a first bid of a
        // suit says what the table makes it say, or nothing.
        let unshown = self.strain.is_some() && self.shown < A_SUIT;
        if reached
            || unshown
            || self.invited == Some(contract)
            || (kind == Kind::Game && (minor || self.game_forced))
            || (kind > Kind::Game && (self.strain.is_some() || !in_notrump))
        {
            return None;
        }
        let below = kind.below()?;
        let placed = self.placing(below).filter(|_| below >= Kind::Game);
        self.legal()
            .take_while(|&b| b != contract)
            .filter(|&b| Kind::of(b) == below && Some(b) != placed)
            .find(|&b| !self.conventional(b))
            // A bid in notrump past 4NT is no game, and invites nothing: with
            // 4NT taken, as when exploring went past 3NT, a slam is not
            // invited.
            .filter(|&b| kind != Kind::SmallSlam || b <= QUANTITATIVE)
    }
}

impl Convention {
    /// Does a table entry, rather than a default, describe the last of
    /// `bids`?
    fn conventional(&self, bids: &[Bid]) -> bool {
        let auction = bids_string(bids);
        self.candidates(&auction).into_iter().any(|i| {
            matches!(
                &self.entries()[i],
                Convention::Simple { regex, .. } | Convention::Rule { regex, .. }
                    if regex.is_match(&auction)
            )
        })
    }

    /// What the player making the last of `bids` has promised before it,
    /// counting `skip` = 4, or what their partner has, counting `skip` = 2.
    fn promise(&self, bids: &[Bid], skip: usize) -> Promise {
        let mut promise = Promise {
            min: HandValuation::MIN,
            max: HandValuation::MAX,
        };
        for earlier in earlier_auctions(bids, skip) {
            let (min, max) = self.valuation(earlier);
            promise.min = promise.min.max(min);
            promise.max = promise.max.min(max);
        }
        promise
    }

    /// What the contract rules make of the last of `bids`, a natural bid in
    /// an auction where the opponents have only passed or doubled, if they make it at
    /// all: a contract it places or invites, or a bid that explores.
    pub(crate) fn contract_meaning(
        &self,
        bids: &[Bid],
        the_name: &'static str,
    ) -> Option<Convention> {
        self.rules_meaning(bids, the_name).flatten()
    }

    /// Do the contract rules decide what the last of `bids` means, even if it
    /// means nothing, as when partner has asked for aces?  Then no natural
    /// bid of the table does.
    pub(crate) fn rules_decide(&self, bids: &[Bid]) -> bool {
        self.rules_meaning(bids, "").is_some()
    }

    /// What the contract rules make of the last of `bids`, when they decide
    /// it: `Some(None)` when it means nothing.
    fn rules_meaning(&self, bids: &[Bid], the_name: &'static str) -> Option<Option<Convention>> {
        let (&bid, history) = bids.split_last()?;
        if !bid.is_contract() || !uncontested(&bids_string(bids)) {
            return None;
        }
        let asked = self.asked(history);
        if let Some(meaning) = self.slam_meaning(bids, asked.as_ref()) {
            return Some(meaning);
        }
        self.contracted_meaning(bids, the_name, asked.as_ref())
            .or_else(|| {
                let ask = self
                    .ace_ask(history, true)
                    .filter(|ask| ask.bid(false) == bid)?;
                asked.is_none().then(|| self.asking(history, ask, None))?
            })
            .or_else(|| self.explored_meaning(bids))
            .map(Some)
    }

    /// Has the side making the last of `bids` forced to game, and not yet
    /// bid one?
    fn game_forced(&self, bids: &[Bid]) -> bool {
        let history = &bids[..bids.len().saturating_sub(1)];
        !ours(history).is_some_and(|c| Kind::of(c) >= Kind::Game)
            && earlier_auctions(bids, 2)
                .chain(earlier_auctions(bids, 4))
                .any(|earlier| self.check_forcing(earlier) == Forcing::GameForcing)
    }

    /// What the last of `bids` shows, if it explores: a suit bid below game,
    /// the cheapest in its suit, in a game force that has not reached game
    /// (see `Exploration`).
    fn explored_meaning(&self, bids: &[Bid]) -> Option<Convention> {
        let (&bid, history) = bids.split_last()?;
        if !self.game_forced(bids) {
            return None;
        }
        let partner = self.promise(bids, 2);
        let mine = self.promise(bids, 4);
        let legal = legal_bids(history);
        // What a bid would show, if it explores.
        let explores = |b: Bid| {
            let Bid::Suit(level, suit) = b else {
                return None;
            };
            let cheapest = legal.iter().find(|c| trump(**c) == Some(suit)) == Some(&b);
            if !cheapest || Kind::of(b) != Kind::PartScore {
                return None;
            }
            let (shown, theirs) = (mine.min.length[suit], partner.min.length[suit]);
            let (explore, length) = if shown >= A_SUIT && shown >= theirs {
                (Explore::Rebid, shown + 1)
            } else if theirs >= 3 {
                (Explore::Raise, TRUMPS.saturating_sub(theirs).max(shown))
            } else {
                (Explore::NewSuit, A_SUIT)
            };
            let below_notrump = level < 4;
            (length <= 13 && (below_notrump || explore == Explore::Raise)).then_some((
                suit,
                explore,
                length,
                below_notrump,
            ))
        };
        let (suit, explore, length, below_notrump) = explores(bid)?;

        let (mut min, mut max) = (HandValuation::MIN, HandValuation::MAX);
        min.length[suit] = length;
        let mut cheaper_past_notrump = Vec::new();
        for &cheaper in legal.iter().take_while(|&&c| c != bid) {
            let mut auction = history.to_vec();
            auction.push(cheaper);
            if self.conventional(&auction) {
                continue;
            }
            match explores(cheaper) {
                Some((s, _, n, true)) => max.length[s] = max.length[s].min(n - 1),
                Some((s, e, n, false)) => cheaper_past_notrump.push((s, n, e.points())),
                None => (),
            }
        }
        if min.exceeds(max) {
            return None;
        }
        // What the rules would place instead.
        let placements = legal
            .iter()
            .filter(|&&p| Kind::of(p) >= Kind::Game)
            .filter_map(|&p| {
                let mut auction = history.to_vec();
                auction.push(p);
                match self.refine(&auction) {
                    Some(c @ Convention::Contracted { .. }) => Some(c),
                    _ => None,
                }
            })
            .collect();
        let (lo, hi) = partner.points();
        let small = contract_points(Bid::NT(6))?;
        let exploration = Exploration {
            explore,
            suit,
            length,
            past_notrump: !below_notrump,
            cheaper_past_notrump,
            slam: small.saturating_sub(reach(lo, hi)),
            placements,
        };
        Some(Convention::Explored {
            the_name: match explore {
                Explore::Raise => "Raise",
                Explore::Rebid => "Rebid",
                Explore::NewSuit => "New suit",
            },
            min,
            max,
            exploration,
        })
    }

    /// The contract that the last of `bids` places or invites, if any.
    fn contracted_meaning(
        &self,
        bids: &[Bid],
        the_name: &'static str,
        asked: Option<&Asked>,
    ) -> Option<Convention> {
        let (&bid, history) = bids.split_last()?;
        let partner = self.promise(bids, 2);
        let mine = self.promise(bids, 4);
        let (lo, hi) = partner.points();
        // What partner's last call means, if they have made one.
        let last = history
            .len()
            .checked_sub(2)
            .and_then(|i| self.refine(&history[..=i]));
        let captain = last.as_ref().is_some_and(is_limit_bid);
        let invited = match &last {
            Some(Convention::Contracted { reason, .. }) if reason.rule == Rule::Invite => {
                Some(reason.contract)
            }
            _ => None,
        };
        let ours = ours(history);
        let game_forced = self.game_forced(bids);
        let here = |strain: Option<Suit>| Strain {
            convention: self,
            history,
            strain,
            invited,
            ours,
            game_forced,
            shown: strain.map_or(0, |suit| {
                partner.min.length[suit].max(mine.min.length[suit])
            }),
        };
        let strain = here(trump(bid));

        // Whether the bid places a contract, with the rule, or invites one.
        // A slam in a suit is bid only when it is sure, and otherwise a
        // contract that could have been invited must be sure too.
        let kind = Kind::of(bid);
        // Rule 3 weighs half partner's range, which an unlimited one lacks,
        // and stands in for an invitation, which a game force does without
        // by exploring, unless partner has just invited.
        let last_chance = if invited.is_some() || (hi < HandValuation::MAX.hcp && !game_forced) {
            Rule::LastChance
        } else {
            Rule::Sure
        };
        let (contract, rule) = if strain.placing(kind) == Some(bid) && kind >= Kind::Game {
            let suit_slam = kind > Kind::Game && strain.strain.is_some();
            if suit_slam || strain.invitation(bid).is_some() {
                (bid, Rule::Sure)
            } else {
                (bid, last_chance)
            }
        } else {
            let next = kind.above().and_then(|k| strain.contract(k))?;
            if strain.invitation(next) != Some(bid) {
                return None;
            }
            (next, Rule::Invite)
        };
        let needs = contract_points(contract)?;
        // A game force has promised the points for game.
        let rule = if game_forced && Kind::of(contract) == Kind::Game && rule != Rule::Invite {
            Rule::Forced
        } else {
            rule
        };
        let counted = |rule| match rule {
            Rule::Sure | Rule::Forced => lo,
            Rule::Invite => hi.min(lo + INVITING),
            Rule::LastChance => hi.min(lo + LAST_CHANCE),
        };
        let least = if rule == Rule::Forced {
            0
        } else {
            needs.saturating_sub(counted(rule))
        };
        // The hand goes no further than this: placing a contract, it would
        // otherwise go on to the next kind, and inviting, it would place
        // the contract itself.
        let most = if rule == Rule::Invite {
            needs.checked_sub(lo + 1)?
        } else if rule == Rule::Forced || asked.is_some_and(|a| a.aces.is_some()) {
            // Short of the fit that a slam needs, a game force still bids
            // game, and short of the aces a grand slam needs, a small slam
            // is bid; with them, the higher contract is the higher bid.
            u8::MAX
        } else {
            match Kind::of(contract).above().and_then(|k| strain.contract(k)) {
                Some(next) => {
                    let next_rule = if strain.invitation(next).is_some() {
                        Rule::Invite
                    } else if strain.strain.is_some() {
                        Rule::Sure
                    } else {
                        last_chance
                    };
                    contract_points(next)?.checked_sub(counted(next_rule) + 1)?
                }
                None => u8::MAX,
            }
        };
        if least > most {
            return None;
        }

        // Partner's suits, which this hand would be dummy in: those they have
        // shown three or more of, and more than this hand has.
        let shown = |suit: Suit| {
            partner.min.length[suit] >= 3 && partner.min.length[suit] > mine.min.length[suit]
        };
        let points_for = |strain: Option<Suit>| match strain {
            None => Points::Hcp,
            Some(suit) if shown(suit) => Points::Shcp,
            Some(_) => Points::Lhcp,
        };
        let points = points_for(strain.strain);
        let (mut min, mut max) = (HandValuation::MIN, HandValuation::MAX);
        let mut unsupported = None;
        points.set(&mut min, least);
        points.set(&mut max, most.min(points.of(&HandValuation::MAX)));
        if let Some(suit) = strain.strain {
            min.length[suit] = TRUMPS.saturating_sub(partner.min.length[suit]);
            // A game force must reach game, so with no fit known it may be
            // played in a suit of this hand's own with six cards, or as many
            // as it has shown.
            if rule == Rule::Forced {
                let shown = mine.min.length[suit];
                let own = shown.max(OWN_SUIT);
                if own < min.length[suit] {
                    unsupported = Some((suit, min.length[suit], shown >= A_SUIT));
                    min.length[suit] = own;
                }
            }
        }

        // A slam waits for the aces, and a grand slam for the kings, if there
        // is a way to ask for them.
        let mut ask_first = None;
        let mut aces_or_none = None;
        let king_ask = asked.is_some_and(|a| {
            a.aces.is_some() && a.kings.is_none() && legal_bids(history).contains(&a.ask.bid(true))
        });
        let slam = Kind::of(contract);
        // The ask for aces that comes before a slam in this strain, if one is
        // possible: before the aces are known, for a slam or the game under it.
        let ace_ask = (asked.is_none() && slam >= Kind::Game && rule != Rule::Invite)
            .then(|| self.ace_ask(history, strain.strain.is_some()))
            .flatten();
        if slam >= Kind::SmallSlam && rule != Rule::Invite {
            match asked {
                None => ask_first = ace_ask.map(|_| false),
                Some(asked) => {
                    let all = if slam == Kind::GrandSlam { 4 } else { 3 };
                    match asked.aces {
                        Some(0) => aces_or_none = Some(all),
                        shown => min.aces = all - shown.unwrap_or(0).min(all),
                    }
                    if slam == Kind::GrandSlam {
                        match asked.kings {
                            Some(kings) => min.kings = 4 - kings.min(4),
                            None => ask_first = king_ask.then_some(true),
                        }
                    }
                }
            }
        }
        // What a hand needs to ask with, if it asks before the contract:
        // for a small slam, for aces or kings, and for a game in a suit, for
        // aces before a slam, if it takes more than a hand may lack.
        let asks = match slam {
            _ if rule == Rule::Invite => None,
            Kind::SmallSlam => ace_ask
                .map(|ask| (false, aces_to_ask(ask, contract)))
                .or(king_ask.then_some((true, 1))),
            Kind::Game => ace_ask
                .zip(strain.contract(Kind::SmallSlam))
                .map(|(ask, slam)| (false, aces_to_ask(ask, slam)))
                .filter(|&(_, needed)| needed > 0),
            _ => None,
        }
        .map(|(kings, needed)| {
            let mut uncapped = max;
            points.set(&mut uncapped, points.of(&HandValuation::MAX));
            Asks {
                kings,
                needed,
                uncapped,
            }
        });

        // The strains the hand could play the same kind of contract in.
        let rival = |s: Option<Suit>, contract: Bid, available: bool| {
            let points = points_for(s);
            // Partner's proper points are those of the part they play, and
            // high card points count as long points too.
            let theirs = match s {
                None => Points::Hcp,
                Some(suit) if shown(suit) => Points::Lhcp,
                Some(_) => Points::Shcp,
            };
            let proper =
                theirs.of(&partner.min) >= lo || (theirs == Points::Lhcp && partner.min.hcp >= lo);
            Rival {
                trumps: s.map(|suit| (suit, partner.min.length[suit], partner.max.length[suit])),
                available,
                needs: contract_points(contract).unwrap_or(u8::MAX),
                points,
                proper,
                order: match s {
                    Some(Suit::Spades) => 4,
                    Some(Suit::Hearts) => 3,
                    None => 2,
                    Some(Suit::Diamonds) => 1,
                    Some(Suit::Clubs) => 0,
                },
            }
        };
        let mut rivals = vec![rival(strain.strain, contract, true)];
        for s in [
            None,
            Some(Suit::Spades),
            Some(Suit::Hearts),
            Some(Suit::Diamonds),
            Some(Suit::Clubs),
        ] {
            if s == strain.strain {
                continue;
            }
            let other = here(s);
            // The contract this side already plays counts too, since a pass
            // keeps it.
            let played = ours.filter(|&c| trump(c) == s && Kind::of(c) == Kind::of(contract));
            let Some(theirs) = other.contract(Kind::of(contract)).or(played) else {
                continue;
            };
            let available = match rule {
                _ if played.is_some() => true,
                Rule::Invite => other.invitation(theirs).is_some(),
                _ => !other.conventional(theirs),
            };
            rivals.push(rival(s, theirs, available));
        }
        let slam = match (strain.strain, rule) {
            (Some(_), Rule::Sure | Rule::LastChance | Rule::Forced) => Kind::of(contract)
                .above()
                .and_then(|k| strain.contract(k))
                .and_then(contract_points),
            // In a game force there is room to look for a slam that notrump
            // cannot invite.
            (None, Rule::Forced) => strain
                .contract(Kind::SmallSlam)
                .filter(|&slam| strain.invitation(slam).is_none())
                .and_then(contract_points),
            // Once the aces are known, a small slam in notrump waits too,
            // when the kings could still be asked for a grand slam.
            (None, _) if Kind::of(contract) == Kind::SmallSlam && king_ask => {
                strain.contract(Kind::GrandSlam).and_then(contract_points)
            }
            _ => None,
        };

        // The bid's name says what it does, unless the table names it.
        let the_name = match (the_name, rule, Kind::of(contract)) {
            ("Natural" | "", Rule::Invite, Kind::Game) if points == Points::Shcp => "Limit raise",
            ("Natural" | "", Rule::Invite, Kind::Game) => "Game invite",
            ("Natural" | "", Rule::Invite, _) => "Slam invite",
            ("Natural" | "", _, Kind::Game) => "Game",
            ("Natural" | "", _, Kind::SmallSlam) => "Small slam",
            ("Natural" | "", _, _) => "Grand slam",
            (name, ..) => name,
        };
        Some(Convention::Contracted {
            the_name,
            min,
            max,
            reason: Reason {
                rule,
                contract,
                needs,
                promised: (lo, hi),
                partner: counted(rule),
                points,
                captain,
            },
            choice: Choice {
                rivals,
                slam,
                ask_first,
                asks,
                aces_or_none,
                unsupported,
            },
        })
    }

    /// `fit`, a limit bid's way of fitting the last of `bids`, with its most
    /// points derived from the rest of the system: one fewer than the fewest
    /// with which the system makes another bid instead.  Another bid takes
    /// those hands only if it accepts every hand `fit` does once it has the
    /// points, so that no hand is left without a bid.
    pub(crate) fn limit(&self, bids: &[Bid], fit: &Fit) -> Fit {
        let mut fit = fit.clone();
        let Some(points) = Points::described(&fit.min, &HandValuation::MAX) else {
            return fit;
        };
        let (&bid, history) = bids.split_last().unwrap();
        let mut most = points.of(&fit.max);
        for other in legal_bids(history).into_iter().filter(|&b| b != bid) {
            let mut auction = history.to_vec();
            auction.push(other);
            for (min, max, shape) in self.ways_to_bid(&auction) {
                let stronger = points.of(&min) > points.of(&fit.min);
                let mut others_min = min;
                points.set(&mut others_min, points.of(&fit.min));
                let mut others_max = max;
                points.set(&mut others_max, points.of(&fit.max));
                let takes_every_hand = !others_min.exceeds(fit.min)
                    && !fit.max.exceeds(others_max)
                    && shape.iter().all(|r| fit.shape.contains(r));
                if stronger && takes_every_hand {
                    most = most.min(points.of(&min) - 1);
                }
            }
        }
        points.set(&mut fit.max, most);
        fit
    }

    /// The ways a hand can fit the last of `bids`, as written in the table,
    /// or as the contract rules make it.
    fn ways_to_bid(
        &self,
        bids: &[Bid],
    ) -> Vec<(HandValuation, HandValuation, Vec<super::ShapeRule>)> {
        let auction = bids_string(bids);
        let mut ways = Vec::new();
        for i in self.candidates(&auction) {
            match &self.entries()[i] {
                Convention::Simple {
                    regex, min, max, ..
                } if regex.is_match(&auction) => ways.push((*min, *max, Vec::new())),
                Convention::Rule { regex, fits, .. } if regex.is_match(&auction) => {
                    ways.extend(fits.iter().map(|f| (f.min, f.max, f.shape.clone())))
                }
                _ => {}
            }
        }
        let ruled = self
            .default_for(&auction, bids)
            .is_some_and(|i| matches!(self.entries()[i], Convention::Contract { .. }));
        if ways.is_empty() && ruled {
            if let Some(Convention::Contracted { min, max, .. }) = self.contract_meaning(bids, "") {
                ways.push((min, max, Vec::new()));
            }
        }
        ways
    }
}
