//! The contract rules, which give natural bids their meaning from the
//! contracts within reach instead of from table entries, in auctions where
//! the opponents have only passed.
//!
//! A contract needs points between the two hands: 25 for game in notrump or
//! a major, 28 in a minor, 33 for a small slam and 37 for a grand slam, and
//! eight trumps in a suit.  Partner has promised a range of points, `lo` to
//! `hi`, and the rules weigh the contract against it:
//!
//! 1. Bid the contract when it makes opposite `lo`.
//! 2. Invite it when it makes opposite `lo` + 2 (but no more than `hi`).
//! 3. With no bid left to invite with, as when partner has just invited,
//!    bid it when it makes opposite `lo` + 1 (but no more than `hi`).
//!
//! An invitation is the lowest bid in the strain, below the contract it
//! invites, that no table entry describes.  A suit is invited in only when
//! one of the partners has shown four cards in it, so that a first bid of a
//! suit, such as a jump shift, is no invitation.  The rules only bid games and
//! slams.  They invite games in notrump and the majors, and slams only in
//! notrump (the quantitative 4NT), so slams in a suit are bid only when
//! they are sure, since bidding them is a matter for Blackwood.
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
//! partner's most.  If nothing explores, the hand bids what it knows.
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
/// How many points more than the fewest partner promised an invitation
/// counts partner for.
const INVITING: u8 = 2;
/// The same for a contract bid when no bid is left to invite it with.
const LAST_CHANCE: u8 = 1;
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
                 and a slam only in notrump, with 4NT."
            ),
            format!(
                "With no bid left to invite with, as when partner has just invited, bid the \
                 contract when your side has the points if partner has {LAST_CHANCE} more \
                 than the fewest."
            ),
            format!(
                "A slam in a suit is bid only when it is sure, and nothing is bid for a \
                 part-score: that is a call an entry describes, or a pass. When partner's \
                 last bid was a limit bid, one that is not forcing and whose points span at \
                 most {LIMIT_SPAN} values, you are the captain: placing the contract shows \
                 nothing more about your hand."
            ),
        ],
    )
}

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
/// `bids_string` writes it) only passed?  After a double, as after a bid,
/// the booklet gives bids other meanings, such as 2NT as a limit raise.
pub(crate) fn uncontested(auction: &str) -> bool {
    auction
        .split(' ')
        .rev()
        .skip(1)
        .step_by(2)
        .all(|call| call == "P")
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
            Rule::Sure | Rule::LastChance => "To play: a",
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
    /// For a game in a suit, the points a slam needs, which the rules cannot
    /// invite, so that a hand that might make one explores instead.
    slam: Option<u8>,
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
/// strain where a fit might yet be found rates as well, and in a suit, no
/// slam is within reach of partner's most, since the rules cannot invite
/// one.  Otherwise the hand explores, with the table entries that come
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
    if hand.exceeds(*max) || min.exceeds(*hand) {
        return false;
    }
    let (lo, hi) = reason.promised;
    let (ours, rivals) = choice.rivals.split_first().unwrap();
    if unsure_explores
        && choice
            .slam
            .is_some_and(|slam| ours.points.of(hand) + hi >= slam)
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

/// Where the rules stand in one strain after `history`.
struct Strain<'a> {
    convention: &'a Convention,
    history: &'a [Bid],
    strain: Option<Suit>,
    /// A contract partner has just invited.
    invited: Option<Bid>,
    /// The contract this side already plays, if it does.
    ours: Option<Bid>,
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
    /// otherwise Blackwood.  Partner's own invitation is answered, not
    /// invited again.
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
            || (kind == Kind::Game && minor)
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
    /// an auction where the opponents have only passed, if they make it at
    /// all.
    pub(crate) fn contract_meaning(
        &self,
        bids: &[Bid],
        the_name: &'static str,
    ) -> Option<Convention> {
        let (&bid, history) = bids.split_last()?;
        if !bid.is_contract() || !uncontested(&bids_string(bids)) {
            return None;
        }
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
        // The contract this side already plays, if it does.
        let ours = history
            .iter()
            .rposition(|b| b.is_contract())
            .and_then(|i| ((history.len() - i) % 2 == 0).then(|| history[i]));
        let here = |strain: Option<Suit>| Strain {
            convention: self,
            history,
            strain,
            invited,
            ours,
            shown: strain.map_or(0, |suit| {
                partner.min.length[suit].max(mine.min.length[suit])
            }),
        };
        let strain = here(trump(bid));

        // Whether the bid places a contract, with the rule, or invites one.
        // A slam in a suit is bid only when it is sure, and otherwise a
        // contract that could have been invited must be sure too.
        let kind = Kind::of(bid);
        let (contract, rule) = if strain.placing(kind) == Some(bid) && kind >= Kind::Game {
            let suit_slam = kind > Kind::Game && strain.strain.is_some();
            if suit_slam || strain.invitation(bid).is_some() {
                (bid, Rule::Sure)
            } else {
                (bid, Rule::LastChance)
            }
        } else {
            let next = kind.above().and_then(|k| strain.contract(k))?;
            if strain.invitation(next) != Some(bid) {
                return None;
            }
            (next, Rule::Invite)
        };
        let needs = contract_points(contract)?;
        let counted = |rule| match rule {
            Rule::Sure => lo,
            Rule::Invite => hi.min(lo + INVITING),
            Rule::LastChance => hi.min(lo + LAST_CHANCE),
        };
        let least = needs.saturating_sub(counted(rule));
        // The hand goes no further than this: placing a contract, it would
        // otherwise go on to the next kind, and inviting, it would place
        // the contract itself.
        let most = if rule == Rule::Invite {
            needs.checked_sub(lo + 1)?
        } else {
            match Kind::of(contract).above().and_then(|k| strain.contract(k)) {
                Some(next) => {
                    let next_rule = if strain.invitation(next).is_some() {
                        Rule::Invite
                    } else if strain.strain.is_some() {
                        Rule::Sure
                    } else {
                        Rule::LastChance
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
        points.set(&mut min, least);
        points.set(&mut max, most.min(points.of(&HandValuation::MAX)));
        if let Some(suit) = strain.strain {
            min.length[suit] = TRUMPS.saturating_sub(partner.min.length[suit]);
        }

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
            (Some(_), Rule::Sure | Rule::LastChance) => Kind::of(contract)
                .above()
                .and_then(|k| strain.contract(k))
                .and_then(contract_points),
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
            choice: Choice { rivals, slam },
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
