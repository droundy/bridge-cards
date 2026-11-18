//! A solver for deals in which every card is known.
//!
//! [`Naive`](crate::Naive) solves those too, when nothing is unknown, but it is
//! built for deals with unknown cards, whose scores are means over samples, so
//! it works out the exact score of every position it reaches.  With every card
//! known, a score is a whole number of tricks, and a question with a yes or no
//! answer, "can the side on lead take at least `target` more tricks?", is much
//! cheaper to answer: a defence that holds the leaders below the target ends
//! the search of a play, whatever else that play might lead to.  The number of
//! tricks is found by asking that question a few times, in a binary search.
//! What each answer shows of a position, a least and a most number of tricks,
//! is kept, so later questions about the same position are answered at once,
//! and so are the many positions in which the side on lead has enough top
//! cards to take the target without any search.

use crate::{likely_plays, PositionHasher, Starting};

use bridge_deck::{Card, Cards, Suit};
use std::collections::HashMap;
use std::hash::BuildHasherDefault;

/// The `hands` renumbered among their cards, so that the gaps that the cards
/// played before them leave are closed up, as they are after each trick the
/// solver plays.  Positions that differ only in cards gone from the deck are
/// then the same.
fn renumbered(hands: [Cards; 4]) -> [Cards; 4] {
    let all = hands[0] + hands[1] + hands[2] + hands[3];
    hands.map(|hand| hand.renumbered_among(all))
}

/// How [`renumbered`] renumbers each card of `hands`.
fn renumbering(hands: [Cards; 4]) -> impl Fn(Card) -> Card {
    let all = hands[0] + hands[1] + hands[2] + hands[3];
    move |card| Cards::singleton(card).renumbered_among(all).max().unwrap()
}

/// What is known of each position.
type Bounds = HashMap<Starting, Known, BuildHasherDefault<PositionHasher>>;

/// What is known of a position: the least and the most tricks that the side
/// on lead can take, and the lead that last made a target, if one has.
#[derive(Clone, Copy)]
struct Known {
    lower: u8,
    upper: u8,
    lead: Option<Card>,
}

/// A double-dummy solver: every hand is known to everyone, and everyone plays
/// as well as can be.
#[derive(Clone)]
pub struct DoubleDummy {
    trump: Option<Suit>,
    bounds: Bounds,
}

impl DoubleDummy {
    /// A solver for deals played with `trump` as trumps, or in notrump.
    pub fn new(trump: Option<Suit>) -> Self {
        DoubleDummy {
            trump,
            bounds: Bounds::default(),
        }
    }

    /// The tricks that the side on lead takes with best play by everyone,
    /// where `hands` are the hands in the order of play from the leader, all
    /// of the same size.
    pub fn tricks(&mut self, hands: [Cards; 4]) -> usize {
        let position = Starting {
            hands: renumbered(hands),
            unknown: Cards::EMPTY,
        };
        position.check();
        let (mut low, mut high) = (0, hands[0].len());
        while low < high {
            let target = (low + high).div_ceil(2);
            if self.at_least(position, target) {
                low = target;
            } else {
                high = target - 1;
            }
        }
        low
    }

    /// The tricks that the side that led this trick takes, with best play by
    /// everyone, after `plays` have been made to it from `hands`, which are
    /// the hands as they were before the trick.  There may be from one to
    /// four plays.
    pub fn tricks_after(&mut self, hands: [Cards; 4], plays: &[Card]) -> usize {
        let renumber = renumbering(hands);
        let plays: Vec<Card> = plays.iter().map(|&card| renumber(card)).collect();
        self.search_after(renumbered(hands), &plays, None)
    }

    /// The tricks that the side that led this trick takes, with best play by
    /// everyone after it, after each card that the next player could play to
    /// `plays`, from `hands`, which are the hands as they were before the
    /// trick.  There may be from none to three plays.
    ///
    /// This is [`DoubleDummy::tricks_after`] for each card, but cheaper: the
    /// cards that touch are worked out once, and the search for each card
    /// starts from the tricks of the card before, which are usually close.
    pub fn tricks_after_each(&mut self, hands: [Cards; 4], plays: &[Card]) -> Vec<(Card, usize)> {
        assert!(plays.len() < 4, "a trick has four cards");
        let renumber = renumbering(hands);
        let mut trick: Vec<Card> = plays.iter().map(|&card| renumber(card)).collect();
        let renumbered_hands = renumbered(hands);
        // The lowest of each run of cards that touch, likeliest best first.
        let mut runs: Vec<(Card, usize)> = Vec::new();
        for card in likely_plays(self.trump, &renumbered_hands, &trick) {
            trick.push(card);
            let guess = runs.last().map(|&(_, tricks)| tricks);
            runs.push((card, self.search_after(renumbered_hands, &trick, guess)));
            trick.pop();
        }
        let lowest = runs
            .iter()
            .fold(Cards::EMPTY, |cards, &(card, _)| cards.insert(card));
        let legal = match plays.first() {
            None => hands[0],
            Some(lead) => hands[plays.len()].following_suit(lead.suit()),
        };
        legal
            .map(|card| {
                // The run of a card is the one of the lowest card at or below it.
                let card_now = renumber(card);
                let run = (lowest.in_suit(card.suit()) - Cards::higher_than(card_now)).max();
                let &(_, tricks) = runs.iter().find(|&&(card, _)| Some(card) == run).unwrap();
                (card, tricks)
            })
            .collect()
    }

    /// The tricks that the side that led this trick takes after `plays`, from
    /// `hands`, both renumbered, searching from `guess` if there is one.
    fn search_after(&mut self, hands: [Cards; 4], plays: &[Card], guess: Option<usize>) -> usize {
        let n = hands[0].len();
        let winner = (1..plays.len()).fold(0, |winner, i| {
            if self.beats(plays[i], plays[winner]) {
                i
            } else {
                winner
            }
        });
        let mut makes = |target: usize| {
            let mut trick = [plays[0]; 4];
            trick[..plays.len()].copy_from_slice(plays);
            target == 0 || self.makes_from(hands, &mut trick, plays.len(), winner, target)
        };
        match guess {
            None => {
                let (mut low, mut high) = (0, n);
                while low < high {
                    let target = (low + high).div_ceil(2);
                    if makes(target) {
                        low = target;
                    } else {
                        high = target - 1;
                    }
                }
                low
            }
            Some(guess) => {
                // Up or down from the guess, one trick at a time.
                let mut tricks = guess.min(n);
                if makes(tricks) {
                    while tricks < n && makes(tricks + 1) {
                        tricks += 1;
                    }
                } else {
                    tricks -= 1;
                    while !makes(tricks) {
                        tricks -= 1;
                    }
                }
                tricks
            }
        }
    }

    /// Whether the side on lead in `position` can take at least `target` of
    /// the tricks that are left.
    fn at_least(&mut self, position: Starting, target: usize) -> bool {
        let n = position.hands[0].len();
        if target == 0 {
            return true;
        }
        if target > n {
            return false;
        }
        // Top cards settle many questions, more cheaply than a look in the
        // table.
        if target > n - self.sure_losers(position.hands) {
            return false;
        }
        let sure = self.sure_tricks(position.hands);
        if sure >= target {
            return true;
        }
        let known = self.bounds.get(&position).copied().unwrap_or(Known {
            lower: 0,
            upper: n as u8,
            lead: None,
        });
        let (lower, upper) = (known.lower as usize, known.upper as usize);
        if target <= lower {
            return true;
        }
        if target > upper {
            return false;
        }
        let mut lead = known.lead;
        let makes = {
            // The lead that made a target before is tried first.
            let mut trick = [Card::S2; 4];
            let tried = known.lead;
            let leads = tried.into_iter().chain(
                likely_plays(self.trump, &position.hands, &[]).filter(|&card| Some(card) != tried),
            );
            let mut makes = false;
            for card in leads {
                trick[0] = card;
                if self.makes_from(position.hands, &mut trick, 1, 0, target) {
                    lead = Some(card);
                    makes = true;
                    break;
                }
            }
            makes
        };
        let (lower, upper) = if makes {
            (lower.max(target).max(sure), upper)
        } else {
            (lower.max(sure), upper.min(target - 1))
        };
        self.bounds.insert(
            position,
            Known {
                lower: lower as u8,
                upper: upper as u8,
                lead,
            },
        );
        makes
    }

    /// Whether the side that leads this trick, from `hands` as they were
    /// before it, can take at least `target` of the tricks that are left,
    /// once the first `played` cards of `trick` have been played to it, of
    /// which the one at `winner` is winning it.
    fn makes_from(
        &mut self,
        hands: [Cards; 4],
        trick: &mut [Card; 4],
        played: usize,
        winner: usize,
        target: usize,
    ) -> bool {
        if played == 4 {
            return self.makes_after(hands, *trick, winner, target);
        }
        // The leader and their partner want the target made, and the others
        // want it not to be.
        let wants_it = played & 1 == 0;
        // Once one card settles the question, the others need not be tried,
        // so the likeliest to are tried first.
        for card in likely_plays(self.trump, &hands, &trick[..played]) {
            trick[played] = card;
            let winner = if self.beats(card, trick[winner]) {
                played
            } else {
                winner
            };
            if self.makes_from(hands, trick, played + 1, winner, target) == wants_it {
                return wants_it;
            }
        }
        !wants_it
    }

    /// Whether `card`, played after `winning`, beats it.
    fn beats(&self, card: Card, winning: Card) -> bool {
        if card.suit() == winning.suit() {
            card.rank() > winning.rank()
        } else {
            Some(card.suit()) == self.trump
        }
    }

    /// Whether the side that led the trick `plays` from `hands`, which the
    /// one at `winner` won, takes at least `target` of the tricks that were
    /// left before it.
    fn makes_after(
        &mut self,
        hands: [Cards; 4],
        plays: [Card; 4],
        winner: usize,
        target: usize,
    ) -> bool {
        let n = hands[0].len();
        let ours = winner & 1 == 0;
        if n == 1 {
            return ours as usize >= target;
        }
        // The cards are renumbered as though the ones played had never been
        // in the deck, and the winner leads to the next trick.
        let played = plays
            .iter()
            .fold(Cards::EMPTY, |cards, &card| cards.insert(card));
        let next = Starting {
            hands: [0, 1, 2, 3].map(|i| hands[(winner + i) % 4].renumbered_without(played)),
            unknown: Cards::EMPTY,
        };
        if ours {
            self.at_least(next, target - 1)
        } else {
            // The others lead next, and the leaders make their target if
            // the others take no more than the rest.
            !self.at_least(next, n - target)
        }
    }

    /// Tricks that the side not on lead takes however the leaders play: the
    /// trumps of one of them that are higher than any of the leaders'.
    fn sure_losers(&self, hands: [Cards; 4]) -> usize {
        let Some(trump) = self.trump else {
            return 0;
        };
        let theirs = (hands[1] + hands[3]).in_suit(trump);
        let above = match (hands[0] + hands[2]).in_suit(trump).max() {
            None => theirs,
            Some(best) => theirs & Cards::higher_than(best),
        };
        (hands[1] & above).len().max((hands[3] & above).len())
    }

    /// Tricks that the leader's side takes however the others play: the top
    /// cards of the leader's suits, which win while they last as long as no
    /// one else can ruff them, and then those of partner in a suit that the
    /// leader can lead to them.
    fn sure_tricks(&self, hands: [Cards; 4]) -> usize {
        let all = hands[0] + hands[1] + hands[2] + hands[3];
        // How many of the top cards of `suit` `hand` holds, from the top.
        let tops = |hand: Cards, suit: Suit| {
            let mut left = all.in_suit(suit);
            let mut tops = 0;
            while let Some(top) = left.max() {
                if !hand.contains(top) {
                    break;
                }
                tops += 1;
                left -= Cards::singleton(top);
            }
            tops
        };
        // The others can ruff unless the leader can draw their trumps with
        // top trumps first.
        let others_can_ruff = |suit: Suit| match self.trump {
            Some(trump) if trump != suit => {
                let longest = hands[1]
                    .in_suit(trump)
                    .len()
                    .max(hands[3].in_suit(trump).len());
                longest > 0 && tops(hands[0], trump) < longest
            }
            _ => false,
        };
        let mut own = 0;
        let mut partners = 0;
        for suit in Suit::ALL {
            if others_can_ruff(suit) {
                continue;
            }
            own += tops(hands[0], suit);
            if !hands[0].in_suit(suit).is_empty() {
                partners = partners.max(tops(hands[2], suit));
            }
        }
        let n = hands[0].len();
        // The leader cashes their own winners first, and then leads to
        // partner, as long as partner has other cards to play to the leader's
        // winners than the ones that win their suit.
        let partner_can_wait = |suit: Suit| {
            !hands[0].in_suit(suit).is_empty()
                && !others_can_ruff(suit)
                && tops(hands[2], suit) == partners
                && (hands[2] - hands[2].in_suit(suit)).len() >= own
        };
        let sure = if partners > 0 && Suit::ALL.iter().any(|&suit| partner_can_wait(suit)) {
            own + partners
        } else {
            own.max(partners)
        };
        sure.min(n)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Naive;
    use rand::rngs::SmallRng;
    use rand::{Rng, SeedableRng};

    fn deal(rng: &mut SmallRng, size: usize) -> [Cards; 4] {
        let mut deck = Cards::ALL;
        [0; 4].map(|_| deck.pick_rng(rng, size).unwrap())
    }

    #[test]
    fn tricks_agree_with_the_naive_solver() {
        let mut rng = SmallRng::seed_from_u64(7);
        for size in 1..=7 {
            for _ in 0..100 {
                let hands = deal(&mut rng, size);
                let trump = [None, Some(Suit::Hearts), Some(Suit::Clubs)][rng.gen_range(0..3)];
                let naive = Naive::new(trump)
                    .score(Starting {
                        hands,
                        unknown: Cards::EMPTY,
                    })
                    .mean();
                let mut solver = DoubleDummy::new(trump);
                assert_eq!(naive, solver.tricks(hands) as f64, "{hands:?} {trump:?}");
                // A second question of the same solver is answered from what
                // it learnt the first time, and must agree.
                assert_eq!(naive, solver.tricks(hands) as f64);
            }
        }
    }

    #[test]
    fn tricks_after_each_card_agree_with_tricks_after_it() {
        let mut rng = SmallRng::seed_from_u64(9);
        for size in 1..=6 {
            for _ in 0..30 {
                // The hands of a deal leave gaps, as cards played before
                // them would.
                let hands = deal(&mut rng, size);
                let trump = [None, Some(Suit::Hearts)][rng.gen_range(0..2)];
                let mut plays = Vec::new();
                for seat in 0..4 {
                    let each = DoubleDummy::new(trump).tricks_after_each(hands, &plays);
                    let legal = match plays.first() {
                        None => hands[0],
                        Some(lead) => hands[seat].following_suit(lead.suit()),
                    };
                    assert_eq!(legal.len(), each.len());
                    for (card, tricks) in each {
                        assert!(legal.contains(card));
                        plays.push(card);
                        let alone = DoubleDummy::new(trump).tricks_after(hands, &plays);
                        plays.pop();
                        assert_eq!(alone, tricks, "{hands:?} {plays:?} {card:?}");
                    }
                    let mut legal = legal;
                    plays.push(legal.pick_rng(&mut rng, 1).unwrap().max().unwrap());
                }
            }
        }
    }

    #[test]
    fn tricks_after_plays_agree_with_trying_each_reply() {
        let mut rng = SmallRng::seed_from_u64(8);
        for size in 2..=5 {
            for _ in 0..30 {
                let hands = deal(&mut rng, size);
                let trump = [None, Some(Suit::Spades)][rng.gen_range(0..2)];
                let mut solver = DoubleDummy::new(trump);
                let whole = solver.tricks(hands);
                // The best lead makes as many tricks as the deal does.
                let best = hands[0]
                    .map(|lead| solver.tricks_after(hands, &[lead]))
                    .max()
                    .unwrap();
                assert_eq!(whole, best, "{hands:?}");
                for lead in hands[0] {
                    let after_lead = solver.tricks_after(hands, &[lead]);
                    // The second hand plays to hold the leaders to as few as
                    // it can.
                    let worst = hands[1]
                        .following_suit(lead.suit())
                        .map(|second| solver.tricks_after(hands, &[lead, second]))
                        .min()
                        .unwrap();
                    assert_eq!(after_lead, worst, "{hands:?} {lead:?}");
                }
            }
        }
    }

    /// How long each solver takes on deals of each size, with the sizes in
    /// `DD_SIZES` (8 to 13 by default) and `DD_DEALS` deals of each (8 by
    /// default), and `Naive` only up to 10, and how long `DoubleDummy` takes
    /// to value every lead.
    #[test]
    #[ignore]
    fn report_the_time_to_solve_a_deal() {
        let sizes: Vec<usize> = std::env::var("DD_SIZES")
            .ok()
            .map(|s| s.split(',').filter_map(|n| n.parse().ok()).collect())
            .unwrap_or((8..=13).collect());
        let deals = std::env::var("DD_DEALS")
            .ok()
            .and_then(|d| d.parse().ok())
            .unwrap_or(8);
        let time = |f: &mut dyn FnMut()| {
            let started = std::time::Instant::now();
            f();
            started.elapsed().as_secs_f64()
        };
        for size in sizes {
            let mut rng = SmallRng::seed_from_u64(size as u64);
            let mut dd = Vec::new();
            let mut leads = Vec::new();
            let mut naive = Vec::new();
            for i in 0..deals {
                let hands = deal(&mut rng, size);
                let trump = [None, Some(Suit::Spades)][i % 2];
                let mut tricks = 0;
                dd.push(time(&mut || tricks = DoubleDummy::new(trump).tricks(hands)));
                leads.push(time(&mut || {
                    let each = DoubleDummy::new(trump).tricks_after_each(hands, &[]);
                    assert_eq!(Some(tricks), each.iter().map(|&(_, tricks)| tricks).max());
                }));
                if size <= 10 {
                    let starting = Starting {
                        hands,
                        unknown: Cards::EMPTY,
                    };
                    let mut score = 0.0;
                    naive.push(time(&mut || {
                        score = Naive::new(trump).score(starting).mean()
                    }));
                    assert_eq!(tricks as f64, score);
                }
            }
            let summary = |times: &mut Vec<f64>| {
                if times.is_empty() {
                    return "-".to_string();
                }
                times.sort_by(f64::total_cmp);
                format!(
                    "mean {:.4} s, most {:.4} s",
                    times.iter().sum::<f64>() / times.len() as f64,
                    times[times.len() - 1]
                )
            };
            println!(
                "{size} cards: double dummy {}, every lead {}; naive {}",
                summary(&mut dd),
                summary(&mut leads),
                summary(&mut naive)
            );
        }
    }

    #[test]
    fn the_leader_takes_their_top_cards() {
        // In notrump, the leader holds the top three spades and the top two
        // hearts: five sure tricks.  Partner's spades and hearts are lower,
        // so add nothing.
        let hands = ["S AKQ H AK", "S 432 H 43", "S 765 H 65", "S T98 H 87"]
            .map(|hand| hand.parse::<Cards>().unwrap());
        let solver = DoubleDummy::new(None);
        assert_eq!(5, solver.sure_tricks(hands));
        // With hearts trumps, the leader's top trumps draw the others' two
        // trumps first, and then the spades are sure.
        let solver = DoubleDummy::new(Some(Suit::Hearts));
        assert_eq!(5, solver.sure_tricks(hands));
        // With a third trump to one of the others, the spades could be
        // ruffed, and only the trumps are sure.
        let hands = ["S AKQ H AK", "S 432 H 4 D 2", "S 765 H 65", "S T9 H 873"]
            .map(|hand| hand.parse::<Cards>().unwrap());
        assert_eq!(2, solver.sure_tricks(hands));
    }
}
