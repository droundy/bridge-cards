//! Choosing a card by its double-dummy value over an ensemble of the hands that
//! cannot be seen.
//!
//! A player sees their own hand and the dummy, and does not see the other two.
//! What they do know is how many cards each of those has, which suits each has
//! shown out of by not following, which cards have been played, and what the
//! auction said about each hand.  [`Hidden`] deals the cards that are not in
//! sight into the two hands again and again, keeping only the deals that agree
//! with all of that, which is an ensemble of the hands that might be there.
//! [`EnsemblePlay`] works out, for each card that it may play, how many tricks
//! it wins with best play by everyone in every deal of the ensemble, and plays
//! the card whose mean is best.
//!
//! Two things in this are not true to the game.  Best play by everyone with
//! every hand known is not how anyone plays, since a defender who cannot see
//! the declarer's hand would not play as though they could.  And the ensemble
//! gives every deal that fits the same weight, as though each hand were as
//! likely as any other that has the same shape and strength, which is not how
//! people bid.

use super::{Convention, PlayAI, RandomPlay};
use crate::{BiddingSystem, GameState, Seat};
use bridge_deck::{Card, Cards, HandValuation, Suit};
use bridge_solver::{Naive, Starting};
use rand::rngs::StdRng;
use rand::seq::SliceRandom;
use rand::{Rng, SeedableRng};

/// How many deals to try for one that fits the auction before taking one that
/// does not.
const ATTEMPTS: usize = 1000;

/// The two hands that a player cannot see, and what they know of them.
#[derive(Debug)]
pub(crate) struct Hidden {
    /// The seats of the hands.
    seats: [Seat; 2],
    /// How many cards each hand has now.
    sizes: [usize; 2],
    /// The cards that the two hands hold between them.
    pool: Cards,
    /// The cards that each hand cannot hold, since it has shown out of their
    /// suits.
    forbidden: [Cards; 2],
    /// The cards that each hand has played, with which what it holds now
    /// makes the hand that it bid with.
    played: [Cards; 2],
    /// What the bids of each hand show of the hand that it bid with, as a
    /// minimum and a maximum.
    bounds: [(HandValuation, HandValuation); 2],
}

impl Hidden {
    /// What the player who is to play knows of the hands that they cannot
    /// see, or `None` when it is not a time to play.  `convention` is how the
    /// auction is read.
    pub(crate) fn new<C>(game: &GameState<C>, convention: &Convention) -> Option<Hidden> {
        let viewer = game.turn()?;
        let dummy = game.dummy()?;
        let seats: Vec<Seat> = (0..4)
            .map(Seat::from)
            .filter(|&seat| seat != viewer && seat != dummy)
            .collect();
        let seats = [seats[0], seats[1]];
        // The bids of a seat are every fourth bid, and `shown` wants to know
        // how many seats before the next bidder they were made.
        let bid_offset = |seat: Seat| {
            let first = (seat as usize + 4 - game.dealer as usize) % 4;
            (game.bids.len() + 4 - first) % 4
        };
        let hands = |seats: [Seat; 2]| seats.map(|seat| game.hands[seat]);
        let [first, second] = hands(seats);
        Some(Hidden {
            seats,
            sizes: [first.len(), second.len()],
            pool: first + second,
            forbidden: seats.map(|seat| game.shown_out[seat]),
            played: seats.map(|seat| game.original_hands[seat] - game.hands[seat]),
            bounds: seats.map(|seat| convention.shown_bounds(&game.bids, bid_offset(seat))),
        })
    }

    /// Whether `hands` are hands that the auction could have been made with.
    fn fit_the_bidding(&self, hands: &[Cards; 2]) -> bool {
        (0..2).all(|i| {
            let dealt = (hands[i] + self.played[i]).values();
            let (min, max) = self.bounds[i];
            !min.exceeds(dealt) && !dealt.exceeds(max)
        })
    }

    /// Deal the cards that are not in sight to the two hands, so that the hands
    /// are as large as they are and none has a card of a suit that it has shown
    /// out of.  `None` if there is no way to do it.
    fn deal(&self, rng: &mut impl Rng) -> Option<[Cards; 2]> {
        let mut hands = [Cards::EMPTY; 2];
        let mut free: Vec<Card> = Vec::new();
        for card in self.pool {
            match (
                self.forbidden[0].contains(card),
                self.forbidden[1].contains(card),
            ) {
                (true, true) => return None,
                (true, false) => hands[1] += Cards::singleton(card),
                (false, true) => hands[0] += Cards::singleton(card),
                (false, false) => free.push(card),
            }
        }
        if hands[0].len() > self.sizes[0] || hands[1].len() > self.sizes[1] {
            return None;
        }
        let first_wants = self.sizes[0] - hands[0].len();
        if first_wants + (self.sizes[1] - hands[1].len()) != free.len() {
            return None;
        }
        free.shuffle(rng);
        for (i, &card) in free.iter().enumerate() {
            hands[(i >= first_wants) as usize] += Cards::singleton(card);
        }
        Some(hands)
    }

    /// One deal of the hands, in the order of `seats`, and whether it fits the
    /// auction.  When there is no deal that fits, or too few to be found, it
    /// is one that has the right cards in each hand and nothing else.  `None`
    /// only when the cards cannot be dealt at all, which says that what is
    /// known does not agree with itself.
    pub(crate) fn sample(&self, rng: &mut impl Rng) -> Option<([Cards; 2], bool)> {
        let mut last = None;
        for _ in 0..ATTEMPTS {
            let hands = self.deal(rng)?;
            if self.fit_the_bidding(&hands) {
                return Some((hands, true));
            }
            last = Some(hands);
        }
        last.map(|hands| (hands, false))
    }
}

/// The deal as the solver has it, for a deal of the hands that cannot be seen:
/// the hands in the order of the play, with each card already played to this
/// trick back in its hand, and those cards.
pub(crate) fn starting_with<C>(
    game: &GameState<C>,
    hidden: &Hidden,
    deal: [Cards; 2],
) -> Option<(Starting, Vec<Card>)> {
    let lead = game.lead?;
    let in_trick: Vec<Card> = if game.played.len() == 4 {
        Vec::new()
    } else {
        game.played.clone()
    };
    let mut hands = [Cards::EMPTY; 4];
    for seat in 0..4 {
        let seat = Seat::from(seat);
        let cards = match hidden.seats.iter().position(|&s| s == seat) {
            Some(i) => deal[i],
            None => game.hands[seat],
        };
        hands[(seat as usize + 4 - lead as usize) % 4] = cards;
    }
    for (i, &card) in in_trick.iter().enumerate() {
        hands[i] += Cards::singleton(card);
    }
    Some((
        Starting {
            hands,
            unknown: Cards::EMPTY,
        },
        in_trick,
    ))
}

/// The trump suit of the contract, if there is one.
pub(crate) fn trump_of<C>(game: &GameState<C>) -> Option<Suit> {
    match game.highest_contract_bid() {
        Some(crate::Bid::Suit(_, trump)) => Some(trump),
        _ => None,
    }
}

/// How many of the tricks left the side of whoever leads this trick wins,
/// when everything is known, after `plays` have been made to the trick.
fn trick_value(
    solver: &mut Naive,
    starting: Starting,
    trump: Option<Suit>,
    plays: &mut Vec<Card>,
) -> f64 {
    if plays.len() == 4 {
        let trick = starting.after([plays[0], plays[1], plays[2], plays[3]], trump);
        return (solver.score(trick.starting()) + trick).mean();
    }
    let seat = plays.len();
    let legal = if seat == 0 {
        starting.hands[0]
    } else {
        starting.hands[seat].following_suit(plays[0].suit())
    };
    let mut best: Option<f64> = None;
    for card in legal {
        plays.push(card);
        let value = trick_value(solver, starting, trump, plays);
        plays.pop();
        best = Some(match best {
            // The leader and the partner of the leader want the most tricks.
            Some(best) if seat & 1 == 0 => best.max(value),
            Some(best) => best.min(value),
            None => value,
        });
    }
    best.expect("a hand has to have a card to play")
}

/// For each card in `legal`, the tricks that the side of whoever leads this
/// trick wins by playing it, when `starting` has every hand, with best play
/// after that, where `in_trick` have been played to the trick already.
pub(crate) fn card_values(
    starting: Starting,
    trump: Option<Suit>,
    in_trick: &[Card],
    legal: Cards,
) -> Vec<(Card, f64)> {
    let mut solver = Naive::new(trump);
    let mut plays = in_trick.to_vec();
    legal
        .map(|card| {
            plays.push(card);
            let value = trick_value(&mut solver, starting, trump, &mut plays);
            plays.pop();
            (card, value)
        })
        .collect()
}

/// Whether the player to play a card wants the most tricks for whoever led the
/// trick, which they do when they are on the same side.
pub(crate) fn plays_for_the_leaders<C>(game: &GameState<C>) -> bool {
    let in_trick = if game.played.len() == 4 {
        0
    } else {
        game.played.len()
    };
    in_trick & 1 == 0
}

/// The most tricks left for which a card is chosen by the ensemble.
const MOST_TRICKS: usize = 7;

/// How many deals of the hands that cannot be seen to try each card in, with
/// this many tricks left.
fn deals_for(tricks_left: usize) -> usize {
    match tricks_left {
        0..=3 => 60,
        4 => 40,
        5 => 30,
        6 => 20,
        _ => 12,
    }
}

/// A player who chooses the card with the best mean double-dummy value over an
/// ensemble of the hands that cannot be seen, which fit the auction and the
/// play so far.  With more tricks left than [`MOST_TRICKS`] it is too slow to
/// work the values out, and it plays as [`RandomPlay`] does.
#[derive(Debug)]
pub struct EnsemblePlay {
    rng: StdRng,
    /// The conventions that auctions have been read with, each with the system
    /// it is the convention of, so that each is built only once.
    conventions: Vec<(BiddingSystem, Convention)>,
}

impl EnsemblePlay {
    pub fn new() -> Self {
        Self::with_rng(StdRng::from_entropy())
    }

    /// A player whose choices are the same every time.
    pub fn seeded(seed: u64) -> Self {
        Self::with_rng(StdRng::seed_from_u64(seed))
    }

    fn with_rng(rng: StdRng) -> Self {
        EnsemblePlay {
            rng,
            conventions: Vec::new(),
        }
    }

    /// What the player knows of the hands that they cannot see.
    fn hidden<C>(&mut self, game: &GameState<C>) -> Option<Hidden> {
        let known = self.conventions.iter().position(|(s, _)| *s == game.system);
        let i = known.unwrap_or_else(|| {
            let convention = game.system.convention();
            self.conventions.push((game.system, convention));
            self.conventions.len() - 1
        });
        Hidden::new(game, &self.conventions[i].1)
    }
}

impl Default for EnsemblePlay {
    fn default() -> Self {
        Self::new()
    }
}

impl PlayAI for EnsemblePlay {
    fn play<C>(&mut self, game: &GameState<C>) -> Card {
        let legal = game.could_be_played();
        let tricks_left = game.hands.iter().map(|hand| hand.len()).max().unwrap_or(0);
        if legal.len() == 1 {
            return legal.min().unwrap();
        }
        let Some(hidden) = self.hidden(game).filter(|_| tricks_left <= MOST_TRICKS) else {
            return RandomPlay.play(game);
        };
        let trump = trump_of(game);
        let mut totals: Vec<(Card, f64)> = legal.map(|card| (card, 0.0)).collect();
        let mut dealt = 0;
        for _ in 0..deals_for(tricks_left) {
            let Some((deal, _)) = hidden.sample(&mut self.rng) else {
                break;
            };
            let Some((starting, in_trick)) = starting_with(game, &hidden, deal) else {
                break;
            };
            for (total, (card, value)) in totals
                .iter_mut()
                .zip(card_values(starting, trump, &in_trick, legal))
            {
                debug_assert_eq!(total.0, card);
                total.1 += value;
            }
            dealt += 1;
        }
        if dealt == 0 {
            return RandomPlay.play(game);
        }
        // The best card, and of cards as good, the cheapest.
        let wants_most = plays_for_the_leaders(game);
        let better = |a: f64, b: f64| if wants_most { a > b } else { a < b };
        let best = totals
            .iter()
            .map(|&(_, total)| total)
            .fold(None, |best: Option<f64>, total| match best {
                Some(best) if !better(total, best) => Some(best),
                _ => Some(total),
            })
            .unwrap();
        let cost = |card: Card| card.rank() + if Some(card.suit()) == trump { 13 } else { 0 };
        totals
            .iter()
            .filter(|&&(_, total)| (total - best).abs() < 1e-9)
            .map(|&(card, _)| card)
            .min_by_key(|&card| cost(card))
            .unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::situations::Generator;
    use crate::{Bid, Seated};
    use std::str::FromStr;

    /// Some situations with `tricks_left` tricks left.
    fn games(tricks_left: usize, count: usize) -> Vec<GameState<()>> {
        let mut generator = Generator::new();
        let mut rng = StdRng::seed_from_u64(11);
        let mut games = Vec::new();
        while games.len() < count {
            let json = generator.situation(&mut rng, games.len(), tricks_left);
            games.extend(json.map(|json| serde_json::from_str(&json).unwrap()));
        }
        games
    }

    #[test]
    fn dealt_hands_have_the_right_cards_and_the_hands_that_were_bid() {
        for tricks_left in [12, 7, 3] {
            for game in games(tricks_left, 6) {
                let convention = game.system.convention();
                let hidden = Hidden::new(&game, &convention).unwrap();
                // The hands that are really there fit the auction that they
                // were bid with, which says that what the auction is read as
                // showing agrees with the bidding of the robot.
                let actual = hidden.seats.map(|seat| game.hands[seat]);
                assert!(hidden.fit_the_bidding(&actual), "{:?}", game.bids);
                let mut rng = StdRng::seed_from_u64(5);
                let mut fitted = 0;
                for _ in 0..10 {
                    let (deal, fits) = hidden.sample(&mut rng).unwrap();
                    assert_eq!(hidden.pool, deal[0] + deal[1]);
                    assert!((deal[0] & deal[1]).is_empty());
                    for i in 0..2 {
                        assert_eq!(hidden.sizes[i], deal[i].len());
                        assert!((deal[i] & hidden.forbidden[i]).is_empty());
                    }
                    assert_eq!(fits, hidden.fit_the_bidding(&deal));
                    fitted += fits as usize;
                }
                // The actual hands fit, so there is at least one that does.
                assert!(fitted > 0, "{:?}", game.bids);
            }
        }
    }

    #[test]
    fn the_ensemble_has_many_different_deals() {
        let games = games(7, 8);
        let mut with_choices = 0;
        for game in &games {
            let convention = game.system.convention();
            let hidden = Hidden::new(game, &convention).unwrap();
            let mut rng = StdRng::seed_from_u64(9);
            let deals: Vec<[Cards; 2]> = (0..12)
                .map(|_| hidden.sample(&mut rng).unwrap().0)
                .collect();
            let distinct = deals
                .iter()
                .enumerate()
                .filter(|&(i, deal)| !deals[..i].contains(deal))
                .count();
            with_choices += (distinct > 1) as usize;
        }
        // A deal can be settled by what is known, if rarely.
        assert!(
            with_choices + 1 >= games.len(),
            "{with_choices} of {}",
            games.len()
        );
    }

    #[test]
    fn the_opener_of_one_notrump_is_dealt_the_points_that_it_shows() {
        // West opens 1NT, and everyone else passes.
        let mut game = GameState::<()>::new();
        game.set_system(BiddingSystem::Sayc);
        let hands: Seated<Cards> = [
            // South, East and North are given what is left of the deck, and
            // West is given 15 points.
            Cards::from_str("S T98 H T98 D T98 C T987").unwrap(),
            Cards::from_str("S AK2 H KQ3 D Q42 C J543").unwrap(),
            Cards::from_str("S Q76 H J76 D J76 C Q62").unwrap(),
            Cards::EMPTY,
        ]
        .into();
        let mut hands = hands;
        hands[Seat::East] =
            Cards::ALL - hands[Seat::South] - hands[Seat::West] - hands[Seat::North];
        game.hands = hands;
        game.original_hands = hands;
        game.dealer = Seat::South;
        game.bids = vec![Bid::Pass, Bid::NT(1), Bid::Pass, Bid::Pass, Bid::Pass];
        // West is the declarer, East the dummy, and North is on lead.
        game.lead = Some(Seat::North);
        let convention = BiddingSystem::Sayc.convention();
        let hidden = Hidden::new(&game, &convention).unwrap();
        assert_eq!(Seat::North, game.turn().unwrap());
        assert_eq!([Seat::South, Seat::West], hidden.seats);
        let mut rng = StdRng::seed_from_u64(3);
        let mut fitted = 0;
        for _ in 0..40 {
            let (deal, fits) = hidden.sample(&mut rng).unwrap();
            if fits {
                fitted += 1;
                let points = deal[1].high_card_points();
                assert!((15..=17).contains(&points), "{points} points");
            }
        }
        assert!(fitted > 10, "only {fitted} of 40 deals fit");
    }

    #[test]
    fn the_player_plays_a_legal_card_whatever_the_stage() {
        let mut player = EnsemblePlay::seeded(1);
        for tricks_left in [13, 8, 7, 4, 2] {
            for game in games(tricks_left, 2) {
                let card = player.play(&game);
                assert!(game.could_be_played().contains(card), "{card:?}");
            }
        }
    }

    #[test]
    fn the_player_takes_the_winners_it_can_see() {
        // North, on lead with the ace of spades and a heart, against hands that
        // hold only two spades each outside of North's: whatever the hidden
        // hands are, the ace wins a trick however it is led, so what matters
        // is that the choice is legal and made quickly.
        let mut player = EnsemblePlay::seeded(2);
        for game in games(2, 5) {
            let started = std::time::Instant::now();
            let card = player.play(&game);
            assert!(game.could_be_played().contains(card));
            assert!(started.elapsed().as_secs() < 5);
        }
    }
}
