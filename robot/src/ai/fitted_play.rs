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

/// The tables, each with its size and the facts that index it.  The role is
/// 0 for the declarer playing their own hand, 1 for the declarer playing the
/// dummy's, and 2 and 3 for the defenders on the declarer's left and right;
/// the place is how many cards have been played to the trick; how a card is
/// played is whether it leads, follows suit, ruffs or is discarded.
pub(crate) const TUPLES: [(usize, &str); 11] = [
    (
        4 * 4 * 4 * 2 * 2,
        "the role, the place, how the card is played, whether partner is \
         winning the trick, and whether the card beats the trick so far",
    ),
    (
        4 * 125,
        "the place, and whether each of the three cards just above is the \
         player's, in the other hand in sight, or hidden",
    ),
    (4 * 125, "the place, and the same of the three cards just below"),
    (
        4 * 4 * 6 * 2,
        "the place, how the card is played, how many cards of its suit are left \
         above it, and whether it is the player's lowest of the suit",
    ),
    (
        4 * 4 * 4 * 2,
        "the place, the player's length in the suit, the length of the other \
         hand in sight, and whether the suit is trumps",
    ),
    (4 * 4 * 6, "the place, how the card is played, and how many tricks are left"),
    (
        4 * 4 * 4 * 6,
        "the role, the place, how the card is played, and how the tricks the \
         declarer still needs for the contract compare with the tricks left: \
         already made, beaten, or with none, one, two, or more to spare",
    ),
    (
        4 * 4 * 20,
        "the place, how the card is played, and the fewest and most cards of \
         its suit that the first hidden hand after the player can hold, by the \
         auction and the play",
    ),
    (4 * 4 * 20, "the same of the second hidden hand"),
    (
        4 * 4 * 20 * 2,
        "the role, the place, the fewest and most points that the first hidden \
         hand can still hold, by the auction and the play, and whether the card \
         is an honour",
    ),
    (4 * 4 * 20 * 2, "the same of the second hidden hand"),
];

/// How many hidden units the play model has unless `PLAY_HIDDEN` says
/// otherwise: each table entry feeds them as well as the score, so that the
/// model can weigh combinations of the facts of different tables.
const HIDDEN: usize = 16;

/// How many units the play model's second hidden layer has unless
/// `PLAY_SECOND` says otherwise (0 for none): each takes all the units of
/// the first, so that the model can weigh combinations of combinations.
const SECOND: usize = 8;

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

/// How many entries the tables have, end to end.
pub(crate) const ENTRIES: usize = {
    let mut entries = 0;
    let mut i = 0;
    while i < TUPLES.len() {
        entries += TUPLES[i].0;
        i += 1;
    }
    entries
};

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
    /// left, as a number below 6 (see [`TUPLES`]).
    contract: usize,
    /// What is shown of the two hidden hands, the first after the player
    /// first.
    shown: [Shown; 2],
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
        })
    }

    /// The cards that the player could play: those of the suit led if they
    /// have any, and otherwise any.
    pub(crate) fn legal(&self) -> Cards {
        match self.lead {
            Some(lead) if !self.mine.in_suit(lead).is_empty() => self.mine.in_suit(lead),
            _ => self.mine,
        }
    }

    /// The entries of the tables of [`TUPLES`], laid end to end, that
    /// playing `card` looks up, one in each table.
    pub(crate) fn entries(&self, card: Card) -> [usize; TUPLES.len()] {
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
        let indices = [
            (((role * 4 + place) * 4 + how) * 2 + self.partner_winning as usize) * 2 + beats as usize,
            place * 125 + Cards::holders_above(&in_sight, card_now, 3),
            place * 125 + Cards::holders_below(&in_sight, card_now, 3),
            ((place * 4 + how) * 6 + above) * 2 + lowest as usize,
            ((place * 4 + length) * 4 + other_length) * 2 + trumps as usize,
            (place * 4 + how) * 6 + tricks,
            ((role * 4 + place) * 4 + how) * 6 + self.contract,
            (place * 4 + how) * 20 + self.shown[0].length_index(suit),
            (place * 4 + how) * 20 + self.shown[1].length_index(suit),
            ((role * 4 + place) * 20 + self.shown[0].points_index()) * 2 + (card.rank() >= 10) as usize,
            ((role * 4 + place) * 20 + self.shown[1].points_index()) * 2 + (card.rank() >= 10) as usize,
        ];
        let mut entries = [0; TUPLES.len()];
        let mut offset = 0;
        for (t, index) in indices.into_iter().enumerate() {
            debug_assert!(index < TUPLES[t].0, "{}", TUPLES[t].1);
            entries[t] = offset + index;
            offset += TUPLES[t].0;
        }
        entries
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
/// by the play model `weights`: the softmax of its scores divided by
/// `temperature`, which is fitted so that these are chances and not only an
/// order.
#[cfg(test)]
pub(crate) fn chances(view: &View, weights: &Network, temperature: f64) -> Vec<(Card, f64)> {
    let scores: Vec<(Card, f64)> = view
        .legal()
        .map(|card| {
            let score = weights.score(&view.entries(card));
            (card, score / temperature)
        })
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
    weights: &Network,
    temperature: f64,
) -> Vec<(Card, f64)> {
    let mut totals: Vec<(Card, f64)> = Vec::new();
    let total_weight: f64 = deals.iter().map(|d| d.1).sum();
    for &(deal, weight) in deals {
        let hands = hands_at(game, seats, deal, trick);
        let Some(view) = View::at(game, convention, hands, trick) else {
            continue;
        };
        for (card, chance) in chances(&view, weights, temperature) {
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
    weights: &Network,
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
                        chances(&view, weights, temperature)
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

/// The play model that [`ModelPlay`] plays by, as little-endian 32-bit words:
/// the number of weights, the network's entries and the sizes of its two
/// hidden layers, then the weights as floats.  `build.rs` takes them from the
/// play model file `src/ai/play_model.bin` (see `fitted_play::model_file`).
/// That file is not kept in git, and `build.rs` puts nothing here when it is
/// missing.
static MODEL: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/play_model.bin"));

/// The play model of [`MODEL`], or `None` if there are no weights, or they
/// are not for a network of this shape, as when they were written for
/// another.
fn model() -> Option<&'static Network> {
    static NETWORK: std::sync::OnceLock<Option<Network>> = std::sync::OnceLock::new();
    NETWORK
        .get_or_init(|| {
            let words = MODEL.as_chunks::<4>().0;
            let shape: Vec<usize> = words.iter().take(4).map(|w| u32::from_le_bytes(*w) as usize).collect();
            let params: Vec<f64> = words.iter().skip(4).map(|w| f32::from_le_bytes(*w) as f64).collect();
            let wanted = [Network::size(ENTRIES, HIDDEN, SECOND), ENTRIES, HIDDEN, SECOND];
            (shape == wanted && params.len() == wanted[0])
                .then(|| Network::with_params(ENTRIES, HIDDEN, SECOND, params))
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
        let Some(network) = model() else {
            return super::RandomPlay.play(game);
        };
        let convention = game.system.convention();
        match View::new(game, &convention) {
            Some(view) => {
                view.legal()
                    .map(|card| (network.score(&view.entries(card)), card))
                    .max_by(|a, b| a.0.total_cmp(&b.0))
                    .unwrap()
                    .1
            }
            None => {
                use super::opening_lead::{likely_opening_leads, Auction};
                match (Auction::new(game), game.lead) {
                    (Some(auction), Some(leader)) => likely_opening_leads(&auction, game.hands[leader])[0],
                    _ => game.could_be_played().min().unwrap(),
                }
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

    fn number(name: &str, default: usize) -> usize {
        var(name, &default.to_string()).parse().unwrap()
    }

    fn pair(card: Card) -> [u8; 2] {
        [card.suit() as u8, card.rank()]
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
                    for (t, entry) in view.entries(card).into_iter().enumerate() {
                        let start: usize = TUPLES[..t].iter().map(|t| t.0).sum();
                        assert!((start..start + TUPLES[t].0).contains(&entry));
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

    /// The model that the build includes, when there is one, is of the shape
    /// that this code plays by.
    #[test]
    fn the_included_model_is_of_this_shape() {
        assert_eq!(MODEL.is_empty(), model().is_none());
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
                seen.push(legal.iter().map(|&card| view.entries(card)).collect::<Vec<_>>());
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
                    let now: Vec<_> = view.legal().map(|card| view.entries(card)).collect();
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
        let mut weights = Network::new(ENTRIES, 4, 3, 3);
        for p in &mut weights.params {
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
    /// the weights in the file `PLAY_WEIGHTS`: the chances that a player
    /// who plays as the ensemble's values say would play each card.
    #[test]
    #[ignore]
    fn fit_play_temperature() {
        let path = std::env::var("PLAY_WEIGHTS").expect("PLAY_WEIGHTS");
        let weights = Network::read(&path, ENTRIES, number("PLAY_HIDDEN", HIDDEN), number("PLAY_SECOND", SECOND));
        let test = read_situations(&std::env::var("PLAY_TEST").expect("PLAY_TEST"), TUPLES.len());
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
                        let Some(hidden) = Hidden::new(&game, convention) else {
                            continue;
                        };
                        let mut rng = StdRng::seed_from_u64(seed * 1_000_000 + i as u64);
                        let legal = game.could_be_played();
                        let trump = trump_of(&game);
                        let mut values: Vec<Vec<f64>> = legal.map(|_| Vec::new()).collect();
                        for deal in hidden.ensemble(&mut rng, deals, Some(convention)) {
                            let Some((starting, in_trick)) = starting_with(&game, &hidden, deal) else {
                                continue;
                            };
                            for (all, (_, value)) in values.iter_mut().zip(card_values(starting, trump, &in_trick, legal)) {
                                all.push(value);
                            }
                        }
                        if values.iter().any(|v| v.is_empty()) {
                            continue;
                        }
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

    /// A situation from the data that `collect_play_data` writes, as the
    /// choices of `fitting`: each card's entries and its mean value to the
    /// player, the tricks of their side, over the ensemble.
    struct Situation {
        stage: usize,
        choice: Vec<(Vec<usize>, f64)>,
    }

    /// The situations in the files `paths`, comma-separated, each card with
    /// the entries of only the first `tables` of the tables of [`TUPLES`].
    fn read_situations(paths: &str, tables: usize) -> Vec<Situation> {
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
                        (view.entries(card)[..tables].to_vec(), value)
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
    /// only the first `PLAY_TABLES` (all) of the tables are used, to see what
    /// the others add.
    #[test]
    #[ignore]
    fn fit_play_model() {
        let tables = number("PLAY_TABLES", TUPLES.len());
        let train = read_situations(&std::env::var("PLAY_DATA").expect("PLAY_DATA"), tables);
        let test = read_situations(&std::env::var("PLAY_TEST").expect("PLAY_TEST"), tables);
        let steps = number("PLAY_STEPS", 500);
        let choices: fitting::Choices = train.into_iter().map(|s| s.choice).collect();
        let (hidden, second) = (number("PLAY_HIDDEN", HIDDEN), number("PLAY_SECOND", SECOND));
        let start = || Network::new(ENTRIES, hidden, second, 1);
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
    /// and the tricks that side had already won.
    struct Decision {
        learner: usize,
        cards: Vec<([usize; TUPLES.len()], f64)>,
        chosen: usize,
        side: usize,
        trick: usize,
        won_before: usize,
    }

    /// How a seat chooses its cards in self-play.
    #[derive(Clone, Copy)]
    enum Player<'a> {
        /// By this learner of the population, which learns from its choices.
        Learning(usize),
        /// By the model with these weights, a snapshot of an earlier one,
        /// taken after this many rounds.
        Past(&'a Network, usize),
        /// Any card that it could play, at random.
        Random,
    }

    /// Plays `json`, a game whose opening lead is next, out to the end, the
    /// opening lead by `likely_opening_leads` and each other card by the
    /// seat that chooses it as its `players` say: a model chooses at random
    /// with its chances at `temperature`, a learner by its weights in
    /// `learners`.  Gives the choices that the learners made and the tricks
    /// each side won, and with `record`, the whole game, for watching.
    fn play_out(
        json: &str,
        convention: &Convention,
        learners: &[Network],
        temperature: f64,
        players: [Player; 4],
        record: bool,
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
                let cards: Vec<([usize; TUPLES.len()], f64)> = chances(&view, weights, temperature)
                    .into_iter()
                    .map(|(card, chance)| (view.entries(card), chance))
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
                    decisions.push(Decision {
                        learner,
                        cards,
                        chosen,
                        side,
                        trick: game.tricks.len(),
                        won_before: if side == 0 { game.ns_tricks } else { game.ew_tricks },
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
                    "by": player_name(player),
                    "chances": considered,
                }));
            }
            game.play(card);
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
                            player_name(players[s])
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
    fn player_name(player: Player) -> String {
        match player {
            Player::Learning(learner) => format!("learner {}", learner + 1),
            Player::Past(_, 0) => "a model before it learned".into(),
            Player::Past(_, round) => format!("a model after {round} rounds"),
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
    /// beside itself: four learners (chosen at random if there are more) are
    /// split into two partnerships in each of the three ways, and each split
    /// plays each side of the deal, and each of those with the partners in
    /// either seat, which is twelve plays; and `PLAY_SELF_WILDCARDS` (4) more
    /// plays, two matches at two tables, of a learner partnered by a past
    /// model or a player of any card (in the proportions of `PLAY_SELF_PAST`
    /// and `PLAY_SELF_RANDOM`) against two other learners.  The round robin
    /// needs at least four learners.  Every seat of a game is played by a
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
    /// `round-NNNNN-learner-K.txt`), and ranks the learners in a tournament
    /// of two-table matches between each two of them on `PLAY_SELF_TOURNAMENT`
    /// (200) other deals; the best is the one that the game plays by.  Every
    /// `PLAY_SELF_REPORT` (10) rounds of `PLAY_SELF_ROUNDS` (100), it writes
    /// the best's weights to the file `PLAY_SELF_WEIGHTS` if that is set,
    /// sample games to `PLAY_SELF_WATCH` (see `self_play_viewer.html`), and a
    /// play model file (see `model_file`) to `PLAY_SELF_CHECKPOINT`, and
    /// reports the tricks a play each learner's choices lose by the
    /// ensemble's values on the data in `PLAY_TEST` if that is set.  It uses
    /// `PLAY_THREADS` threads, or else every core.
    ///
    /// With `PLAY_SELF_RESUME` naming a play model file, a run takes up from
    /// it, its rounds numbered on from the file's, with its learners and
    /// league; learners that the file lacks start from nothing.  A learner
    /// with no state of Adam (see `make_play_model_file`) starts with steps
    /// that grow from nothing over 20 rounds, so that its weights are not
    /// jolted while Adam learns their scale.
    #[test]
    #[ignore]
    fn self_play() {
        let float = |name: &str, default: f64| -> f64 { var(name, &default.to_string()).parse().unwrap() };
        let rounds = number("PLAY_SELF_ROUNDS", 100);
        let batch = number("PLAY_SELF_DEALS", 256);
        let group = number("PLAY_SELF_GROUP", 8);
        let report = number("PLAY_SELF_REPORT", 10);
        let temperature = float("PLAY_SELF_TEMPERATURE", 1.0);
        let rate = float("PLAY_SELF_RATE", 0.01);
        let random_share = float("PLAY_SELF_RANDOM", 0.0);
        let past_share = float("PLAY_SELF_PAST", 0.0);
        let entropy_target = float("PLAY_SELF_ENTROPY", 0.25);
        let decay = float("PLAY_SELF_DECAY", 0.0);
        let mut bonus = 0.01;
        let every = number("PLAY_SELF_SNAPSHOT", 50);
        let population = number("PLAY_SELF_LEARNERS", 4);
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
        let test = std::env::var("PLAY_TEST")
            .ok()
            .map(|paths| read_situations(&paths, TUPLES.len()));
        let (hidden, second) = (number("PLAY_HIDDEN", HIDDEN), number("PLAY_SECOND", SECOND));
        let mut learners: Vec<Network> = Vec::new();
        let mut adams: Vec<fitting::Adam> = Vec::new();
        let mut warmups: Vec<usize> = Vec::new();
        let mut snapshots: Vec<(usize, Network)> = Vec::new();
        let mut first_round = 1;
        if let Ok(path) = std::env::var("PLAY_SELF_RESUME") {
            let file = model_file::ModelFile::read(&path, ENTRIES, hidden, second);
            first_round = file.round + 1;
            for learner in file.learners {
                warmups.push(if learner.adam.is_some() { 0 } else { 20 });
                adams.push(learner.adam.unwrap_or_else(|| fitting::Adam::new(learner.weights.params.len())));
                learners.push(learner.weights);
            }
            snapshots = file.league;
            println!(
                "resuming after round {} with {} learners, and past models from rounds {:?}",
                file.round,
                learners.len(),
                snapshots.iter().map(|s| s.0).collect::<Vec<_>>()
            );
        }
        while learners.len() < population {
            let network = Network::new(ENTRIES, hidden, second, 11 + learners.len() as u64);
            adams.push(fitting::Adam::new(network.params.len()));
            warmups.push(0);
            learners.push(network);
        }
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
        // The same threads play every round.  Each regex of the bidding
        // tables keeps a cache for each thread that uses it, so new threads
        // every round would leave more and more caches behind.
        let workers = rayon::ThreadPoolBuilder::new().num_threads(threads).build().unwrap();
        let started = std::time::Instant::now();
        let size = learners[0].params.len();
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
            type Results = (Vec<Vec<f64>>, Vec<usize>, f64, Vec<serde_json::Value>);
            let results: Vec<Results> = workers.install(|| {
                deals
                    .par_chunks(chunk)
                    .enumerate()
                    .map(|(t, deals)| {
                        {
                            let mut rng = StdRng::seed_from_u64(seed + t as u64);
                            let mut conventions = std::collections::HashMap::new();
                            let mut gradients = vec![vec![0.0; size]; learners_now.len()];
                            let mut counts = vec![0; learners_now.len()];
                            let mut entropy = 0.0;
                            let mut sampled = Vec::new();
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
                                    let mut four: Vec<usize> = (0..population).collect();
                                    four.shuffle(&mut rng);
                                    let [a, b, c, d] = [four[0], four[1], four[2], four[3]].map(Player::Learning);
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
                                        four.shuffle(&mut rng);
                                        let [x, y, z] = [four[0], four[1], four[2]].map(Player::Learning);
                                        let wild = if rng.gen_range(0.0..past_share + random_share) < past_share
                                            || random_share == 0.0
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
                                        let (decisions, won, game) =
                                            play_out(json, convention, learners_now, temperature, players, record, &mut rng);
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
                                        for (j, (entries, chance)) in d.cards.iter().enumerate() {
                                            let wanted = if j == d.chosen { 1.0 } else { 0.0 };
                                            // The slope of the entropy of the
                                            // choice with this card's score.
                                            let log = if *chance > 0.0 { chance.ln() } else { 0.0 };
                                            let spreading = -chance * (log + spread);
                                            let g = (advantage * (wanted - chance) + bonus_now * spreading) / temperature;
                                            learners_now[d.learner].add_gradient(entries, g, &mut gradients[d.learner]);
                                        }
                                        counts[d.learner] += 1;
                                    }
                                }
                            }
                            (gradients, counts, entropy, sampled)
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
            for (i, learner) in learners.iter_mut().enumerate() {
                let gradient: Vec<f64> = (0..size)
                    .map(|j| results.iter().map(|r| r.0[i][j]).sum::<f64>() / counts[i].max(1) as f64)
                    .collect();
                let ramp = if warmups[i] > 0 {
                    ((round - first_round + 1) as f64 / warmups[i] as f64).min(1.0)
                } else {
                    1.0
                };
                for p in &mut learner.params {
                    *p *= 1.0 - rate * ramp * decay;
                }
                adams[i].step(&mut learner.params, &gradient, rate * ramp);
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
                        learner.write(&format!("{directory}/round-{round:05}-learner-{}.txt", i + 1));
                    }
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
                            let loss = test
                                .iter()
                                .map(|s| {
                                    let most = s.choice.iter().map(|c| c.1).fold(f64::MIN, f64::max);
                                    most - s.choice[learner.choice(&s.choice)].1
                                })
                                .sum::<f64>()
                                / test.len() as f64;
                            format!("{loss:.4}")
                        })
                        .collect();
                    print!(", the test data's choices lose {} tricks a play", losses.join(", "));
                }
                println!();
                if let Ok(path) = std::env::var("PLAY_SELF_WEIGHTS") {
                    learners[best].write(&path);
                }
                if let Ok(path) = std::env::var("PLAY_SELF_CHECKPOINT") {
                    model_file::ModelFile {
                        round,
                        best: learners[best].clone(),
                        learners: learners
                            .iter()
                            .zip(&adams)
                            .map(|(weights, adam)| model_file::Learner {
                                weights: weights.clone(),
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
    fn tournament(learners: &[Network], deals: &[String]) -> Vec<f64> {
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

    /// The past models that a play model file keeps of `snapshots`: the
    /// first four, and the last eight.
    fn league_of(snapshots: &[(usize, Network)]) -> Vec<(usize, Network)> {
        let first = snapshots.len().min(4);
        let last = snapshots.len().saturating_sub(8).max(first);
        snapshots[..first].iter().chain(&snapshots[last..]).cloned().collect()
    }

    /// Writes a play model file (see `model_file`) to `PLAY_MODEL_FILE` for a
    /// run of self-play that did not write one: the weights in the file
    /// `PLAY_WEIGHTS` after round `PLAY_ROUND`, as the game's model and as a
    /// learner with no state of Adam, and as its league, of the snapshots in
    /// the directory `PLAY_SELF_SNAPSHOTS` (whose names start `round-` and
    /// the round), the first four and the last eight.
    #[test]
    #[ignore]
    fn make_play_model_file() {
        let (hidden, second) = (number("PLAY_HIDDEN", HIDDEN), number("PLAY_SECOND", SECOND));
        let weights = Network::read(&std::env::var("PLAY_WEIGHTS").expect("PLAY_WEIGHTS"), ENTRIES, hidden, second);
        let directory = std::env::var("PLAY_SELF_SNAPSHOTS").expect("PLAY_SELF_SNAPSHOTS");
        let mut snapshots: Vec<(usize, Network)> = std::fs::read_dir(&directory)
            .unwrap()
            .filter_map(|entry| {
                let path = entry.unwrap().path();
                let name = path.file_stem()?.to_str()?;
                let digits: String = name.strip_prefix("round-")?.chars().take_while(char::is_ascii_digit).collect();
                Some((digits.parse().ok()?, Network::read(path.to_str()?, ENTRIES, hidden, second)))
            })
            .collect();
        snapshots.sort_by_key(|s| s.0);
        model_file::ModelFile {
            round: number("PLAY_ROUND", 0),
            best: weights.clone(),
            learners: vec![model_file::Learner { weights, adam: None }],
            league: league_of(&snapshots),
        }
        .write(&std::env::var("PLAY_MODEL_FILE").expect("PLAY_MODEL_FILE"));
    }

    /// The play model in `path`: a play model file, or a file of its
    /// weights one a line, as self-play writes them.
    fn read_model(path: &str) -> Network {
        let (hidden, second) = (number("PLAY_HIDDEN", HIDDEN), number("PLAY_SECOND", SECOND));
        let start = std::fs::read(path).unwrap().into_iter().take(8).collect::<Vec<u8>>();
        if start == model_file::MAGIC {
            model_file::ModelFile::read(path, ENTRIES, hidden, second).best
        } else {
            Network::read(path, ENTRIES, hidden, second)
        }
    }

    /// The card that the play model `network` scores highest for the player
    /// to play in `game`, or for the opening lead, the first of
    /// `likely_opening_leads`.
    fn best_card(game: &GameState<()>, convention: &Convention, network: &Network) -> Card {
        use crate::ai::opening_lead::{likely_opening_leads, Auction};
        match View::new(game, convention) {
            Some(view) => view
                .legal()
                .max_by(|&a, &b| network.score(&view.entries(a)).total_cmp(&network.score(&view.entries(b))))
                .unwrap(),
            None => {
                let leader = game.lead.unwrap();
                likely_opening_leads(&Auction::new(game).unwrap(), game.hands[leader])[0]
            }
        }
    }

    /// Plays `json`, a game whose opening lead is next, to the end: the side
    /// `model_side` (0 for north and south) by the play model `model`, and
    /// the other by `other` if there is one, or else by the robot.  Gives the
    /// tricks of north and south.
    fn play_at_a_table(
        json: &str,
        convention: &Convention,
        model: &Network,
        model_side: usize,
        other: Option<&Network>,
    ) -> usize {
        use crate::ai::PlayAI;
        let mut game: GameState<()> = serde_json::from_str(json).unwrap();
        let mut robot = super::super::RandomPlay;
        while !game.hand_done {
            let place = current_trick(&game).len();
            let seat = chooser(&game, game.lead.unwrap() + place);
            let card = if seat as usize % 2 == model_side {
                best_card(&game, convention, model)
            } else {
                match other {
                    Some(network) => best_card(&game, convention, network),
                    None => robot.play(&game),
                }
            };
            game.play(card);
        }
        game.ns_tricks
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
