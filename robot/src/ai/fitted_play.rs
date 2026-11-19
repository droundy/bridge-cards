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
#[cfg(test)]
const HIDDEN: usize = 16;

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
    /// the hand of `seat`, which has `played` and holds some of `hidden`.
    fn new<C>(
        game: &GameState<C>,
        convention: &Convention,
        seat: Seat,
        played: Cards,
        hidden: Cards,
    ) -> Shown {
        let first = (seat as usize + 4 - game.dealer as usize) % 4;
        let offset = (game.bids.len() + 4 - first) % 4;
        let (min, max) = convention.shown_bounds(&game.bids, offset);
        let length = Suit::ALL.map(|suit| {
            if !game.shown_out[seat].in_suit(suit).is_empty() {
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
    /// with the hands then `hands` and the cards that each had played before
    /// the trick `played`: any moment of the play of `game`, or one that
    /// might come.
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
        let won = match declarer {
            Seat::North | Seat::South => game.ns_tricks,
            _ => game.ew_tricks,
        };
        let left_to_play = 13 - game.ns_tricks - game.ew_tricks;
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
            Shown::new(game, convention, hidden_seat, played[hidden_seat], hidden)
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
        }
        for (i, &card) in cards.iter().enumerate() {
            played[lead + i] += Cards::singleton(card);
        }
    }
    likelihood
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

    #[test]
    fn the_chances_add_up_and_cover_only_cards_that_could_be_held() {
        let mut rng = StdRng::seed_from_u64(3);
        let mut weights = Network::new(ENTRIES, 4, 3);
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
        let weights = Network::read(&path, ENTRIES, number("PLAY_HIDDEN", HIDDEN));
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
        let hidden = number("PLAY_HIDDEN", HIDDEN);
        let start = || Network::new(ENTRIES, hidden, 1);
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

    /// One choice of a card in a game of self-play: each card that could
    /// be played, with its entries and chance, which one was played, the
    /// side that chose it (0 for north and south) and the tricks that side
    /// had already won.
    struct Decision {
        cards: Vec<([usize; TUPLES.len()], f64)>,
        chosen: usize,
        side: usize,
        trick: usize,
        won_before: usize,
    }

    /// How a seat chooses its cards in self-play.
    #[derive(Clone, Copy)]
    enum Player<'a> {
        /// By the model that is learning, whose choices are learned from.
        Learning,
        /// By the model with these weights, a snapshot of an earlier one,
        /// taken after this many rounds.
        Past(&'a Network, usize),
        /// Any card that it could play, at random.
        Random,
    }

    /// Plays `json`, a game whose opening lead is next, out to the end, the
    /// opening lead by `likely_opening_leads` and each other card by the
    /// seat that chooses it as its `players` say: a model chooses at random
    /// with its chances at `temperature`, the learning one with `weights`.
    /// Gives the choices that the learning model made and the tricks each
    /// side won, and with `record`, the whole game, for watching.
    fn play_out(
        json: &str,
        convention: &Convention,
        weights: &Network,
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
                Player::Past(past, _) => Some((past, false)),
                Player::Learning => Some((weights, true)),
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
                if learning {
                    decisions.push(Decision {
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
            Player::Learning => "the model learning".into(),
            Player::Past(_, 0) => "the model before it learned".into(),
            Player::Past(_, round) => format!("the model after {round} rounds"),
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

    /// Learns the weights of the play model from nothing, by playing
    /// against itself.  Each round plays `PLAY_SELF_DEALS` (256) deals of a
    /// pool of `PLAY_SELF_POOL` (4000) deals that the robot bid, each
    /// `PLAY_SELF_GROUP` (8) times, every seat choosing its cards at random
    /// with the chances of the model at `PLAY_SELF_TEMPERATURE` (1), and
    /// moves the weights by a step of Adam of size `PLAY_SELF_RATE` (0.01)
    /// up the gradient of the tricks each side takes after each of its
    /// choices, less the mean of that over the plays of the same deal (so
    /// that what is learned is what made a play of a deal better than the
    /// others).  So that the model does not learn only to play with itself,
    /// each seat of a game is instead played by a snapshot of the model from
    /// an earlier round, chosen at random, with the chance `PLAY_SELF_PAST`
    /// (0), or else by a player of any card at random with the chance
    /// `PLAY_SELF_RANDOM` (0); that is partners as well as opponents, and
    /// only the learning model's own choices are learned from.  A snapshot is
    /// kept every `PLAY_SELF_SNAPSHOT` (50) rounds, the first of the model
    /// before it learned anything, and each is written to the directory
    /// `PLAY_SELF_SNAPSHOTS` if that is set, as `round-NNNNN.txt`, to play
    /// against later.  It starts from the weights in the file
    /// `PLAY_SELF_START` if that is set, and otherwise from nothing.  Every
    /// `PLAY_SELF_REPORT` (10) rounds of `PLAY_SELF_ROUNDS` (100), it writes
    /// the weights to the file `PLAY_SELF_WEIGHTS` if that is set, and
    /// reports the tricks a play its choices lose by the ensemble's values on
    /// the data in `PLAY_TEST` if that is set.  It uses every core.
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
        let every = number("PLAY_SELF_SNAPSHOT", 50);
        let directory = std::env::var("PLAY_SELF_SNAPSHOTS").ok();
        if let Some(directory) = &directory {
            std::fs::create_dir_all(directory).unwrap();
        }
        let pool: Vec<String> = crate::ai::opening_lead::candidates::opening_leads(7, number("PLAY_SELF_POOL", 4000))
            .iter()
            .map(|game| serde_json::to_string(game).unwrap())
            .collect();
        let test = std::env::var("PLAY_TEST")
            .ok()
            .map(|paths| read_situations(&paths, TUPLES.len()));
        let hidden = number("PLAY_HIDDEN", HIDDEN);
        let mut weights = match std::env::var("PLAY_SELF_START") {
            Ok(path) => Network::read(&path, ENTRIES, hidden),
            Err(_) => Network::new(ENTRIES, hidden, 11),
        };
        let mut adam = fitting::Adam::new(weights.params.len());
        let mut snapshots: Vec<(usize, Network)> = vec![(0, weights.clone())];
        let mut watched: Vec<serde_json::Value> = Vec::new();
        let samples = number("PLAY_SELF_SAMPLES", 2);
        let mut rng = StdRng::seed_from_u64(11);
        let threads = std::thread::available_parallelism().map_or(4, |n| n.get());
        for round in 1..=rounds {
            let deals: Vec<usize> = (0..batch).map(|_| rng.gen_range(0..pool.len())).collect();
            let seed = rng.gen::<u64>();
            let chunk = batch.div_ceil(threads);
            let weights_now = &weights;
            let pool = &pool;
            let past = &snapshots;
            let watching = round % report == 0 || round == rounds;
            let results: Vec<(Vec<f64>, usize, f64, Vec<serde_json::Value>)> = std::thread::scope(|scope| {
                let handles: Vec<_> = deals
                    .chunks(chunk)
                    .enumerate()
                    .map(|(t, deals)| {
                        scope.spawn(move || {
                            let mut rng = StdRng::seed_from_u64(seed + t as u64);
                            let mut conventions = std::collections::HashMap::new();
                            let mut gradient = vec![0.0; weights_now.params.len()];
                            let (mut count, mut entropy) = (0, 0.0);
                            let mut sampled = Vec::new();
                            for (d, &deal) in deals.iter().enumerate() {
                                let json = &pool[deal];
                                let system: GameState<()> = serde_json::from_str(json).unwrap();
                                let convention = conventions
                                    .entry(format!("{:?}", system.system))
                                    .or_insert_with(|| system.system.convention());
                                let games: Vec<(Vec<Decision>, [usize; 2])> = (0..group)
                                    .map(|g| {
                                        let players = [0; 4].map(|_| {
                                            let draw = rng.gen_range(0.0..1.0);
                                            if draw < past_share {
                                                let (round, ref weights) = past[rng.gen_range(0..past.len())];
                                                Player::Past(weights, round)
                                            } else if draw < past_share + random_share {
                                                Player::Random
                                            } else {
                                                Player::Learning
                                            }
                                        });
                                        let record = watching && t == 0 && g == 0 && d < samples;
                                        let (decisions, won, game) =
                                            play_out(json, convention, weights_now, temperature, players, record, &mut rng);
                                        sampled.extend(game);
                                        (decisions, won)
                                    })
                                    .collect();
                                // The mean over the plays of this deal of the
                                // tricks each side took from each trick on.
                                let mut sums = [[0.0; 2]; 14];
                                let mut counts = [[0.0; 2]; 14];
                                for (decisions, won) in &games {
                                    for d in decisions {
                                        sums[d.trick][d.side] += (won[d.side] - d.won_before) as f64;
                                        counts[d.trick][d.side] += 1.0;
                                    }
                                }
                                for (decisions, won) in &games {
                                    for d in decisions {
                                        let to_go = (won[d.side] - d.won_before) as f64;
                                        let advantage = to_go - sums[d.trick][d.side] / counts[d.trick][d.side];
                                        for (j, (entries, chance)) in d.cards.iter().enumerate() {
                                            let wanted = if j == d.chosen { 1.0 } else { 0.0 };
                                            let g = advantage * (wanted - chance) / temperature;
                                            weights_now.add_gradient(entries, g, &mut gradient);
                                            if *chance > 0.0 {
                                                entropy -= chance * chance.ln();
                                            }
                                        }
                                        count += 1;
                                    }
                                }
                            }
                            (gradient, count, entropy, sampled)
                        })
                    })
                    .collect();
                handles.into_iter().map(|h| h.join().unwrap()).collect()
            });
            let decisions: usize = results.iter().map(|r| r.1).sum();
            let entropy: f64 = results.iter().map(|r| r.2).sum::<f64>() / decisions.max(1) as f64;
            let gradient: Vec<f64> = (0..weights.params.len())
                .map(|j| results.iter().map(|r| r.0[j]).sum::<f64>() / decisions.max(1) as f64)
                .collect();
            adam.step(&mut weights.params, &gradient, rate);
            for mut game in results.into_iter().flat_map(|r| r.3) {
                game["round"] = round.into();
                watched.push(game);
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
                        include_str!("self_play_viewer.html").replace("[/*GAMES*/]", &data)
                    );
                    std::fs::write(path, page).unwrap();
                }
                print!("round {round}: {decisions} choices, entropy {entropy:.3} a choice");
                if let Some(test) = &test {
                    let loss: f64 = test
                        .iter()
                        .map(|s| {
                            let best = s.choice.iter().map(|c| c.1).fold(f64::MIN, f64::max);
                            best - s.choice[weights.choice(&s.choice)].1
                        })
                        .sum::<f64>()
                        / test.len() as f64;
                    print!(", the test data's choices lose {loss:.4} tricks a play");
                }
                println!();
                if let Ok(path) = std::env::var("PLAY_SELF_WEIGHTS") {
                    weights.write(&path);
                }
            }
            if round % every == 0 {
                snapshots.push((round, weights.clone()));
                if let Some(directory) = &directory {
                    weights.write(&format!("{directory}/round-{round:05}.txt"));
                }
            }
        }
    }
}
