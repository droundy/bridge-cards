//! Beliefs about where the cards that a player cannot see lie (see
//! `robot/MODELS.md`, "Card locations"): a distribution over the ways that
//! the unseen cards can be split between the two hidden hands, the first
//! and the second after the player, as the play model's `FirstLength` and
//! `SecondLength` have them.
//!
//! With x<sub>c</sub> = 1 when card c is in the first hidden hand, and
//! L<sub>s</sub> and H that hand's length in suit s and its high card
//! points,
//!
//!   P(x) ∝ exp(Σ<sub>c</sub> h<sub>c</sub>x<sub>c</sub> +
//!   Σ<sub>s</sub> φ<sub>s</sub>(L<sub>s</sub>) + ψ(H)),
//!
//! over the splits that give the first hand the number of cards that it
//! holds and that agree with the cards whose place is known (the suits a
//! hand has shown out of).  It is the maximum-entropy distribution with
//! given chances for each card, each suit's length and the points, so those
//! cannot contradict each other, and with every parameter 0 it is vacant
//! places.  Each suit's sum over its own splits is a polynomial in two
//! variables, one counting the first hand's cards and one its points, and
//! the product of the four gives the normalization, the chances and exact
//! deals.

use bridge_deck::{Card, Cards, Suit};
use rand::Rng;

/// The most cards, and one more than the most points, that a hand can hold.
const COUNTS: usize = 14;
const POINTS: usize = 41;

/// The parameters are kept within this, beyond which a card's place is
/// certain to within e<sup>-30</sup> anyway, so that no sum overflows.
const MOST: f64 = 30.0;

/// The number of `card` among the 52.
pub(crate) fn index(card: Card) -> usize {
    card.suit() as usize * 13 + card.rank() as usize - 2
}

/// The high card points of `card`.
fn points(card: Card) -> usize {
    card.rank().saturating_sub(10) as usize
}

/// The parameters of a [`Beliefs`], or a slope of the log likelihood in
/// them: a field for each card, a potential for each suit and length of the
/// first hidden hand, and one for its points.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Parameters {
    pub(crate) h: [f64; 52],
    pub(crate) phi: [[f64; COUNTS]; 4],
    pub(crate) psi: [f64; POINTS],
}

impl Default for Parameters {
    fn default() -> Self {
        Parameters { h: [0.0; 52], phi: [[0.0; COUNTS]; 4], psi: [0.0; POINTS] }
    }
}

/// A distribution over the splits of the unseen cards between the two
/// hidden hands.
#[derive(Clone, Debug)]
pub(crate) struct Beliefs {
    /// The cards of the two hidden hands.
    pub(crate) unseen: Cards,
    /// Those known to be in the first, and in the second.
    pub(crate) in_first: Cards,
    pub(crate) in_second: Cards,
    /// How many cards the first holds.
    pub(crate) first_size: usize,
    pub(crate) parameters: Parameters,
}

/// The chances that a [`Beliefs`] gives.
#[derive(Clone, Debug)]
pub(crate) struct Chances {
    /// Of each card being in the first hidden hand (0 for a card that is not
    /// unseen).
    pub(crate) card: [f64; 52],
    /// Of the first hand holding each number of cards of each suit.
    pub(crate) length: [[f64; COUNTS]; 4],
    /// Of the first hand holding each number of high card points.
    pub(crate) points: [f64; POINTS],
    /// The log of the normalization.
    pub(crate) log_normalization: f64,
}

/// Weights by the first hand's cards (from 0 to 13) and points (from 0 to
/// 40), each times e<sup>`log_scale`</sup>.
#[derive(Clone)]
struct Poly {
    log_scale: f64,
    c: [[f64; POINTS]; COUNTS],
}

impl Poly {
    fn one() -> Poly {
        let mut c = [[0.0; POINTS]; COUNTS];
        c[0][0] = 1.0;
        Poly { log_scale: 0.0, c }
    }

    /// The same weights with the largest made 1.
    fn normalized(mut self) -> Poly {
        let most = self.c.iter().flatten().copied().fold(0.0, f64::max);
        if most > 0.0 {
            self.c.iter_mut().flatten().for_each(|w| *w /= most);
            self.log_scale += most.ln();
        }
        self
    }

    /// One more than the most cards and the most points with any weight.
    fn extent(&self) -> (usize, usize) {
        let (mut counts, mut points) = (0, 0);
        for (k, row) in self.c.iter().enumerate() {
            if let Some(p) = row.iter().rposition(|&w| w > 0.0) {
                counts = k + 1;
                points = points.max(p + 1);
            }
        }
        (counts, points)
    }

    /// The weights of two sets of cards together.
    fn times(&self, other: &Poly) -> Poly {
        let mut c = [[0.0; POINTS]; COUNTS];
        let (counts, points) = self.extent();
        let (other_counts, other_points) = other.extent();
        for (k, row) in self.c.iter().enumerate().take(counts) {
            for (p, &w) in row.iter().enumerate().take(points).filter(|(_, &w)| w > 0.0) {
                for (l, other_row) in other.c.iter().enumerate().take(other_counts.min(COUNTS - k)) {
                    let out = &mut c[k + l][p..];
                    for (o, &v) in out.iter_mut().zip(&other_row[..other_points.min(POINTS - p)]) {
                        *o += w * v;
                    }
                }
            }
        }
        Poly { log_scale: self.log_scale + other.log_scale, c }.normalized()
    }
}

/// The log of the sum of e<sup>x</sup> over `xs`.
fn log_sum_exp(xs: impl Iterator<Item = f64>) -> f64 {
    let xs: Vec<f64> = xs.collect();
    let most = xs.iter().copied().fold(f64::NEG_INFINITY, f64::max);
    if most == f64::NEG_INFINITY {
        return most;
    }
    most + xs.iter().map(|x| (x - most).exp()).sum::<f64>().ln()
}

impl Beliefs {
    /// Vacant places: every split of the `unseen` cards that gives the first
    /// hand `first_size` of them equally likely.
    pub(crate) fn vacant(unseen: Cards, first_size: usize) -> Beliefs {
        Beliefs { unseen, in_first: Cards::EMPTY, in_second: Cards::EMPTY, first_size, parameters: Parameters::default() }
    }

    /// The unseen cards whose place is not known.
    fn free(&self) -> Cards {
        self.unseen - self.in_first - self.in_second
    }

    /// How many of the free cards the first hand holds.
    fn wanted(&self) -> usize {
        self.first_size - self.in_first.len()
    }

    fn h(&self, card: Card) -> f64 {
        self.parameters.h[index(card)].clamp(-MOST, MOST)
    }

    fn phi(&self, suit: Suit, length: usize) -> f64 {
        self.parameters.phi[suit as usize][length].clamp(-MOST, MOST)
    }

    fn psi(&self, points: usize) -> f64 {
        self.parameters.psi[points].clamp(-MOST, MOST)
    }

    /// The weights of the free cards of `suit` that the first hand could
    /// hold, by how many of them and its points in the suit (with those of
    /// the cards known to be there), with the suit's potential.
    fn suit(&self, suit: Suit) -> Poly {
        let mut poly = Poly::one();
        let mut log_scale = 0.0;
        for card in self.free().in_suit(suit) {
            let w = self.h(card).exp();
            let mut c = [[0.0; POINTS]; COUNTS];
            for k in 0..COUNTS {
                for p in 0..POINTS {
                    let v = poly.c[k][p];
                    if v == 0.0 {
                        continue;
                    }
                    c[k][p] += v;
                    if k + 1 < COUNTS && p + points(card) < POINTS {
                        c[k + 1][p + points(card)] += v * w;
                    }
                }
            }
            poly = Poly { log_scale: poly.log_scale, c }.normalized();
            log_scale = poly.log_scale;
        }
        // The known cards' points and length, and the suit's potential.
        let known = self.in_first.in_suit(suit);
        let (known_length, known_points) = (known.len(), known.high_card_points());
        let mut c = [[0.0; POINTS]; COUNTS];
        for k in 0..COUNTS - known_length {
            let potential = self.phi(suit, k + known_length).exp();
            for p in 0..POINTS - known_points {
                c[k][p + known_points] = poly.c[k][p] * potential;
            }
        }
        Poly { log_scale, c }.normalized()
    }

    /// The four suits' weights.
    fn suits(&self) -> [Poly; 4] {
        Suit::ALL.map(|suit| self.suit(suit))
    }

    /// The weights of the other suits than `suit`, summed with the points
    /// potential, by how many of the free cards and how many points `suit`
    /// gives the first hand: so that any weights of `suit` alone times this,
    /// summed, give the normalization of those splits.
    fn context(&self, others: &Poly, exp_psi: &[f64; POINTS]) -> Poly {
        let wanted = self.wanted();
        let (_, points) = others.extent();
        let mut c = [[0.0; POINTS]; COUNTS];
        for (k, row) in c.iter_mut().enumerate().take(wanted + 1) {
            let other_row = &others.c[wanted - k];
            for (p, w) in row.iter_mut().enumerate() {
                *w = other_row[..points.min(POINTS - p)].iter().zip(&exp_psi[p..]).map(|(a, b)| a * b).sum();
            }
        }
        Poly { log_scale: others.log_scale, c }
    }

    /// e<sup>ψ</sup> of each number of points.
    fn exp_psi(&self) -> [f64; POINTS] {
        std::array::from_fn(|p| self.psi(p).exp())
    }

    /// For each suit, the product of the weights of the other suits.
    fn others(suits: &[Poly; 4]) -> [Poly; 4] {
        let mut before = vec![Poly::one()];
        for poly in &suits[..3] {
            before.push(before[before.len() - 1].times(poly));
        }
        let mut after = vec![Poly::one()];
        for poly in suits[1..].iter().rev() {
            after.push(after[after.len() - 1].times(poly));
        }
        std::array::from_fn(|s| before[s].times(&after[3 - s]))
    }

    /// The chance of each free card of `suit` being in the first hand, given
    /// the `context` of the suit and the log of the normalization: by the
    /// weights of the subsets of the cards before each card (forward) and
    /// of those after it with the context (backward), so that a card's
    /// chance is the sum over both of the splits that give it to the first.
    fn card_chances(&self, suit: Suit, context: &Poly, log_z: f64) -> Vec<(Card, f64)> {
        let cards: Vec<Card> = self.free().in_suit(suit).collect();
        let m = cards.len();
        let weights: Vec<f64> = cards.iter().map(|&card| self.h(card).exp()).collect();
        let mut forward = vec![Poly::one()];
        for (&card, &w) in cards.iter().zip(&weights) {
            let before = &forward[forward.len() - 1];
            let mut c = before.c;
            for k in 0..COUNTS - 1 {
                for p in 0..POINTS - points(card) {
                    c[k + 1][p + points(card)] += before.c[k][p] * w;
                }
            }
            forward.push(Poly { log_scale: before.log_scale, c }.normalized());
        }
        let known = self.in_first.in_suit(suit);
        let (known_length, known_points) = (known.len(), known.high_card_points());
        let mut c = [[0.0; POINTS]; COUNTS];
        for (k, row) in c.iter_mut().enumerate().take(COUNTS - known_length) {
            let potential = self.phi(suit, k + known_length).exp();
            for (p, w) in row.iter_mut().enumerate().take(POINTS - known_points) {
                *w = potential * context.c[k][p + known_points];
            }
        }
        let mut backward = vec![Poly { log_scale: context.log_scale, c }.normalized()];
        for i in (0..m).rev() {
            let after = &backward[backward.len() - 1];
            let mut c = after.c;
            for k in 0..COUNTS - 1 {
                for p in 0..POINTS - points(cards[i]) {
                    c[k][p] += weights[i] * after.c[k + 1][p + points(cards[i])];
                }
            }
            backward.push(Poly { log_scale: after.log_scale, c }.normalized());
        }
        backward.reverse();
        (0..m)
            .map(|i| {
                let (before, after) = (&forward[i], &backward[i + 1]);
                let mut sum = 0.0;
                for k in 0..COUNTS - 1 {
                    for p in 0..POINTS - points(cards[i]) {
                        sum += before.c[k][p] * after.c[k + 1][p + points(cards[i])];
                    }
                }
                let chance = if sum > 0.0 { (before.log_scale + after.log_scale + (weights[i] * sum).ln() - log_z).exp() } else { 0.0 };
                (cards[i], chance)
            })
            .collect()
    }

    /// The log of the sum of the weights of `poly` times those of `context`.
    fn log_total(poly: &Poly, context: &Poly) -> f64 {
        let sum: f64 = poly.c.iter().flatten().zip(context.c.iter().flatten()).map(|(a, b)| a * b).sum();
        poly.log_scale + context.log_scale + sum.ln()
    }

    /// The log of the normalization: the sum of the weights of every split.
    pub(crate) fn log_normalization(&self) -> f64 {
        let suits = self.suits();
        let others = suits[1..].iter().fold(Poly::one(), |product, poly| product.times(poly));
        Beliefs::log_total(&suits[0], &self.context(&others, &self.exp_psi()))
    }

    /// The chances of each card's place, each suit's length and the points
    /// of the first hand.
    pub(crate) fn chances(&self) -> Chances {
        let suits = self.suits();
        let exp_psi = self.exp_psi();
        let others = Beliefs::others(&suits);
        let contexts: [Poly; 4] = std::array::from_fn(|s| self.context(&others[s], &exp_psi));
        let log_z = Beliefs::log_total(&suits[0], &contexts[0]);
        let mut card = [0.0; 52];
        for c in self.in_first {
            card[index(c)] = 1.0;
        }
        for suit in Suit::ALL {
            for (c, chance) in self.card_chances(suit, &contexts[suit as usize], log_z) {
                card[index(c)] = chance;
            }
        }
        let mut length = [[0.0; COUNTS]; 4];
        for suit in Suit::ALL {
            let s = suit as usize;
            let known = self.in_first.in_suit(suit).len();
            for k in 0..COUNTS - known {
                let sum: f64 = suits[s].c[k].iter().zip(&contexts[s].c[k]).map(|(a, b)| a * b).sum();
                if sum > 0.0 {
                    length[s][k + known] = (suits[s].log_scale + contexts[s].log_scale + sum.ln() - log_z).exp();
                }
            }
        }
        let all = others[0].times(&suits[0]);
        let mut points = [0.0; POINTS];
        for (p, chance) in points.iter_mut().enumerate() {
            let w = all.c[self.wanted()][p];
            if w > 0.0 {
                *chance = (all.log_scale + w.ln() + self.psi(p) - log_z).exp();
            }
        }
        Chances { card, length, points, log_normalization: log_z }
    }

    /// The log of the weight of the split that gives the first hand the
    /// unseen cards `first`, before normalization.
    fn log_weight(&self, first: Cards) -> f64 {
        assert!(
            first.difference(self.unseen).is_empty()
                && self.in_first.difference(first).is_empty()
                && (first & self.in_second).is_empty()
                && first.len() == self.first_size,
            "{first} is not a first hand of these beliefs"
        );
        let fields: f64 = (first - self.in_first).map(|card| self.h(card)).sum();
        let potentials: f64 = Suit::ALL.map(|suit| self.phi(suit, first.in_suit(suit).len())).iter().sum();
        fields + potentials + self.psi(first.high_card_points())
    }

    /// The log of the chance of the split that gives the first hand the
    /// unseen cards `first`.
    pub(crate) fn log_likelihood(&self, first: Cards) -> f64 {
        self.log_weight(first) - self.log_normalization()
    }

    /// The slope of [`Beliefs::log_likelihood`] of `first` in the
    /// parameters: what the split shows less what the beliefs expect, which
    /// is 0 for the cards whose place is known.
    pub(crate) fn slope(&self, first: Cards) -> Parameters {
        let chances = self.chances();
        let mut slope = Parameters::default();
        for card in self.free() {
            slope.h[index(card)] = f64::from(u8::from(first.contains(card))) - chances.card[index(card)];
        }
        for suit in Suit::ALL {
            let s = suit as usize;
            for (l, chance) in chances.length[s].iter().enumerate() {
                slope.phi[s][l] = -chance;
            }
            slope.phi[s][first.in_suit(suit).len()] += 1.0;
        }
        for (p, chance) in chances.points.iter().enumerate() {
            slope.psi[p] = -chance;
        }
        slope.psi[first.high_card_points()] += 1.0;
        slope
    }

    /// A first hand dealt from the beliefs, exactly: each suit's length and
    /// points in turn, given those of the suits before it, and then its
    /// cards given those.
    pub(crate) fn deal(&self, rng: &mut impl Rng) -> Cards {
        let suits = self.suits();
        let mut first = self.in_first;
        let mut left = self.wanted();
        let mut so_far = 0;
        for suit in Suit::ALL {
            let s = suit as usize;
            let later = suits[s + 1..].iter().fold(Poly::one(), |product, poly| product.times(poly));
            // The weight of each count and points of this suit, with all
            // that the later suits can add to them.
            let mut weights = Vec::new();
            for k in 0..=left.min(COUNTS - 1) {
                for (p, &w) in suits[s].c[k].iter().enumerate().take(POINTS - so_far) {
                    if w == 0.0 {
                        continue;
                    }
                    let rest = log_sum_exp(
                        (0..POINTS - so_far - p)
                            .filter(|&q| later.c[left - k][q] > 0.0)
                            .map(|q| later.c[left - k][q].ln() + self.psi(so_far + p + q)),
                    );
                    if rest > f64::NEG_INFINITY {
                        weights.push(((k, p), w.ln() + rest));
                    }
                }
            }
            let log_sum = log_sum_exp(weights.iter().map(|w| w.1));
            let mut draw = rng.gen_range(0.0..1.0);
            let mut pick = weights[weights.len() - 1].0;
            for &(choice, log_w) in &weights {
                let chance = (log_w - log_sum).exp();
                if draw < chance {
                    pick = choice;
                    break;
                }
                draw -= chance;
            }
            let (k, p) = pick;
            first += self.deal_in_suit(suit, k, p - self.in_first.in_suit(suit).high_card_points(), rng);
            left -= k;
            so_far += p;
        }
        first
    }

    /// `count` of the free cards of `suit` with `points` among them, dealt
    /// with chances in proportion to e<sup>Σh</sup>.
    fn deal_in_suit(&self, suit: Suit, count: usize, points_wanted: usize, rng: &mut impl Rng) -> Cards {
        let cards: Vec<Card> = self.free().in_suit(suit).collect();
        // The log weights of the subsets of the cards from `i` on, by their
        // number and points.
        let n = cards.len();
        let mut rest = vec![[[f64::NEG_INFINITY; POINTS]; COUNTS]; n + 1];
        rest[n][0][0] = 0.0;
        for i in (0..n).rev() {
            for k in 0..COUNTS {
                for p in 0..POINTS {
                    let without = rest[i + 1][k][p];
                    let with = if k > 0 && p >= points(cards[i]) {
                        rest[i + 1][k - 1][p - points(cards[i])] + self.h(cards[i])
                    } else {
                        f64::NEG_INFINITY
                    };
                    rest[i][k][p] = log_sum_exp([without, with].into_iter());
                }
            }
        }
        let (mut k, mut p) = (count, points_wanted);
        let mut dealt = Cards::EMPTY;
        for i in 0..n {
            if k == 0 {
                break;
            }
            let with = if p >= points(cards[i]) {
                rest[i + 1][k - 1][p - points(cards[i])] + self.h(cards[i])
            } else {
                f64::NEG_INFINITY
            };
            if rng.gen_range(0.0..1.0) < (with - rest[i][k][p]).exp() {
                dealt += Cards::singleton(cards[i]);
                k -= 1;
                p -= points(cards[i]);
            }
        }
        dealt
    }
}

/// The sizes of what the evidence of a play is indexed by: who played it
/// (the declarer or a defender), its place in the trick, how (a lead, a card
/// that follows suit, a ruff or a discard), the trumps (none, the card's
/// suit, another), the state of the trick before it (the player's side
/// winning or nobody yet, the card beating the winner, or losing to it),
/// and the card's rank among the unplayed cards of its suit (how many are
/// higher, up to 4).
const ROLES: usize = 2;
const PLACES: usize = 4;
const HOWS: usize = 4;
const TRUMPS: usize = 3;
const STATES: usize = 3;
const RANKS: usize = 5;
const CONTEXTS: usize = ROLES * PLACES * HOWS * TRUMPS * STATES * RANKS;
/// And for each unseen card of the suit, its rank among the unplayed cards
/// once the card is played, and how it lies to the card played: just below
/// it with nothing unplayed between (an equal), below it and below the card
/// that was winning (or with none), below it and above the card that was
/// winning (a cheaper winner), just above it, or further above.
const RELATIONS: usize = 5;
const FIELD_WEIGHTS: usize = CONTEXTS * RANKS * RELATIONS;
const LENGTH_WEIGHTS: usize = CONTEXTS * COUNTS;

/// What the play of a hidden hand shows: a field for each unseen card of the
/// suit played, as log odds of the card being in the hand that played, and
/// a potential for each length of that suit in that hand, both from one
/// table entry for each situation (see [`CONTEXTS`] and [`RELATIONS`]), all
/// in one vector of weights, the fields first.
#[derive(Clone)]
pub(crate) struct Evidence {
    pub(crate) weights: Vec<f64>,
}

impl Evidence {
    /// No evidence: vacant places, with the cards' places kept exactly.
    pub(crate) fn none() -> Evidence {
        Evidence { weights: vec![0.0; FIELD_WEIGHTS + LENGTH_WEIGHTS] }
    }
}

/// The prior's tables: for each hidden hand's role (the declarer or a
/// defender) and the fewest and most cards of a suit that its calls showed
/// (by the bidding system's tables, see `Convention::shown_bounds`), a
/// potential for each length it could hold, inside those bounds or not;
/// and the same for its high card points.
const LENGTH_PRIOR: usize = ROLES * COUNTS * COUNTS * COUNTS;
const POINTS_PRIOR: usize = ROLES * POINTS * POINTS * POINTS;

/// What the auction shows of the hidden hands before the opening lead, as
/// potentials on each hand's lengths and points that depend only on what
/// the calls showed, never on the calls themselves.
#[derive(Clone)]
pub(crate) struct Prior {
    pub(crate) weights: Vec<f64>,
}

impl Prior {
    /// No prior: vacant places.
    pub(crate) fn none() -> Prior {
        Prior { weights: vec![0.0; LENGTH_PRIOR + POINTS_PRIOR] }
    }

    /// The auction alone: each length and number of points outside what
    /// the calls showed costs `penalty`, and those inside are equally
    /// likely.
    pub(crate) fn walls(penalty: f64) -> Prior {
        let mut prior = Prior::none();
        for role in 0..ROLES {
            for low in 0..COUNTS {
                for high in low..COUNTS {
                    for v in (0..low).chain(high + 1..COUNTS) {
                        prior.weights[length_entry(role, low, high, v)] = -penalty;
                    }
                }
            }
            for low in 0..POINTS {
                for high in low..POINTS {
                    for v in (0..low).chain(high + 1..POINTS) {
                        prior.weights[points_entry(role, low, high, v)] = -penalty;
                    }
                }
            }
        }
        prior
    }
}

fn length_entry(role: usize, low: usize, high: usize, length: usize) -> usize {
    ((role * COUNTS + low) * COUNTS + high) * COUNTS + length
}

fn points_entry(role: usize, low: usize, high: usize, points: usize) -> usize {
    LENGTH_PRIOR + ((role * POINTS + low) * POINTS + high) * POINTS + points
}

/// What the calls of a hidden hand showed: its role, the fewest and most
/// cards of each suit, and the fewest and most points.
#[derive(Clone, Copy, Debug)]
struct Showed {
    role: usize,
    lengths: [(usize, usize); 4],
    points: (usize, usize),
}

impl Showed {
    /// Whether `hand` is outside what the calls showed.
    fn outside(&self, hand: Cards) -> bool {
        let out = |(low, high): (usize, usize), v: usize| v < low || v > high;
        Suit::ALL.iter().any(|&suit| out(self.lengths[suit as usize], hand.in_suit(suit).len()))
            || out(self.points, hand.high_card_points())
    }
}

/// The entries of the prior that a [`Tracker`]'s beliefs started from.
#[derive(Clone)]
pub(crate) struct Opening {
    showed: [Showed; 2],
    /// The unseen cards of each suit, and their points.
    unseen: [usize; 4],
    points: usize,
}

impl Opening {
    /// The slope of the log likelihood of `beliefs`, the beliefs before the
    /// opening lead, giving the first hand `first`, in the weights of the
    /// prior, added to `slope`.
    pub(crate) fn add_slope(&self, beliefs: &Beliefs, first: Cards, slope: &mut [f64]) {
        let of_parameters = beliefs.slope(first);
        let [one, two] = self.showed;
        for suit in 0..4 {
            let n = self.unseen[suit];
            let (l1, h1) = one.lengths[suit];
            let (l2, h2) = two.lengths[suit];
            for (l, &s) in of_parameters.phi[suit].iter().enumerate().take(n + 1) {
                slope[length_entry(one.role, l1, h1, l)] += s;
                slope[length_entry(two.role, l2, h2, n - l)] += s;
            }
        }
        let ((l1, h1), (l2, h2)) = (one.points, two.points);
        for (p, &s) in of_parameters.psi.iter().enumerate().take(self.points.min(POINTS - 1) + 1) {
            slope[points_entry(one.role, l1, h1, p)] += s;
            if self.points - p < POINTS {
                slope[points_entry(two.role, l2, h2, self.points - p)] += s;
            }
        }
    }

    /// Whether either hidden hand, `first` and the rest of the unseen
    /// cards, is outside what its calls showed.
    pub(crate) fn outside(&self, first: Cards, second: Cards) -> bool {
        self.showed[0].outside(first) || self.showed[1].outside(second)
    }
}

/// The entries of the evidence that one play added to a [`Tracker`]'s
/// beliefs, for the slope of later log likelihoods in them.
#[derive(Clone)]
pub(crate) struct Update {
    /// Each unseen card of the suit, with its entry.
    fields: Vec<(Card, usize)>,
    /// The entry for the length 0 of the suit in the hand that played.
    lengths: usize,
    suit: Suit,
    /// Whether the first hidden hand played.
    first: bool,
    /// The unseen cards of the suit, after the play.
    unseen: usize,
}

impl Update {
    /// The slope of the log likelihood of `beliefs`, those just after this
    /// update, giving the first hand `first`, in the weights of the
    /// evidence, added to `slope`.
    pub(crate) fn add_slope(&self, beliefs: &Beliefs, first: Cards, slope: &mut [f64]) {
        let of_parameters = beliefs.slope(first);
        let sign = if self.first { 1.0 } else { -1.0 };
        for &(card, entry) in &self.fields {
            slope[entry] += sign * of_parameters.h[index(card)];
        }
        let phi = &of_parameters.phi[self.suit as usize];
        for (l, &s) in phi.iter().enumerate().take(self.unseen + 1) {
            let entry = if self.first { self.lengths + l } else { self.lengths + self.unseen - l };
            slope[entry] += s;
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

/// What a player who remembers the play believes about the two hands that it
/// cannot see, kept from the opening lead to the end: the declarer about
/// the defenders, and each defender about partner and the declarer.
#[derive(Clone)]
pub(crate) struct Tracker {
    /// The two hidden hands, the first after the player first.
    pub(crate) first: crate::Seat,
    pub(crate) second: crate::Seat,
    declarer: crate::Seat,
    pub(crate) beliefs: Beliefs,
}

impl Tracker {
    /// The beliefs of `viewer`, the declarer or a defender, before the
    /// opening lead of `game`, with the dummy in sight: vacant places.
    pub(crate) fn new<C>(game: &crate::GameState<C>, viewer: crate::Seat) -> Option<Tracker> {
        let dummy = game.dummy()?;
        if viewer == dummy {
            return None;
        }
        let mut hidden = (1..4).map(|n| viewer + n).filter(|&s| s != dummy);
        let (first, second) = (hidden.next()?, hidden.next()?);
        let unseen = Cards::ALL - game.original_hands[viewer] - game.original_hands[dummy];
        Some(Tracker { first, second, declarer: dummy + 2, beliefs: Beliefs::vacant(unseen, 13) })
    }

    /// Adds to the beliefs, before the opening lead of `game`, what the
    /// `prior` makes of what the calls of each hidden hand showed, read by
    /// `convention`: the first hand's lengths and points directly, and the
    /// second's as what the first's leave it.  Gives the entries it used.
    pub(crate) fn start<C>(&mut self, game: &crate::GameState<C>, convention: &super::Convention, prior: &Prior) -> Opening {
        let showed = [self.first, self.second].map(|seat| {
            let caller = game.hand_called_by(seat);
            let place = (caller as usize + 4 - game.dealer as usize) % 4;
            let offset = (game.bids.len() + 4 - place) % 4;
            let (min, max) = convention.shown_bounds(&game.bids, offset);
            let bounds = |low: u8, high: u8, most: usize| {
                let low = (low as usize).min(most);
                (low, (high as usize).clamp(low, most))
            };
            Showed {
                role: usize::from(seat != self.declarer),
                lengths: Suit::ALL.map(|suit| bounds(min.length[suit], max.length[suit], COUNTS - 1)),
                points: bounds(min.hcp, max.hcp, POINTS - 1),
            }
        });
        let unseen = Suit::ALL.map(|suit| self.beliefs.unseen.in_suit(suit).len());
        let points = self.beliefs.unseen.high_card_points();
        let opening = Opening { showed, unseen, points };
        let [one, two] = showed;
        let parameters = &mut self.beliefs.parameters;
        for (suit, &n) in unseen.iter().enumerate() {
            let (l1, h1) = one.lengths[suit];
            let (l2, h2) = two.lengths[suit];
            for (l, potential) in parameters.phi[suit].iter_mut().enumerate().take(n + 1) {
                *potential += prior.weights[length_entry(one.role, l1, h1, l)] + prior.weights[length_entry(two.role, l2, h2, n - l)];
            }
        }
        let ((l1, h1), (l2, h2)) = (one.points, two.points);
        for (p, potential) in parameters.psi.iter_mut().enumerate().take(points.min(POINTS - 1) + 1) {
            *potential += prior.weights[points_entry(one.role, l1, h1, p)];
            if points - p < POINTS {
                *potential += prior.weights[points_entry(two.role, l2, h2, points - p)];
            }
        }
        opening
    }

    /// Takes in the play of `card` next in `game`, as `game` is before it:
    /// the card leaves the unseen cards, a hand that does not follow suit
    /// has shown out, and the play of a hidden hand adds the `evidence` of
    /// how it was played, which it gives as an [`Update`].
    pub(crate) fn observe<C>(&mut self, game: &crate::GameState<C>, card: Card, evidence: &Evidence) -> Option<Update> {
        let seat = game.hand_playing()?;
        if seat != self.first && seat != self.second {
            return None;
        }
        let first = seat == self.first;
        let trick: &[Card] = if game.played.len() == 4 { &[] } else { &game.played };
        let trump = super::ensemble::trump_of(game);
        let unplayed = game.hands.iter().fold(Cards::EMPTY, |all, &hand| all + hand);
        let suit = card.suit();
        let led = trick.first().map(|c| c.suit());
        let how = match led {
            None => 0,
            Some(led) if led == suit => 1,
            Some(_) if Some(suit) == trump => 2,
            Some(_) => 3,
        };
        let trumps = match trump {
            None => 0,
            Some(t) if t == suit => 1,
            Some(_) => 2,
        };
        let leader = game.lead?;
        let mut winner = 0;
        for (i, &c) in trick.iter().enumerate().skip(1) {
            if beats(trump, c, trick[winner]) {
                winner = i;
            }
        }
        let winning = trick.get(winner).copied();
        let state = match winning {
            None => 0,
            Some(_) if leader + winner == seat + 2 => 0,
            Some(w) if beats(trump, card, w) => 1,
            Some(_) => 2,
        };
        let role = usize::from(seat != self.declarer);
        let rank_among = |c: Card, cards: Cards| cards.in_suit(c.suit()).filter(|x| x.rank() > c.rank()).count().min(RANKS - 1);
        let context = ((((role * PLACES + trick.len()) * HOWS + how) * TRUMPS + trumps) * STATES + state) * RANKS
            + rank_among(card, unplayed);

        // What is known exactly.
        let beliefs = &mut self.beliefs;
        let gone = Cards::singleton(card);
        beliefs.unseen -= gone;
        beliefs.in_first -= gone;
        beliefs.in_second -= gone;
        let parameters = &mut beliefs.parameters;
        if first {
            // The first hand now holds one card fewer of the suit and fewer
            // points, so each potential moves to the length and points it
            // now stands for.
            beliefs.first_size -= 1;
            let phi = &mut parameters.phi[suit as usize];
            phi.rotate_left(1);
            phi[COUNTS - 1] = 0.0;
            let p = points(card);
            parameters.psi.rotate_left(p);
            parameters.psi[POINTS - p..].fill(0.0);
        }
        parameters.h[index(card)] = 0.0;
        if let Some(led) = led.filter(|&led| led != suit) {
            let rest = beliefs.unseen.in_suit(led) - beliefs.in_first - beliefs.in_second;
            if first {
                beliefs.in_second += rest;
            } else {
                beliefs.in_first += rest;
            }
        }

        // What the way it was played shows.
        let after = unplayed - gone;
        let sign = if first { 1.0 } else { -1.0 };
        let mut fields = Vec::new();
        for c in (beliefs.unseen - beliefs.in_first - beliefs.in_second).in_suit(suit) {
            let (low, high) = if c.rank() < card.rank() { (c, card) } else { (card, c) };
            let touching = !after.in_suit(suit).any(|x| x.rank() > low.rank() && x.rank() < high.rank());
            let relation = if c.rank() < card.rank() {
                if touching {
                    0
                } else if winning.is_some_and(|w| w.suit() == suit && c.rank() > w.rank()) {
                    2
                } else {
                    1
                }
            } else if touching {
                3
            } else {
                4
            };
            let entry = (context * RANKS + rank_among(c, after)) * RELATIONS + relation;
            beliefs.parameters.h[index(c)] += sign * evidence.weights[entry];
            fields.push((c, entry));
        }
        let lengths = FIELD_WEIGHTS + context * COUNTS;
        let unseen = beliefs.unseen.in_suit(suit).len();
        let phi = &mut beliefs.parameters.phi[suit as usize];
        for (l, potential) in phi.iter_mut().enumerate().take(unseen + 1) {
            *potential += evidence.weights[if first { lengths + l } else { lengths + unseen - l }];
        }
        Some(Update { fields, lengths, suit, first, unseen })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::rngs::StdRng;
    use rand::SeedableRng;

    /// Every first hand of `beliefs`, with the log of its weight.
    fn every_split(beliefs: &Beliefs) -> Vec<(Cards, f64)> {
        let free: Vec<Card> = beliefs.free().collect();
        let mut splits = Vec::new();
        for bits in 0u32..1 << free.len() {
            if bits.count_ones() as usize != beliefs.wanted() {
                continue;
            }
            let chosen = (0..free.len()).filter(|i| bits & (1 << i) != 0).fold(Cards::EMPTY, |cards, i| cards + Cards::singleton(free[i]));
            let first = beliefs.in_first + chosen;
            splits.push((first, beliefs.log_weight(first)));
        }
        splits
    }

    /// Beliefs about a random part of a deal, with random parameters and
    /// some cards' places known.
    fn random_beliefs(rng: &mut StdRng, size: usize) -> Beliefs {
        let mut deck = Cards::ALL;
        let unseen = deck.pick_rng(rng, size).unwrap();
        let mut beliefs = Beliefs::vacant(unseen, size / 2);
        // A suit that the second hand has shown out of.
        let void = Suit::ALL[rng.gen_range(0..4)];
        let known = unseen.in_suit(void);
        if known.len() <= beliefs.first_size {
            beliefs.in_first = known;
        }
        let mut free = beliefs.free();
        if let Some(card) = free.pick_rng(rng, 1) {
            beliefs.in_second = card;
        }
        let p = &mut beliefs.parameters;
        p.h.iter_mut().for_each(|h| *h = rng.gen_range(-2.0..2.0));
        p.phi.iter_mut().flatten().for_each(|phi| *phi = rng.gen_range(-1.5..1.5));
        p.psi.iter_mut().for_each(|psi| *psi = rng.gen_range(-1.0..1.0));
        beliefs
    }

    #[test]
    fn chances_are_those_of_every_split_counted() {
        let mut rng = StdRng::seed_from_u64(3);
        for size in [2, 7, 12, 15, 18] {
            for _ in 0..5 {
                let beliefs = random_beliefs(&mut rng, size);
                let splits = every_split(&beliefs);
                let log_z = log_sum_exp(splits.iter().map(|s| s.1));
                let chances = beliefs.chances();
                assert!((chances.log_normalization - log_z).abs() < 1e-9, "{} against {log_z}", chances.log_normalization);
                let mut card = [0.0; 52];
                let mut length = [[0.0; COUNTS]; 4];
                let mut points = [0.0; POINTS];
                for (first, log_w) in &splits {
                    let chance = (log_w - log_z).exp();
                    for c in *first {
                        card[index(c)] += chance;
                    }
                    for suit in Suit::ALL {
                        length[suit as usize][first.in_suit(suit).len()] += chance;
                    }
                    points[first.high_card_points()] += chance;
                    assert!((beliefs.log_likelihood(*first) - (log_w - log_z)).abs() < 1e-9);
                }
                let close = |a: &[f64], b: &[f64]| a.iter().zip(b).all(|(x, y)| (x - y).abs() < 1e-9);
                assert!(close(&chances.card, &card), "{:?}\n{:?}", chances.card, card);
                assert!(close(chances.length.as_flattened(), length.as_flattened()));
                assert!(close(&chances.points, &points));
            }
        }
    }

    #[test]
    fn the_slope_is_that_of_the_log_likelihood() {
        let mut rng = StdRng::seed_from_u64(4);
        let beliefs = random_beliefs(&mut rng, 16);
        let first = beliefs.deal(&mut rng);
        let slope = beliefs.slope(first);
        let e = 1e-6;
        let along = |change: &dyn Fn(&mut Parameters, f64)| {
            let mut up = beliefs.clone();
            change(&mut up.parameters, e);
            let mut down = beliefs.clone();
            change(&mut down.parameters, -e);
            (up.log_likelihood(first) - down.log_likelihood(first)) / (2.0 * e)
        };
        for card in beliefs.free() {
            let i = index(card);
            assert!((along(&|p, e| p.h[i] += e) - slope.h[i]).abs() < 1e-6);
        }
        for s in 0..4 {
            for l in 0..COUNTS {
                assert!((along(&|p, e| p.phi[s][l] += e) - slope.phi[s][l]).abs() < 1e-6);
            }
        }
        for q in 0..POINTS {
            assert!((along(&|p, e| p.psi[q] += e) - slope.psi[q]).abs() < 1e-6);
        }
    }

    #[test]
    fn deals_come_with_their_chances() {
        let mut rng = StdRng::seed_from_u64(5);
        let beliefs = random_beliefs(&mut rng, 10);
        let splits = every_split(&beliefs);
        let log_z = log_sum_exp(splits.iter().map(|s| s.1));
        let draws = 200_000;
        let mut counts = std::collections::HashMap::new();
        for _ in 0..draws {
            *counts.entry(beliefs.deal(&mut rng)).or_insert(0usize) += 1;
        }
        let mut chi_squared = 0.0;
        for (first, log_w) in &splits {
            let expected = (log_w - log_z).exp() * draws as f64;
            let seen = *counts.get(first).unwrap_or(&0) as f64;
            chi_squared += (seen - expected).powi(2) / expected;
        }
        assert_eq!(counts.keys().filter(|first| !splits.iter().any(|s| s.0 == **first)).count(), 0);
        // Far beyond what chance gives for this many splits.
        let freedom = splits.len() as f64 - 1.0;
        assert!(chi_squared < freedom + 6.0 * (2.0 * freedom).sqrt(), "chi squared {chi_squared} on {freedom}");
    }

    /// Trackers for the declarer and both defenders of `game`.
    fn trackers(game: &crate::GameState<()>) -> Vec<Tracker> {
        let declarer = game.dummy().unwrap() + 2;
        [declarer, declarer + 1, declarer + 3].iter().map(|&viewer| Tracker::new(game, viewer).unwrap()).collect()
    }

    #[test]
    fn trackers_keep_the_cards_places_exactly() {
        let mut rng = StdRng::seed_from_u64(7);
        let mut evidence = Evidence::none();
        evidence.weights.iter_mut().for_each(|w| *w = rng.gen_range(-1.0..1.0));
        let mut updates = 0;
        for game in crate::ai::opening_lead::candidates::opening_leads(29, 20) {
            let mut game = crate::ai::playout::copy_of(&game);
            let mut trackers = trackers(&game);
            while !game.hand_done {
                let legal: Vec<Card> = game.could_be_played().collect();
                let card = legal[rng.gen_range(0..legal.len())];
                let before = trackers.clone();
                let observed: Vec<Option<Update>> = trackers.iter_mut().map(|t| t.observe(&game, card, &evidence)).collect();
                let at = crate::ai::playout::copy_of(&game);
                game.play(card);
                for ((tracker, update), before) in trackers.iter().zip(&observed).zip(&before) {
                    let beliefs = &tracker.beliefs;
                    let first = game.hands[tracker.first];
                    assert_eq!(beliefs.unseen, first + game.hands[tracker.second]);
                    assert!(beliefs.in_first.difference(first).is_empty());
                    assert!(beliefs.in_second.difference(game.hands[tracker.second]).is_empty());
                    assert_eq!(beliefs.first_size, first.len());
                    let Some(update) = update else { continue };
                    if beliefs.unseen.is_empty() {
                        continue;
                    }
                    updates += 1;
                    // The slope through this update alone, against a finite
                    // difference of the update with each entry moved.
                    let mut slope = vec![0.0; evidence.weights.len()];
                    update.add_slope(beliefs, first, &mut slope);
                    let used: Vec<usize> = update.fields.iter().map(|f| f.1).chain([update.lengths, update.lengths + 1]).collect();
                    for entry in used {
                        let moved = |by: f64| {
                            let mut moved = evidence.clone();
                            moved.weights[entry] += by;
                            let mut tracker = before.clone();
                            tracker.observe(&at, card, &moved);
                            tracker.beliefs.log_likelihood(first)
                        };
                        let e = 1e-5;
                        let difference = (moved(e) - moved(-e)) / (2.0 * e);
                        assert!((difference - slope[entry]).abs() < 1e-6, "entry {entry}: {difference} against {}", slope[entry]);
                    }
                }
            }
        }
        assert!(updates > 500, "only {updates} updates");
    }

    #[test]
    fn the_prior_slope_is_that_of_the_log_likelihood() {
        let mut rng = StdRng::seed_from_u64(8);
        let mut prior = Prior::walls(3.0);
        prior.weights.iter_mut().for_each(|w| *w += rng.gen_range(-0.5..0.5));
        let mut checked = 0;
        for game in crate::ai::opening_lead::candidates::opening_leads(31, 10) {
            let convention = game.system.convention();
            for tracker in trackers(&game) {
                let first = game.hands[tracker.first];
                let mut started = tracker.clone();
                let opening = started.start(&game, &convention, &prior);
                let mut slope = vec![0.0; prior.weights.len()];
                opening.add_slope(&started.beliefs, first, &mut slope);
                let used: Vec<usize> = (0..slope.len()).filter(|&i| slope[i] != 0.0).take(40).collect();
                for entry in used {
                    let moved = |by: f64| {
                        let mut moved = prior.clone();
                        moved.weights[entry] += by;
                        let mut t = tracker.clone();
                        t.start(&game, &convention, &moved);
                        t.beliefs.log_likelihood(first)
                    };
                    let e = 1e-5;
                    let difference = (moved(e) - moved(-e)) / (2.0 * e);
                    assert!((difference - slope[entry]).abs() < 1e-6, "entry {entry}: {difference} against {}", slope[entry]);
                    checked += 1;
                }
            }
        }
        assert!(checked > 500, "only {checked}");
    }

    #[test]
    fn vacant_places_gives_each_card_its_share() {
        let unseen: Cards = Cards::ALL.in_suit(Suit::Spades) + Cards::ALL.in_suit(Suit::Hearts);
        let chances = Beliefs::vacant(unseen, 13).chances();
        for card in unseen {
            assert!((chances.card[index(card)] - 0.5).abs() < 1e-12);
        }
        // A 7–6 or 6–7 split of a suit of 13 between two hands of 13 of 26.
        let six = chances.length[Suit::Spades as usize][6];
        let exact = (binomial(13, 6) * binomial(13, 7)) as f64 / binomial(26, 13) as f64;
        assert!((six - exact).abs() < 1e-12);
    }

    fn binomial(n: u64, k: u64) -> u64 {
        (0..k).fold(1, |b, i| b * (n - i) / (i + 1))
    }
}

#[cfg(test)]
mod timing {
    use super::*;
    use rand::rngs::StdRng;
    use rand::SeedableRng;

    /// How long the chances and a deal take for 26 unseen cards.
    #[test]
    #[ignore]
    fn time_the_chances() {
        let mut rng = StdRng::seed_from_u64(6);
        let mut deck = Cards::ALL;
        let mut beliefs = Beliefs::vacant(deck.pick_rng(&mut rng, 26).unwrap(), 13);
        beliefs.parameters.h.iter_mut().for_each(|h| *h = rng.gen_range(-2.0..2.0));
        let started = std::time::Instant::now();
        let n = 200;
        let mut sum = 0.0;
        for _ in 0..n {
            sum += beliefs.chances().card.iter().sum::<f64>();
        }
        println!("chances: {:?} each ({sum})", started.elapsed() / n);
        let started = std::time::Instant::now();
        for _ in 0..n {
            sum += beliefs.suits()[0].log_scale;
        }
        println!("suits: {:?} each", started.elapsed() / n);
        let suits = beliefs.suits();
        let started = std::time::Instant::now();
        for _ in 0..n {
            sum += Beliefs::others(&suits)[0].log_scale;
        }
        println!("others: {:?} each", started.elapsed() / n);
        let others = Beliefs::others(&suits);
        let exp_psi = beliefs.exp_psi();
        let started = std::time::Instant::now();
        for _ in 0..n {
            sum += beliefs.context(&others[0], &exp_psi).log_scale;
        }
        println!("context: {:?} each", started.elapsed() / n);

        let started = std::time::Instant::now();
        for _ in 0..n {
            sum += beliefs.log_normalization();
        }
        println!("normalization: {:?} each", started.elapsed() / n);
        let started = std::time::Instant::now();
        for _ in 0..n {
            sum += beliefs.deal(&mut rng).len() as f64;
        }
        println!("deal: {:?} each ({sum})", started.elapsed() / n);
    }
}
