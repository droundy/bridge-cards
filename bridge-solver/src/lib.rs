pub use bridge_deck::{Card, Cards, Suit};
use rand::rngs::SmallRng;
use rand::SeedableRng;
use std::hash::{BuildHasherDefault, Hasher};

#[cfg(feature = "benchmarks")]
mod benches;

#[cfg(test)]
pub fn normalize(all: Cards, hand: Cards) -> Cards {
    let mut c = Cards::EMPTY;
    for &s in [Suit::Clubs, Suit::Diamonds, Suit::Hearts, Suit::Spades].iter() {
        for (n, x) in all.in_suit(s).enumerate() {
            // println!("  {} would be {}", x, Card::new(s, 2 + n as u8));
            if hand.contains(x) {
                // println!("adding {} as {}", x, Card::new(s, 2 + n as u8));
                c = c.insert(Card::new(s, 2 + n as u8));
            }
        }
    }
    c
}

#[test]
fn test_normalize() {
    assert_eq!(Cards::SPADES, normalize(Cards::ALL, Cards::SPADES));
    let twos = Cards::singleton(Card::S2)
        + Cards::singleton(Card::H2)
        + Cards::singleton(Card::D2)
        + Cards::singleton(Card::C2);
    println!("\n\nTwos:\n\n");
    assert_eq!(twos, normalize(Cards::ACES, Cards::ACES));
    assert_eq!(
        Cards::ACES.renumbered_among(Cards::ACES),
        normalize(Cards::ACES, Cards::ACES)
    );
    println!("\n\nWeird all aces:\n\n");
    assert_eq!(Cards::ACES, normalize(Cards::ALL, Cards::ACES));
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Starting {
    /// The cards that we have, starting with the leader
    pub hands: [Cards; 4],
    /// The cards that I don't know who has
    pub unknown: Cards,
}

impl std::hash::Hash for Starting {
    /// Hashes just the five sets of cards, and not the length of the array
    /// of hands that a derived `Hash` would also hash.
    fn hash<H: Hasher>(&self, state: &mut H) {
        for hand in self.hands {
            hand.hash(state);
        }
        self.unknown.hash(state);
    }
}

/// Which of these plays wins the trick, counting from the leader
fn trick_winner(plays: [Card; 4], trump: Option<Suit>) -> usize {
    let played = Cards::singleton(plays[0])
        + Cards::singleton(plays[1])
        + Cards::singleton(plays[2])
        + Cards::singleton(plays[3]);
    // The highest trump wins, and when nobody trumped, the highest card of
    // the suit that was led.
    let winning_suit = match trump {
        Some(t) if !played.in_suit(t).is_empty() => t,
        _ => plays[0].suit(),
    };
    let best = played.in_suit(winning_suit).max().unwrap();
    plays.iter().position(|&c| c == best).unwrap()
}

#[test]
fn trick_winner_is_the_highest_card_of_the_suit_that_wins() {
    use rand::Rng;
    let mut rng = SmallRng::seed_from_u64(5);
    for _ in 0..20_000 {
        let mut deck = Cards::ALL;
        let mut hands = [Cards::EMPTY; 4];
        for hand in &mut hands {
            *hand = deck.pick_rng(&mut rng, 13).unwrap();
        }
        let plays = hands.map(|h| h.into_iter().nth(rng.gen_range(0..13)).unwrap());
        let trump = [None, Some(Suit::Clubs), Some(Suit::Spades)][rng.gen_range(0..3)];
        // Worked out as the rules of the game say.
        let beats = |a: Card, b: Card| {
            if a.suit() == b.suit() {
                a.rank() > b.rank()
            } else {
                Some(a.suit()) == trump
            }
        };
        let mut winner = 0;
        for i in 1..4 {
            if beats(plays[i], plays[winner]) {
                winner = i;
            }
        }
        assert_eq!(winner, trick_winner(plays, trump), "{plays:?} {trump:?}");
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TrickTaken {
    Us(Starting),
    Them(Starting),
}
impl TrickTaken {
    pub const fn starting(self) -> Starting {
        match self {
            TrickTaken::Us(s) => s,
            TrickTaken::Them(s) => s,
        }
    }
}

impl Starting {
    pub fn random_hands(&self, rng: &mut SmallRng) -> Vec<[Cards; 4]> {
        let n = self.tricks_remaining();
        let mut unknown = self.unknown;
        let mut hands = self.hands;
        // for seat in 0..4 {
        //     println!(
        //         "  hand {}: {} ({})",
        //         seat,
        //         self.hands[seat],
        //         self.hands[seat].len()
        //     );
        // }
        // println!("  extra: {} ({})", self.unknown, self.unknown.len());
        let mut needs_least = n;
        let mut need_cards = [0, 0, 0, 0];
        let mut num_needs = 0;
        for (seat, hand) in hands.iter().enumerate() {
            if hand.len() < n {
                need_cards[num_needs] = seat;
                num_needs += 1;
                needs_least = std::cmp::min(needs_least, n - hand.len());
            }
        }
        match num_needs {
            0 => vec![hands],
            1 => {
                for hand in &mut hands {
                    if hand.len() < n {
                        *hand += unknown;
                    }
                }
                vec![hands]
            }
            2 => {
                for seat in need_cards[..2].iter().cloned() {
                    if hands[seat].len() < n - needs_least {
                        let extra = unknown
                            .pick_rng(rng, n - needs_least - self.hands[seat].len())
                            .expect("bad number of cardss");
                        hands[seat] += extra;
                    }
                }
                let h1 = unknown.pick_rng(rng, needs_least).unwrap();
                let h2 = unknown;
                let mut hands1 = hands;
                let mut hands2 = hands;
                hands1[need_cards[0]] += h1;
                hands1[need_cards[1]] += h2;

                hands2[need_cards[0]] += h2;
                hands2[need_cards[1]] += h1;
                vec![hands1, hands2]
            }
            3 => {
                for seat in need_cards[..3].iter().cloned() {
                    if hands[seat].len() < n - needs_least {
                        let extra = unknown
                            .pick_rng(rng, n - needs_least - self.hands[seat].len())
                            .expect("bad number of cardss");
                        hands[seat] += extra;
                    }
                }
                let h1 = unknown.pick_rng(rng, needs_least).unwrap();
                let h2 = unknown.pick_rng(rng, needs_least).unwrap();
                let h3 = unknown;

                let mut v = Vec::with_capacity(6);

                for (i, j, k) in [
                    (0, 1, 2),
                    (1, 2, 0),
                    (2, 0, 1),
                    (2, 1, 0),
                    (1, 0, 2),
                    (0, 2, 1),
                ]
                .iter()
                .cloned()
                {
                    let mut hands1 = hands;
                    hands1[need_cards[i]] += h1;
                    hands1[need_cards[j]] += h2;
                    hands1[need_cards[k]] += h3;
                    v.push(hands1)
                }
                v
            }
            _ => unreachable!(),
        }
    }
    fn printme(&self) {
        for (seat, hand) in self.hands.iter().enumerate() {
            println!("  hand {seat}: {hand} ({})", hand.len());
        }
        println!("  extra: {} ({})", self.unknown, self.unknown.len());
    }
    pub fn check(&self) {
        let lengths = self.hands.map(|hand| hand.len());
        let n = (self.unknown.len() + lengths.iter().sum::<usize>()) / 4;
        if lengths.iter().any(|&length| length > n) {
            self.fail_check(n);
        }
    }
    /// Say what is wrong with a position that has too many cards, and panic.
    ///
    /// This is kept out of `check` so that `check`, which is run for every
    /// position the solver solves, stays small enough to be inlined.
    #[cold]
    #[inline(never)]
    fn fail_check(&self, n: usize) -> ! {
        println!("Thi sis crazy! with {n} tricks left");
        self.printme();
        panic!("a hand has more than the {} cards that are left to play", n);
    }
    pub fn after(self, plays: [Card; 4], trump: Option<Suit>) -> TrickTaken {
        self.check();
        self.after_unchecked(plays, trump)
    }
    /// [`Starting::after`] for a position that has been checked already,
    /// which the solver does once for each position it solves, and not again
    /// for each of the tricks it tries from that position.
    fn after_unchecked(mut self, plays: [Card; 4], trump: Option<Suit>) -> TrickTaken {
        let played = Cards::singleton(plays[0])
            + Cards::singleton(plays[1])
            + Cards::singleton(plays[2])
            + Cards::singleton(plays[3]);
        let winner = trick_winner(plays, trump);
        let tricks_left = self.tricks_remaining();
        let unknown_in_suit = self.unknown.in_suit(plays[0].suit());
        if !unknown_in_suit.is_empty() {
            for i in 1..4 {
                // A void we could already see tells us nothing new.
                if plays[i].suit() != plays[0].suit() && self.hands[i].len() < tricks_left {
                    // i showed void.  if only one other player has unknown cards, they must
                    // have all the cards in this suit.
                    let lengths: Vec<usize> = self.hands.iter().map(|h| h.len()).collect();
                    let max_length = lengths.iter().cloned().max().unwrap();
                    if lengths.iter().filter(|&&l| l != max_length).count() <= 2 {
                        for j in 0..4 {
                            if self.hands[j].len() < max_length
                                && j != i
                                && self.hands[j].len() + unknown_in_suit.len() <= tricks_left
                            {
                                self.hands[j] += unknown_in_suit;
                                self.unknown -= unknown_in_suit;
                                break;
                            }
                        }
                    }
                    break;
                }
            }
        }
        let cards_left =
            (self.unknown + self.hands[0] + self.hands[1] + self.hands[2] + self.hands[3]) - played;

        let renumber = |cards: Cards| (cards - played).renumbered_among(cards_left);
        let unknown = renumber(self.unknown);
        let hands = [
            renumber(self.hands[winner]),
            renumber(self.hands[(winner + 1) % 4]),
            renumber(self.hands[(winner + 2) % 4]),
            renumber(self.hands[(winner + 3) % 4]),
        ];

        let mut next = Starting { hands, unknown };
        if !next.unknown.is_empty() {
            let per_hand = next.tricks_remaining();
            // println!("per-hand = {}", per_hand);
            for i in 0..4 {
                if next.hands[i].len() + next.unknown.len() == per_hand {
                    next.hands[i] += next.unknown;
                    next.unknown = Cards::EMPTY;
                    break;
                }
            }
        }
        if cfg!(debug_assertions) {
            next.check();
        }
        if winner & 1 == 1 {
            // They won!
            TrickTaken::Them(next)
        } else {
            TrickTaken::Us(next)
        }
    }
    pub const fn tricks_remaining(self) -> usize {
        (self.unknown.len()
            + self.hands[0].len()
            + self.hands[1].len()
            + self.hands[2].len()
            + self.hands[3].len())
            / 4
    }
}

#[test]
fn test_random_hands() {
    use std::str::FromStr;
    let mut rng = SmallRng::seed_from_u64(0);
    let two_unknown = Starting {
        hands: [
            Cards::from_str("S AKQ H AKQ").unwrap(),
            Cards::EMPTY,
            Cards::from_str("S JT9 H JT9").unwrap(),
            Cards::from_str("S 8").unwrap(),
        ],
        unknown: Cards::from_str("S 76543 H 876543").unwrap(),
    };
    for _ in 0..100 {
        let deals = two_unknown.random_hands(&mut rng);
        assert_eq!(2, deals.len());
        for deal in deals.iter() {
            assert!(deal.iter().all(|hand| hand.len() == 6));
            assert_eq!(
                two_unknown.unknown,
                deal[1] + deal[3] - two_unknown.hands[3]
            );
        }
        // The second deal swaps cards between the two hands we cannot see.
        assert_ne!(deals[0], deals[1]);
        assert_eq!(deals[0][1] + deals[0][3], deals[1][1] + deals[1][3]);
        assert_eq!(5, (deals[0][1] & deals[1][3]).len());
    }

    let three_unknown = Starting {
        hands: [
            Cards::from_str("S AKQJ").unwrap(),
            Cards::EMPTY,
            Cards::EMPTY,
            Cards::EMPTY,
        ],
        unknown: Cards::from_str("S T98765432 H AKQ").unwrap(),
    };
    for _ in 0..100 {
        let deals = three_unknown.random_hands(&mut rng);
        assert_eq!(6, deals.len());
        for deal in deals.iter() {
            assert!(deal.iter().all(|hand| hand.len() == 4));
            assert_eq!(three_unknown.unknown, deal[1] + deal[2] + deal[3]);
        }
    }
}

#[test]
fn test_after() {
    let start = Starting {
        hands: [
            Cards::singleton(Card::S2),
            Cards::singleton(Card::D3),
            Cards::singleton(Card::D4),
            Cards::singleton(Card::S5),
        ],
        unknown: Cards::EMPTY,
    };
    let after = Starting {
        hands: [Cards::EMPTY, Cards::EMPTY, Cards::EMPTY, Cards::EMPTY],
        unknown: Cards::EMPTY,
    };
    let plays = [Card::S2, Card::D3, Card::D4, Card::S5];
    assert_eq!(TrickTaken::Them(after), start.after(plays, None));
    assert_eq!(
        TrickTaken::Them(after),
        start.after(plays, Some(Suit::Clubs))
    );
    assert_eq!(
        TrickTaken::Us(after),
        start.after(plays, Some(Suit::Diamonds))
    );
}

#[test]
fn test_after_void() {
    let start = Starting {
        hands: [
            Cards::singleton(Card::S2) + Cards::singleton(Card::S3),
            Cards::EMPTY,
            Cards::singleton(Card::S4) + Cards::singleton(Card::S7),
            Cards::EMPTY,
        ],
        unknown: Cards::singleton(Card::SK)
            + Cards::singleton(Card::SA)
            + Cards::singleton(Card::S5)
            + Cards::singleton(Card::S6),
    };
    let plays = [Card::S2, Card::SA, Card::S7, Card::S5];
    let after = Starting {
        hands: [
            Cards::EMPTY,
            Cards::singleton(Card::S3),
            Cards::EMPTY,
            Cards::singleton(Card::S2),
        ],
        unknown: Cards::singleton(Card::S4) + Cards::singleton(Card::S5),
    };
    assert_eq!(TrickTaken::Them(after), start.after(plays, None));

    let start = Starting {
        hands: [
            Cards::singleton(Card::S2) + Cards::singleton(Card::S3),
            Cards::EMPTY,
            Cards::singleton(Card::D4) + Cards::singleton(Card::S7),
            Cards::EMPTY,
        ],
        unknown: Cards::singleton(Card::D3)
            + Cards::singleton(Card::D5)
            + Cards::singleton(Card::S5)
            + Cards::singleton(Card::S6),
    };
    let plays = [Card::S2, Card::D3, Card::S7, Card::S5];
    let after = Starting {
        hands: [
            Cards::singleton(Card::D2),
            Cards::singleton(Card::S3),
            Cards::singleton(Card::S2),
            Cards::singleton(Card::D3),
        ],
        unknown: Cards::EMPTY,
    };
    println!("start:");
    for h in start.hands.iter() {
        println!("     hand: {}", h);
    }
    println!("  unknown: {}", start.unknown);
    println!(
        "plays are {} {} {} {}",
        plays[0], plays[1], plays[2], plays[3]
    );
    if let TrickTaken::Us(after) = start.after(plays, None) {
        println!("after plays:");
        for h in after.hands.iter() {
            println!("     hand: {h}");
        }
        println!("  unknown: {}", after.unknown);
    }
    assert_eq!(TrickTaken::Us(after), start.after(plays, None));
}

#[test]
fn test_after_known_void() {
    use std::str::FromStr;
    // We can see hands 0 and 2, but not hands 1 and 3.
    let start = Starting {
        hands: [
            Cards::from_str("S 32 D 2").unwrap(),
            Cards::EMPTY,
            Cards::from_str("H 32 D 3").unwrap(),
            Cards::EMPTY,
        ],
        unknown: Cards::from_str("S 654 H 654").unwrap(),
    };
    // Hand 2 shows out of spades, which we already knew, so either hand 1 or
    // hand 3 could still have the last spade.
    let after = Starting {
        hands: [
            Cards::EMPTY,
            Cards::from_str("S 2 D 2").unwrap(),
            Cards::EMPTY,
            Cards::from_str("H 2 D 3").unwrap(),
        ],
        unknown: Cards::from_str("S 3 H 543").unwrap(),
    };
    let plays = [Card::S2, Card::S4, Card::H2, Card::S5];
    assert_eq!(TrickTaken::Them(after), start.after(plays, None));

    // We can see hands 0 and 1, but not hands 2 and 3.
    let start = Starting {
        hands: [
            Cards::from_str("S 32 D 2").unwrap(),
            Cards::from_str("H 32 D 3").unwrap(),
            Cards::EMPTY,
            Cards::EMPTY,
        ],
        unknown: Cards::from_str("S 654 H 654").unwrap(),
    };
    // Hand 2 shows out of spades too, so hand 3 has the last two spades.
    let after = Starting {
        hands: [
            Cards::from_str("S 43").unwrap(),
            Cards::from_str("S 2 D 2").unwrap(),
            Cards::from_str("H 2 D 3").unwrap(),
            Cards::from_str("H 43").unwrap(),
        ],
        unknown: Cards::EMPTY,
    };
    let plays = [Card::S2, Card::H2, Card::H4, Card::S4];
    assert_eq!(TrickTaken::Them(after), start.after(plays, None));
}

/// How many tricks the side on lead expects to take.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Score {
    tricks: f64,
}

impl Score {
    pub fn mean(self) -> f64 {
        self.tricks
    }

    /// You can't score more than this!
    pub const MAX: Score = Score { tricks: 14.0 };
    /// You can't score less than this!
    pub const MIN: Score = Score { tricks: -1.0 };
}

impl Eq for Score {}

impl PartialOrd for Score {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Score {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // The number of tricks is never NaN, so this is a plain comparison
        // that the solver's loops do not need to check for NaN.
        if self.tricks < other.tricks {
            std::cmp::Ordering::Less
        } else if self.tricks > other.tricks {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Equal
        }
    }
}

impl std::ops::Add<TrickTaken> for Score {
    type Output = Score;

    /// The score of a position, from this score for what is left of it after
    /// its first trick.
    fn add(self, rhs: TrickTaken) -> Self::Output {
        let tricks = if let TrickTaken::Them(s) = rhs {
            s.tricks_remaining() as f64 - self.tricks
        } else {
            self.tricks + 1.0
        };
        Score { tricks }
    }
}

/// The scores of the deals sampled for a position, which make up its score.
///
/// Every deal counts the same, however many deals were sampled further on to
/// find its score: a deal after which every card is known, as when a defender
/// shows out, is as likely as one that leaves the rest to be sampled again.
#[derive(Default, Debug, Clone, Copy)]
struct Sampled {
    tricks: f64,
    deals: u32,
}

impl Sampled {
    fn add(&mut self, score: Score) {
        self.tricks += score.tricks;
        self.deals += 1;
    }
    /// The mean over the deals, or `Score::MIN` when there are none.
    fn score(self) -> Score {
        if self.deals == 0 {
            Score::MIN
        } else {
            Score {
                tricks: self.tricks / self.deals as f64,
            }
        }
    }
}

/// How carefully do we want to solve?
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
enum Thoroughness {
    /// Only consider playing high/low card in a suit
    HighLow,
    /// Imagine N random deals
    Statistical(usize),
    /// One round
    OneRound,
}

/// The scores of positions we have already solved
///
/// Looking positions up here is the solver's most common operation, so we
/// use a much faster hash than the default SipHash, whose protection against
/// hash flooding we do not need.
type Cache = std::collections::HashMap<Starting, Score, BuildHasherDefault<PositionHasher>>;

/// A fast hash for the few `u64`s that make up a `Starting`
///
/// Each word is multiplied by a constant of its own, so that the multiplies
/// do not wait for one another, and then the MurmurHash3 finalizer mixes
/// every bit into the low bits that `HashMap` uses to pick a bucket.
#[derive(Default)]
struct PositionHasher {
    sum: u64,
    words: usize,
}

/// A different odd constant for each of the five words of a `Starting`.
const WORD_MULTIPLIERS: [u64; 5] = [
    0x9e37_79b9_7f4a_7c15,
    0xc2b2_ae3d_27d4_eb4f,
    0x1656_67b1_9e37_79f9,
    0x27d4_eb2f_1656_67c5,
    0x517c_c1b7_2722_0a95,
];

impl Hasher for PositionHasher {
    fn write(&mut self, bytes: &[u8]) {
        for &b in bytes {
            self.write_u64(b.into());
        }
    }
    fn write_u64(&mut self, word: u64) {
        let multiplier = WORD_MULTIPLIERS[self.words % WORD_MULTIPLIERS.len()];
        self.words += 1;
        self.sum = self.sum.wrapping_add(word.wrapping_mul(multiplier));
    }
    fn write_usize(&mut self, word: usize) {
        self.write_u64(word as u64);
    }
    fn finish(&self) -> u64 {
        let h = (self.sum ^ (self.sum >> 32)).wrapping_mul(0xff51_afd7_ed55_8ccd);
        h ^ (h >> 29)
    }
}

/// The Naive solver assumes no knowledge from the bidding.
//  It's really only suitable for declarer play when opponents did not bid, and
//  even then is suboptimal on weak partnerships.
#[derive(Clone)]
pub struct Naive {
    cache: Cache,
    // cache: dashmap::DashMap<Starting, Score>,
    /// What is trump?
    trump: Option<Suit>,
    rng: SmallRng,
    care: Thoroughness,
}

impl Naive {
    pub fn new(trump: Option<Suit>) -> Self {
        Naive::with_thoroughness(trump, Thoroughness::Statistical(1))
    }
    pub fn statistical(trump: Option<Suit>, samples: usize) -> Self {
        Naive::with_thoroughness(trump, Thoroughness::Statistical(samples))
    }
    pub fn high_low(trump: Option<Suit>) -> Self {
        Naive::with_thoroughness(trump, Thoroughness::HighLow)
    }
    pub fn oneround(trump: Option<Suit>) -> Self {
        Naive::with_thoroughness(trump, Thoroughness::OneRound)
    }
    fn with_thoroughness(trump: Option<Suit>, care: Thoroughness) -> Self {
        let mut cache = Cache::default();
        // let cache = dashmap::DashMap::new();
        // Prepopulate cache with stopping point of no cards->no points.
        cache.insert(
            Starting {
                hands: [Cards::EMPTY; 4],
                unknown: Cards::EMPTY,
            },
            Score { tricks: 0.0 },
        );
        Naive {
            cache,
            trump,
            rng: SmallRng::from_entropy(),
            care,
        }
    }

    /// The plays of a hand that are worth trying, from the `plays` that it
    /// may make.  `all` is every card in the deal being solved, and `known`
    /// the cards that this hand is known to hold.
    fn thorough_plays(care: Thoroughness, plays: Cards, all: Cards, known: Cards) -> Cards {
        if care == Thoroughness::HighLow {
            let mut p = Cards::EMPTY;
            for suit in Suit::ALL.clone().iter().cloned() {
                let s = plays.in_suit(suit);
                if s.len() > 2 {
                    p = p.insert(s.max().unwrap());
                    p = p.insert(s.min().unwrap());
                } else {
                    p += s;
                }
            }
            p
        } else {
            // Of cards that touch, any one is as good as another: they win
            // and lose the same tricks, and once one is played the position
            // is the same whichever it was.  The cards that we only guessed
            // are in the hand are not the same as those that we know of,
            // since they are what was left over when the others were played.
            (plays & known).lowest_of_sequences(all) + (plays - known).lowest_of_sequences(all)
        }
    }

    fn num_statistics(&self) -> usize {
        if let Thoroughness::Statistical(n) = self.care {
            n
        } else if Thoroughness::OneRound == self.care {
            1 << 12
        } else {
            1
        }
    }

    /// Score the hand from the start of this trick, given its plays
    fn score_trick(&mut self, starting: Starting, plays: [Card; 4]) -> Score {
        if self.care == Thoroughness::OneRound {
            // We only count this trick, so all we need is who won it.
            let tricks = if trick_winner(plays, self.trump) & 1 == 1 {
                0.0
            } else {
                1.0
            };
            Score { tricks }
        } else if starting.unknown.is_empty() && starting.hands[0].len() == 2 {
            // The trick after this one is the last, and it is decided by the
            // cards that are left, so there is no need to look it up.
            let winner = trick_winner(plays, self.trump);
            let last = [0, 1, 2, 3].map(|i| {
                let seat = (winner + i) % 4;
                (starting.hands[seat] - Cards::singleton(plays[seat]))
                    .min()
                    .unwrap()
            });
            let last_winner = (winner + trick_winner(last, self.trump)) % 4;
            let ours = |seat: usize| (seat & 1 == 0) as u8 as f64;
            Score {
                tricks: ours(winner) + ours(last_winner),
            }
        } else {
            let trick_taken = starting.after_unchecked(plays, self.trump);
            self.solve(trick_taken.starting()) + trick_taken
        }
    }

    pub fn score(&mut self, starting: Starting) -> Score {
        starting.check();
        self.reserve_for(starting);
        self.solve(starting)
    }

    /// Make room in the cache for the positions that solving `starting`
    /// will put there, so that the cache is not grown (and everything in it
    /// rehashed) over and over as the search fills it.
    ///
    /// Each trick that is left multiplies the number of positions by about
    /// five.
    fn reserve_for(&mut self, starting: Starting) {
        let tricks = starting.tricks_remaining();
        let positions = 4 * 5usize.pow(tricks.saturating_sub(2) as u32);
        self.cache.reserve(positions.min(1 << 17));
    }

    /// [`Naive::score`] for a position that has been checked already, as
    /// every position reached from a position that was checked is.
    fn solve(&mut self, starting: Starting) -> Score {
        if cfg!(debug_assertions) {
            starting.check();
        }
        if let Some(score) = self.cache.get(&starting) {
            // println!("found score {:?}", score);
            return *score;
        }
        let mut sampled = Sampled::default();
        let care = self.care;
        let num_stats = if starting.unknown.is_empty() {
            1
        } else {
            self.num_statistics()
        };
        for _ in 0..num_stats {
            for hands in starting.random_hands(&mut self.rng).into_iter() {
                // We pick the plays that are best for us (c0 and c2) and
                // worst for us (c1 and c3).  As soon as a play can no longer
                // be strictly better for whoever chooses it than what they
                // already have, we skip that play's remaining responses.
                // Since we only ever replace a choice by a strictly better
                // one, this finds exactly the scores we would get by trying
                // every response.
                let all = hands[0] + hands[1] + hands[2] + hands[3];
                let plays = |seat: usize, cards: Cards| {
                    Naive::thorough_plays(care, cards, all, starting.hands[seat])
                };
                let mut best0 = Score::MIN;
                for c0 in plays(0, hands[0]) {
                    let mut worst1 = Score::MAX;
                    for c1 in plays(1, hands[1].following_suit(c0.suit())) {
                        let mut best2 = Score::MIN;
                        for c2 in plays(2, hands[2].following_suit(c0.suit())) {
                            let mut worst3 = Score::MAX;
                            for c3 in plays(3, hands[3].following_suit(c0.suit())) {
                                let mysc = self.score_trick(starting, [c0, c1, c2, c3]);
                                // println!(" {} {} {} {} -> {}", c0, c1, c2, c3, mysc.mean());
                                if mysc < worst3 {
                                    worst3 = mysc;
                                }
                                if worst3 <= best2 || worst3 <= best0 {
                                    break;
                                }
                            }
                            if worst3 > best2 {
                                best2 = worst3;
                            }
                            if best2 >= worst1 {
                                break;
                            }
                        }
                        if worst1 > best2 {
                            worst1 = best2;
                        }
                        if worst1 <= best0 {
                            break;
                        }
                    }
                    if worst1 > best0 {
                        best0 = worst1;
                        // println!("Move {} gives {}", c0, best0.mean());
                    }
                }
                sampled.add(best0);
            }
        }
        let score = sampled.score();
        self.cache.insert(starting, score);
        score
    }

    pub fn score_after(&mut self, mut starting: Starting, plays: &[Card]) -> (Score, Card) {
        // First update the hands to ensure that the plays correspond to cards
        // in the various hands.
        for (i, card) in plays.iter().cloned().enumerate() {
            assert!(starting.unknown.contains(card) || starting.hands[i].contains(card));
            starting.hands[i] += Cards::singleton(card);
            starting.unknown -= Cards::singleton(card);
        }
        starting.check();
        self.reserve_for(starting);
        let mut sampled = Sampled::default();
        let statistics = if starting.unknown.is_empty() {
            1
        } else {
            std::cmp::max(16, self.num_statistics())
        };
        // What each card that could be played next scores, over the deals.
        let mut play_result: std::collections::HashMap<Card, Sampled> =
            std::collections::HashMap::new();
        for _ in 0..statistics {
            for hands in starting.random_hands(&mut self.rng).into_iter() {
                let mut possible_plays = hands;
                for (i, c) in plays.iter().cloned().enumerate() {
                    possible_plays[i] = Cards::singleton(c);
                }
                // for i in 0..4 {
                //     println!("possible_plays[{}] are {}", i, possible_plays[i]);
                // }
                let mut best = Score::MIN;
                for c0 in possible_plays[0] {
                    // println!("considering playing first {}", c0);
                    let mut worst = Score::MAX;
                    for c1 in possible_plays[1].following_suit(c0.suit()) {
                        let mut best = Score::MIN;
                        // println!("considering {}", c1);
                        for c2 in possible_plays[2].following_suit(c0.suit()) {
                            let mut worst = Score::MAX;
                            for c3 in possible_plays[3].following_suit(c0.suit()) {
                                // println!("considering playing fourth {}", c3);
                                let mysc = self.score_trick(starting, [c0, c1, c2, c3]);
                                // println!(" {} {} {} {} -> {}", c0, c1, c2, c3, mysc.mean());
                                if plays.len() == 3 {
                                    play_result.entry(c3).or_default().add(mysc);
                                }
                                if mysc <= worst {
                                    worst = mysc;
                                // println!("worst = {}", worst.mean());
                                } else {
                                    // println!("worst = {} but sc = {}", worst.mean(), sc.mean());
                                }
                            }
                            if plays.len() == 2 {
                                play_result.entry(c2).or_default().add(worst);
                            }
                            if worst >= best {
                                best = worst;
                            }
                        }
                        if plays.len() == 1 {
                            play_result.entry(c1).or_default().add(best);
                        }
                        if worst >= best {
                            worst = best;
                        }
                    }
                    if plays.is_empty() {
                        play_result.entry(c0).or_default().add(worst);
                    }
                    if worst >= best {
                        best = worst;
                        // println!("Move {} gives {}", c0, best.mean());
                    }
                }
                sampled.add(best);
            }
        }
        let score = sampled.score();
        println!("Score: {:?}", score);
        if plays.is_empty() {
            self.cache.insert(starting, score);
        }
        println!("Best plays:");
        let play_result: std::collections::HashMap<Card, Score> = play_result
            .into_iter()
            .map(|(card, sampled)| (card, sampled.score()))
            .collect();
        for (p, s) in play_result.iter() {
            println!("    {} -> {:.3}", p, s.mean());
        }
        let risk_worth_taking = if self.care == Thoroughness::OneRound {
            0.1
        } else if !starting.unknown.is_empty() {
            0.5 / (statistics as f64).sqrt()
        } else {
            0.0
        };
        let mut best_plays = if plays.len() & 1 == 0 {
            // Us is playing next, want highest score
            let m = play_result.values().cloned().max().unwrap().mean();
            play_result
                .iter()
                .filter(|(_, s)| s.mean() >= m - risk_worth_taking)
                .map(|(c, _)| *c)
                .collect::<Vec<_>>()
        } else {
            // Them is playing next, want lowest score
            let m = play_result.values().cloned().min().unwrap().mean();
            play_result
                .iter()
                .filter(|(_, s)| s.mean() <= m + risk_worth_taking)
                .map(|(c, _)| *c)
                .collect::<Vec<_>>()
        };
        best_plays.sort_by_key(|c| c.rank() + if Some(c.suit()) == self.trump { 13 } else { 0 });
        (score, best_plays[0])
    }
}

/// How many tricks the side of the hands at seats 0 and 2 takes with the best
/// play on both sides, found by trying every play of every card.
///
/// This knows nothing about which plays are equivalent or about what the
/// solver remembers, so that it can check them.
#[cfg(test)]
fn exhaustive_tricks(hands: [Cards; 4], trump: Option<Suit>) -> usize {
    let tricks_left = hands[0].len();
    if tricks_left == 0 {
        return 0;
    }
    let mut best0 = 0;
    for c0 in hands[0] {
        let mut worst1 = tricks_left;
        for c1 in hands[1].following_suit(c0.suit()) {
            let mut best2 = 0;
            for c2 in hands[2].following_suit(c0.suit()) {
                let mut worst3 = tricks_left;
                for c3 in hands[3].following_suit(c0.suit()) {
                    let plays = [c0, c1, c2, c3];
                    let winner = trick_winner(plays, trump);
                    // The next trick is led by the winner.
                    let mut next = [Cards::EMPTY; 4];
                    for seat in 0..4 {
                        next[seat] = hands[(winner + seat) % 4]
                            - Cards::singleton(plays[(winner + seat) % 4]);
                    }
                    let theirs_or_ours = exhaustive_tricks(next, trump);
                    let ours = if winner & 1 == 0 {
                        1 + theirs_or_ours
                    } else {
                        tricks_left - 1 - theirs_or_ours
                    };
                    worst3 = worst3.min(ours);
                }
                best2 = best2.max(worst3);
            }
            worst1 = worst1.min(best2);
        }
        best0 = best0.max(worst1);
    }
    best0
}

#[test]
fn solver_agrees_with_trying_every_play() {
    let mut rng = SmallRng::seed_from_u64(7);
    // Positions from two dense suits are full of cards that touch, and so
    // of plays that the solver does not try; positions from the whole deck
    // have fewer of them.
    for (deck, cards_each) in [
        (Cards::SPADES + Cards::HEARTS, 4),
        (Cards::SPADES + Cards::HEARTS, 3),
        (Cards::ALL, 4),
        (Cards::ALL, 3),
    ] {
        for _ in 0..25 {
            let mut deck = deck;
            let mut hands = [Cards::EMPTY; 4];
            for hand in &mut hands {
                *hand = deck.pick_rng(&mut rng, cards_each).unwrap();
            }
            for trump in [None, Some(Suit::Spades), Some(Suit::Diamonds)] {
                let want = exhaustive_tricks(hands, trump) as f64;
                let start = Starting {
                    hands,
                    unknown: Cards::EMPTY,
                };
                let got = Naive::new(trump).score(start).mean();
                assert_eq!(want, got, "{hands:?} with trump {trump:?}");
            }
        }
    }
}

#[test]
fn naive_score() {
    let mut nt = Naive::new(None);
    let mut sp = Naive::new(Some(Suit::Spades));
    use std::str::FromStr;
    assert_eq!(
        1.0,
        nt.score_after(
            Starting {
                hands: [
                    Cards::from_str("SA2").unwrap(),
                    Cards::from_str("S34").unwrap(),
                    Cards::from_str("S56").unwrap(),
                    Cards::from_str("S78").unwrap(),
                ],
                unknown: Cards::EMPTY,
            },
            &[Card::S2]
        )
        .0
        .mean()
    );
    assert_eq!(
        0.0,
        nt.score_after(
            Starting {
                hands: [
                    Cards::from_str("SA2").unwrap(),
                    Cards::from_str("S34").unwrap(),
                    Cards::from_str("S56").unwrap(),
                    Cards::from_str("S7D8").unwrap(),
                ],
                unknown: Cards::EMPTY,
            },
            &[Card::S2]
        )
        .0
        .mean()
    );
    assert_eq!(
        2.0,
        nt.score_after(
            Starting {
                hands: [
                    Cards::from_str("SA2").unwrap(),
                    Cards::from_str("S34").unwrap(),
                    Cards::from_str("S56").unwrap(),
                    Cards::from_str("S7 D8").unwrap(),
                ],
                unknown: Cards::EMPTY,
            },
            &[Card::SA]
        )
        .0
        .mean()
    );
    assert_eq!(
        Card::SA,
        nt.score_after(
            Starting {
                hands: [
                    Cards::from_str("SA2").unwrap(),
                    Cards::from_str("S34").unwrap(),
                    Cards::from_str("S56").unwrap(),
                    Cards::from_str("S7 D8").unwrap(),
                ],
                unknown: Cards::EMPTY,
            },
            &[]
        )
        .1
    );

    assert_eq!(
        0.0,
        nt.score_after(
            Starting {
                hands: [
                    Cards::singleton(Card::S2) + Cards::singleton(Card::SA),
                    Cards::singleton(Card::D3) + Cards::singleton(Card::D5),
                    Cards::singleton(Card::D4) + Cards::singleton(Card::C3),
                    Cards::singleton(Card::S5) + Cards::singleton(Card::D8),
                ],
                unknown: Cards::EMPTY,
            },
            &[Card::S2]
        )
        .0
        .mean()
    );

    assert_eq!(
        0.0,
        nt.score(Starting {
            hands: [
                Cards::singleton(Card::S2),
                Cards::singleton(Card::D3),
                Cards::singleton(Card::D4),
                Cards::singleton(Card::S5),
            ],
            unknown: Cards::EMPTY,
        })
        .mean()
    );
    assert_eq!(
        2.0,
        nt.score(Starting {
            hands: [
                Cards::singleton(Card::S2) + Cards::singleton(Card::SA),
                Cards::singleton(Card::D3) + Cards::singleton(Card::D5),
                Cards::singleton(Card::D4) + Cards::singleton(Card::C3),
                Cards::singleton(Card::S5) + Cards::singleton(Card::D8),
            ],
            unknown: Cards::EMPTY,
        })
        .mean()
    );
    assert_eq!(
        2.0,
        nt.score_after(
            Starting {
                hands: [
                    Cards::singleton(Card::S2) + Cards::singleton(Card::SA),
                    Cards::singleton(Card::D3) + Cards::singleton(Card::D5),
                    Cards::singleton(Card::D4) + Cards::singleton(Card::C3),
                    Cards::singleton(Card::S5) + Cards::singleton(Card::D8),
                ],
                unknown: Cards::EMPTY,
            },
            &[]
        )
        .0
        .mean()
    );
    assert_eq!(
        13.0,
        nt.score(Starting {
            hands: [Cards::SPADES, Cards::HEARTS, Cards::DIAMONDS, Cards::CLUBS,],
            unknown: Cards::EMPTY,
        })
        .mean()
    );
    assert_eq!(
        13.0,
        nt.score_after(
            Starting {
                hands: [Cards::SPADES, Cards::HEARTS, Cards::DIAMONDS, Cards::CLUBS,],
                unknown: Cards::EMPTY,
            },
            &[]
        )
        .0
        .mean()
    );
    assert_eq!(
        13.0,
        nt.score_after(
            Starting {
                hands: [Cards::SPADES, Cards::HEARTS, Cards::DIAMONDS, Cards::CLUBS,],
                unknown: Cards::EMPTY,
            },
            &[Card::SA]
        )
        .0
        .mean()
    );
    assert_eq!(
        13.0,
        sp.score(Starting {
            hands: [Cards::SPADES, Cards::HEARTS, Cards::DIAMONDS, Cards::CLUBS,],
            unknown: Cards::EMPTY,
        })
        .mean()
    );
    assert_eq!(
        0.0,
        sp.score(Starting {
            hands: [Cards::HEARTS, Cards::SPADES, Cards::DIAMONDS, Cards::CLUBS,],
            unknown: Cards::EMPTY,
        })
        .mean()
    );
}

/// The most tricks the side on lead can take when every hand is known, found
/// by trying every way to play out the hand (or just its first trick).
#[cfg(test)]
fn plain_minimax(hands: [Cards; 4], trump: Option<Suit>, one_trick: bool) -> usize {
    fn best(
        hands: [Cards; 4],
        plays: &mut Vec<Card>,
        trump: Option<Suit>,
        one_trick: bool,
    ) -> usize {
        let seat = plays.len();
        if seat == 4 {
            // The highest trump wins, or else the highest card of the suit led.
            let led = plays[0].suit();
            let winner = (0..4)
                .max_by_key(|&i| {
                    let card = plays[i];
                    (Some(card.suit()) == trump, card.suit() == led, card.rank())
                })
                .unwrap();
            let ours = winner & 1 == 0;
            let mut left = hands;
            for (hand, &card) in left.iter_mut().zip(plays.iter()) {
                *hand -= Cards::singleton(card);
            }
            if one_trick || left[winner].is_empty() {
                return ours as usize;
            }
            let left = [0, 1, 2, 3].map(|i| left[(winner + i) % 4]);
            let later = best(left, &mut Vec::new(), trump, one_trick);
            return if ours {
                1 + later
            } else {
                left[0].len() - later
            };
        }
        let choices = if seat == 0 {
            hands[0]
        } else {
            hands[seat].following_suit(plays[0].suit())
        };
        let mut tricks = Vec::new();
        for card in choices {
            plays.push(card);
            tricks.push(best(hands, plays, trump, one_trick));
            plays.pop();
        }
        if seat & 1 == 0 {
            tricks.into_iter().max().unwrap()
        } else {
            tricks.into_iter().min().unwrap()
        }
    }
    best(hands, &mut Vec::new(), trump, one_trick)
}

#[test]
fn score_matches_plain_minimax() {
    let mut rng = SmallRng::seed_from_u64(0);
    for n in 1..=4 {
        for _ in 0..25 {
            let mut deck = Cards::ALL;
            let hands = [(); 4].map(|_| deck.pick_rng(&mut rng, n).unwrap());
            let starting = Starting {
                hands,
                unknown: Cards::EMPTY,
            };
            for trump in [None, Some(Suit::Hearts)] {
                assert_eq!(
                    plain_minimax(hands, trump, false) as f64,
                    Naive::new(trump).score(starting).mean(),
                    "{:?} with {:?} trump",
                    hands,
                    trump
                );
                assert_eq!(
                    plain_minimax(hands, trump, true) as f64,
                    Naive::oneround(trump).score(starting).mean(),
                    "first trick of {:?} with {:?} trump",
                    hands,
                    trump
                );
            }
        }
    }
}

#[test]
fn high_low_plays_top_and_bottom_of_each_suit() {
    use std::str::FromStr;
    // On a lead, every long suit contributes its own top and bottom card, and
    // short suits contribute every card.
    assert_eq!(
        Cards::from_str("S: A2 H: A2 D: 2 C: 2").unwrap(),
        Naive::thorough_plays(
            Thoroughness::HighLow,
            Cards::from_str("S: AKQ32 H: AKQ32 D: 2 C: 2").unwrap(),
            Cards::from_str("S: AKQ32 H: AKQ32 D: 2 C: 2").unwrap(),
            Cards::EMPTY
        )
    );
    assert_eq!(
        Cards::from_str("S: A5 H: KQ D: J4 C: 8").unwrap(),
        Naive::thorough_plays(
            Thoroughness::HighLow,
            Cards::from_str("S: A9765 H: KQ D: JT54 C: 8").unwrap(),
            Cards::from_str("S: A9765 H: KQ D: JT54 C: 8").unwrap(),
            Cards::EMPTY
        )
    );
    // Following suit, only one suit is playable.
    assert_eq!(
        Cards::from_str("H: A2").unwrap(),
        Naive::thorough_plays(
            Thoroughness::HighLow,
            Cards::from_str("H: AKQ32").unwrap(),
            Cards::from_str("H: AKQ32").unwrap(),
            Cards::EMPTY
        )
    );
}

#[test]
fn test_ruffing() {
    use std::str::FromStr;
    assert_eq!(
        Card::S5,
        Naive::statistical(Some(Suit::Spades), 16)
            .score_after(
                Starting {
                    hands: [
                        Cards::from_str("S: AKQJ").unwrap(),
                        Cards::from_str("S: 5 D: A2 H: 9").unwrap(),
                        Cards::from_str("").unwrap(),
                        Cards::from_str("").unwrap(),
                    ],
                    unknown: Cards::from_str("C: 2345 H: 2345").unwrap(),
                },
                &[Card::SA]
            )
            .1
    );
    assert_eq!(
        Card::S5,
        Naive::new(Some(Suit::Spades))
            .score_after(
                Starting {
                    hands: [
                        Cards::from_str("S: AKQJ").unwrap(),
                        Cards::from_str("S: 5 D: A2 H: 9").unwrap(),
                        Cards::from_str("").unwrap(),
                        Cards::from_str("").unwrap(),
                    ],
                    unknown: Cards::from_str("C: 2345 H: 2345").unwrap(),
                },
                &[Card::SA]
            )
            .1
    );
    assert_eq!(
        Card::S5,
        Naive::oneround(Some(Suit::Spades))
            .score_after(
                Starting {
                    hands: [
                        Cards::from_str("S: AKQJ").unwrap(),
                        Cards::from_str("S: 5 D: A2 H: 9").unwrap(),
                        Cards::from_str("").unwrap(),
                        Cards::from_str("").unwrap(),
                    ],
                    unknown: Cards::from_str("C: 2345 H: 2345").unwrap(),
                },
                &[Card::SA]
            )
            .1
    );
}

#[test]
fn test_bad_bid() {
    use std::str::FromStr;
    println!("first test");
    assert_eq!(
        Card::CA,
        Naive::oneround(Some(Suit::Spades))
            .score_after(
                Starting {
                    hands: [
                        Cards::from_str("♣J").unwrap(),
                        Cards::from_str("♠J962♦AKQJ76♣AQ5").unwrap(),
                        Cards::from_str("").unwrap(),
                        Cards::from_str("♠A84♥AKQJ9542♦9♣T").unwrap(),
                    ],
                    unknown: Cards::from_str("♠KQT753♥T8763♦T85432♣K9876432").unwrap(),
                },
                &[Card::CJ]
            )
            .1
    );
    println!("second test");
    assert_eq!(
        Card::CA,
        Naive::statistical(Some(Suit::Spades), 16)
            .score_after(
                Starting {
                    hands: [
                        Cards::from_str("♣J").unwrap(),
                        Cards::from_str("♣A95").unwrap(),
                        Cards::from_str("").unwrap(),
                        Cards::from_str("♥A♦9♣T").unwrap(),
                    ],
                    unknown: Cards::from_str("♦T8♣432").unwrap(),
                },
                &[Card::CJ]
            )
            .1
    );

    let score = Naive::oneround(Some(Suit::Spades)).score_after(
        Starting {
            hands: [
                Cards::from_str("♣T").unwrap(),
                Cards::from_str("♠A84♥AKQJ9542♦9♣J").unwrap(),
                Cards::from_str("♠J962♦AKQJ76♣AQ5").unwrap(),
                Cards::from_str("").unwrap(),
            ],
            unknown: Cards::from_str("♠KQT753♥T8763♦T85432♣K9876432").unwrap(),
        },
        &[Card::C10, Card::CJ],
    );
    println!("mean score: {}", score.0.mean());
    assert_eq!(Card::CA, score.1);
}

/// Every choice of `k` of `cards`.
#[cfg(test)]
fn subsets(cards: Cards, k: usize) -> Vec<Cards> {
    if k == 0 {
        return vec![Cards::EMPTY];
    }
    let mut rest = cards;
    let mut found = Vec::new();
    while let Some(first) = rest.next() {
        for others in subsets(rest, k - 1) {
            found.push(others.insert(first));
        }
    }
    found
}

/// Every way to deal the unknown cards of `starting` to the hands that are
/// short of cards.
#[cfg(test)]
fn every_deal(starting: Starting) -> Vec<[Cards; 4]> {
    let n = starting.tricks_remaining();
    let mut deals = vec![(starting.hands, starting.unknown)];
    for seat in 0..4 {
        let mut dealt = Vec::new();
        for (hands, unknown) in deals {
            for extra in subsets(unknown, n - hands[seat].len()) {
                let mut hands = hands;
                hands[seat] += extra;
                dealt.push((hands, unknown - extra));
            }
        }
        deals = dealt;
    }
    deals.into_iter().map(|(hands, _)| hands).collect()
}

/// The tricks the side on lead expects from `starting` when every deal of the
/// unknown cards is equally likely, which is what `Naive::score` estimates
/// from a sample of the deals.
#[cfg(test)]
fn expected_tricks(
    starting: Starting,
    trump: Option<Suit>,
    known: &mut std::collections::HashMap<Starting, f64>,
) -> f64 {
    if starting.tricks_remaining() == 0 {
        return 0.0;
    }
    if let Some(&tricks) = known.get(&starting) {
        return tricks;
    }
    let deals = every_deal(starting);
    let mut total = 0.0;
    for hands in deals.iter() {
        let mut best0 = f64::MIN;
        for c0 in hands[0] {
            let mut worst1 = f64::MAX;
            for c1 in hands[1].following_suit(c0.suit()) {
                let mut best2 = f64::MIN;
                for c2 in hands[2].following_suit(c0.suit()) {
                    let mut worst3 = f64::MAX;
                    for c3 in hands[3].following_suit(c0.suit()) {
                        let tricks = match starting.after([c0, c1, c2, c3], trump) {
                            TrickTaken::Us(next) => 1.0 + expected_tricks(next, trump, known),
                            TrickTaken::Them(next) => {
                                next.tricks_remaining() as f64 - expected_tricks(next, trump, known)
                            }
                        };
                        worst3 = worst3.min(tricks);
                    }
                    best2 = best2.max(worst3);
                }
                worst1 = worst1.min(best2);
            }
            best0 = best0.max(worst1);
        }
        total += best0;
    }
    let tricks = total / deals.len() as f64;
    known.insert(starting, tricks);
    tricks
}

/// `naive`, sampling the deals that `seed` gives it.
#[cfg(test)]
fn seeded(mut naive: Naive, seed: u64) -> Naive {
    naive.rng = SmallRng::seed_from_u64(seed);
    naive
}

#[test]
fn every_sampled_deal_counts_the_same() {
    use std::str::FromStr;
    // At notrump with two tricks to go, the hand on lead has the ace and
    // queen of spades.  The queen is a second trick when the king and jack
    // are in different hands, as in four of the six ways to deal the four
    // unknown cards.
    let starting = Starting {
        hands: [
            Cards::from_str("S AQ").unwrap(),
            Cards::EMPTY,
            Cards::from_str("H 32").unwrap(),
            Cards::EMPTY,
        ],
        unknown: Cards::from_str("S KJ D AK").unwrap(),
    };
    let expected = expected_tricks(starting, None, &mut Default::default());
    assert!((expected - 5.0 / 3.0).abs() < 1e-9, "{}", expected);
    // In the other two deals a defender shows out on the ace, and then every
    // card is known.  Those deals count as much as the rest.
    let score = seeded(Naive::statistical(None, 256), 0).score(starting);
    assert!((score.mean() - expected).abs() < 0.1, "{}", score.mean());
}

#[test]
fn score_estimates_the_expected_tricks() {
    let mut rng = SmallRng::seed_from_u64(1);
    for tricks in 2..=3 {
        for i in 0..24 {
            // The two hands that are seen, in turn the leader's and their
            // partner's, the other two, and the leader's with a neighbour's.
            let seen = [[0, 2], [1, 3], [0, 1], [0, 3]][i % 4];
            let mut deck = Cards::ALL;
            let mut hands = [Cards::EMPTY; 4];
            for seat in seen {
                hands[seat] = deck.pick_rng(&mut rng, tricks).unwrap();
            }
            let unknown = deck.pick_rng(&mut rng, 2 * tricks).unwrap();
            let starting = Starting { hands, unknown };
            for trump in [None, Some(Suit::Hearts)] {
                let expected = expected_tricks(starting, trump, &mut Default::default());
                let score = seeded(Naive::statistical(trump, 256), i as u64).score(starting);
                assert!(
                    (score.mean() - expected).abs() < 0.15,
                    "{} rather than {expected} for {:?} and {unknown} with {trump:?} trump",
                    score.mean(),
                    hands.map(|hand| hand.to_string())
                );
            }
        }
    }
}
