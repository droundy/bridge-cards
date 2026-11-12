pub use bridge_deck::{Card, CardMap, Cards, Suit};
use rand::rngs::SmallRng;
use rand::SeedableRng;
use std::hash::{BuildHasherDefault, Hasher};

#[cfg(feature = "benchmarks")]
mod benches;

pub fn normalize_create_map(all: Cards) -> CardMap {
    let mut map = CardMap::default();
    for &s in [Suit::Clubs, Suit::Diamonds, Suit::Hearts, Suit::Spades].iter() {
        for (n, x) in all.in_suit(s).enumerate() {
            map[x] = Card::new(s, 2 + n as u8);
        }
    }
    map
}

pub fn map_normalize(map: CardMap, hand: Cards) -> Cards {
    let mut c = Cards::EMPTY;
    for x in hand {
        c = c.insert(map[x]);
    }
    c
}

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
    let map = normalize_create_map(Cards::ACES);
    println!("\n\nTwos:\n\n");
    assert_eq!(twos, normalize(Cards::ACES, Cards::ACES));
    assert_eq!(
        map_normalize(map, Cards::ACES),
        normalize(Cards::ACES, Cards::ACES)
    );
    println!("\n\nWeird all aces:\n\n");
    assert_eq!(Cards::ACES, normalize(Cards::ALL, Cards::ACES));
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Starting {
    /// The cards that we have, starting with the leader
    pub hands: [Cards; 4],
    /// The cards that I don't know who has
    pub unknown: Cards,
}

/// Which of these plays wins the trick, counting from the leader
fn trick_winner(plays: [Card; 4], trump: Option<Suit>) -> usize {
    let played = Cards::singleton(plays[0])
        + Cards::singleton(plays[1])
        + Cards::singleton(plays[2])
        + Cards::singleton(plays[3]);
    let mut winning_suit = plays[0].suit();
    if let Some(t) = trump {
        if !played.in_suit(t).is_empty() {
            winning_suit = t;
        }
    }
    // println!("Winning suit is {:?}", winning_suit);
    let mut winner = 0;
    let mut best_rank = 0;
    for i in plays
        .iter()
        .enumerate()
        .filter(|(_, c)| c.suit() == winning_suit)
        .map(|(i, _)| i)
    {
        if plays[i].rank() > best_rank {
            best_rank = plays[i].rank();
            winner = i;
        }
    }
    winner
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
        for seat in 0..4 {
            println!(
                "  hand {}: {} ({})",
                seat,
                self.hands[seat],
                self.hands[seat].len()
            );
        }
        println!("  extra: {} ({})", self.unknown, self.unknown.len());
    }
    pub fn check(&self) {
        let n = self.tricks_remaining();
        for seat in 0..4 {
            if self.hands[seat].len() > n {
                println!("Thi sis crazy! with {} tricks left", n);
                self.printme();
            }
            assert!(self.hands[seat].len() <= n);
        }
    }
    pub fn after(mut self, plays: [Card; 4], trump: Option<Suit>) -> TrickTaken {
        self.check();
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

        let map = normalize_create_map(cards_left);
        let unknown = map_normalize(map, self.unknown - played);
        let hands = [
            map_normalize(map, self.hands[winner] - played),
            map_normalize(map, self.hands[(winner + 1) % 4] - played),
            map_normalize(map, self.hands[(winner + 2) % 4] - played),
            map_normalize(map, self.hands[(winner + 3) % 4] - played),
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
        next.check();
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
            println!("     hand: {}", h);
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
        self.tricks.partial_cmp(&other.tricks).unwrap()
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
/// FxHash combines the words, and then the MurmurHash3 finalizer mixes every
/// bit into the low bits that `HashMap` uses to pick a bucket.
#[derive(Default)]
struct PositionHasher(u64);

impl Hasher for PositionHasher {
    fn write(&mut self, bytes: &[u8]) {
        for &b in bytes {
            self.write_u64(b.into());
        }
    }
    fn write_u64(&mut self, word: u64) {
        self.0 = (self.0.rotate_left(5) ^ word).wrapping_mul(0x517c_c1b7_2722_0a95);
    }
    fn write_usize(&mut self, word: usize) {
        self.write_u64(word as u64);
    }
    fn finish(&self) -> u64 {
        let mut h = self.0;
        h ^= h >> 33;
        h = h.wrapping_mul(0xff51_afd7_ed55_8ccd);
        h ^= h >> 33;
        h = h.wrapping_mul(0xc4ce_b9fe_1a85_ec53);
        h ^ (h >> 33)
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

    fn thorough_plays(&self, plays: Cards) -> Cards {
        if self.care == Thoroughness::HighLow {
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
            plays
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
        } else {
            let trick_taken = starting.after(plays, self.trump);
            self.score(trick_taken.starting()) + trick_taken
        }
    }

    pub fn score(&mut self, starting: Starting) -> Score {
        if let Some(score) = self.cache.get(&starting) {
            // println!("found score {:?}", score);
            return *score;
        }
        let mut sampled = Sampled::default();
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
                let mut best0 = Score::MIN;
                for c0 in self.thorough_plays(hands[0]) {
                    let mut worst1 = Score::MAX;
                    for c1 in self.thorough_plays(hands[1].following_suit(c0.suit())) {
                        let mut best2 = Score::MIN;
                        for c2 in self.thorough_plays(hands[2].following_suit(c0.suit())) {
                            let mut worst3 = Score::MAX;
                            for c3 in self.thorough_plays(hands[3].following_suit(c0.suit())) {
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
    let naive = Naive::high_low(None);
    // On a lead, every long suit contributes its own top and bottom card, and
    // short suits contribute every card.
    assert_eq!(
        Cards::from_str("S: A2 H: A2 D: 2 C: 2").unwrap(),
        naive.thorough_plays(Cards::from_str("S: AKQ32 H: AKQ32 D: 2 C: 2").unwrap())
    );
    assert_eq!(
        Cards::from_str("S: A5 H: KQ D: J4 C: 8").unwrap(),
        naive.thorough_plays(Cards::from_str("S: A9765 H: KQ D: JT54 C: 8").unwrap())
    );
    // Following suit, only one suit is playable.
    assert_eq!(
        Cards::from_str("H: A2").unwrap(),
        naive.thorough_plays(Cards::from_str("H: AKQ32").unwrap())
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
