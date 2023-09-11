pub use bridge_deck::{Card, CardMap, Cards, Suit};
use rand::rngs::SmallRng;
use rand::SeedableRng;

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
        let mut needs_least = 0; // = hands.iter().cloned().map(|h| n - h.len()).min().unwrap();
        let mut need_cards = [0, 0, 0, 0];
        let mut num_needs = 0;
        for seat in 0..4 {
            if hands[seat].len() < n {
                need_cards[num_needs] = seat;
                num_needs += 1;
                needs_least = std::cmp::min(needs_least, n - hands[seat].len());
            }
        }
        match num_needs {
            0 => vec![hands],
            1 => {
                for seat in 0..4 {
                    if hands[seat].len() < n {
                        hands[seat] += unknown;
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
                for seat in need_cards[..2].iter().cloned() {
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
        let mut winning_suit = plays[0].suit();
        if let Some(t) = trump {
            if played.in_suit(t).len() > 0 {
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
        let tricks_left = self.tricks_remaining();
        let unknown_in_suit = self.unknown.in_suit(plays[0].suit());
        if unknown_in_suit.len() > 0 {
            for i in 1..4 {
                if plays[i].suit() != plays[0].suit() {
                    // i showed void.  if only one player has unknown cards, they must have
                    // all the cards in this suit.
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
        if next.unknown.len() > 0 {
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

#[derive(Default, Debug, Clone, Copy, Hash)]
pub struct Score {
    tot_score: u32,
    num: u32,
}

impl Score {
    pub fn mean(self) -> f64 {
        if self.num == 0 {
            -1.0
        } else {
            self.tot_score as f64 / self.num as f64
        }
    }

    /// You can't score more than this!
    pub const MAX: Score = Score {
        tot_score: 14,
        num: 1,
    };
    /// You can't score less than this!
    pub const MIN: Score = Score {
        tot_score: 0,
        num: 0,
    };
}

impl PartialEq for Score {
    fn eq(&self, other: &Self) -> bool {
        self.mean() == other.mean()
    }
}
impl Eq for Score {
    fn assert_receiver_is_total_eq(&self) {}
}

impl PartialOrd for Score {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.mean().partial_cmp(&other.mean())
    }
}
impl Ord for Score {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.mean().partial_cmp(&other.mean()).unwrap()
    }
}

impl std::ops::Add<Score> for Score {
    type Output = Score;

    fn add(self, rhs: Score) -> Self::Output {
        Score {
            tot_score: self.tot_score + rhs.tot_score,
            num: self.num + rhs.num,
        }
    }
}

impl std::ops::Add<usize> for Score {
    type Output = Score;

    fn add(self, rhs: usize) -> Self::Output {
        Score {
            tot_score: self.tot_score + rhs as u32,
            num: self.num + 1,
        }
    }
}

impl std::ops::Add<TrickTaken> for Score {
    type Output = Score;

    fn add(self, rhs: TrickTaken) -> Self::Output {
        if let TrickTaken::Them(s) = rhs {
            Score {
                tot_score: self.num * s.tricks_remaining() as u32 - self.tot_score,
                num: self.num,
            }
        } else {
            Score {
                tot_score: self.tot_score + self.num,
                num: self.num,
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

/// The Naive solver assumes no knowledge from the bidding.
//  It's really only suitable for declarer play when opponents did not bid, and
//  even then is suboptimal on weak partnerships.
#[derive(Clone)]
pub struct Naive {
    cache: std::collections::HashMap<Starting, Score>,
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
        let mut cache = std::collections::HashMap::new();
        // let cache = dashmap::DashMap::new();
        // Prepopulate cache with stopping point of no cards->no points.
        cache.insert(
            Starting {
                hands: [Cards::EMPTY; 4],
                unknown: Cards::EMPTY,
            },
            Score {
                tot_score: 0,
                num: 1,
            },
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
                    p = p.insert(plays.max().unwrap());
                    p = p.insert(plays.min().unwrap());
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

    pub fn score(&mut self, starting: Starting) -> Score {
        if let Some(score) = self.cache.get(&starting) {
            // println!("found score {:?}", score);
            return *score;
        }
        let mut score = Score {
            tot_score: 0,
            num: 0,
        };
        let num_stats = if starting.unknown.is_empty() {
            1
        } else {
            self.num_statistics()
        };
        for _ in 0..num_stats {
            for hands in starting.random_hands(&mut self.rng).into_iter() {
                let mut best = Score::MIN;
                for c0 in self.thorough_plays(hands[0]) {
                    let mut worst = Score::MAX;
                    for c1 in self.thorough_plays(hands[1].following_suit(c0.suit())) {
                        let mut best = Score::MIN;
                        for c2 in self.thorough_plays(hands[2].following_suit(c0.suit())) {
                            let mut worst = Score::MAX;
                            for c3 in self.thorough_plays(hands[3].following_suit(c0.suit())) {
                                let trick_taken = starting.after([c0, c1, c2, c3], self.trump);
                                let sc = if self.care == Thoroughness::OneRound {
                                    Score {
                                        tot_score: 0,
                                        num: 1,
                                    }
                                } else {
                                    self.score(trick_taken.starting())
                                };
                                let mysc = if self.care != Thoroughness::OneRound {
                                    sc + trick_taken
                                } else {
                                    if let TrickTaken::Them(_) = trick_taken {
                                        sc
                                    } else {
                                        Score {
                                            tot_score: 1,
                                            num: 1,
                                        }
                                    }
                                };
                                // println!(
                                //     " {} {} {} {} -> {} from {:?} and {:?}",
                                //     c0,
                                //     c1,
                                //     c2,
                                //     c3,
                                //     mysc.mean(),
                                //     sc,
                                //     trick_taken
                                // );
                                if mysc < worst {
                                    worst = mysc;
                                // println!("worst = {}", worst.mean());
                                } else {
                                    // println!("worst = {} but sc = {}", worst.mean(), sc.mean());
                                }
                            }
                            if worst > best {
                                best = worst;
                            }
                        }
                        if worst > best {
                            worst = best;
                        }
                    }
                    if worst > best {
                        best = worst;
                        // println!("Move {} gives {}", c0, best.mean());
                    }
                }
                score = score + best;
            }
        }
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
        let mut score = Score {
            tot_score: 0,
            num: 0,
        };
        let statistics = if starting.unknown.is_empty() {
            1
        } else {
            std::cmp::max(16, self.num_statistics())
        };
        let mut play_result: std::collections::HashMap<Card, Score> =
            std::collections::HashMap::new();
        for _ in 0..statistics {
            for hands in starting.random_hands(&mut self.rng).into_iter() {
                let mut possible_plays = hands.clone();
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
                                let trick_taken = starting.after([c0, c1, c2, c3], self.trump);
                                // println!("{} {} {} {} => {:?}", c0, c1, c2, c3, trick_taken);
                                let sc = if self.care == Thoroughness::OneRound {
                                    Score {
                                        tot_score: 0,
                                        num: 1,
                                    }
                                } else {
                                    self.score(trick_taken.starting())
                                };
                                // println!("score is {:?}", sc);
                                // println!("trick taken is {:?}", trick_taken);
                                let mysc = if self.care != Thoroughness::OneRound {
                                    sc + trick_taken
                                } else {
                                    if let TrickTaken::Them(_) = trick_taken {
                                        sc
                                    } else {
                                        Score {
                                            tot_score: 1,
                                            num: 1,
                                        }
                                    }
                                };
                                // println!(
                                //     " {} {} {} {} -> {} from {:?} and {:?}",
                                //     c0, c1, c2, c3, mysc.mean(), sc, trick_taken
                                // );
                                if plays.len() == 3 {
                                    let s = play_result
                                        .get(&c3)
                                        .unwrap_or(&Score {
                                            tot_score: 0,
                                            num: 0,
                                        })
                                        .clone();
                                    play_result.insert(c3, s + mysc);
                                }
                                if mysc <= worst {
                                    worst = mysc;
                                // println!("worst = {}", worst.mean());
                                } else {
                                    // println!("worst = {} but sc = {}", worst.mean(), sc.mean());
                                }
                            }
                            if plays.len() == 2 {
                                let s = play_result
                                    .get(&c2)
                                    .unwrap_or(&Score {
                                        tot_score: 0,
                                        num: 0,
                                    })
                                    .clone();
                                play_result.insert(c2, s + worst);
                            }
                            if worst >= best {
                                best = worst;
                            }
                        }
                        if plays.len() == 1 {
                            let s = play_result
                                .get(&c1)
                                .unwrap_or(&Score {
                                    tot_score: 0,
                                    num: 0,
                                })
                                .clone();
                            play_result.insert(c1, s + best);
                        }
                        if worst >= best {
                            worst = best;
                        }
                    }
                    if plays.len() == 0 {
                        let s = play_result
                            .get(&c0)
                            .unwrap_or(&Score {
                                tot_score: 0,
                                num: 0,
                            })
                            .clone();
                        play_result.insert(c0, s + worst);
                    }
                    if worst >= best {
                        best = worst;
                        // println!("Move {} gives {}", c0, best.mean());
                    }
                }
                score = score + best;
            }
        }
        println!("Score: {:?}", score);
        if plays.len() == 0 {
            self.cache.insert(starting, score);
        }
        println!("Best plays:");
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
