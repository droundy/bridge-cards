pub use bridge_deck::{Card, CardMap, Cards, Suit};
use rand::rngs::SmallRng;
use rand::SeedableRng;

pub fn normalize_create_map(all: Cards) -> CardMap {
    let mut map = CardMap::new();
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
    pub fn random_hands(&self, rng: &mut SmallRng) -> [Cards; 4] {
        let n = self.tricks_remaining();
        let mut unknown = self.unknown;
        let mut hands = self.hands;
        // for seat in 0..4 {
        //     println!("  hand {}: {} ({})", seat, self.hands[seat], self.hands[seat].len());
        // }
        // println!("  extra: {} ({})", self.unknown, self.unknown.len());
        for seat in 0..4 {
            if hands[seat].len() < n {
                let extra = unknown
                    .pick_rng(rng, n - self.hands[seat].len())
                    .expect("bad number of cardss");
                hands[seat] += extra;
            }
        }
        hands
    }
    pub fn after(mut self, plays: [Card; 4], trump: Option<Suit>) -> TrickTaken {
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
        let unknown_in_suit = self.unknown.in_suit(plays[0].suit());
        if unknown_in_suit.len() > 0 {
            for i in 1..4 {
                if plays[i].suit() != plays[0].suit() {
                    // i showed void.  if only one player has unknown cards, they must have
                    // all the cards in this suit.
                    let lengths: Vec<usize> = self.hands.iter().map(|h| h.len()).collect();
                    let max_length = lengths.iter().cloned().max().unwrap();
                    if lengths.iter().filter(|&&l| l != max_length).count() == 1 {
                        for j in 0..4 {
                            if self.hands[j].len() < max_length {
                                self.hands[j] += unknown_in_suit;
                                self.unknown -= unknown_in_suit;
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
    /// Imagine one random deal
    Normal,
    /// Imagine N random deals
    Statistical(usize),
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
        Naive::with_thoroughness(trump, Thoroughness::Normal)
    }
    pub fn statistical(trump: Option<Suit>, samples: usize) -> Self {
        Naive::with_thoroughness(trump, Thoroughness::Statistical(samples))
    }
    pub fn high_low(trump: Option<Suit>) -> Self {
        Naive::with_thoroughness(trump, Thoroughness::HighLow)
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
        for _ in 0..self.num_statistics() {
            let hands = starting.random_hands(&mut self.rng);
            let mut best = Score::MIN;
            for c0 in self.thorough_plays(hands[0]) {
                let mut worst = Score::MAX;
                for c1 in self.thorough_plays(hands[1].following_suit(c0.suit())) {
                    let mut best = Score::MIN;
                    for c2 in self.thorough_plays(hands[2].following_suit(c0.suit())) {
                        let mut worst = Score::MAX;
                        for c3 in self.thorough_plays(hands[3].following_suit(c0.suit())) {
                            let trick_taken = starting.after([c0, c1, c2, c3], self.trump);
                            let sc = self.score(trick_taken.starting());
                            let mysc = sc + trick_taken;
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
        self.cache.insert(starting, score);
        score
    }

    pub fn score_after(&mut self, mut starting: Starting, plays: &[Card]) -> (Score, Card) {
        // First update the hands to ensure that the plays correspond to cards
        // in the various hands.
        for (i, card) in plays.iter().cloned().enumerate() {
            starting.hands[i] += Cards::singleton(card);
            starting.unknown -= Cards::singleton(card);
        }
        let mut score = Score {
            tot_score: 0,
            num: 0,
        };
        let mut play_result: std::collections::HashMap<Card, Score> = std::collections::HashMap::new();
        let mut card_to_play = starting.hands[plays.len() % 4]
            .clone()
            .pick(1)
            .expect("There is no card to play?!")
            .next()
            .unwrap();
        for _ in 0..std::cmp::max(16, self.num_statistics()) {
            let hands = starting.random_hands(&mut self.rng);
            let mut possible_plays = hands.clone();
            for (i, c) in plays.iter().cloned().enumerate() {
                possible_plays[i] = Cards::singleton(c);
            }
            // for i in 0..4 {
            //     println!("possible_plays are {}", possible_plays[i]);
            // }
            let mut best = Score::MIN;
            for c0 in possible_plays[0] {
                // println!("considering playing first {}", c0);
                let mut worst = Score::MAX;
                for c1 in possible_plays[1].following_suit(c0.suit()) {
                    let mut best = Score::MIN;
                    for c2 in possible_plays[2].following_suit(c0.suit()) {
                        let mut worst = Score::MAX;
                        for c3 in possible_plays[3].following_suit(c0.suit()) {
                            // println!("considering playing fourth {}", c3);
                            let trick_taken = starting.after([c0, c1, c2, c3], self.trump);
                            let sc = self.score(trick_taken.starting());
                            // println!("score is {:?}", sc);
                            // println!("trick taken is {:?}", trick_taken);
                            let mysc = sc + trick_taken;
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
                                if plays.len() == 3 {
                                    let s = play_result.get(&c3).unwrap_or(&Score { tot_score: 0, num: 0}).clone();
                                    play_result.insert(c3, s + worst);
                                    card_to_play = c3;
                                }
                            // println!("worst = {}", worst.mean());
                            } else {
                                // println!("worst = {} but sc = {}", worst.mean(), sc.mean());
                            }
                        }
                        if worst > best {
                            best = worst;
                            if plays.len() == 2 {
                                let s = play_result.get(&c2).unwrap_or(&Score { tot_score: 0, num: 0}).clone();
                                play_result.insert(c2, s + best);
                                card_to_play = c2;
                            }
                        }
                    }
                    if worst > best {
                        worst = best;
                        if plays.len() == 1 {
                            let s = play_result.get(&c1).unwrap_or(&Score { tot_score: 0, num: 0}).clone();
                            play_result.insert(c1, s + best);
                            card_to_play = c1;
                        }
                    }
                }
                if worst > best {
                    best = worst;
                    if plays.len() == 0 {
                        let s = play_result.get(&c0).unwrap_or(&Score { tot_score: 0, num: 0}).clone();
                        play_result.insert(c0, s + best);
                        card_to_play = c0;
                    }
                    // println!("Move {} gives {}", c0, best.mean());
                }
            }
            score = score + best;
        }
        if plays.len() == 0 {
            self.cache.insert(starting, score);
        }
        let bestscore = play_result.values().cloned().max().unwrap();
        for (c,s) in play_result.iter() {
            if *s == bestscore {
                return (score, *c);
            }
        }
        (score, card_to_play)
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
