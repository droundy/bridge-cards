//! A benchmark of how advanced the bidders are: the bidding question from each
//! of Frank Stewart's daily bridge columns, which each bidding system either
//! answers as the column does or doesn't.  Where one doesn't, the puzzle is a
//! source of inspiration for improving that system's conventions.
//!
//! The columns are listed at
//! <https://baronbarclay.com/pages/frank-stewart-daily-bridge-column>, each as
//! a one page PDF.  Not every column asks a bidding question, so only those
//! that do have a puzzle here.  Every system in [`BiddingSystem::ALL`] takes
//! the quiz, since the column's answers assume Standard American methods and
//! so SAYC and 2/1 are the fairest judges.  To add a puzzle, copy the "You
//! hold" hand, the auction and the answer into [`PUZZLES`], run the test and
//! paste in the `solved_by` list that it reports.  The test fails whenever a
//! `solved_by` is wrong, so a change that teaches a bidder a new answer must
//! also add its system to the list, and the failure message says how.

use crate::ai::{BidAI, BiddingSystem, ConventionalBid};
use crate::{bid_follows, Bid};
use bridge_deck::{Cards, Suit};
use std::str::FromStr;

/// One column's bidding question.
struct Puzzle {
    /// The column, a one page PDF.
    column: &'static str,
    /// The hand we hold, as in [`Cards::from_str`].
    hand: &'static str,
    /// The auction so far, ending with the bid before ours, as in
    /// [`bids`].
    auction: &'static str,
    /// The bids the column accepts as the answer, most preferred first.
    answers: &'static [&'static str],
    /// The systems, in the order of [`BiddingSystem::ALL`], whose bidder makes
    /// one of the `answers`.  When the test reports a wrong list, paste in the
    /// list it gives.
    solved_by: &'static [BiddingSystem],
}

const PUZZLES: &[Puzzle] = &[
    Puzzle {
        // Squeeze or finesse?  A balanced 11 with club strength bids 2NT.
        column: "https://baronbarclay.com/cdn/shop/files/20261102pzdbc-a.pdf",
        hand: "S J3 H KJ42 D 752 C AQ42",
        auction: "1D P 1H P 1S P",
        answers: &["2N"],
        solved_by: &[BiddingSystem::Sayc, BiddingSystem::TwoOverOne],
    },
    Puzzle {
        // Cy's two-way guess: borderline, but with no points in partner's long suits the conservative 2H is right.
        column: "https://baronbarclay.com/cdn/shop/files/20261103pzdbc-a.pdf",
        hand: "S AQJ5 H 74 D 92 C Q9542",
        auction: "1H P 1S P 2D P",
        answers: &["2H"],
        solved_by: &[],
    },
    Puzzle {
        // Forcing defense: partner's jump rebid invites game with six spades, and our singleton king is adequate support.
        column: "https://baronbarclay.com/cdn/shop/files/20261104pzdbc-a.pdf",
        hand: "S K H KJT98 D AKQ5 C 752",
        auction: "1H P 1S P 2D P 3S P",
        answers: &["4S"],
        solved_by: &[],
    },
    Puzzle {
        // Matchpoint woes: a balanced minimum rebids 1NT rather than reversing.
        column: "https://baronbarclay.com/cdn/shop/files/20261105pzdbc-a.pdf",
        hand: "S A H KJ52 D 76432 C AQ3",
        auction: "1D P 1S P",
        answers: &["1N"],
        solved_by: &[BiddingSystem::Sheets],
    },
    Puzzle {
        // Assess the blame: 3D shows good three-card support for partner's first suit.
        column: "https://baronbarclay.com/cdn/shop/files/20261106pzdbc-a.pdf",
        hand: "S KT952 H AK52 D A95 C 3",
        auction: "1S P 2D P 2H P 3C P",
        answers: &["3D"],
        solved_by: &[],
    },
    Puzzle {
        // Simple Saturday: raise to 3H, since 3C would commit us to game.
        column: "https://baronbarclay.com/cdn/shop/files/20261107pzdbc-a.pdf",
        hand: "S T4 H K3 D AJ952 C K763",
        auction: "1H P 2D P 2H P",
        answers: &["3H"],
        solved_by: &[],
    },
    Puzzle {
        // Louie vs. Murphy: partner's 3C is a game try, so bid 4H, or 3D to ask whether partner likes our diamonds.
        column: "https://baronbarclay.com/cdn/shop/files/20261109pzdbc-a.pdf",
        hand: "S T864 H KT42 D KT42 C 6",
        auction: "1H P 2H P 3C P",
        answers: &["4H", "3D"],
        solved_by: &[BiddingSystem::Sayc, BiddingSystem::TwoOverOne],
    },
    Puzzle {
        // Resisting temptation: partner's new suit after 2NT shows a singleton opposite our king, so sign off in 4S.
        column: "https://baronbarclay.com/cdn/shop/files/20261110pzdbc-a.pdf",
        hand: "S AJ52 H K73 D J6 C A962",
        auction: "1S P 2N P 3H P",
        answers: &["4S"],
        solved_by: &[],
    },
    Puzzle {
        // Inventing a reverse: partner bid a third time, so he has game interest and we bid 4S.
        column: "https://baronbarclay.com/cdn/shop/files/20261111pzdbc-a.pdf",
        hand: "S Q7543 H K10 D 7532 C K4",
        auction: "1H P 1S P 2C P 2H P 2S P",
        answers: &["4S"],
        solved_by: &[BiddingSystem::Sayc, BiddingSystem::TwoOverOne],
    },
    Puzzle {
        // Eliminating the gain: partner's 3D rejects notrump, so the balanced hand passes.
        column: "https://baronbarclay.com/cdn/shop/files/20261112pzdbc-a.pdf",
        hand: "S K2 H AJ94 D J42 C Q753",
        auction: "1S P 2C P 2D P 2N P 3D P",
        answers: &["P"],
        solved_by: &[BiddingSystem::Sheets],
    },
    Puzzle {
        // Louie's insurance: a balanced minimum has no reason to bid a third time, so pass 1NT.
        column: "https://baronbarclay.com/cdn/shop/files/20261113pzdbc-a.pdf",
        hand: "S AQ73 H K64 D K8 C J1098",
        auction: "1C P 1H P 1S P 1N P",
        answers: &["P"],
        solved_by: &[
            BiddingSystem::Sheets,
            BiddingSystem::Sayc,
            BiddingSystem::TwoOverOne,
        ],
    },
    Puzzle {
        // Simple Saturday: a defensive 14 count is a matter of style, and the column passes.
        column: "https://baronbarclay.com/cdn/shop/files/20261114pzdbc-a.pdf",
        hand: "S KJ2 H A93 D A94 C Q653",
        auction: "1C",
        answers: &["P", "X"],
        solved_by: &[
            BiddingSystem::Sheets,
            BiddingSystem::Sayc,
            BiddingSystem::TwoOverOne,
        ],
    },
    Puzzle {
        // First things first: partner's 3H shows six hearts and extras, so raise to game.
        column: "https://baronbarclay.com/cdn/shop/files/20261116pzdbc-a.pdf",
        hand: "S AT54 H AQ D J73 C 6532",
        auction: "1H P 1S P 2D P 2N P 3H P",
        answers: &["4H"],
        solved_by: &[BiddingSystem::Sayc, BiddingSystem::TwoOverOne],
    },
    Puzzle {
        // Count your tricks: a balanced 12 with two and a half defensive tricks opens 1D.
        column: "https://baronbarclay.com/cdn/shop/files/20261117pzdbc-a.pdf",
        hand: "S AQ43 H QT9 D AT43 C 62",
        auction: "",
        answers: &["1D"],
        solved_by: &[],
    },
    Puzzle {
        // Seeing things: game is nearly certain but the contract is not placed, so bid the fourth-suit 3C.
        column: "https://baronbarclay.com/cdn/shop/files/20261118pzdbc-a.pdf",
        hand: "S AK H QT7 D AQ962 C 964",
        auction: "1S P 2D P 2H P",
        answers: &["3C"],
        solved_by: &[],
    },
    Puzzle {
        // Overberried: with a weak hand and awkward shape, a false preference to 2H is right.
        column: "https://baronbarclay.com/cdn/shop/files/20261119pzdbc-a.pdf",
        hand: "S KQ973 H 96 D AJ5 C 743",
        auction: "1H P 1S P 2D P",
        answers: &["2H"],
        solved_by: &[],
    },
    Puzzle {
        // Test your counting: with a good hand and five hearts, rebid 2H rather than reversing into 2S.
        column: "https://baronbarclay.com/cdn/shop/files/20261120pzdbc-a.pdf",
        hand: "S K654 H AQJ84 D QJ C J6",
        auction: "1H P 2D P",
        answers: &["2H"],
        solved_by: &[BiddingSystem::Sayc, BiddingSystem::TwoOverOne],
    },
    Puzzle {
        // Simple Saturday: no call is ideal, but a reluctant raise to 3C is the choice.
        column: "https://baronbarclay.com/cdn/shop/files/20261121pzdbc-a.pdf",
        hand: "S QJT95 H 743 D A5 C QJ6",
        auction: "1D P 1S P 2C P",
        answers: &["3C"],
        solved_by: &[],
    },
    Puzzle {
        // Interactive bridge: after 2NT, partner's 3D shows a singleton, and we cue-bid 3S to encourage slam.
        column: "https://baronbarclay.com/cdn/shop/files/20261123pzdbc-a.pdf",
        hand: "S AQ H J7432 D AKJT C 65",
        auction: "1H P 2N P 3D P",
        answers: &["3S"],
        solved_by: &[],
    },
    Puzzle {
        // Negative double: four spades is enough to show, and 1S would promise five.
        column: "https://baronbarclay.com/cdn/shop/files/20261124pzdbc-a.pdf",
        hand: "S KT74 H J876 D K532 C J",
        auction: "1D 1H",
        answers: &["X"],
        solved_by: &[],
    },
    Puzzle {
        // Raise partner's response: we doubled, he bid 1H, and a simple raise keeps us from going wild.
        column: "https://baronbarclay.com/cdn/shop/files/20261125pzdbc-a.pdf",
        hand: "S AKQ6 H Q743 D A2 C A92",
        auction: "1D X P 1H P",
        answers: &["2H"],
        solved_by: &[],
    },
    Puzzle {
        // Jump shift: strong enough for slam, so bid 2S and enlist partner's cooperation.
        column: "https://baronbarclay.com/cdn/shop/files/20261126pzdbc-a.pdf",
        hand: "S KQJ74 H K7 D J4 C AKJ3",
        auction: "1H P",
        answers: &["2S"],
        solved_by: &[],
    },
    Puzzle {
        // Three clubs, forcing: a sound opener makes game certain; 2NT only if partner opens light.
        column: "https://baronbarclay.com/cdn/shop/files/20261127pzdbc-a.pdf",
        hand: "S A5 H 74 D KT963 C AJ83",
        auction: "1H P 2D P 2H P",
        answers: &["3C", "2N"],
        solved_by: &[],
    },
    Puzzle {
        // Rebid 2H, not a jump shift: extra strength but not enough to force game opposite a six-point response.
        column: "https://baronbarclay.com/cdn/shop/files/20261128pzdbc-a.pdf",
        hand: "S AK532 H QT87 D AK C 85",
        auction: "1S P 1N P",
        answers: &["2H"],
        solved_by: &[BiddingSystem::Sayc, BiddingSystem::TwoOverOne],
    },
    Puzzle {
        // Support partner's spades with a minimum: a free bid may be the only chance to raise.
        column: "https://baronbarclay.com/cdn/shop/files/20261130pzdbc-a.pdf",
        hand: "S K72 H J92 D 754 C QJT6",
        auction: "1S 2H",
        answers: &["2S"],
        solved_by: &[
            BiddingSystem::Sheets,
            BiddingSystem::Sayc,
            BiddingSystem::TwoOverOne,
        ],
    },
];

/// The bids named by `s`, which are separated by spaces and written as in
/// auctions elsewhere: `P`, `X`, `XX`, `1C`, `3N`.
fn bids(s: &str) -> Vec<Bid> {
    s.split_whitespace()
        .map(|b| match b {
            "P" => Bid::Pass,
            "X" => Bid::Double,
            "XX" => Bid::Redouble,
            _ => {
                let level = b[..1].parse().unwrap();
                match &b[1..] {
                    "N" => Bid::NT(level),
                    "C" => Bid::Suit(level, Suit::Clubs),
                    "D" => Bid::Suit(level, Suit::Diamonds),
                    "H" => Bid::Suit(level, Suit::Hearts),
                    "S" => Bid::Suit(level, Suit::Spades),
                    _ => panic!("unknown bid {:?}", b),
                }
            }
        })
        .collect()
}

/// Whether `system`'s bidder makes one of the puzzle's answers, and the bid it
/// makes.
fn bid_and_solves(system: BiddingSystem, puzzle: &Puzzle) -> (Bid, bool) {
    let hand = Cards::from_str(puzzle.hand).unwrap();
    let ours = ConventionalBid(system.convention()).bid(&bids(puzzle.auction), hand);
    let solves = puzzle.answers.iter().any(|answer| bids(answer) == [ours]);
    (ours, solves)
}

/// The Rust source of a `solved_by` list holding `systems`.
fn solved_by_source(systems: &[BiddingSystem]) -> String {
    let names: Vec<String> = systems
        .iter()
        .map(|s| format!("BiddingSystem::{:?}", s))
        .collect();
    format!("&[{}]", names.join(", "))
}

/// Quizzes every bidding system on every puzzle, and fails if a puzzle's
/// `solved_by` is not the list of systems that solve it.
#[test]
fn daily_column_puzzles() {
    for puzzle in PUZZLES {
        let hand = Cards::from_str(puzzle.hand).unwrap();
        assert_eq!(13, hand.len(), "not 13 cards: {}", puzzle.column);
        let auction = bids(puzzle.auction);
        for (i, &bid) in auction.iter().enumerate() {
            assert!(
                bid_follows(&auction[..i], bid),
                "illegal auction {}: {}",
                puzzle.auction,
                puzzle.column
            );
        }
        for answer in puzzle.answers {
            assert!(
                bids(answer).iter().all(|&bid| bid_follows(&auction, bid)),
                "answer {} is not legal after {}: {}",
                answer,
                puzzle.auction,
                puzzle.column
            );
        }
    }
    for system in BiddingSystem::ALL {
        let passed = PUZZLES
            .iter()
            .filter(|puzzle| bid_and_solves(system, puzzle).1)
            .count();
        println!("{}: {} of {} columns", system.name(), passed, PUZZLES.len());
    }
    let mut wrong = Vec::new();
    for puzzle in PUZZLES {
        let results: Vec<(BiddingSystem, Bid, bool)> = BiddingSystem::ALL
            .iter()
            .map(|&system| {
                let (ours, solves) = bid_and_solves(system, puzzle);
                (system, ours, solves)
            })
            .collect();
        let solvers: Vec<BiddingSystem> = results.iter().filter(|r| r.2).map(|r| r.0).collect();
        if solvers != puzzle.solved_by {
            let bid_lines: Vec<String> = results
                .iter()
                .map(|(system, ours, _)| format!("{} bids {:?}", system.name(), ours))
                .collect();
            wrong.push(format!(
                "{}\n    {} after {:?}; the column wants {}\n    {}\n    solved_by is {} but should be:\n        solved_by: {},",
                puzzle.column,
                puzzle.hand,
                puzzle.auction,
                puzzle.answers.join(" or "),
                bid_lines.join(", "),
                solved_by_source(puzzle.solved_by),
                solved_by_source(&solvers),
            ));
        }
    }
    assert!(wrong.is_empty(), "wrong solved_by:\n{}", wrong.join("\n"));
}
