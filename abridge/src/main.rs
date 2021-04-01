use ai::Convention;
use bridge_deck::{Card, Cards, Suit};
use display_as::{display, format_as, with_template, DisplayAs, HTML};
use futures::{FutureExt, StreamExt};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::{mpsc, RwLock};
use warp::reply::Reply;
use warp::{path, Filter};

mod ai;

#[tokio::main]
async fn main() {
    let players = Arc::new(RwLock::new(Players::default()));
    let game = Arc::new(RwLock::new(GameState::new()));
    // Turns our "state" into a new filter.
    let players = warp::any().map(move || players.clone());
    let game = warp::any().map(move || game.clone());

    let style_css = path!("abridge" / "style.css").map(|| {
        const STYLE: &'static str = include_str!("style.css");
        Ok(warp::http::Response::builder()
            .status(200)
            .header("content-length", STYLE.len())
            .header("content-type", "text/css")
            .body(STYLE)
            .unwrap())
    });
    let audio = path!("abridge" / "your-play.mp3").map(|| {
        const AUDIO: &'static [u8] = include_bytes!("your-play.mp3");
        Ok(warp::http::Response::builder()
            .status(200)
            .header("content-length", AUDIO.len())
            .header("content-type", "audio/mpeg")
            .body(AUDIO)
            .unwrap())
    });
    let index = path!("abridge").and(players.clone()).and_then(
        |players: Arc<RwLock<Players>>| async move {
            let p = players.read().await;
            let r: Result<warp::http::Response<warp::hyper::Body>, warp::Rejection> =
                Ok(display(HTML, &Index { players: &*p }).into_response());
            r
        },
    );
    let seat = path!("abridge" / Seat)
        .and(game.clone())
        .and(warp::filters::cookie::optional("name"))
        .and_then(
            |seat: Seat, game: Arc<RwLock<GameState>>, name: Option<String>| async move {
                let mut g = game.write().await;
                g.check_timeout();
                if let Some(name) = name {
                    g.names[seat] = name;
                }
                let r: Result<warp::http::Response<warp::hyper::Body>, warp::Rejection> =
                    Ok(display(HTML, &PlayerPage(Player { seat, game: &*g })).into_response());
                r
            },
        );

    let randomseat = path!("abridge" / "random").and(players.clone()).and_then(
        |players: Arc<RwLock<Players>>| async move {
            let p = players.read().await;
            let uri = if let Some(seat) = p.randomseat() {
                format!("/abridge/{}", seat.long_name())
            } else {
                format!("/abridge/")
            };
            let r: Result<warp::reply::WithHeader<warp::http::StatusCode>, warp::Rejection> =
                Ok(warp::reply::with_header(
                    warp::http::StatusCode::TEMPORARY_REDIRECT,
                    warp::http::header::LOCATION,
                    uri,
                ));
            r
        },
    );
    let sock = path!("abridge" / "ws" / String)
        .and(warp::ws())
        .and(game)
        .and(players)
        .map(|seat: String, ws: warp::ws::Ws, game, players| {
            ws.on_upgrade(move |socket| ws_connected(seat, socket, players, game))
        });

    println!("listening on 8087");
    warp::serve(
        style_css
            .or(audio)
            .or(index)
            .or(sock)
            .or(seat)
            .or(randomseat),
    )
    .run(([0, 0, 0, 0], 8087))
    .await;
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Bid {
    Pass,
    Double,
    Redouble,
    Suit(usize, bridge_deck::Suit),
    NT(usize),
}
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
enum Action {
    Redeal,
    SitAI,
    Bid(Bid),
    Play(Card),
    Name(String),
    ToggleCountForMe,
}
#[with_template(r#" onclick="send_message('"# serde_json::to_string(self).unwrap() r#"')""#)]
impl DisplayAs<HTML> for Action {}

impl PartialOrd for Bid {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Bid::*;
        if let Some(self_l) = self.level() {
            if let Some(other_l) = other.level() {
                if self == other {
                    return Some(std::cmp::Ordering::Equal);
                } else if self_l != other_l {
                    return self_l.partial_cmp(&other_l);
                } else if let NT(_) = self {
                    return Some(std::cmp::Ordering::Greater);
                } else if let NT(_) = other {
                    return Some(std::cmp::Ordering::Less);
                } else if let (Suit(_, me), Suit(_, them)) = (*self, *other) {
                    return me.partial_cmp(&them);
                }
            }
        }
        if self == other {
            return Some(std::cmp::Ordering::Equal);
        }
        if *self == Pass || other.is_contract() || *self == Redouble {
            return Some(std::cmp::Ordering::Greater);
        }
        if *other == Pass || self.is_contract() || *other == Redouble {
            return Some(std::cmp::Ordering::Less);
        }
        None
    }
}

#[test]
fn bid_comparisons() {
    assert!(Bid::NT(1) == Bid::NT(1));
    assert!(!(Bid::NT(1) > Bid::NT(1)));
    assert!(Bid::NT(1).is_contract());
    assert_eq!(Some(1), Bid::NT(1).level());
    assert_eq!(Some(3), Bid::NT(3).level());
}

static BIDS: &[Bid] = &[
    Bid::Suit(1, Suit::Clubs),
    Bid::Suit(1, Suit::Diamonds),
    Bid::Suit(1, Suit::Hearts),
    Bid::Suit(1, Suit::Spades),
    Bid::NT(1),
    Bid::Suit(2, Suit::Clubs),
    Bid::Suit(2, Suit::Diamonds),
    Bid::Suit(2, Suit::Hearts),
    Bid::Suit(2, Suit::Spades),
    Bid::NT(2),
    Bid::Suit(3, Suit::Clubs),
    Bid::Suit(3, Suit::Diamonds),
    Bid::Suit(3, Suit::Hearts),
    Bid::Suit(3, Suit::Spades),
    Bid::NT(3),
    Bid::Suit(4, Suit::Clubs),
    Bid::Suit(4, Suit::Diamonds),
    Bid::Suit(4, Suit::Hearts),
    Bid::Suit(4, Suit::Spades),
    Bid::NT(4),
    Bid::Suit(5, Suit::Clubs),
    Bid::Suit(5, Suit::Diamonds),
    Bid::Suit(5, Suit::Hearts),
    Bid::Suit(5, Suit::Spades),
    Bid::NT(5),
    Bid::Suit(6, Suit::Clubs),
    Bid::Suit(6, Suit::Diamonds),
    Bid::Suit(6, Suit::Hearts),
    Bid::Suit(6, Suit::Spades),
    Bid::NT(6),
    Bid::Suit(7, Suit::Clubs),
    Bid::Suit(7, Suit::Diamonds),
    Bid::Suit(7, Suit::Hearts),
    Bid::Suit(7, Suit::Spades),
    Bid::NT(7),
    Bid::Pass,
    Bid::Double,
    Bid::Redouble,
];

#[with_template("[%" "%]" "bid.html")]
impl<'a> DisplayAs<HTML> for Bid {}

impl Bid {
    fn level(self) -> Option<usize> {
        match self {
            Bid::Suit(l, _) => Some(l),
            Bid::NT(l) => Some(l),
            _ => None,
        }
    }

    fn is_contract(self) -> bool {
        self.level().is_some()
    }

    fn same_suit(self, other: Bid) -> bool {
        match (self, other) {
            (Bid::Suit(_, me), Bid::Suit(_, she)) => me == she,
            (Bid::NT(_), Bid::NT(_)) => true,
            _ => false,
        }
    }
}

pub struct GameState {
    names: Seated<String>,
    hands: Seated<Cards>,
    original_hands: Seated<Cards>,

    count_for_me: Seated<bool>,

    hand_done: bool,

    dealer: Seat,
    bids: Vec<Bid>,

    lead: Option<Seat>,
    played: Vec<Card>,
    ns_tricks: usize,
    ew_tricks: usize,

    last_action: std::time::Instant,

    conventions: Vec<Convention>,
}

impl GameState {
    fn new() -> GameState {
        let mut deck = Cards::ALL;
        let north = deck.pick(13).unwrap();
        let south = deck.pick(13).unwrap();
        let east = deck.pick(13).unwrap();
        let west = deck;
        GameState {
            names: [
                memorable_wordlist::camel_case(18),
                memorable_wordlist::camel_case(18),
                memorable_wordlist::camel_case(18),
                memorable_wordlist::camel_case(18),
            ]
            .into(),
            hands: [south, west, north, east].into(),
            original_hands: [south, west, north, east].into(),
            count_for_me: [false, false, false, false].into(),
            hand_done: false,
            dealer: Seat::South,
            bids: Vec::new(),
            lead: None,
            played: Vec::new(),
            ns_tricks: 0,
            ew_tricks: 0,
            last_action: std::time::Instant::now(),
            conventions: vec![ai::Convention::sheets()],
        }
    }
    fn starting(&self) -> Option<bridge_solver::Starting> {
        let lead = self.lead?;
        let playing = self.turn()?;
        let mut unknown = self.hands[Seat::South]
            + self.hands[Seat::North]
            + self.hands[Seat::East]
            + self.hands[Seat::West];
        let mut hands = [Cards::EMPTY, Cards::EMPTY, Cards::EMPTY, Cards::EMPTY];
        unknown -= self.hands[playing];
        hands[(playing as usize + 4 - lead as usize) % 4] = self.hands[playing];
        for (i,c) in self.played.iter().cloned().enumerate() {
            hands[i] += Cards::singleton(c);
            unknown -= Cards::singleton(c);
        }
        if let Some(dummy) = self.dummy() {
            hands[(dummy as usize + 4 - lead as usize) % 4] = self.hands[dummy];
            unknown -= self.hands[dummy];
            Some(bridge_solver::Starting { hands, unknown })
        } else {
            Some(bridge_solver::Starting { hands, unknown })
        }
    }
    fn bid_convention(&self, bids: &[Bid]) -> Option<impl DisplayAs<HTML>> {
        self.conventions[0].refine(bids).map(|c| c.description())
    }
    fn bid_convention2(&self, bid: Bid, oldbids: &[Bid]) -> Option<impl DisplayAs<HTML>> {
        let mut bids: Vec<_> = oldbids.iter().cloned().collect();
        bids.push(bid);
        self.bid_convention(&bids)
    }
    fn bid_understood2(&self, bid: Bid, otherbids: &[Bid]) -> bool {
        self.conventions[0].refine2(bid, otherbids).is_some()
    }
    fn check_timeout(&mut self) {
        let now = std::time::Instant::now();
        if now.duration_since(self.last_action) > std::time::Duration::from_secs(60 * 60) {
            *self = GameState::new();
        } else {
            self.last_action = now;
        }
    }
    fn redeal(&mut self) {
        let dealer = self.dealer.next();
        let oldnames = self.names.clone();
        *self = GameState::new();
        self.names = oldnames;
        self.dealer = dealer;
    }

    fn bidder(&self) -> Option<Seat> {
        let n = self.bids.len();
        if n > 3 && &self.bids[n - 3..] == &[Bid::Pass, Bid::Pass, Bid::Pass] {
            None
        } else {
            Some(self.dealer + n)
        }
    }

    fn highest_contract_bid(&self) -> Option<Bid> {
        for &x in self.bids.iter().rev() {
            if x.is_contract() {
                return Some(x);
            }
        }
        None
    }

    fn find_declarer(&self) -> Option<Seat> {
        let contract = self.highest_contract_bid()?;
        let mut winning_seat = self.dealer;
        for &x in self.bids.iter() {
            if x == contract {
                break;
            }
            winning_seat = winning_seat.next();
        }
        let mut declarer = self.dealer;
        let mut bids = self.bids.iter();
        if winning_seat != self.dealer && winning_seat != self.dealer.next().next() {
            bids.next();
            declarer = declarer.next();
        }
        for &x in bids.step_by(2) {
            if x.same_suit(contract) {
                return Some(declarer);
            }
            declarer = declarer.next().next();
        }
        None
    }

    fn dummy(&self) -> Option<Seat> {
        if self.bids.len() < 4
            || &self.bids[self.bids.len() - 3..] != &[Bid::Pass, Bid::Pass, Bid::Pass]
        {
            return None;
        }
        self.find_declarer().map(|s| s.next().next())
    }

    fn turn(&self) -> Option<Seat> {
        if let Some(seat) = self.bidder() {
            Some(seat)
        } else if let (Some(lead), Some(dummy)) = (self.lead, self.dummy()) {
            let declarer = dummy.next().next();
            let play_seat = match self.played.len() {
                0 | 4 => lead,
                n => lead + n,
            };
            if play_seat == dummy {
                Some(declarer)
            } else {
                Some(play_seat)
            }
        } else {
            None
        }
    }

    fn hand_playing(&self) -> Option<Seat> {
        if let Some(seat) = self.bidder() {
            Some(seat)
        } else {
            self.lead.map(|s| s + self.played.len())
        }
    }

    fn hand_visible_to(&self, hand: Seat, who: Seat) -> bool {
        let dummy = self.dummy();
        hand == who
            || (Some(hand) == dummy && Some(who.next().next()) == dummy)
            || (Some(hand.next().next()) == dummy && Some(who) == dummy)
            || (Some(hand) == dummy && self.hands.iter().map(|h| h.len()).sum::<usize>() < 52)
    }

    fn bid_is_legal(&self, seat: Seat, bid: Bid) -> bool {
        if Some(seat) != self.bidder() {
            return false;
        }
        let n = self.bids.len();
        match bid {
            Bid::Pass => true,
            Bid::Double => {
                (n >= 1 && self.bids[n - 1].is_contract())
                    || (n >= 3
                        && &self.bids[n - 2..] == &[Bid::Pass, Bid::Pass]
                        && self.bids[n - 3].is_contract())
            }
            Bid::Redouble => false,
            _ => {
                if let Some(hb) = self.highest_contract_bid() {
                    bid > hb
                } else {
                    true
                }
            }
        }
    }

    fn playable_cards(&self, seat: Seat, player: Seat) -> PlayableHand {
        let hand = self.hands[seat];
        let played_already = if self.count_for_me[player] {
            self.original_hands[seat] - hand
        } else {
            Cards::EMPTY
        };
        if !self.hand_visible_to(seat, player) {
            return PlayableHand {
                hand: Cards::EMPTY,
                playable: Cards::EMPTY,
                played_already,
            };
        }
        let playable = if let (Some(lead), Some(dummy)) = (self.lead, self.dummy()) {
            let declarer = dummy.next().next();
            if (player != seat && seat != dummy) || (seat == dummy && player != declarer) {
                return PlayableHand {
                    hand,
                    playable: Cards::EMPTY,
                    played_already,
                };
            }
            match self.played.len() {
                0 | 4 => {
                    if seat == lead {
                        hand
                    } else {
                        Cards::EMPTY
                    }
                }
                n => {
                    if seat == lead + n {
                        let suit = self.played[0].suit();
                        let mysuit = hand.in_suit(suit);
                        if mysuit.len() == 0 {
                            hand
                        } else {
                            mysuit
                        }
                    } else {
                        Cards::EMPTY
                    }
                }
            }
        } else {
            Cards::EMPTY
        };
        PlayableHand {
            hand,
            playable,
            played_already,
        }
    }

    fn could_be_played(&self) -> Cards {
        if let Some(lead) = self.lead {
            match self.played.len() {
                0 | 4 => self.hands[lead],
                n => {
                    let seat = lead + n;
                    let suit = self.played[0].suit();
                    let mysuit = self.hands[seat].in_suit(suit);
                    if mysuit.len() == 0 {
                        self.hands[seat]
                    } else {
                        mysuit
                    }
                }
            }
        } else {
            Cards::EMPTY
        }
    }

    fn trick_finish(&mut self) {
        if self.played.len() == 4 {
            let led = self.played[0].suit();
            let mut cards = Cards::EMPTY;
            for c in self.played.iter().cloned() {
                cards += Cards::singleton(c);
            }
            let mut winner = cards.in_suit(led).rev().next().unwrap();
            if let Some(Bid::Suit(_, trump)) = self.highest_contract_bid() {
                if let Some(trump_winner) = cards.in_suit(trump).rev().next() {
                    winner = trump_winner;
                }
            }
            let mut winning_seat = self.lead.unwrap();
            for c in self.played.iter().cloned() {
                if c == winner {
                    break;
                }
                winning_seat = winning_seat.next();
            }
            // Shift is how far the lead has shifted from last time.
            let shift = winning_seat as i32 - self.lead.unwrap() as i32;
            let shift = (shift + 4) % 4;
            self.lead = Some(winning_seat);
            if winning_seat == Seat::North || winning_seat == Seat::South {
                self.ns_tricks += 1;
            } else {
                self.ew_tricks += 1;
            }
            // Adjust the "played" cards so they will appear in front of the
            // proper player.
            for _ in 0..shift {
                let x = self.played.remove(0);
                self.played.push(x);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct PlayableHand {
    hand: Cards,
    playable: Cards,
    played_already: Cards,
}

#[with_template("[%" "%]" "hand.html")]
impl DisplayAs<HTML> for PlayableHand {}

impl PlayableHand {
    fn hcp(&self) -> usize {
        (self.hand + self.played_already).high_card_points()
    }
    fn shcp(&self) -> usize {
        (self.hand + self.played_already).protected_high_card_points()
            + (self.hand + self.played_already).short_card_points()
    }
    fn lhcp(&self) -> usize {
        (self.hand + self.played_already).high_card_points()
            + (self.hand + self.played_already).long_card_points()
    }
    fn nltc(&self) -> f64 {
        (self.hand + self.played_already).new_losing_trick_count()
    }
}

#[derive(Debug)]
enum PlayerConnection {
    Human(mpsc::UnboundedSender<Result<warp::ws::Message, warp::Error>>),
    Ai {
        bidder: Box<dyn ai::BidAI + Sync + Send>,
        player: Box<dyn ai::PlayAI + Sync + Send>,
    },
    None,
}
impl Default for PlayerConnection {
    fn default() -> Self {
        PlayerConnection::None
    }
}
impl PlayerConnection {
    fn is_empty(&self) -> bool {
        if self.is_ai() {
            true
        } else if let PlayerConnection::None = self {
            true
        } else {
            false
        }
    }
    fn is_ai(&self) -> bool {
        if let PlayerConnection::Ai { .. } = self {
            true
        } else {
            false
        }
    }
}

#[derive(Default, Debug)]
struct Players(Seated<PlayerConnection>);

impl Players {
    fn randomseat(&self) -> Option<Seat> {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        for _ in 0..30 {
            let seat: Seat = rng.gen::<usize>().into();
            if self.0[seat].is_empty() {
                return Some(seat);
            }
        }
        None
    }
}

async fn ws_connected(
    seat: String,
    ws: warp::ws::WebSocket,
    players: Arc<RwLock<Players>>,
    game: Arc<RwLock<GameState>>,
) {
    // Split the socket into a sender and receive of messages.
    let (user_ws_tx, mut user_ws_rx) = ws.split();

    // Use an unbounded channel to handle buffering and flushing of messages
    // to the websocket...
    let (tx, rx) = mpsc::unbounded_channel();
    let myseat;
    {
        // Save the sender in our list of connected users.
        let mut e = players.write().await;
        if let Ok(s) = std::str::FromStr::from_str(seat.as_str()) {
            myseat = s;
        } else {
            println!("bad seat");
            return;
        }
        e.0[myseat] = PlayerConnection::Human(tx);
    }
    tokio::task::spawn(rx.forward(user_ws_tx).map(|result| {
        if let Err(e) = result {
            eprintln!("websocket send error: {}", e);
        }
    }));

    // Every time the user sends a message, broadcast it to
    // all other users...
    while let Some(result) = user_ws_rx.next().await {
        let msg = match result {
            Ok(msg) => msg,
            Err(e) => {
                eprintln!("websocket error: {}", e);
                break;
            }
        };
        if msg.is_close() {
            println!("got a close");
            let mut e = players.write().await;
            e.0[myseat] = PlayerConnection::None;
            return;
        }
        match msg.to_str().map(|s| serde_json::from_str::<Action>(s)) {
            Err(e) => {
                eprintln!("Bad UTF8: {:?} {:?}", e, msg);
            }
            Ok(Err(e)) => {
                eprintln!("Bad JSON: {:?}", e);
            }
            Ok(Ok(action)) => {
                println!("Doing {:?}", action);
                let mut p = players.write().await;
                let mut g = game.write().await;
                match action {
                    Action::ToggleCountForMe => {
                        g.count_for_me[myseat] = !g.count_for_me[myseat];
                    }
                    Action::Redeal => {
                        g.redeal();
                    }
                    Action::SitAI => {
                        for s in [Seat::North, Seat::East, Seat::South, Seat::West]
                            .iter()
                            .cloned()
                        {
                            if p.0[s].is_empty() {
                                g.names[s] = "Robot".into();
                                p.0[s] = PlayerConnection::Ai {
                                    bidder: Box::new(ai::ConventionalBid(ai::Convention::sheets())),
                                    player: Box::new(ai::RandomPlay),
                                };
                            }
                        }
                    }
                    Action::Bid(b) => {
                        g.bids.push(b);
                        if g.bids.len() > 3
                            && &g.bids[g.bids.len() - 3..] == &[Bid::Pass, Bid::Pass, Bid::Pass]
                        {
                            println!("Bidding is complete");
                            if let Some(declarer) = g.find_declarer() {
                                g.lead = Some(declarer.next());
                            } else {
                                g.hand_done = true;
                            }
                        }
                    }
                    Action::Play(card) => {
                        let seat = myseat;
                        // FIXME check for playable
                        if g.played.len() == 4 {
                            g.played.clear();
                        }
                        g.played.push(card);
                        // Be lazy and don't even bother checking whether we
                        // were playing for dummy, just remove from our hand AND
                        // partner's hand.
                        g.hands[seat] = g.hands[seat] - Cards::singleton(card);
                        g.hands[seat + 2] = g.hands[seat + 2] - Cards::singleton(card);
                        g.trick_finish();
                        if g.ns_tricks + g.ew_tricks == 13 {
                            g.hand_done = true;
                        }
                    }
                    Action::Name(name) => {
                        g.names[myseat] = name;
                    }
                }
                g.check_timeout();
                // Now we need to run any AI that is relevant.
                while let Some(turn) = g.turn() {
                    if p.0[turn].is_ai() {
                        // Send out an update before we even start thinking.
                        if let PlayerConnection::Human(s) = &p.0[Seat::North] {
                            let pp = Player {
                                seat: Seat::North,
                                game: &*g,
                            };
                            let msg = format_as!(HTML, "" pp);
                            s.send(Ok(warp::ws::Message::text(msg.into_string()))).ok();
                        }
                        if let PlayerConnection::Human(s) = &p.0[Seat::South] {
                            let pp = Player {
                                seat: Seat::South,
                                game: &*g,
                            };
                            let msg = format_as!(HTML, "" pp);
                            s.send(Ok(warp::ws::Message::text(msg.into_string()))).ok();
                        }
                        if let PlayerConnection::Human(s) = &p.0[Seat::East] {
                            let pp = Player {
                                seat: Seat::East,
                                game: &*g,
                            };
                            let msg = format_as!(HTML, "" pp);
                            s.send(Ok(warp::ws::Message::text(msg.into_string()))).ok();
                        }
                        if let PlayerConnection::Human(s) = &p.0[Seat::West] {
                            let pp = Player {
                                seat: Seat::West,
                                game: &*g,
                            };
                            let msg = format_as!(HTML, "" pp);
                            s.send(Ok(warp::ws::Message::text(msg.into_string()))).ok();
                        }
                    }
                    if let PlayerConnection::Ai { bidder, player } = &mut p.0[turn] {
                        // It's an AI's move!
                        tokio::time::delay_for(std::time::Duration::from_secs(2)).await;
                        if g.bidder().is_some() {
                            let bid = bidder.bid(&g.bids, g.hands[turn]);
                            g.bids.push(bid);
                            if g.bids.len() > 3
                                && &g.bids[g.bids.len() - 3..] == &[Bid::Pass, Bid::Pass, Bid::Pass]
                            {
                                println!("Bidding is complete");
                                if let Some(declarer) = g.find_declarer() {
                                    g.lead = Some(declarer.next());
                                } else {
                                    g.hand_done = true;
                                }
                            }
                        } else {
                            let seat = turn;
                            let card = player.play(&g);
                            if g.played.len() == 4 {
                                g.played.clear();
                                // Give players some time to see the finished trick...
                                tokio::time::delay_for(std::time::Duration::from_secs(10)).await;
                            }
                            g.played.push(card);
                            // Be lazy and don't even bother checking whether we
                            // were playing for dummy, just remove from our hand AND
                            // partner's hand.
                            g.hands[seat] = g.hands[seat] - Cards::singleton(card);
                            g.hands[seat + 2] = g.hands[seat + 2] - Cards::singleton(card);
                            g.trick_finish();
                            if g.ns_tricks + g.ew_tricks == 13 {
                                g.hand_done = true;
                            }
                        }
                    } else {
                        break;
                    }
                }
                if let PlayerConnection::Human(s) = &p.0[Seat::North] {
                    let pp = Player {
                        seat: Seat::North,
                        game: &*g,
                    };
                    let msg = format_as!(HTML, "" pp);
                    s.send(Ok(warp::ws::Message::text(msg.into_string()))).ok();
                }
                if let PlayerConnection::Human(s) = &p.0[Seat::South] {
                    let pp = Player {
                        seat: Seat::South,
                        game: &*g,
                    };
                    let msg = format_as!(HTML, "" pp);
                    s.send(Ok(warp::ws::Message::text(msg.into_string()))).ok();
                }
                if let PlayerConnection::Human(s) = &p.0[Seat::East] {
                    let pp = Player {
                        seat: Seat::East,
                        game: &*g,
                    };
                    let msg = format_as!(HTML, "" pp);
                    s.send(Ok(warp::ws::Message::text(msg.into_string()))).ok();
                }
                if let PlayerConnection::Human(s) = &p.0[Seat::West] {
                    let pp = Player {
                        seat: Seat::West,
                        game: &*g,
                    };
                    let msg = format_as!(HTML, "" pp);
                    s.send(Ok(warp::ws::Message::text(msg.into_string()))).ok();
                }
            }
        }
    }
}
struct Index<'a> {
    players: &'a Players,
}
#[with_template("[%" "%]" "index.html")]
impl<'a> DisplayAs<HTML> for Index<'a> {}

struct Player<'a> {
    seat: Seat,
    game: &'a GameState,
}
#[with_template("[%" "%]" "player.html")]
impl<'a> DisplayAs<HTML> for Player<'a> {}

struct PlayerPage<'a>(Player<'a>);
#[with_template("[%" "%]" "player-page.html")]
impl<'a> DisplayAs<HTML> for PlayerPage<'a> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Seat {
    South = 0,
    West = 1,
    North = 2,
    East = 3,
}
impl From<usize> for Seat {
    fn from(s: usize) -> Self {
        match s % 4 {
            x if x == Seat::South as usize => Seat::South,
            x if x == Seat::West as usize => Seat::West,
            x if x == Seat::North as usize => Seat::North,
            x if x == Seat::East as usize => Seat::East,
            _ => unreachable!(),
        }
    }
}
impl std::ops::Add<usize> for Seat {
    type Output = Seat;

    fn add(self, rhs: usize) -> Self::Output {
        (self as usize + rhs).into()
    }
}
impl Seat {
    fn next(self) -> Self {
        self + 1
    }
    fn name(self) -> &'static str {
        const NAMES: Seated<&'static str> = Seated::new(["S", "W", "N", "E"]);
        &*NAMES[self]
    }
    fn long_name(self) -> &'static str {
        const NAMES: Seated<&'static str> = Seated::new(["south", "west", "north", "east"]);
        &*NAMES[self]
    }
}

impl std::str::FromStr for Seat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "north" => Ok(Seat::North),
            "south" => Ok(Seat::South),
            "west" => Ok(Seat::West),
            "east" => Ok(Seat::East),
            _ => Err(format!("invalid seat: {}", s)),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Seated<T> {
    internal: [T; 4],
}

impl<T> From<[T; 4]> for Seated<T> {
    fn from(internal: [T; 4]) -> Self {
        Seated { internal }
    }
}
impl<T> Seated<T> {
    const fn new(internal: [T; 4]) -> Self {
        Seated { internal }
    }
    fn iter(&self) -> std::slice::Iter<'_, T> {
        self.internal.iter()
    }
}

impl<T> std::ops::Index<Seat> for Seated<T> {
    type Output = T;

    fn index(&self, index: Seat) -> &T {
        &self.internal[index as usize]
    }
}

impl<T> std::ops::IndexMut<Seat> for Seated<T> {
    fn index_mut(&mut self, index: Seat) -> &mut T {
        &mut self.internal[index as usize]
    }
}
