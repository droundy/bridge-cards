use bridge_deck::{Cards, Suit};
use display_as::{display, format_as, with_template, DisplayAs, HTML};
use futures::{FutureExt, StreamExt};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::{mpsc, RwLock};
use warp::reply::Reply;
use warp::{path, Filter};

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
    let index = path!("abridge").and(players.clone()).and_then(
        |players: Arc<RwLock<Players>>| async move {
            let p = players.read().await;
            let r: Result<warp::http::Response<warp::hyper::Body>, warp::Rejection> =
                Ok(display(HTML, &Index { players: &*p }).into_response());
            r
        },
    );
    let seat = path!("abridge" / Seat)
        .and(players.clone())
        .and(game.clone())
        .and_then(
            |seat: Seat, players: Arc<RwLock<Players>>, game: Arc<RwLock<GameState>>| async move {
                let p = players.read().await;
                let g = game.read().await;
                let r: Result<warp::http::Response<warp::hyper::Body>, warp::Rejection> =
                    Ok(display(
                        HTML,
                        &PlayerPage(Player {
                            seat,
                            players: &*p,
                            game: &*g,
                        }),
                    )
                    .into_response());
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

    warp::serve(style_css.or(index).or(sock).or(seat))
        .run(([0, 0, 0, 0], 8087))
        .await;
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
enum Bid {
    Pass,
    Double,
    Redouble,
    Suit(usize, bridge_deck::Suit),
    NT(usize),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
enum Action {
    Redeal,
    Bid(Bid),
}
#[with_template(r#" onclick="send_message('"# serde_json::to_string(self).unwrap() r#"')""#)]
impl DisplayAs<HTML> for Action {}

impl PartialOrd for Bid {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Bid::*;
        if let Some(self_l) = self.level() {
            if let Some(other_l) = other.level() {
                if self_l != other_l {
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

static BIDS: &[Bid] = &[
    Bid::Suit(1, Suit::Clubs),
    Bid::Suit(1, Suit::Diamonds),
    Bid::Suit(1, Suit::Hearts),
    Bid::Suit(1, Suit::Spades),
    Bid::Suit(2, Suit::Clubs),
    Bid::Suit(2, Suit::Diamonds),
    Bid::Suit(2, Suit::Hearts),
    Bid::Suit(2, Suit::Spades),
    Bid::Suit(3, Suit::Clubs),
    Bid::Suit(3, Suit::Diamonds),
    Bid::Suit(3, Suit::Hearts),
    Bid::Suit(3, Suit::Spades),
    Bid::Suit(4, Suit::Clubs),
    Bid::Suit(4, Suit::Diamonds),
    Bid::Suit(4, Suit::Hearts),
    Bid::Suit(4, Suit::Spades),
    Bid::Suit(5, Suit::Clubs),
    Bid::Suit(5, Suit::Diamonds),
    Bid::Suit(5, Suit::Hearts),
    Bid::Suit(5, Suit::Spades),
    Bid::Suit(6, Suit::Clubs),
    Bid::Suit(6, Suit::Diamonds),
    Bid::Suit(6, Suit::Hearts),
    Bid::Suit(6, Suit::Spades),
    Bid::Suit(7, Suit::Clubs),
    Bid::Suit(7, Suit::Diamonds),
    Bid::Suit(7, Suit::Hearts),
    Bid::Suit(7, Suit::Spades),
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

struct GameState {
    north: Cards,
    south: Cards,
    east: Cards,
    west: Cards,

    dealer: Seat,
    bids: Vec<Bid>,

    lead: Option<Seat>,
}

impl GameState {
    fn new() -> GameState {
        let mut deck = Cards::ALL;
        let north = deck.pick(13).unwrap();
        let south = deck.pick(13).unwrap();
        let east = deck.pick(13).unwrap();
        let west = deck;
        GameState {
            west,
            south,
            east,
            north,
            dealer: Seat::South,
            bids: Vec::new(),
            lead: None,
        }
    }
    fn redeal(&mut self) {
        let mut deck = Cards::ALL;
        self.north = deck.pick(13).unwrap();
        self.south = deck.pick(13).unwrap();
        self.east = deck.pick(13).unwrap();
        self.west = deck;
        self.dealer = self.dealer.next();
        self.bids = Vec::new();
    }

    fn bidder(&self) -> Option<Seat> {
        let n = self.bids.len();
        if n >= 3 && &self.bids[n - 3..] == &[Bid::Pass, Bid::Pass, Bid::Pass] {
            None
        } else {
            Seat::try_from((n + self.dealer as usize) % 4)
        }
    }
    fn player(&self) -> Option<Seat> {
        None
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
        if self.bids.len() >= 3 && &self.bids[self.bids.len() - 3..] != &[Bid::Pass, Bid::Pass, Bid::Pass] {
            return None;
        }
        self.find_declarer().map(|s| s.next().next())
    }

    fn hand_visible_to(&self, hand: Seat, who: Seat) -> bool {
        let dummy = self.dummy();
        hand == who
            || (Some(hand) == dummy && Some(who.next().next()) == dummy)
            || (Some(hand.next().next()) == dummy && Some(who) == dummy)
            || (Some(hand) == dummy
                && self.north.len() + self.south.len() + self.east.len() + self.west.len() < 52)
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
}
#[derive(Default, Debug)]
struct Players {
    north: Option<mpsc::UnboundedSender<Result<warp::ws::Message, warp::Error>>>,
    south: Option<mpsc::UnboundedSender<Result<warp::ws::Message, warp::Error>>>,
    east: Option<mpsc::UnboundedSender<Result<warp::ws::Message, warp::Error>>>,
    west: Option<mpsc::UnboundedSender<Result<warp::ws::Message, warp::Error>>>,
    kibitzers: Vec<mpsc::UnboundedSender<Result<warp::ws::Message, warp::Error>>>,
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
    tokio::task::spawn(rx.forward(user_ws_tx).map(|result| {
        if let Err(e) = result {
            eprintln!("websocket send error: {}", e);
        }
    }));

    {
        // Save the sender in our list of connected users.
        let mut e = players.write().await;
        match seat.as_str() {
            "north" => {
                e.north = Some(tx);
            }
            "south" => {
                e.south = Some(tx);
            }
            "east" => {
                e.east = Some(tx);
            }
            "west" => {
                e.west = Some(tx);
            }
            _ => {
                e.kibitzers.push(tx);
            }
        }
        println!("got {} kibitzers now", e.kibitzers.len());
    }

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
            match seat.as_str() {
                "north" => {
                    e.north = None;
                }
                "south" => {
                    e.south = None;
                }
                "east" => {
                    e.east = None;
                }
                "west" => {
                    e.west = None;
                }
                _ => (),
            }
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
                let p = players.read().await;
                let mut g = game.write().await;
                match action {
                    Action::Redeal => {
                        g.redeal();
                    }
                    Action::Bid(b) => {
                        g.bids.push(b);
                        if g.bids.len() >= 3
                            && &g.bids[g.bids.len() - 3..] == &[Bid::Pass, Bid::Pass, Bid::Pass]
                        {
                            println!("Bidding is complete");
                            if g.bids.len() == 3 {
                                g.redeal();
                            } else {
                                let declarer = g.find_declarer().unwrap();
                                g.lead = Some(declarer.next());
                            }
                        }
                    }
                }
                if let Some(s) = &p.north {
                    let pp = Player {
                        seat: Seat::North,
                        players: &*p,
                        game: &*g,
                    };
                    let msg = format_as!(HTML, "" pp);
                    s.send(Ok(warp::ws::Message::text(msg))).ok();
                }
                if let Some(s) = &p.south {
                    let pp = Player {
                        seat: Seat::South,
                        players: &*p,
                        game: &*g,
                    };
                    let msg = format_as!(HTML, "" pp);
                    s.send(Ok(warp::ws::Message::text(msg))).ok();
                }
                if let Some(s) = &p.east {
                    let pp = Player {
                        seat: Seat::East,
                        players: &*p,
                        game: &*g,
                    };
                    let msg = format_as!(HTML, "" pp);
                    s.send(Ok(warp::ws::Message::text(msg))).ok();
                }
                if let Some(s) = &p.west {
                    let pp = Player {
                        seat: Seat::West,
                        players: &*p,
                        game: &*g,
                    };
                    let msg = format_as!(HTML, "" pp);
                    s.send(Ok(warp::ws::Message::text(msg))).ok();
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
    players: &'a Players,
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
    West,
    North,
    East,
}
impl Seat {
    fn try_from(v: usize) -> Option<Self> {
        match v {
            x if x == Seat::North as usize => Some(Seat::North),
            x if x == Seat::South as usize => Some(Seat::South),
            x if x == Seat::East as usize => Some(Seat::East),
            x if x == Seat::West as usize => Some(Seat::West),
            _ => None,
        }
    }
    fn next(self) -> Self {
        match self {
            Seat::South => Seat::West,
            Seat::West => Seat::North,
            Seat::North => Seat::East,
            Seat::East => Seat::South,
        }
    }
    fn name(self) -> &'static str {
        match self {
            Seat::South => "S",
            Seat::West => "W",
            Seat::North => "N",
            Seat::East => "E",
        }
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
