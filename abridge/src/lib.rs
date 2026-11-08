use axum::extract::ws::{CloseFrame, Message, WebSocket};
use bridge_deck::{Cards, Suit};
use dashmap::DashMap;
use display_as::{format_as, with_template, DisplayAs, HTML};
use futures::{SinkExt, StreamExt};
use robot::{Action, Bid, GameState, PlayerName, Seat, Seated};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::broadcast::Sender;
use tokio::sync::{mpsc, RwLock};

mod axum_server;
pub use axum_server::{serve_abridge, Config};

#[derive(Debug, Clone, Default)]
struct TableMap(Arc<DashMap<String, Arc<RwLock<GameState<WsSender>>>>>);

impl TableMap {
    fn get(
        &self,
        table: &str,
    ) -> Option<dashmap::mapref::one::Ref<'_, String, Arc<RwLock<GameState<WsSender>>>>> {
        self.0.get(table)
    }
    fn entry(
        &self,
        table: String,
    ) -> dashmap::mapref::one::RefMut<'_, String, Arc<RwLock<GameState<WsSender>>>> {
        self.0.entry(table).or_default()
    }
    fn remove(&self, table: &str) {
        self.0.remove(table);
    }
}

/// A table where nothing has happened for this long is closed, even if some
/// tabs are still connected to it, since nobody is playing there.
const IDLE_TABLE: Duration = Duration::from_hours(1);
/// A table that nobody is sitting at is closed once nothing has happened for
/// this long, which gives a player whose connection dropped time to come back.
const EMPTY_TABLE: Duration = Duration::from_mins(10);
/// The websocket close code that tells a page its table was closed for
/// inactivity, so that it shouldn't reconnect.
const CLOSED_FOR_INACTIVITY: u16 = 4000;

async fn clean_tables(tables: TableMap) {
    loop {
        tokio::time::sleep(EMPTY_TABLE).await;
        close_idle_tables(&tables, Instant::now()).await;
    }
}

/// Closes the tables where nobody is playing at `now`, and tells any tabs
/// still connected to them that their table is gone.
async fn close_idle_tables(tables: &TableMap, now: Instant) {
    let keys = tables.0.iter().map(|r| r.key().clone()).collect::<Vec<_>>();
    for k in keys {
        // Don't hold the map's lock while waiting for the table's.
        let Some(game) = tables.get(&k).map(|g| Arc::clone(&g)) else {
            continue;
        };
        let idle = {
            let g = game.read().await;
            let idle = g
                .last_action
                .map_or(Duration::ZERO, |t| now.saturating_duration_since(t));
            let empty = g.connections.iter().all(Option::is_none);
            idle > IDLE_TABLE || (empty && idle > EMPTY_TABLE)
        };
        if idle {
            println!("Closing table {k}, where nobody is playing.");
            // Remove it first, so that nobody new can join it.
            tables.remove(&k);
            let g = game.read().await;
            let closed = Message::Close(Some(CloseFrame {
                code: CLOSED_FOR_INACTIVITY,
                reason: "This table closed because nobody was playing.".into(),
            }));
            for c in g.connections.iter().chain([&g.ai]).flatten() {
                c.send(closed.clone()).ok();
            }
        }
    }
}

fn random_name() -> String {
    let name = memorable_wordlist::space_delimited(18);
    let mut out = String::with_capacity(name.len());
    let mut needs_cap = true;
    for c in name.chars() {
        if needs_cap {
            out.extend(c.to_uppercase());
            needs_cap = false;
        } else {
            out.push(c);
            needs_cap = c == ' ';
        }
    }
    out
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

enum IsMe<T> {
    Me(T),
    Other(T, Seat),
}

#[with_template("[%" "%]" "player-name.html")]
impl<'a> DisplayAs<HTML> for IsMe<PlayerName> {}

type WsSender = mpsc::UnboundedSender<Message>;

/// Names as robots the seats the robot plays: every seat nobody is
/// connected to, while a robot tab is, including one whose page was loaded
/// but never connected.  Returns whether any name changed.
fn name_robot_seats(g: &mut GameState<WsSender>) -> bool {
    let mut changed = false;
    if g.ai.is_some() {
        for seat in [Seat::North, Seat::South, Seat::East, Seat::West] {
            if g.connections[seat].is_none() && !matches!(g.names[seat], PlayerName::Robot(_)) {
                g.names[seat] = PlayerName::Robot(random_name());
                changed = true;
            }
        }
    }
    changed
}

async fn ws_connected(
    myseat: Option<Seat>,
    name: Option<String>,
    ws: WebSocket,
    game: Arc<RwLock<GameState<WsSender>>>,
    table_updates: Sender<()>,
) {
    // Split the socket into a sender and receive of messages.
    let (mut user_ws_tx, mut user_ws_rx) = ws.split();

    // Use an unbounded channel to handle buffering and flushing of messages
    // to the websocket...
    let (tx, mut rx) = mpsc::unbounded_channel();
    {
        // Save the sender in our list of connected users.
        let mut g = game.write().await;
        if let Some(seat) = myseat {
            g.connections[seat] = Some(tx.clone());
            // The seat is taken now, under the name from the cookie, or else
            // the one its page showed.
            g.names[seat] = PlayerName::Human(match (name, &g.names[seat]) {
                (Some(name), _) => name,
                (None, PlayerName::Human(shown)) => shown.clone(),
                (None, _) => random_name(),
            });
        } else {
            g.ai = Some(tx.clone());
        }
        g.touch();
        name_robot_seats(&mut g);
        send_player_updates(&g);
        println!("sending table update: {:?}", table_updates.send(()).is_ok());
    }
    tokio::task::spawn(async move {
        while let Some(x) = rx.recv().await {
            if let Err(e) = user_ws_tx.send(x).await {
                println!("Got a ws send error: {e}");
            }
        }
    });

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
        let text = match msg {
            Message::Close(_) => {
                println!("got a close");
                break;
            }
            Message::Text(text) => text,
            // Pings, which axum answers itself, and whatever else.
            _ => continue,
        };
        match serde_json::from_str::<Action>(text.as_str()) {
            Err(e) => {
                eprintln!("Bad JSON: {:?}", e);
            }
            Ok(action) => {
                println!("Doing {:?}", action);
                let mut g = game.write().await;
                match action {
                    Action::ToggleCountForMe => {
                        if let Some(myseat) = myseat {
                            g.count_for_me[myseat] = !g.count_for_me[myseat];
                        }
                    }
                    Action::SetSystem(system) => {
                        if g.bids.is_empty() {
                            g.set_system(system);
                            println!("sending table update: {:?}", table_updates.send(()).is_ok());
                        }
                    }
                    Action::Redeal => {
                        g.redeal();
                        println!("sending table update: {:?}", table_updates.send(()).is_ok());
                    }
                    Action::Bid(seat, b) => {
                        if may_act_for(&g, myseat, seat) && g.bid_is_legal(seat, b) {
                            g.bids.push(b);
                            if g.bids.len() > 3
                                && g.bids[g.bids.len() - 3..] == [Bid::Pass, Bid::Pass, Bid::Pass]
                            {
                                println!("Bidding is complete");
                                if let Some(declarer) = g.find_declarer() {
                                    g.lead = Some(declarer.next());
                                } else {
                                    g.hand_done = true;
                                }
                            }
                            println!("sending table update: {:?}", table_updates.send(()).is_ok());
                        } else {
                            println!("Ignoring a bid of {b:?} for {seat:?} from {myseat:?}");
                        }
                    }
                    Action::Play(seat, card) => {
                        if let (Some(turn), Some(playing)) = (g.turn(), g.hand_playing()) {
                            if may_act_for(&g, myseat, turn) && [turn, playing].contains(&seat) {
                                if g.playable_cards(playing, turn).playable.contains(card) {
                                    if g.played.len() == 4 {
                                        g.played.clear();
                                    }
                                    g.played.push(card);
                                    g.hands[playing] -= Cards::singleton(card);
                                    g.trick_finish();
                                    if g.ns_tricks + g.ew_tricks == 13 {
                                        g.hand_done = true;
                                    }
                                }
                            }
                        }
                    }
                    Action::Name(name) => {
                        if let Some(myseat) = myseat {
                            g.names[myseat] = PlayerName::Human(name);
                            println!("sending table update: {:?}", table_updates.send(()).is_ok());
                        }
                    }
                }
                g.touch();
                if name_robot_seats(&mut g) {
                    println!("sending table update: {:?}", table_updates.send(()).is_ok());
                }
                send_player_updates(&g);
            }
        }
    }

    // The socket is gone, whether or not it closed cleanly, so give up our
    // place at the table, unless a newer connection has already taken it.
    let mut g = game.write().await;
    g.touch();
    let ours = |c: &Option<WsSender>| c.as_ref().is_some_and(|c| c.same_channel(&tx));
    if let Some(seat) = myseat {
        if !ours(&g.connections[seat]) {
            return;
        }
        println!("A human closed!");
        g.connections[seat] = None;
        if g.ai.is_some() {
            g.names[seat] = PlayerName::Robot(random_name());
        } else {
            g.names[seat] = PlayerName::None;
        }
    } else {
        if !ours(&g.ai) {
            return;
        }
        println!("An ai closed!");
        g.ai = None;
        for s in [Seat::North, Seat::East, Seat::South, Seat::West] {
            if g.connections[s].is_none() {
                g.names[s] = PlayerName::None;
            }
        }
    }
    println!("sending table update: {:?}", table_updates.send(()).is_ok());
    send_player_updates(&g);
}

/// Whether the connection for `myseat` (`None` for the robot) may act for
/// `seat`.  Players act only for their own seat, and the robot only for seats
/// that nobody is connected to.
fn may_act_for(g: &GameState<WsSender>, myseat: Option<Seat>, seat: Seat) -> bool {
    match myseat {
        Some(me) => me == seat,
        None => g.connections[seat].is_none(),
    }
}

fn send_player_updates(g: &GameState<WsSender>) {
    for seat in [Seat::North, Seat::South, Seat::East, Seat::West] {
        if let Some(s) = &g.connections[seat] {
            let pp = Player { seat, game: &g };
            let msg = format_as!(HTML, "" pp);
            s.send(Message::Text(msg.into_string().into())).ok();
        }
    }
    if let Some(seat) = g.turn() {
        if g.connections[seat].is_none() {
            if let Some(s) = &g.ai {
                s.send(Message::Text(serde_json::to_string(&*g).unwrap().into()))
                    .ok();
            }
        }
    }
}

struct Index {
    room: Room,
}
#[with_template("[%" "%]" "index.html")]
impl<'a> DisplayAs<HTML> for Index {}

struct Room {
    tables: Vec<TableToJoin>,
}
#[with_template("[%" "%]" "room.html")]
impl<'a> DisplayAs<HTML> for Room {}

struct Player<'a> {
    seat: Seat,
    game: &'a GameState<WsSender>,
}
#[with_template("[%" "%]" "player.html")]
impl<'a> DisplayAs<HTML> for Player<'a> {}

impl<'a> Player<'a> {
    fn get_name(&self, seat: Seat) -> IsMe<PlayerName> {
        if self.seat == seat {
            IsMe::Me(self.game.names[seat].clone())
        } else {
            IsMe::Other(self.game.names[seat].clone(), seat)
        }
    }
}

struct PlayerPage<'a>(Player<'a>);
#[with_template("[%" "%]" "player-page.html")]
impl<'a> DisplayAs<HTML> for PlayerPage<'a> {}

struct RobotPage;

#[with_template("[%" "%]" "robot-page.html")]
impl DisplayAs<HTML> for RobotPage {}

struct TableToJoin {
    table: String,
    dealer: Seat,
    names: Seated<PlayerName>,
    pub bids: Vec<Bid>,
}

#[with_template("[%" "%]" "table-to-join.html")]
impl DisplayAs<HTML> for TableToJoin {}

impl TableToJoin {
    fn new(table: String, game: &GameState<WsSender>) -> Self {
        TableToJoin {
            table,
            dealer: game.dealer,
            names: game.names.clone(),
            bids: game.bids.clone(),
        }
    }
}
