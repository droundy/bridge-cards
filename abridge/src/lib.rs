use bridge_deck::{Cards, Suit};
use dashmap::DashMap;
use display_as::{display, format_as, with_template, DisplayAs, HTML};
use futures::{SinkExt, StreamExt};
use robot::{Action, Bid, GameState, PlayerName, Seat, Seated};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::broadcast::Sender;
use tokio::sync::{mpsc, RwLock};
use warp::reply::Reply;
use warp::{path, Filter};

/// The configuration for running the server
#[derive(clap::Parser)]
pub struct Config {
    /// The email associated with the domain (if using let's encrypt)
    #[arg(long)]
    email: Option<String>,
    /// The domain (if using let's encrypt)
    #[arg(long)]
    domain: Option<String>,
    /// The port (if not using let's encrypt)
    #[arg(long, default_value = "8087")]
    port: u16,
}

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
            let closed = warp::ws::Message::close_with(
                CLOSED_FOR_INACTIVITY,
                "This table closed because nobody was playing.",
            );
            for c in g.connections.iter().chain([&g.ai]).flatten() {
                c.send(closed.clone()).ok();
            }
        }
    }
}

pub async fn serve_abridge(config: Config) {
    let (table_updates, _) = tokio::sync::broadcast::channel::<()>(1);
    let tables: TableMap = TableMap::default();
    tokio::spawn(clean_tables(tables.clone()));
    // Turns our "state" into a new filter.
    let tables = warp::any().map(move || tables.clone());
    let table_updates = warp::any().map(move || table_updates.clone());

    let style_css = path!("style.css").map(|| {
        const STYLE: &str = include_str!("style.css");
        warp::http::Response::builder()
            .status(200)
            .header("content-length", STYLE.len())
            .header("content-type", "text/css")
            .body(STYLE)
    });
    let audio = path!("your-play.mp3").map(|| {
        const AUDIO: &[u8] = include_bytes!("your-play.mp3");
        warp::http::Response::builder()
            .status(200)
            .header("content-length", AUDIO.len())
            .header("content-type", "audio/mpeg")
            .body(AUDIO)
    });
    let robot = path!("robot.js").map(|| {
        const ROBOT_JS: &[u8] = include_bytes!("../../robot/pkg/robot.js");
        warp::http::Response::builder()
            .status(200)
            .header("content-length", ROBOT_JS.len())
            .header("content-type", "text/javascript")
            .body(ROBOT_JS)
    });
    let robot_bg_wasm = path!("robot_bg.wasm").map(|| {
        const ROBOT_BG_WASM: &[u8] = include_bytes!("../../robot/pkg/robot_bg.wasm");
        warp::http::Response::builder()
            .status(200)
            .header("content-length", ROBOT_BG_WASM.len())
            .header("content-type", "application/wasm")
            .body(ROBOT_BG_WASM)
    });
    let index = tables.clone()
        .and(warp::query::<Vec<(String, String)>>())
        .and(table_updates.clone())
        .and_then(
            |table_map: TableMap,
             query: Vec<(String, String)>,
             table_updates: Sender<()>| async move {
                let r: Result<warp::http::Response<warp::hyper::Body>, warp::Rejection> =
                    if query.is_empty() {
                        let mut tables = Vec::new();
                        let first_guards = table_map.0.iter().collect::<Vec<_>>();
                        let mut guards = Vec::new();
                        for guard in first_guards.iter() {
                            guards.push((guard.key(), guard.value().read().await));
                        }
                        for (k, g) in guards.iter() {
                            tables.push(TableToJoin::new(k.to_string(), &g));
                        }
                        Ok(display(
                            HTML,
                            &Index {
                                room: Room { tables },
                            },
                        )
                        .into_response())
                    } else {
                        println!("Someone is long polling");
                        table_updates.subscribe().recv().await.ok();
                        println!("Got a change.");

                        let mut tables = Vec::new();
                        let first_guards = table_map.0.iter().collect::<Vec<_>>();
                        let mut guards = Vec::new();
                        for guard in first_guards.iter() {
                            guards.push((guard.key(), guard.value().read().await));
                        }
                        for (k, g) in guards.iter() {
                            tables.push(TableToJoin::new(k.to_string(), &g));
                        }
                        Ok(display(HTML, &Room { tables }).into_response())
                    };
                r
            },
        );
    let table_seat = path!(String / Seat)
        .and(tables.clone())
        .and(warp::filters::cookie::optional("name"))
        .and_then(
            |table: String, seat: Seat, tables: TableMap, name: Option<String>| async move {
                let table = percent_encoding::percent_decode_str(&table)
                    .decode_utf8_lossy()
                    .to_string();
                let g = tables.entry(table);
                let mut g = g.write().await;
                g.touch();
                if let Some(name) = name {
                    g.names[seat] = PlayerName::Human(name);
                } else {
                    g.names[seat] = PlayerName::Human(random_name());
                }
                let r: Result<warp::http::Response<warp::hyper::Body>, warp::Rejection> =
                    Ok(display(HTML, &PlayerPage(Player { seat, game: &g })).into_response());
                r
            },
        );
    let table_robot_tab =
        path!(String / Seat / "robot").and_then(|_table: String, _seat: Seat| async move {
            let r: Result<warp::http::Response<warp::hyper::Body>, warp::Rejection> =
                Ok(display(HTML, &RobotPage).into_response());
            r
        });

    let randomseat =
        path!("random")
            .and(tables.clone())
            .and_then(move |tables: TableMap| async move {
                for t in tables.0.iter() {
                    let g = t.value().read().await;
                    if let Some(seat) = g.randomseat() {
                        let uri = format!("/{}/{}", t.key(), seat.long_name());
                        let r: Result<
                            warp::reply::WithHeader<warp::http::StatusCode>,
                            warp::Rejection,
                        > = Ok(warp::reply::with_header(
                            warp::http::StatusCode::TEMPORARY_REDIRECT,
                            warp::http::header::LOCATION,
                            uri,
                        ));
                        return r;
                    }
                }
                let r: Result<warp::reply::WithHeader<warp::http::StatusCode>, warp::Rejection> =
                    Ok(warp::reply::with_header(
                        warp::http::StatusCode::TEMPORARY_REDIRECT,
                        warp::http::header::LOCATION,
                        format!("/{}/random", random_name()),
                    ));
                r
            });

    let table_randomseat = path!(String / "random").and(tables.clone()).and_then(
        move |table: String, tables: TableMap| async move {
            let table = percent_encoding::percent_decode_str(&table)
                .decode_utf8_lossy()
                .to_string();
            println!("random sitting in {table}");
            let g = tables.entry(table.clone());
            let g = g.read().await;
            let uri = if let Some(seat) = g.randomseat() {
                format!("/{table}/{}", seat.long_name())
            } else {
                format!("/")
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
    let table_sock = path!(String / Seat / "ws")
        .and(warp::ws())
        .and(tables.clone())
        .and(table_updates.clone())
        .map(
            |table: String,
             seat: Seat,
             ws: warp::ws::Ws,
             tables: TableMap,
             table_updates: Sender<()>| {
                let table = percent_encoding::percent_decode_str(&table)
                    .decode_utf8_lossy()
                    .to_string();
                let game = Arc::clone(&tables.entry(table));
                ws.on_upgrade(move |socket| ws_connected(Some(seat), socket, game, table_updates))
            },
        );
    let tabl_ai_sock = path!(String / Seat / "robot" / "ws")
        .and(warp::ws())
        .and(tables)
        .and(table_updates)
        .map(
            |table: String,
             _seat: Seat,
             ws: warp::ws::Ws,
             tables: TableMap,
             table_updates: Sender<()>| {
                let table = percent_encoding::percent_decode_str(&table)
                    .decode_utf8_lossy()
                    .to_string();
                let game = Arc::clone(&tables.entry(table));
                ws.on_upgrade(move |socket| ws_connected(None, socket, game, table_updates))
            },
        );
    let filter = style_css
        .or(audio)
        .or(robot)
        .or(robot_bg_wasm)
        .or(randomseat)
        .or(table_robot_tab)
        .or(table_randomseat)
        .or(table_sock)
        .or(tabl_ai_sock)
        .or(table_seat)
        .or(index);

    if let Some(domain) = config.domain {
        println!("Using lets encrypt for {domain}...");
        lets_encrypt_warp::lets_encrypt(filter, &config.email.unwrap(), &domain)
            .await
            .unwrap();
    } else {
        warp::serve(filter).run(([0, 0, 0, 0], config.port)).await;
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

type WsSender = mpsc::UnboundedSender<warp::ws::Message>;

async fn ws_connected(
    myseat: Option<Seat>,
    ws: warp::ws::WebSocket,
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
        } else {
            g.ai = Some(tx.clone());
        }
        g.touch();
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
        if msg.is_close() {
            println!("got a close");
            break;
        }
        match msg.to_str().map(serde_json::from_str::<Action>) {
            Err(e) => {
                eprintln!("Bad UTF8: {:?} {:?}", e, msg);
            }
            Ok(Err(e)) => {
                eprintln!("Bad JSON: {:?}", e);
            }
            Ok(Ok(action)) => {
                println!("Doing {:?}", action);
                let mut g = game.write().await;
                match action {
                    Action::ToggleCountForMe => {
                        if let Some(myseat) = myseat {
                            g.count_for_me[myseat] = !g.count_for_me[myseat];
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
                for seat in [Seat::North, Seat::South, Seat::East, Seat::West] {
                    if g.names[seat] == PlayerName::None && g.connections[seat].is_none() && g.ai.is_some() {
                        g.names[seat] = PlayerName::Robot(random_name());
                        println!("sending table update: {:?}", table_updates.send(()).is_ok());
                    }
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
            s.send(warp::ws::Message::text(msg.into_string())).ok();
        }
    }
    if let Some(seat) = g.turn() {
        if g.connections[seat].is_none() {
            if let Some(s) = &g.ai {
                s.send(warp::ws::Message::text(
                    &serde_json::to_string(&*g).unwrap(),
                ))
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

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::io::AsyncWriteExt;

    /// The websocket for one seat at this table, or for its robot if `seat`
    /// is `None`.
    fn socket(
        seat: Option<Seat>,
        game: Arc<RwLock<GameState<WsSender>>>,
    ) -> impl Filter<Extract = (impl Reply,), Error = warp::Rejection> + Clone {
        let (table_updates, _) = tokio::sync::broadcast::channel(1);
        warp::ws().map(move |ws: warp::ws::Ws| {
            let game = game.clone();
            let table_updates = table_updates.clone();
            ws.on_upgrade(move |socket| ws_connected(seat, socket, game, table_updates))
        })
    }

    async fn send(client: &mut warp::test::WsClient, action: &Action) {
        client
            .send_text(serde_json::to_string(action).unwrap())
            .await;
    }

    /// Whether `ready` becomes true of the game within a few seconds.
    async fn eventually(
        game: &RwLock<GameState<WsSender>>,
        ready: impl Fn(&GameState<WsSender>) -> bool,
    ) -> bool {
        for _ in 0..300 {
            if ready(&*game.read().await) {
                return true;
            }
            tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        }
        false
    }

    /// Table "t" in `tables`, with South sitting at it.
    async fn table_with_south(
        tables: &TableMap,
    ) -> (Arc<RwLock<GameState<WsSender>>>, warp::test::WsClient) {
        let game = Arc::clone(&tables.entry("t".to_string()));
        let mut south = warp::test::ws()
            .handshake(socket(Some(Seat::South), game.clone()))
            .await
            .unwrap();
        south.recv().await.unwrap();
        (game, south)
    }

    #[tokio::test]
    async fn a_table_closes_once_nobody_has_played_for_an_hour() {
        use tokio_tungstenite::tungstenite::Message;
        let tables = TableMap::default();
        let game = Arc::clone(&tables.entry("t".to_string()));
        // The test client hides close frames, so use a real connection.
        let (addr, server) =
            warp::serve(socket(Some(Seat::South), game)).bind_ephemeral(([127, 0, 0, 1], 0));
        tokio::spawn(server);
        let (mut south, _) = tokio_tungstenite::connect_async(format!("ws://{addr}/"))
            .await
            .unwrap();
        south.next().await.unwrap().unwrap();

        close_idle_tables(&tables, Instant::now() + Duration::from_mins(59)).await;
        assert!(tables.get("t").is_some(), "a table closed while in use");

        close_idle_tables(&tables, Instant::now() + Duration::from_mins(61)).await;
        assert!(tables.get("t").is_none(), "an idle table stayed open");
        match south.next().await.unwrap().unwrap() {
            Message::Close(Some(frame)) => {
                assert_eq!(u16::from(frame.code), CLOSED_FOR_INACTIVITY)
            }
            m => panic!("expected the table to close, but got {m:?}"),
        }
    }

    #[tokio::test]
    async fn an_empty_table_waits_for_its_players_to_come_back() {
        let tables = TableMap::default();
        let (game, mut south) = table_with_south(&tables).await;
        south.send(warp::ws::Message::close()).await;
        south.recv_closed().await.unwrap();
        assert!(eventually(&game, |g| g.connections[Seat::South].is_none()).await);

        close_idle_tables(&tables, Instant::now() + Duration::from_mins(9)).await;
        assert!(tables.get("t").is_some(), "an empty table closed too soon");
        close_idle_tables(&tables, Instant::now() + Duration::from_mins(11)).await;
        assert!(tables.get("t").is_none(), "an empty table stayed open");
    }

    #[tokio::test]
    async fn playing_after_a_long_pause_keeps_the_table() {
        let tables = TableMap::default();
        let (game, mut south) = table_with_south(&tables).await;
        // A clock that started under two hours ago can't go back that far.
        let Some(long_ago) = Instant::now().checked_sub(Duration::from_hours(2)) else {
            return;
        };
        game.write().await.last_action = Some(long_ago);

        south
            .send_text(serde_json::to_string(&Action::ToggleCountForMe).unwrap())
            .await;
        assert!(
            eventually(&game, |g| g.count_for_me[Seat::South]).await,
            "the table was reset"
        );
        assert!(game.read().await.connections[Seat::South].is_some());
    }

    #[tokio::test]
    async fn closing_an_old_connection_keeps_the_new_one() {
        for seat in [Some(Seat::South), None] {
            let game: Arc<RwLock<GameState<WsSender>>> = Default::default();
            let socket = socket(seat, game.clone());
            // The same player opens the table twice, and then closes the
            // first one.
            let mut old = warp::test::ws().handshake(socket.clone()).await.unwrap();
            old.recv().await.unwrap();
            let mut new = warp::test::ws().handshake(socket).await.unwrap();
            new.recv().await.unwrap();
            old.send(warp::ws::Message::close()).await;
            old.recv_closed().await.unwrap();

            let g = game.read().await;
            if let Some(seat) = seat {
                assert!(g.connections[seat].is_some(), "{seat:?} lost its seat");
            } else {
                assert!(g.ai.is_some(), "the robot lost its seat");
            }
        }
    }

    #[tokio::test]
    async fn a_dropped_connection_gives_up_its_seat() {
        let game: Arc<RwLock<GameState<WsSender>>> = Default::default();
        let (addr, server) = warp::serve(socket(Some(Seat::South), game.clone()))
            .bind_ephemeral(([127, 0, 0, 1], 0));
        tokio::spawn(server);
        let mut tcp = tokio::net::TcpStream::connect(addr).await.unwrap();
        tcp.write_all(
            b"GET / HTTP/1.1\r\nHost: localhost\r\nConnection: upgrade\r\nUpgrade: websocket\r\n\
              Sec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n",
        )
        .await
        .unwrap();
        assert!(eventually(&game, |g| g.connections[Seat::South].is_some()).await);

        // Hang up without the websocket's closing handshake, as when a
        // connection drops.
        drop(tcp);
        assert!(
            eventually(&game, |g| g.connections[Seat::South].is_none()).await,
            "a dead connection kept its seat"
        );
    }

    // In the tests below, every action sends South an update once the server
    // has handled it, so South's `recv` waits for that.

    #[tokio::test]
    async fn players_act_only_for_their_own_seat() {
        let game: Arc<RwLock<GameState<WsSender>>> = Default::default();
        let mut south = warp::test::ws()
            .handshake(socket(Some(Seat::South), game.clone()))
            .await
            .unwrap();
        south.recv().await.unwrap();

        // South passes, and then tries to bid for West too.
        send(&mut south, &Action::Bid(Seat::South, Bid::Pass)).await;
        south.recv().await.unwrap();
        send(
            &mut south,
            &Action::Bid(Seat::West, Bid::Suit(1, Suit::Clubs)),
        )
        .await;
        south.recv().await.unwrap();
        assert_eq!(vec![Bid::Pass], game.read().await.bids);

        // South declares 1NT, so West leads, and South can't lead for West.
        let west_card = {
            let mut g = game.write().await;
            g.bids = vec![Bid::NT(1), Bid::Pass, Bid::Pass, Bid::Pass];
            g.lead = Some(Seat::West);
            g.hands[Seat::West].min().unwrap()
        };
        send(&mut south, &Action::Play(Seat::West, west_card)).await;
        south.recv().await.unwrap();
        assert!(game.read().await.played.is_empty());

        // Once West has led, South plays dummy's cards.
        let north_card = {
            let mut g = game.write().await;
            g.played = vec![west_card];
            g.hands[Seat::West] -= Cards::singleton(west_card);
            g.playable_cards(Seat::North, Seat::South)
                .playable
                .min()
                .unwrap()
        };
        send(&mut south, &Action::Play(Seat::North, north_card)).await;
        south.recv().await.unwrap();
        assert_eq!(vec![west_card, north_card], game.read().await.played);
    }

    #[tokio::test]
    async fn only_legal_bids_count() {
        let game: Arc<RwLock<GameState<WsSender>>> = Default::default();
        let mut south = warp::test::ws()
            .handshake(socket(Some(Seat::South), game.clone()))
            .await
            .unwrap();
        south.recv().await.unwrap();

        // South opens the bidding, so there is nothing to redouble, and
        // there is no eighth level.
        send(&mut south, &Action::Bid(Seat::South, Bid::Redouble)).await;
        south.recv().await.unwrap();
        send(
            &mut south,
            &Action::Bid(Seat::South, Bid::Suit(8, Suit::Clubs)),
        )
        .await;
        south.recv().await.unwrap();
        assert!(game.read().await.bids.is_empty());

        // East declares 1NT, and South, on lead, can't reopen the bidding.
        {
            let mut g = game.write().await;
            g.dealer = Seat::East;
            g.bids = vec![Bid::NT(1), Bid::Pass, Bid::Pass, Bid::Pass];
            g.lead = Some(Seat::South);
        }
        send(&mut south, &Action::Bid(Seat::South, Bid::NT(7))).await;
        south.recv().await.unwrap();
        assert_eq!(4, game.read().await.bids.len());
    }

    #[tokio::test]
    async fn the_robot_acts_only_for_seats_nobody_is_at() {
        let game: Arc<RwLock<GameState<WsSender>>> = Default::default();
        let mut robot = warp::test::ws()
            .handshake(socket(None, game.clone()))
            .await
            .unwrap();
        robot.recv().await.unwrap();
        let mut south = warp::test::ws()
            .handshake(socket(Some(Seat::South), game.clone()))
            .await
            .unwrap();
        south.recv().await.unwrap();

        // South is here, so the robot can't bid for South.
        send(&mut robot, &Action::Bid(Seat::South, Bid::Pass)).await;
        south.recv().await.unwrap();
        assert!(game.read().await.bids.is_empty());

        // Nobody is at West, so the robot bids for West.
        send(&mut south, &Action::Bid(Seat::South, Bid::Pass)).await;
        south.recv().await.unwrap();
        send(&mut robot, &Action::Bid(Seat::West, Bid::Pass)).await;
        south.recv().await.unwrap();
        assert_eq!(vec![Bid::Pass, Bid::Pass], game.read().await.bids);
    }
}
