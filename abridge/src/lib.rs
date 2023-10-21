use bridge_deck::{Cards, Suit};
use dashmap::DashMap;
use display_as::{display, format_as, with_template, DisplayAs, HTML};
use futures::{SinkExt, StreamExt};
use robot::{Action, Bid, GameState, PlayerName, Seat, Seated};
use std::sync::Arc;
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

type TableMap = Arc<DashMap<String, Arc<RwLock<GameState<WsSender>>>>>;

async fn clean_tables(tables: TableMap) {
    loop {
        tokio::time::sleep(std::time::Duration::from_secs(60 * 10)).await;
        let keys = tables.iter().map(|r| r.key().clone()).collect::<Vec<_>>();
        for k in keys {
            if let Some(g) = tables.get(&k) {
                let g = g.read().await;
                if g.connections.iter().all(Option::is_none) {
                    println!("Cleaning up table {k} where no one is sitting.");
                    tables.remove(&k);
                } else if let Some(t) = g.last_action {
                    if t.elapsed() > std::time::Duration::from_secs(60 * 60) {
                        println!("Cleaning up table {k} where no one has played recently.");
                        tables.remove(&k);
                    }
                }
            }
        }
        // tables.retain(|_, t| t.read().await.connections.any(|c| c.is_some()));
    }
}

pub async fn serve_abridge(config: Config) {
    let (table_updates, _) = tokio::sync::broadcast::channel::<()>(1);
    let tables: TableMap = Arc::new(DashMap::new());
    tokio::spawn(clean_tables(tables.clone()));
    let game = Arc::new(RwLock::new(GameState::new()));
    // Turns our "state" into a new filter.
    let tables = warp::any().map(move || tables.clone());
    let table_updates = warp::any().map(move || table_updates.clone());
    let game = warp::any().map(move || game.clone());

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
    let index = game
        .clone()
        .and(warp::query::<Vec<(String, String)>>())
        .and(table_updates.clone())
        .and_then(
            |game: Arc<RwLock<GameState<WsSender>>>,
             query: Vec<(String, String)>,
             table_updates: Sender<()>| async move {
                let r: Result<warp::http::Response<warp::hyper::Body>, warp::Rejection> =
                    if query.is_empty() {
                        let g = game.read().await;
                        Ok(display(
                            HTML,
                            &Index {
                                table: TableToJoin::new("".to_string(), &g),
                            },
                        )
                        .into_response())
                    } else {
                        println!("Someone is long polling");
                        table_updates.subscribe().recv().await.ok();
                        println!("Got a change.");
                        let g = game.read().await;
                        Ok(display(HTML, &TableToJoin::new("".to_string(), &g)).into_response())
                    };
                r
            },
        );
    let table_seat = path!(String / Seat)
        .and(tables.clone())
        .and(warp::filters::cookie::optional("name"))
        .and_then(
            |table: String, seat: Seat, tables: TableMap, name: Option<String>| async move {
                let g = tables.entry(table).or_default();
                let mut g = g.write().await;
                g.check_timeout();
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
    let seat = path!(Seat)
        .and(game.clone())
        .and(warp::filters::cookie::optional("name"))
        .and_then(
            |seat: Seat, game: Arc<RwLock<GameState<WsSender>>>, name: Option<String>| async move {
                let mut g = game.write().await;
                g.check_timeout();
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
    let robot_tab = path!(Seat / "robot").and(game.clone()).and_then(
        |seat: Seat, game: Arc<RwLock<GameState<WsSender>>>| async move {
            let mut g = game.write().await;
            g.check_timeout();
            let r: Result<warp::http::Response<warp::hyper::Body>, warp::Rejection> =
                Ok(display(HTML, &RobotPage { seat }).into_response());
            r
        },
    );
    let table_robot_tab = path!(String / Seat / "robot").and(game.clone()).and_then(
        |_table: String, seat: Seat, game: Arc<RwLock<GameState<WsSender>>>| async move {
            let mut g = game.write().await;
            g.check_timeout();
            let r: Result<warp::http::Response<warp::hyper::Body>, warp::Rejection> =
                Ok(display(HTML, &RobotPage { seat }).into_response());
            r
        },
    );

    let randomseat = path!("random").and(game.clone()).and_then(
        move |game: Arc<RwLock<GameState<WsSender>>>| async move {
            let g = game.read().await;
            let uri = if let Some(seat) = g.randomseat() {
                format!("/{}", seat.long_name())
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

    let table_randomseat = path!(String / "random").and(tables.clone()).and_then(
        move |table: String, tables: TableMap| async move {
            let g = tables.entry(table).or_default();
            let g = g.read().await;
            let uri = if let Some(seat) = g.randomseat() {
                format!("/{}", seat.long_name())
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
    let sock = path!(Seat / "ws")
        .and(warp::ws())
        .and(game.clone())
        .and(table_updates.clone())
        .map(|seat: Seat, ws: warp::ws::Ws, game, table_updates| {
            ws.on_upgrade(move |socket| ws_connected(Some(seat), socket, game, table_updates))
        });
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
                let game = Arc::clone(&tables.entry(table).or_default());
                ws.on_upgrade(move |socket| ws_connected(Some(seat), socket, game, table_updates))
            },
        );
    let ai_sock = path!(Seat / "robot" / "ws")
        .and(warp::ws())
        .and(game)
        .and(table_updates.clone())
        .map(|_seat: Seat, ws: warp::ws::Ws, game, table_updates| {
            ws.on_upgrade(move |socket| ws_connected(None, socket, game, table_updates))
        });
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
                let game = Arc::clone(&tables.entry(table).or_default());
                ws.on_upgrade(move |socket| ws_connected(None, socket, game, table_updates))
            },
        );
    let filter = style_css
        .or(audio)
        .or(robot_tab)
        .or(robot)
        .or(robot_bg_wasm)
        .or(sock)
        .or(ai_sock)
        .or(seat)
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
            g.connections[seat] = Some(tx);
        } else {
            g.ai = Some(tx);
        }
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
            let mut g = game.write().await;
            if let Some(seat) = myseat {
                println!("A human closed!");
                g.connections[seat] = None;
                if g.ai.is_some() {
                    g.names[seat] = PlayerName::Robot(random_name());
                } else {
                    g.names[seat] = PlayerName::None;
                }
            } else {
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
            return;
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
                    Action::Bid(myseat, b) => {
                        if g.turn() == Some(myseat) {
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
                            println!("We got a message from {myseat:?} but it is not playing");
                        }
                    }
                    Action::Play(myseat, card) => {
                        if let (Some(seat), Some(playing)) = (g.turn(), g.hand_playing()) {
                            if [seat, playing].contains(&myseat) {
                                if g.playable_cards(playing, seat).playable.contains(card) {
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
                g.check_timeout();
                for seat in [Seat::North, Seat::South, Seat::East, Seat::West] {
                    if g.names[seat] == PlayerName::None && g.connections[seat].is_none() {
                        g.names[seat] = PlayerName::Robot(random_name());
                        println!("sending table update: {:?}", table_updates.send(()).is_ok());
                    }
                }
                send_player_updates(&g);
            }
        }
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
    table: TableToJoin,
}
#[with_template("[%" "%]" "index.html")]
impl<'a> DisplayAs<HTML> for Index {}

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

struct RobotPage {
    seat: Seat,
}

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
