use bridge_deck::{Cards, Suit};
use display_as::{display, format_as, with_template, DisplayAs, HTML, URL};
use futures::{SinkExt, StreamExt};
use robot::{Action, Bid, GameState, PlayerName, Seat, Seated};
use std::sync::Arc;
use tokio::sync::{mpsc, RwLock};
use warp::reply::Reply;
use warp::{path, Filter};

/// The configuration for running the server
#[derive(clap::Parser)]
pub struct Config {
    /// The root at which the pages are served
    #[arg(long, default_value = "")]
    root: String,
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

pub async fn serve_abridge(config: Config) {
    let root = internment::Intern::new(config.root.clone());
    let players = Arc::new(RwLock::new(Players::default()));
    let game = Arc::new(RwLock::new(GameState::new(root.to_string())));
    // Turns our "state" into a new filter.
    let players = warp::any().map(move || players.clone());
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
    let index = players
        .clone()
        .and_then(|players: Arc<RwLock<Players>>| async move {
            let p = players.read().await;
            let r: Result<warp::http::Response<warp::hyper::Body>, warp::Rejection> =
                Ok(display(HTML, &Index { players: &p }).into_response());
            r
        });
    let seat = path!(Seat)
        .and(game.clone())
        .and(warp::filters::cookie::optional("name"))
        .and_then(
            |seat: Seat, game: Arc<RwLock<GameState>>, name: Option<String>| async move {
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
        |seat: Seat, game: Arc<RwLock<GameState>>| async move {
            let mut g = game.write().await;
            g.check_timeout();
            let r: Result<warp::http::Response<warp::hyper::Body>, warp::Rejection> =
                Ok(display(HTML, &RobotPage { seat, game: &g }).into_response());
            r
        },
    );

    let randomseat = path!("random").and(players.clone()).and_then(
        move |players: Arc<RwLock<Players>>| async move {
            let p = players.read().await;
            let uri = if let Some(seat) = p.randomseat() {
                format!("{root}/{}", seat.long_name())
            } else {
                format!("{root}/")
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
    let sock = path!(Seat/ "ws")
        .and(warp::ws())
        .and(game.clone())
        .and(players.clone())
        .map(|seat: Seat, ws: warp::ws::Ws, game, players| {
            ws.on_upgrade(move |socket| {
                ws_connected(seat, WhichWebsocket::Human, socket, players, game)
            })
        });
    let ai_sock = path!(Seat / "ai")
        .and(warp::ws())
        .and(game)
        .and(players)
        .map(|seat: Seat, ws: warp::ws::Ws, game, players| {
            ws.on_upgrade(move |socket| {
                ws_connected(seat, WhichWebsocket::Robot, socket, players, game)
            })
        });
    let filter = style_css
        .or(audio)
        .or(robot_tab)
        .or(robot)
        .or(robot_bg_wasm)
        .or(sock)
        .or(ai_sock)
        .or(seat)
        .or(randomseat)
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

#[derive(Debug, Default)]
enum PlayerConnection {
    Human(mpsc::UnboundedSender<warp::ws::Message>),
    WasmAi(mpsc::UnboundedSender<warp::ws::Message>),
    #[default]
    None,
}
impl PlayerConnection {
    fn not_human(&self) -> bool {
        !matches!(self, PlayerConnection::Human(_))
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum WhichWebsocket {
    Human,
    Robot,
}

#[derive(Default, Debug)]
struct Players(Seated<PlayerConnection>);

impl Players {
    fn randomseat(&self) -> Option<Seat> {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        for _ in 0..30 {
            let seat: Seat = rng.gen::<usize>().into();
            if self.0[seat].not_human() {
                return Some(seat);
            }
        }
        None
    }
}

async fn ws_connected(
    myseat: Seat,
    which_websocket: WhichWebsocket,
    ws: warp::ws::WebSocket,
    players: Arc<RwLock<Players>>,
    game: Arc<RwLock<GameState>>,
) {
    // Split the socket into a sender and receive of messages.
    let (mut user_ws_tx, mut user_ws_rx) = ws.split();

    // Use an unbounded channel to handle buffering and flushing of messages
    // to the websocket...
    let (tx, mut rx) = mpsc::unbounded_channel();
    {
        // Save the sender in our list of connected users.
        let mut p = players.write().await;
        match which_websocket {
            WhichWebsocket::Human => {
                p.0[myseat] = PlayerConnection::Human(tx);
            }
            WhichWebsocket::Robot => {
                for seat in [Seat::North, Seat::South, Seat::East, Seat::West] {
                    if p.0[seat].not_human() {
                        p.0[seat] = PlayerConnection::WasmAi(tx.clone());
                    }
                }
            }
        }
        if matches!(&p.0[myseat], PlayerConnection::WasmAi(_)) {
            let g = game.read().await;
            send_player_updates(&p, &g);
        }
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
            let mut p = players.write().await;
            if !p.0[myseat].not_human() {
                let g = game.read().await;
                p.0[myseat] = PlayerConnection::None;
                send_player_updates(&p, &g);
            }
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
                let p = players.read().await;
                let mut g = game.write().await;
                match action {
                    Action::ToggleCountForMe => {
                        g.count_for_me[myseat] = !g.count_for_me[myseat];
                    }
                    Action::Redeal => {
                        g.redeal();
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
                        g.names[myseat] = PlayerName::Human(name);
                    }
                }
                g.check_timeout();
                for seat in [Seat::North, Seat::South, Seat::East, Seat::West] {
                    if g.names[seat] == PlayerName::None && matches!(&p.0[seat], PlayerConnection::WasmAi(_)) {
                        g.names[seat] = PlayerName::Robot(random_name());
                    }
                }
                send_player_updates(&p, &g);
            }
        }
    }
}

fn send_player_updates(p: &Players, g: &GameState) {
    for seat in [Seat::North, Seat::South, Seat::East, Seat::West] {
        if let PlayerConnection::Human(s) = &p.0[seat] {
            let pp = Player { seat, game: &g };
            let msg = format_as!(HTML, "" pp);
            s.send(warp::ws::Message::text(msg.into_string())).ok();
        }
    }
    if let Some(seat) = g.turn() {
        if let PlayerConnection::WasmAi(s) = &p.0[seat] {
            s.send(warp::ws::Message::text(
                &serde_json::to_string(&*g).unwrap(),
            ))
            .ok();
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

struct RobotPage<'a> {
    seat: Seat,
    game: &'a GameState,
}

#[with_template("[%" "%]" "robot-page.html")]
impl<'a> DisplayAs<HTML> for RobotPage<'a> {}
