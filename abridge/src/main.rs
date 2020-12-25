use display_as::{display, with_template, DisplayAs, HTML};
use futures::{FutureExt, StreamExt};
use std::sync::Arc;
use tokio::sync::{mpsc, RwLock};
use warp::reply::Reply;
use warp::{path, Filter};

#[tokio::main]
async fn main() {
    let players = Arc::new(RwLock::new(Players::default()));
    // Turns our "state" into a new filter.
    let players = warp::any().map(move || players.clone());

    let style_css = path!("style.css").map(|| {
        const STYLE: &'static str = include_str!("style.css");
        Ok(warp::http::Response::builder()
            .status(200)
            .header("content-length", STYLE.len())
            .header("content-type", "text/css")
            .body(STYLE)
            .unwrap())
    });
    let index = (warp::path::end().or(path!("index.html")))
        .and(players.clone())
        .and_then(|_, players: Arc<RwLock<Players>>| async move {
            println!("I am doing index.");
            let p = players.read().await;
            let r: Result<warp::http::Response<warp::hyper::Body>,warp::Rejection> = Ok(display(HTML, &Index(&*p)).into_response());
            r
        });
    let sock = path!("abridge" / "ws" / String)
        .and(warp::ws())
        .and(players)
        .map(|seat: String, ws: warp::ws::Ws, players| {
            ws.on_upgrade(move |socket| editor_connected(seat, socket, players))
        });

    warp::serve(style_css.or(index))
        .run(([0, 0, 0, 0], 8087))
        .await;
}

#[derive(Default)]
struct Players {
    north: Option<mpsc::UnboundedSender<Result<warp::ws::Message, warp::Error>>>,
    south: Option<mpsc::UnboundedSender<Result<warp::ws::Message, warp::Error>>>,
    east: Option<mpsc::UnboundedSender<Result<warp::ws::Message, warp::Error>>>,
    west: Option<mpsc::UnboundedSender<Result<warp::ws::Message, warp::Error>>>,
    kibitzers: Vec<mpsc::UnboundedSender<Result<warp::ws::Message, warp::Error>>>,
}

async fn editor_connected(seat: String, ws: warp::ws::WebSocket, players: Arc<RwLock<Players>>) {
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
        e.kibitzers.push(tx);
        println!("got {} kibitzers now", e.kibitzers.len());
    }

    // Return a `Future` that is basically a state machine managing
    // this specific user's connection.

    // Make an extra clone to give to our disconnection handler...
    let players2 = players.clone();

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
        // process_message(&code, &character, msg, &editors).await;
    }

    // user_ws_rx stream will keep processing as long as the user stays
    // connected. Once they disconnect, then...
    //ws_disconnected(&players2).await;
}
struct Index<'a>(&'a Players);
#[with_template("[%" "%]" "index.html")]
impl<'a> DisplayAs<HTML> for Index<'a> {}
