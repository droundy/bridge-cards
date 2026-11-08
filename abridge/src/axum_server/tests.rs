use super::*;
use crate::{close_idle_tables, WsSender, CLOSED_FOR_INACTIVITY};
use bridge_deck::{Cards, Suit};
use futures::{SinkExt, StreamExt};
use robot::{Action, Bid, GameState};
use std::future::IntoFuture;
use std::time::{Duration, Instant};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::sync::RwLock;
use tokio_tungstenite::tungstenite;

/// Serves the site on a free port.
async fn serve() -> (SocketAddr, AppState) {
    let (table_updates, _) = tokio::sync::broadcast::channel(1);
    let app = AppState {
        tables: TableMap::default(),
        table_updates,
    };
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(axum::serve(listener, router(app.clone())).into_future());
    (addr, app)
}

/// The head (status line and headers) and body of the response to a GET with
/// these extra header lines.
async fn get_with(addr: SocketAddr, path: &str, extra: &str) -> (String, String) {
    let mut tcp = tokio::net::TcpStream::connect(addr).await.unwrap();
    tcp.write_all(
        format!("GET {path} HTTP/1.1\r\nHost: localhost\r\n{extra}Connection: close\r\n\r\n")
            .as_bytes(),
    )
    .await
    .unwrap();
    let mut response = String::new();
    tcp.read_to_string(&mut response).await.unwrap();
    let (head, body) = response.split_once("\r\n\r\n").unwrap();
    (head.to_string(), body.to_string())
}

async fn get(addr: SocketAddr, path: &str) -> (String, String) {
    get_with(addr, path, "").await
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
        tokio::time::sleep(Duration::from_millis(10)).await;
    }
    false
}

fn header<'a>(head: &'a str, name: &str) -> Option<&'a str> {
    head.lines().find_map(|l| {
        let (n, v) = l.split_once(':')?;
        n.eq_ignore_ascii_case(name).then(|| v.trim())
    })
}

fn ws_url(addr: SocketAddr, path: &str) -> String {
    format!("ws://{addr}/{path}")
}

#[tokio::test]
async fn serves_the_pages() {
    let (addr, app) = serve().await;
    for path in ["/", "/index.html", "/anything/else/at/all"] {
        let (head, body) = get(addr, path).await;
        assert!(head.starts_with("HTTP/1.1 200"), "{path}: {head}");
        assert!(body.contains("New table"), "{path} is not the index");
    }
    let (head, _) = get(addr, "/").await;
    assert!(
        header(&head, "content-type").is_some_and(|t| t.starts_with("text/html")),
        "{head}"
    );
    let (head, _) = get(addr, "/style.css").await;
    assert_eq!(header(&head, "content-type"), Some("text/css"));

    let (head, body) = get(addr, "/a%20table/south").await;
    assert!(head.starts_with("HTTP/1.1 200"), "{head}");
    assert!(body.contains("/ws"), "this is not a player's page");
    let game = Arc::clone(&app.tables.entry("a table".to_string()));
    assert!(matches!(
        game.read().await.names[Seat::South],
        PlayerName::Human(_)
    ));

    let (head, _) = get(addr, "/a%20table/south/robot").await;
    assert!(head.starts_with("HTTP/1.1 200"), "{head}");
    // A bad seat is a 404, with a way back to the home page.
    for path in ["/a%20table/sideways", "/a%20table/sideways/robot"] {
        let (head, body) = get(addr, path).await;
        assert!(head.starts_with("HTTP/1.1 404"), "{path}: {head}");
        assert!(body.contains(r#"href="/""#), "{path} has no link home");
    }
    // Nor does a websocket to a bad seat get upgraded.
    for path in ["a%20table/sideways/ws", "a%20table/sideways/robot/ws"] {
        match tokio_tungstenite::connect_async(ws_url(addr, path)).await {
            Err(tungstenite::Error::Http(response)) => {
                assert_eq!(response.status(), 404, "{path}")
            }
            other => panic!("{path} should be refused, but got {other:?}"),
        }
    }
}

#[tokio::test]
async fn a_name_from_the_cookie_is_used() {
    let (addr, app) = serve().await;
    get_with(addr, "/t/north", "Cookie: name=Ada\r\n").await;
    let game = Arc::clone(&app.tables.entry("t".to_string()));
    let g = game.read().await;
    assert_eq!(g.names[Seat::North], PlayerName::Human("Ada".to_string()));
}

#[tokio::test]
async fn random_seats_redirect_to_a_free_seat() {
    let (addr, _) = serve().await;
    // A table with a funny name, which has to survive the round trip.
    let (head, _) = get(addr, "/caf%C3%A9%20%2B%20x/random").await;
    assert!(head.starts_with("HTTP/1.1 307"), "{head}");
    let location = header(&head, "location").unwrap();
    assert!(
        location.starts_with("/caf%C3%A9%20%2B%20x/"),
        "bad redirect {location}"
    );
    let (head, _) = get(addr, location).await;
    assert!(head.starts_with("HTTP/1.1 200"), "{head}");

    // Now that there is a table with room, /random finds it.
    let (head, _) = get(addr, "/random").await;
    assert!(head.starts_with("HTTP/1.1 307"), "{head}");
    assert!(header(&head, "location")
        .unwrap()
        .starts_with("/caf%C3%A9%20%2B%20x/"));
}

#[tokio::test]
async fn the_index_long_poll_waits_for_a_change() {
    let (addr, _app) = serve().await;
    let poll = tokio::spawn(async move { get(addr, "/index.html?update=true").await });
    tokio::time::sleep(Duration::from_millis(200)).await;
    assert!(!poll.is_finished(), "the long poll returned immediately");
    // Sitting down changes the tables.
    let (mut south, _) = tokio_tungstenite::connect_async(ws_url(addr, "t/south/ws"))
        .await
        .unwrap();
    south.next().await.unwrap().unwrap();
    let (head, _) = tokio::time::timeout(Duration::from_secs(5), poll)
        .await
        .expect("the long poll never returned")
        .unwrap();
    assert!(head.starts_with("HTTP/1.1 200"), "{head}");
}

#[tokio::test]
async fn a_websocket_sits_and_gives_up_its_seat() {
    let (addr, app) = serve().await;
    let game = Arc::clone(&app.tables.entry("t".to_string()));
    let (mut south, _) = tokio_tungstenite::connect_async(ws_url(addr, "t/south/ws"))
        .await
        .unwrap();
    let first = south.next().await.unwrap().unwrap();
    assert!(first.is_text(), "expected the page, got {first:?}");
    assert!(eventually(&game, |g| g.connections[Seat::South].is_some()).await);

    // Pings are not actions, and neither is junk.
    south
        .send(tungstenite::Message::Ping(vec![1]))
        .await
        .unwrap();
    south
        .send(tungstenite::Message::text("not json"))
        .await
        .unwrap();
    // The connection survives, and a real action still works.
    south
        .send(tungstenite::Message::text(
            serde_json::to_string(&Action::ToggleCountForMe).unwrap(),
        ))
        .await
        .unwrap();
    assert!(eventually(&game, |g| g.count_for_me[Seat::South]).await);

    // Hang up without the closing handshake, as when a connection drops.
    drop(south);
    assert!(
        eventually(&game, |g| g.connections[Seat::South].is_none()).await,
        "a dead connection kept its seat"
    );
}

#[tokio::test]
async fn closing_an_old_connection_keeps_the_new_one() {
    let (addr, app) = serve().await;
    let game = Arc::clone(&app.tables.entry("t".to_string()));
    for path in ["t/south/ws", "t/south/robot/ws"] {
        let (mut old, _) = tokio_tungstenite::connect_async(ws_url(addr, path))
            .await
            .unwrap();
        old.next().await.unwrap().unwrap();
        let (mut new, _) = tokio_tungstenite::connect_async(ws_url(addr, path))
            .await
            .unwrap();
        new.next().await.unwrap().unwrap();
        old.close(None).await.unwrap();
        tokio::time::sleep(Duration::from_millis(100)).await;
        let g = game.read().await;
        if path.contains("robot") {
            assert!(g.ai.is_some(), "the robot lost its seat");
        } else {
            assert!(g.connections[Seat::South].is_some(), "South lost its seat");
        }
        drop(g);
        drop(new);
    }
}

#[tokio::test]
async fn an_idle_table_tells_its_tabs_why_it_closed() {
    let (addr, app) = serve().await;
    let (mut south, _) = tokio_tungstenite::connect_async(ws_url(addr, "t/south/ws"))
        .await
        .unwrap();
    south.next().await.unwrap().unwrap();

    close_idle_tables(&app.tables, Instant::now() + Duration::from_secs(59 * 60)).await;
    assert!(app.tables.get("t").is_some(), "a table closed while in use");
    close_idle_tables(&app.tables, Instant::now() + Duration::from_secs(61 * 60)).await;
    assert!(app.tables.get("t").is_none(), "an idle table stayed open");
    match south.next().await.unwrap().unwrap() {
        tungstenite::Message::Close(Some(frame)) => {
            assert_eq!(u16::from(frame.code), CLOSED_FOR_INACTIVITY)
        }
        m => panic!("expected the table to close, but got {m:?}"),
    }
}

type Client =
    tokio_tungstenite::WebSocketStream<tokio_tungstenite::MaybeTlsStream<tokio::net::TcpStream>>;

/// Connects to the websocket at `path`, and waits for the first thing it sends.
async fn connect(addr: SocketAddr, path: &str) -> Client {
    let (mut client, _) = tokio_tungstenite::connect_async(ws_url(addr, path))
        .await
        .unwrap();
    recv(&mut client).await;
    client
}

/// The next message to `client`, which has to arrive within a few seconds.
async fn recv(client: &mut Client) -> tungstenite::Message {
    tokio::time::timeout(Duration::from_secs(5), client.next())
        .await
        .expect("the server sent nothing")
        .expect("the connection closed")
        .unwrap()
}

async fn send(client: &mut Client, action: &Action) {
    client
        .send(tungstenite::Message::text(
            serde_json::to_string(action).unwrap(),
        ))
        .await
        .unwrap();
}

// In the tests below, every action sends South an update once the server has
// handled it, so South's `recv` waits for that.

#[tokio::test]
async fn an_empty_table_waits_for_its_players_to_come_back() {
    let (addr, app) = serve().await;
    let game = Arc::clone(&app.tables.entry("t".to_string()));
    let mut south = connect(addr, "t/south/ws").await;
    south.close(None).await.unwrap();
    assert!(eventually(&game, |g| g.connections[Seat::South].is_none()).await);

    close_idle_tables(&app.tables, Instant::now() + Duration::from_mins(9)).await;
    assert!(
        app.tables.get("t").is_some(),
        "an empty table closed too soon"
    );
    close_idle_tables(&app.tables, Instant::now() + Duration::from_mins(11)).await;
    assert!(app.tables.get("t").is_none(), "an empty table stayed open");
}

#[tokio::test]
async fn playing_after_a_long_pause_keeps_the_table() {
    let (addr, app) = serve().await;
    let game = Arc::clone(&app.tables.entry("t".to_string()));
    let mut south = connect(addr, "t/south/ws").await;
    // A clock that started under two hours ago can't go back that far.
    let Some(long_ago) = Instant::now().checked_sub(Duration::from_hours(2)) else {
        return;
    };
    game.write().await.last_action = Some(long_ago);

    send(&mut south, &Action::ToggleCountForMe).await;
    assert!(
        eventually(&game, |g| g.count_for_me[Seat::South]).await,
        "the table was reset"
    );
    assert!(game.read().await.connections[Seat::South].is_some());
}

#[tokio::test]
async fn players_act_only_for_their_own_seat() {
    let (addr, app) = serve().await;
    let game: Arc<RwLock<GameState<WsSender>>> = Arc::clone(&app.tables.entry("t".to_string()));
    let mut south = connect(addr, "t/south/ws").await;

    // South passes, and then tries to bid for West too.
    send(&mut south, &Action::Bid(Seat::South, Bid::Pass)).await;
    recv(&mut south).await;
    send(
        &mut south,
        &Action::Bid(Seat::West, Bid::Suit(1, Suit::Clubs)),
    )
    .await;
    recv(&mut south).await;
    assert_eq!(vec![Bid::Pass], game.read().await.bids);

    // South declares 1NT, so West leads, and South can't lead for West.
    let west_card = {
        let mut g = game.write().await;
        g.bids = vec![Bid::NT(1), Bid::Pass, Bid::Pass, Bid::Pass];
        g.lead = Some(Seat::West);
        g.hands[Seat::West].min().unwrap()
    };
    send(&mut south, &Action::Play(Seat::West, west_card)).await;
    recv(&mut south).await;
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
    recv(&mut south).await;
    assert_eq!(vec![west_card, north_card], game.read().await.played);
}

#[tokio::test]
async fn only_legal_bids_count() {
    let (addr, app) = serve().await;
    let game = Arc::clone(&app.tables.entry("t".to_string()));
    let mut south = connect(addr, "t/south/ws").await;

    // South opens the bidding, so there is nothing to redouble, and there is
    // no eighth level.
    send(&mut south, &Action::Bid(Seat::South, Bid::Redouble)).await;
    recv(&mut south).await;
    send(
        &mut south,
        &Action::Bid(Seat::South, Bid::Suit(8, Suit::Clubs)),
    )
    .await;
    recv(&mut south).await;
    assert!(game.read().await.bids.is_empty());

    // East declares 1NT, and South, on lead, can't reopen the bidding.
    {
        let mut g = game.write().await;
        g.dealer = Seat::East;
        g.bids = vec![Bid::NT(1), Bid::Pass, Bid::Pass, Bid::Pass];
        g.lead = Some(Seat::South);
    }
    send(&mut south, &Action::Bid(Seat::South, Bid::NT(7))).await;
    recv(&mut south).await;
    assert_eq!(4, game.read().await.bids.len());
}

#[tokio::test]
async fn the_robot_acts_only_for_seats_nobody_is_at() {
    let (addr, app) = serve().await;
    let game = Arc::clone(&app.tables.entry("t".to_string()));
    let mut robot = connect(addr, "t/south/robot/ws").await;
    let mut south = connect(addr, "t/south/ws").await;

    // South is here, so the robot can't bid for South.
    send(&mut robot, &Action::Bid(Seat::South, Bid::Pass)).await;
    recv(&mut south).await;
    assert!(game.read().await.bids.is_empty());

    // Nobody is at West, so the robot bids for West.
    send(&mut south, &Action::Bid(Seat::South, Bid::Pass)).await;
    recv(&mut south).await;
    send(&mut robot, &Action::Bid(Seat::West, Bid::Pass)).await;
    recv(&mut south).await;
    assert_eq!(vec![Bid::Pass, Bid::Pass], game.read().await.bids);
}

#[tokio::test]
async fn a_seat_whose_page_never_connected_is_shown_as_a_robot() {
    let (addr, app) = serve().await;
    let game = Arc::clone(&app.tables.entry("t".to_string()));
    // West's page is loaded, but its websocket never connects.
    get(addr, "/t/west").await;
    assert!(matches!(
        game.read().await.names[Seat::West],
        PlayerName::Human(_)
    ));
    let _robot = connect(addr, "t/south/robot/ws").await;
    let mut south = connect(addr, "t/south/ws").await;
    send(&mut south, &Action::ToggleCountForMe).await;
    recv(&mut south).await;
    // The robot plays West, so West is named as a robot.
    let g = game.read().await;
    assert!(
        matches!(g.names[Seat::West], PlayerName::Robot(_)),
        "{:?}",
        g.names[Seat::West]
    );
    assert!(matches!(g.names[Seat::South], PlayerName::Human(_)));
}

#[tokio::test]
async fn a_websocket_takes_its_seat_with_its_name() {
    use tungstenite::client::IntoClientRequest;
    let (addr, app) = serve().await;
    let game = Arc::clone(&app.tables.entry("t".to_string()));
    // With a name cookie, that name.
    let mut request = ws_url(addr, "t/south/ws").into_client_request().unwrap();
    request
        .headers_mut()
        .insert("Cookie", "name=Ada".parse().unwrap());
    let (mut south, _) = tokio_tungstenite::connect_async(request).await.unwrap();
    recv(&mut south).await;
    assert_eq!(
        PlayerName::Human("Ada".to_string()),
        game.read().await.names[Seat::South]
    );
    // Without one, the name the page showed.
    get(addr, "/t/east").await;
    let shown = game.read().await.names[Seat::East].clone();
    let _east = connect(addr, "t/east/ws").await;
    assert_eq!(shown, game.read().await.names[Seat::East]);
}
