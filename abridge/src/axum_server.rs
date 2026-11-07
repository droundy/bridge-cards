//! The routes of the site, and serving them, with the certificate for
//! `--domain` obtained and renewed by `rustls-acme`.

use super::{
    clean_tables, random_name, ws_connected, Index, Player, PlayerPage, RobotPage, Room, TableMap,
    TableToJoin,
};
use axum::extract::ws::WebSocketUpgrade;
use axum::extract::{Path, RawQuery, State};
use axum::http::{header, HeaderValue, StatusCode};
use axum::response::{Html, IntoResponse, Response};
use axum::routing::get;
use axum::Router;
use axum_extra::extract::CookieJar;
use display_as::{display, HTML};
use futures::StreamExt;
use robot::{PlayerName, Seat};
use rustls_acme::caches::DirCache;
use rustls_acme::AcmeConfig;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::broadcast::Sender;

/// The configuration for running the server
#[derive(clap::Parser)]
pub struct Config {
    /// The domain to serve over https, using a certificate from let's encrypt.
    /// This serves https on port 443, and redirects port 80 to it.
    #[arg(long, requires = "email")]
    domain: Option<String>,
    /// The email associated with the domain
    #[arg(long)]
    email: Option<String>,
    /// Get the certificate from let's encrypt's staging server, which browsers
    /// do not trust, but which has generous rate limits for testing.
    #[arg(long)]
    staging: bool,
    /// Where to keep the certificate and account key, so that they survive a
    /// restart.  Let's encrypt limits how often we may ask for a certificate.
    #[arg(long, default_value = "acme-cache")]
    cache_dir: PathBuf,
    /// The port (if not using a domain)
    #[arg(long, default_value = "8087")]
    port: u16,
    /// The address to listen on
    #[arg(long, default_value_t = IpAddr::V4(Ipv4Addr::UNSPECIFIED))]
    bind: IpAddr,
}

#[cfg(test)]
mod tests;

#[derive(Clone)]
struct AppState {
    tables: TableMap,
    table_updates: Sender<()>,
}

/// Redirect (temporarily) to a path on this site, in which `segments` are
/// percent-encoded, since a table's name can be anything.
fn redirect_to(segments: &[&str]) -> Response {
    use percent_encoding::{utf8_percent_encode, NON_ALPHANUMERIC};
    let mut uri = String::new();
    for s in segments {
        uri.push('/');
        uri.extend(utf8_percent_encode(s, NON_ALPHANUMERIC));
    }
    let location = HeaderValue::try_from(uri).expect("a percent-encoded path is a valid header");
    (StatusCode::TEMPORARY_REDIRECT, [(header::LOCATION, location)]).into_response()
}

/// The response when a path doesn't name anything: a page that links back to
/// the home page.
struct NotFound;

impl IntoResponse for NotFound {
    fn into_response(self) -> Response {
        const PAGE: &str = r#"<!DOCTYPE html>
<html>

<head>
    <title>Not found</title>
    <link rel="stylesheet" href="/style.css">
    <meta name="viewport" content="width=device-width, initial-scale=1">
</head>

<body>
    <p>There is nothing here.</p>
    <p><a href="/">Back to the home page</a></p>
</body>

</html>
"#;
        (StatusCode::NOT_FOUND, Html(PAGE)).into_response()
    }
}

/// The seat that `/<table>/<seat>` names.
fn parse_seat(seat: &str) -> Result<Seat, NotFound> {
    seat.parse().map_err(|_| NotFound)
}

fn router(app: AppState) -> Router {
    Router::new()
        .route("/", get(index))
        .route("/index.html", get(index))
        .route("/style.css", get(style_css))
        .route("/your-play.mp3", get(audio))
        .route("/robot.js", get(robot_js))
        .route("/robot_bg.wasm", get(robot_bg_wasm))
        .route("/random", get(random_seat))
        .route("/{table}/random", get(table_random_seat))
        .route("/{table}/{seat}", get(table_seat))
        .route("/{table}/{seat}/robot", get(table_robot_tab))
        .route("/{table}/{seat}/ws", get(table_sock))
        .route("/{table}/{seat}/robot/ws", get(table_robot_sock))
        // Show the list of tables for any other path, as the site always has.
        .fallback(get(index))
        .with_state(app)
}

async fn style_css() -> impl IntoResponse {
    ([(header::CONTENT_TYPE, "text/css")], include_str!("style.css"))
}

async fn audio() -> impl IntoResponse {
    (
        [(header::CONTENT_TYPE, "audio/mpeg")],
        include_bytes!("your-play.mp3").as_slice(),
    )
}

async fn robot_js() -> impl IntoResponse {
    (
        [(header::CONTENT_TYPE, "text/javascript")],
        include_bytes!("../../robot/pkg/robot.js").as_slice(),
    )
}

async fn robot_bg_wasm() -> impl IntoResponse {
    (
        [(header::CONTENT_TYPE, "application/wasm")],
        include_bytes!("../../robot/pkg/robot_bg.wasm").as_slice(),
    )
}

/// The tables that are open right now.
async fn room(tables: &TableMap) -> Room {
    // Don't hold the map's lock while waiting for the tables'.
    let games = tables
        .0
        .iter()
        .map(|t| (t.key().clone(), Arc::clone(t.value())))
        .collect::<Vec<_>>();
    let mut room = Room { tables: Vec::new() };
    for (name, game) in games {
        room.tables.push(TableToJoin::new(name, &*game.read().await));
    }
    room
}

async fn index(State(app): State<AppState>, RawQuery(query): RawQuery) -> Response {
    if query.as_deref().map_or(true, str::is_empty) {
        display(
            HTML,
            &Index {
                room: room(&app.tables).await,
            },
        )
        .into_response()
    } else {
        // The page asks for this to learn about the next change to a table.
        println!("Someone is long polling");
        app.table_updates.subscribe().recv().await.ok();
        println!("Got a change.");
        display(HTML, &room(&app.tables).await).into_response()
    }
}

async fn table_seat(
    State(app): State<AppState>,
    Path((table, seat)): Path<(String, String)>,
    jar: CookieJar,
) -> Result<Response, NotFound> {
    let seat = parse_seat(&seat)?;
    let game = Arc::clone(&app.tables.entry(table));
    let mut g = game.write().await;
    g.touch();
    g.names[seat] = PlayerName::Human(
        jar.get("name")
            .map_or_else(random_name, |c| c.value().to_string()),
    );
    Ok(display(HTML, &PlayerPage(Player { seat, game: &g })).into_response())
}

async fn table_robot_tab(
    Path((_table, seat)): Path<(String, String)>,
) -> Result<Response, NotFound> {
    parse_seat(&seat)?;
    Ok(display(HTML, &RobotPage).into_response())
}

async fn random_seat(State(app): State<AppState>) -> Response {
    let games = app
        .tables
        .0
        .iter()
        .map(|t| (t.key().clone(), Arc::clone(t.value())))
        .collect::<Vec<_>>();
    for (name, game) in games {
        if let Some(seat) = game.read().await.randomseat() {
            return redirect_to(&[&name, seat.long_name()]);
        }
    }
    redirect_to(&[&random_name(), "random"])
}

async fn table_random_seat(State(app): State<AppState>, Path(table): Path<String>) -> Response {
    println!("random sitting in {table}");
    let game = Arc::clone(&app.tables.entry(table.clone()));
    let seat = game.read().await.randomseat();
    match seat {
        Some(seat) => redirect_to(&[&table, seat.long_name()]),
        None => redirect_to(&[]),
    }
}

async fn table_sock(
    State(app): State<AppState>,
    Path((table, seat)): Path<(String, String)>,
    ws: WebSocketUpgrade,
) -> Result<Response, NotFound> {
    let seat = parse_seat(&seat)?;
    let game = Arc::clone(&app.tables.entry(table));
    Ok(ws.on_upgrade(move |socket| ws_connected(Some(seat), socket, game, app.table_updates)))
}

async fn table_robot_sock(
    State(app): State<AppState>,
    Path((table, seat)): Path<(String, String)>,
    ws: WebSocketUpgrade,
) -> Result<Response, NotFound> {
    parse_seat(&seat)?;
    let game = Arc::clone(&app.tables.entry(table));
    Ok(ws.on_upgrade(move |socket| ws_connected(None, socket, game, app.table_updates)))
}

/// Sends everything on port 80 to the same path on `domain` over https.
async fn redirect_to_https(domain: String, bind: IpAddr) {
    let app = Router::new().fallback(move |uri: axum::http::Uri| async move {
        let path = uri.path_and_query().map_or("/", |p| p.as_str());
        axum::response::Redirect::permanent(&format!("https://{domain}{path}"))
    });
    let listener = match tokio::net::TcpListener::bind((bind, 80)).await {
        Ok(l) => l,
        Err(e) => return eprintln!("Unable to serve redirects on port 80: {e}"),
    };
    println!("Redirecting port 80 to https");
    if let Err(e) = axum::serve(listener, app).await {
        eprintln!("The server on port 80 failed: {e}");
    }
}

pub async fn serve_abridge(config: Config) {
    let (table_updates, _) = tokio::sync::broadcast::channel::<()>(1);
    let tables = TableMap::default();
    tokio::spawn(clean_tables(tables.clone()));
    let app = router(AppState {
        tables,
        table_updates,
    });

    let Some(domain) = config.domain else {
        let listener = tokio::net::TcpListener::bind((config.bind, config.port))
            .await
            .expect("unable to listen");
        axum::serve(listener, app).await.unwrap();
        return;
    };

    let email = config.email.expect("clap requires an email with a domain");
    println!(
        "Using let's encrypt{} for {domain}...",
        if config.staging { " staging" } else { "" }
    );
    let mut acme = AcmeConfig::new([&domain])
        .contact_push(format!("mailto:{email}"))
        .cache(DirCache::new(config.cache_dir))
        .directory_lets_encrypt(!config.staging)
        .state();
    let acceptor = acme.axum_acceptor(acme.default_rustls_config());
    // The certificate is obtained and renewed as this stream is polled.  The
    // certificates are validated on port 443 itself (TLS-ALPN-01), and the
    // acceptor answers those requests without bothering the site.
    tokio::spawn(async move {
        while let Some(event) = acme.next().await {
            match event {
                Ok(ok) => println!("acme event: {ok:?}"),
                Err(err) => eprintln!("acme error: {err:?}"),
            }
        }
    });
    tokio::spawn(redirect_to_https(domain, config.bind));

    axum_server::bind(SocketAddr::new(config.bind, 443))
        .acceptor(acceptor)
        .serve(app.into_make_service())
        .await
        .unwrap();
}
