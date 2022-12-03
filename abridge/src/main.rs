#[tokio::main]
async fn main() {
    let router = abridge::serve_abridge("abridge").await;
    // Typical hyper setup...

    // run it with hyper on localhost:3000
    axum::Server::bind(&"0.0.0.0:8087".parse().unwrap())
        .serve(router.into_make_service())
        .await
        .unwrap();
}
