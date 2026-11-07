use clap::Parser;

#[tokio::main]
async fn main() {
    let config = abridge::AxumConfig::parse();
    abridge::serve_abridge_axum(config).await;
}
