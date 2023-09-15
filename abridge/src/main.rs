use clap::Parser;

#[tokio::main]
async fn main() {
    let config = abridge::Config::parse();
    abridge::serve_abridge(config).await;
}
