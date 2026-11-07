# abridge

This is going to be a pretty simple online bridge
game. I have no plans to implement partner finding
at all, so you'll need to hand your own friends.

It has a pretty crude AI to play against and can also give bidding advice based on a fixed bidding convention.

## Getting started with development:

```
git clone git@github.com:droundy/bridge-cards
cd bridge-cards/abridge
cargo run --release
```

And then point your browser at
[http://localhost:8087/random](http://localhost:8087/random) and you should be
able to play on your local version of the code.

## Two servers

`abridge` is served with warp, and gets its certificate from the blocking
`lets-encrypt-warp`.  `abridge-axum` is the same site served with axum, which
gets its certificate from `rustls-acme`.  They share the game, the templates
and the websocket logic, so they differ only in routing and TLS.

```
cargo run --release --bin abridge-axum -- --port 8088
```

With `--domain` and `--email` it serves https on port 443, and redirects port 80
to it.  The certificate is validated on port 443 itself (TLS-ALPN-01), so unlike
`lets-encrypt-warp` it needs no web server on port 80 for the validation, and
renewal needs no restart.  Certificates and the account key are kept in
`--cache-dir` (`./acme-cache`), so that a restart doesn't ask Let's Encrypt for
a new one.

By default it gets certificates from Let's Encrypt's staging server, which
browsers do not trust but which has generous rate limits.  Once that works,
add `--production`.  It listens on IPv4 only unless you give `--bind ::`, so
do that if the domain has an AAAA record.