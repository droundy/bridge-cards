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

## Serving over https

With `--domain` and `--email` the server serves https on port 443, and
redirects port 80 to it.  The certificate comes from Let's Encrypt, and
`rustls-acme` obtains and renews it.  It is validated on port 443 itself
(TLS-ALPN-01), so no web server is needed on port 80 for that, and renewing
needs no restart.  Certificates and the account key are kept in `--cache-dir`
(`./acme-cache`), so that a restart doesn't ask Let's Encrypt for a new one.

Add `--staging` to get certificates from Let's Encrypt's staging server, which
browsers do not trust but which has generous rate limits, for testing.  The
server listens on IPv4 only unless you give `--bind ::`, so do that if the
domain has an AAAA record.
