# Remote Access

Remote connections let you reach your server from outside your local network — when traveling, at work, or on any other network. StartOS supports several strategies for remote access, each with different tradeoffs for speed, privacy, and setup complexity. These strategies will not work from the same LAN as your server; for that, see [Local Access](lan.md).

## VPN, Tor, and Clearnet

- **[VPN](inbound-vpn.md)** — Extends your local network to anywhere in the world. Your device gets a secure tunnel to your server as if it were on the same LAN. Requires either a router with VPN server capability or a [StartTunnel](/start-tunnel/) gateway, and [trusting your Root CA](trust-ca.md).

- **[Tor](tor.md)** — Access your server via `.onion` addresses from anywhere. No router configuration, DNS, or Root CA trust required. Connections are slower and occasionally unreliable. Requires installing the **Tor** service from the marketplace.

- **[Clearnet](clearnet.md)** — Standard domain-based access on the public Internet. Requires a [gateway](gateways.md), a domain name, and DNS configuration. Typically used for [public](public-access.md) hosting, but can be combined with authentication for private use.
