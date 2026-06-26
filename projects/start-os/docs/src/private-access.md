# Private Access

Private connections keep your services accessible only to you and your authorized devices — nothing is exposed to the public Internet. Most people will use private connections for most of their services most of the time. Unless you are intentionally hosting something for others (a website, a blog, a Lightning node), your services should be privately accessible only to you and your trusted devices.

## LAN, VPN, and Tor

- **[LAN](lan.md)** — The fastest option. Traffic stays on your local network and never reaches the Internet. Requires being connected to the same network as your server and [trusting your Root CA](trust-ca.md).

- **[VPN](inbound-vpn.md)** — Extends your local network to anywhere in the world. Authorized devices get a secure tunnel to your server as if they were on the same LAN. Requires either a router with VPN server capability or [StartTunnel](/start-tunnel/), and [trusting your Root CA](trust-ca.md).

- **[Tor](tor.md)** — Anonymous, censorship-resistant access using `.onion` addresses. Works from anywhere without any router configuration or Root CA trust. Connections are slower and occasionally unreliable. Requires installing the **Tor** service from the marketplace.

## Common Combinations

These are not mutually exclusive. A common setup is:

- **LAN** for daily use at home
- **VPN** for remote access from your own devices
- **Tor** for access from untrusted networks or when anonymity matters
