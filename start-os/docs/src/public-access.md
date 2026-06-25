# Public Access

Public connections make your services reachable by anyone on the Internet, not just your own devices. This is for services you intentionally want to share — a personal website, a Nostr relay, a Lightning node, or a Nextcloud instance for your family. If only you and your own devices need access, use [private](private-access.md) connections instead.

> [!WARNING]
> If your ISP uses [CGNAT](cgnat.md), your router cannot accept inbound connections. Clearnet hosting and public IP access require a [StartTunnel](/start-tunnel/) gateway. Tor works regardless of CGNAT.

## Clearnet, Tor, and Public IP

- **[Clearnet](clearnet.md)** — Host services on the public Internet using standard domains (`.com`, `.net`, etc.). Requires [gateway](gateways.md) selection, a domain name, DNS configuration, and port forwarding. Anyone can access your service using a normal browser.

- **[Tor](tor.md)** — Host services as `.onion` addresses on the Tor network. No domain, DNS, or port forwarding required. Anyone with a Tor-enabled browser can reach your service. Tor hosting is censorship-resistant and can be anonymous if you don't associate the address with your identity. Requires installing the **Tor** service from the marketplace.

- **[Public IP](public-ip.md)** — Expose a service interface directly using a gateway's IP address and port, without a domain name. Useful for peer-to-peer protocols like Bitcoin P2P and Lightning that communicate using raw addresses. Not suitable for browser-based access, since Let's Encrypt does not sign certificates for IP addresses.

> [!TIP]
> Tor is listed under both [private](private-access.md) and public access because the difference is simply whether you share the `.onion` address. Keep it secret and it's a private tunnel to your service. Publish it and it becomes a public endpoint — no configuration change needed.

## Clearnet and Tor Together

You can host the same service on both clearnet and Tor simultaneously. This is useful when you want a standard domain for everyday access but also want a censorship-resistant fallback that doesn't depend on DNS or your gateway.
