# Private Domains

A private domain works like your server's [mDNS address](mdns.md), except it also works over [VPN](inbound-vpn.md) and it can be _anything_. It can be a real domain you control, a made up domain, or even a domain controlled by someone else.

Private domains can only be added to wired (Ethernet) or wireless (WiFi) gateways — not WireGuard (StartTunnel) gateways. They can only be accessed when connected to the same LAN as your server, either physically or via VPN, and they require trusting your server's Root CA.

## Adding a Private Domain

> [!WARNING]
> Private domains can only be added to **Ethernet** and **Wireless** gateways. They cannot be added to WireGuard (StartTunnel) gateways. This is because private domains rely on your local network's DNS configuration, which WireGuard gateways do not control.

1. If you haven't already, assign a static IP address to your server on the LAN. Refer to your router's user manual for detailed instructions.

1. On the service interface page, click "Add Domain" on the desired gateway table and select "Private Domain".

1. Enter a fully qualified domain name. It can be _anything_. For example: `domain.com`, `private.domain.internal`, `nextcloud.private`, `nextcloud.fake-tld`, or `facebook.com`.

1. Click "Save".

1. StartOS will automatically test your DNS configuration. If the test passes, the domain is ready to use. If it fails, a setup modal will appear with instructions to configure your DNS server and the ability to re-test.

## DNS for Private Domains

Private domains require your gateway to use StartOS for DNS. StartOS will test this automatically when you add a private domain and guide you through the setup if needed.

Set StartOS as your router's primary DNS server. All routers support this feature. Refer to your router's user manual for detailed instructions.

> [!WARNING]
> It is possible that StartOS is already using your router for DNS. In this case, you cannot instruct your router to use StartOS for DNS, as this would be circular. If StartOS detects a potential circular DNS situation, it will warn you. To resolve this, switch to [static DNS servers](dns.md#static-dns-servers) so StartOS no longer relies on your router.

> [!TIP]
> If your private domain is a real domain that you control, you can alternatively configure its DNS record at your registrar to resolve to your server's _LAN IP address_. In this case, the StartOS DNS server is not needed.

## Split DNS: the Same Domain, Public and Private

You can add the _same_ domain as both a [clearnet](clearnet.md) (public) domain on a StartTunnel gateway and a private domain on an Ethernet or WiFi gateway. StartOS serves it as split DNS:

- When you are on your LAN or connected over [VPN](inbound-vpn.md), StartOS resolves the domain to your server's local IP address, so traffic stays on your network at full LAN speed.
- When you are away, the same domain resolves through public DNS to your StartTunnel gateway, so the service is reachable over the internet.

It's the same domain and the same TLS certificate either way, with no [hairpin routing](https://en.wikipedia.org/wiki/Hairpinning) (LAN traffic looping out to the gateway and back).

> [!TIP]
> This is especially useful for services that embed their access URL in generated links, such as Nextcloud or Immich share links. Configure the service with the public domain so the links work for external recipients, and you'll still get direct LAN-speed access when you're home.
