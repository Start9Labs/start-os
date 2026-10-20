# DNS

StartOS resolves domain names through its own DNS server. The OS and services query this local server, which chooses upstream DNS servers according to the requester's outbound gateway.

## WATCH THE VIDEO

<div class="yt-video" data-id="_vnAqNTaBwM" data-title="DNS"></div>

## Gateway DNS

Each gateway has its own DNS settings. Navigate to `System > DNS` and select a gateway to view its effective servers and their source. You can also choose "DNS Servers" from a gateway's `⋮` menu under `System > Gateways`.

In **Auto** mode, StartOS uses DNS servers supplied by that gateway: [DHCP](https://en.wikipedia.org/wiki/Dynamic_Host_Configuration_Protocol) for a physical connection, or the WireGuard configuration for a tunnel.

> [!WARNING]
> When a gateway supplies no DNS servers, StartOS uses **Cloudflare's public resolvers at `1.1.1.1:53` and `[2606:4700:4700::1111]:53`**. The DNS page explicitly labels this "Public fallback (Cloudflare)". These queries travel through the selected gateway, including when that gateway is a VPN.

A service's explicit [outbound gateway](outbound-vpn.md#route-individual-services-through-vpn) takes precedence over the system-wide setting. Services without an override inherit the system-wide setting, as does the OS itself. With the system set to **Auto**, StartOS selects the physical connection with the preferred default route, preferring IPv4 and using IPv6 when no physical IPv4 default route is available. A disconnected selected gateway blocks its assigned DNS queries.

Host lookups of configured WireGuard endpoint hostnames use the automatic physical connection's DNS settings, allowing tunnels to reconnect. This exception applies to those exact endpoint names; service-originated lookups still follow the service's outbound gateway.

## Static DNS Servers

Under `System > DNS`, select the gateway, choose **Static**, and provide one to three DNS servers. Each address may include a port; the default is `53`. For IPv6 with a port, use brackets, for example `[2606:4700:4700::1111]:53`. Click "Save" to apply the override to that gateway. Choose **Auto** and save to restore gateway-provided DNS or the public fallback.

DNS overrides remain saved while a gateway is disconnected. When upgrading from system-wide static DNS settings, StartOS applies those servers to every known gateway that does not already have its own override. Newly added gateways start in Auto mode.

> [!NOTE]
> If you want every device on your network to use a specific DNS provider, configure it in your router. A gateway's settings in StartOS affect requests routed through that gateway by StartOS and its services.

## Private Domains

StartOS runs its own DNS server to resolve [private domains](private-domains.md) on your network. For setup details, see [DNS for Private Domains](private-domains.md#dns-for-private-domains).
