# Devices

Every device on a StartTunnel subnet gets its own WireGuard configuration. Devices come in two kinds, listed in separate tables on the `Devices` page:

- **Servers** — a StartOS server that may configure the gateway on its own behalf: injecting DNS records for its private domains and auto-creating port forwards (PCP/UPnP). Both capabilities are on by default for a Server.
- **Clients** — a plain peer such as a phone or laptop that uses the tunnel for connectivity only, with no gateway-configuration abilities.

## Adding a Device

1. In StartTunnel, navigate to `Devices` and click "Add".

1. Fill out the form:
   - Give the device a name (e.g. "Start9 Server", "Phone", "Laptop").
   - Choose the **kind**: **Server** for a StartOS box, **Client** for a phone or laptop.
   - For a Server, **Allow DNS injection** and **Allow auto port forwarding** are enabled by default; uncheck either to withhold that capability. A Client has neither.
   - Select a subnet (the default is fine).
   - Accept or choose an IP address on the subnet (the default is fine).
   - Click "Save".

1. Download the resulting `start-tunnel.conf` (or copy to your clipboard).

1. Import the config into the appropriate app on the device:
   - **StartOS server**: Navigate to `System > Gateways`, click "Add", name the gateway (e.g. "StartTunnel"), upload or paste the config, and click "Save". StartOS will now see the VPS as a gateway, and each service interface will automatically acquire new addresses corresponding to it.
   - **Phone or tablet**: Scan the QR code shown in StartTunnel using the [WireGuard app](https://www.wireguard.com/install/).
   - **Laptop or desktop**: Download the config and import it into the [WireGuard app](https://www.wireguard.com/install/).

## Server capabilities

A Server has two independently-toggleable capabilities, shown as switches in the Servers table:

- **DNS injection** — lets the server manage the DNS records StartTunnel serves for your private domains (see [DNS Records](/start-tunnel/dns-records.html)).
- **Auto port forwarding** — lets the server create its own port forwards via PCP/UPnP (see [Port Forwarding](/start-tunnel/port-forwarding.html)).

Only enable these for servers you trust. Clients have neither capability.

## Promoting and demoting

Use a device's actions menu to **Promote to Server** or **Demote to Client**. Promoting turns both Server capabilities on; demoting turns them off and moves the device to the Clients table.

## Removing a Device

1. Navigate to `Devices`, select the device, and click "Remove".
