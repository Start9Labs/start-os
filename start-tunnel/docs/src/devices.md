# Devices

Every device on a StartTunnel subnet — whether it's a server, phone, or laptop — is added the same way. Each device gets a unique WireGuard configuration file.

## Adding a Device

1. In StartTunnel, navigate to `Devices` and click "Add".

1. Fill out the form:
   - Give the device a name (e.g. "Start9 Server", "Phone", "Laptop").
   - Select a subnet (the default is fine).
   - Accept or choose an IP address on the subnet (the default is fine).
   - Click "Save".

1. Download the resulting `start-tunnel.conf` (or copy to your clipboard).

1. Import the config into the appropriate app on the device:
   - **StartOS server**: Navigate to `System > Gateways`, click "Add", name the gateway (e.g. "StartTunnel"), upload or paste the config, and click "Save". StartOS will now see the VPS as a gateway, and each service interface will automatically acquire new addresses corresponding to it.
   - **Phone or tablet**: Scan the QR code shown in StartTunnel using the [WireGuard app](https://www.wireguard.com/install/).
   - **Laptop or desktop**: Download the config and import it into the [WireGuard app](https://www.wireguard.com/install/).

## Removing a Device

1. Navigate to `Devices`, select the device, and click "Remove".
