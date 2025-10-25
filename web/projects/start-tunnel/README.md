# StartTunnel

StartTunnel is a self-hosted Wiregaurd VPN optimized for reverse tunneling to personal servers.

You can think of StartTunnel as a "virtual router in the cloud".

Use it for private, remote access, to self-hosted services running on a personal server, or to expose self-hosted services to the public Internet without revealing the host server's IP address.

## Features

### Subnets

Create subnets (private networks/VLANs).

### Devices

Invite devices to join specific subnets. Each device receives a unique Wireguard config that can be copied, downloaded, or scanned to join the network.

### Port Forwards

Expose specific ports on specific devices to the public Internet.

## CLI

StartTunnel comes with a command line interface to manage Subnets, Devices, and Port Forwards.

## UI

The StartTunnel UI is available at `https://<IP Address>` and ships with a self-signed SSL certificate. Users will need to bypass the browser's security warning to access the interface.

Users can provide their own SSL certificate using the CLI:

```
st certificate add </path/to/cert.pem>
```
