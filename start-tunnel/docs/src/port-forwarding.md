# Port Forwarding

Port forwarding exposes a device's port on your VPS's public IP address. This is how you make services reachable from the public Internet.

> [!WARNING]
> Port forwarding requires a **dedicated public IPv4 address** on your VPS. Shared IPv4 addresses (CGNAT, shared NAT, load-balanced IPs) and IPv6-only VPSes cannot be used to expose services to the clearnet. Confirm with your VPS provider before purchasing.

1. In StartTunnel, navigate to `Port Forwards` and click "Add".

1. Select the external IP address you want to use (there is usually only one).

1. Enter the external port and the internal (device) port as. In almost all cases, they will be the same.

1. If you are forwarding port `443 -> 443`, you will see a checkbox to also forward port `80 -> 443`. This is highly recommended, as it will automatically redirect HTTP to HTTPS.

1. Click "Save".
