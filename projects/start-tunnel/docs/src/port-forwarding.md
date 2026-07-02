# Port Forwarding

Port forwarding exposes a device's port on your VPS's public IP address. This is how you make services reachable from the public Internet.

> [!WARNING]
> Port forwarding requires a **dedicated public IPv4 address** on your VPS. Shared IPv4 addresses (CGNAT, shared NAT, load-balanced IPs) and IPv6-only VPSes cannot be used to expose services to the clearnet. Confirm with your VPS provider before purchasing.

> [!NOTE]
> StartTunnel acts as a port-control gateway for connected devices, speaking PCP (preferred) and UPnP. A StartOS server using this tunnel opens the ports it needs **automatically** when you enable a public address — the same way it would behind a home router — and removes them when the address is disabled or deleted. For security, an automatically created forward always targets the requesting device's own tunnel IP; a device can only open ports to itself. The steps below are for adding or managing forwards manually.

## Manual and automatic forwards

The `Port Forwards` page shows two tables: **Manual** forwards you added by hand, and **Automatic** forwards opened by connected devices via PCP/UPnP. You can enable, disable, or remove either; automatic forwards have no editable label (they're owned by the device that created them) and may be re-created if you remove one while the device still wants it.

## Add a forward manually

1. In StartTunnel, navigate to `Port Forwards` and click "Add".

1. Select the external IP address you want to use (there is usually only one).

1. Enter the external port and the internal (device) port. In almost all cases, they will be the same.

1. To forward a **range** of ports, set "Number of Ports" to the size of the range. It counts up from both the external and internal ports you entered — for example external `49152`, internal `49152`, count `100` forwards `49152–49251` on each side. Leave it at `1` for a single port. Ranges are plain port forwards and cannot be combined with an SNI hostname.

1. If you are forwarding port `443 -> 443`, you will see a checkbox to also forward port `80 -> 443`. This is highly recommended, as it will automatically redirect HTTP to HTTPS.

1. Click "Save".
