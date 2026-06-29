# DNS Records

StartTunnel can serve DNS records for your private domains, so names resolve to the right device for everyone connected to the tunnel. Records can be added by hand, or injected automatically over [RFC 2136](https://www.rfc-editor.org/rfc/rfc2136) by devices you have explicitly trusted.

> [!NOTE]
> A StartOS server using this tunnel injects the records for its private domains **automatically**, once you allow DNS injection for its device (see below). You normally won't need to add records by hand.

## Allowing a device to inject records

DNS injection is **off by default** for every device. Only enable it for devices you control and trust.

> [!WARNING]
> A device allowed to inject DNS records can create, overwrite, or delete any record StartTunnel serves. Enable this only for trusted devices, such as your own StartOS server.

1. In StartTunnel, navigate to `Devices`. DNS injection is a **Server** capability — if the device is a Client, promote it to a Server first (see [Devices](/start-tunnel/devices.html)).

1. In the Servers table, toggle **DNS injection** on for the device.

The device may now add, update, and remove records via RFC 2136 DNS UPDATE. StartTunnel authorizes each request by the device's tunnel IP, so only that device's allowance is in effect.

## Viewing and managing records

1. In StartTunnel, navigate to `DNS Records`.

1. Records are shown in two tables: **Manual** (records you added by hand) and **Automatic** (records injected by a device, each showing the injecting device's IP as its source).

1. To add a record manually, click "Add" on the Manual table, enter the name, type (A, AAAA, CNAME, or TXT), value, and TTL, and click "Save".

1. To remove a record, select it and click "Remove".
