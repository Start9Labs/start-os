# Inbound VPN

Access your server privately from anywhere in the world using a VPN. Only authorized devices can reach your server and its installed services.

Think of your server's [gateway](gateways.md) as a defense perimeter with hundreds of locked doors, each door leading to a unique service interface. One door might say "Vaultwarden UI", another "Bitcoin RPC", and another "Bitcoin P2P". An inbound VPN gives authorized devices a key to the doors they need — without opening them to the public Internet.

## WATCH THE VIDEO

<div class="yt-video" data-id="mYsx7iTZgl4" data-title="Inbound VPN"></div>

## LAN IP and Private Domains over VPN

Once connected to your VPN, you can reach your server and its services using your server's LAN IP address. The StartOS dashboard is available at the base IP, while each service interface is available on a different port of that same IP.

> [!NOTE]
> Most VPN clients do not support mDNS, so your server's `.local` address will typically not work over VPN. Use the LAN IP address instead, or set up [private domains](private-domains.md) for friendlier names that work reliably over VPN.

## Option 1: Router

Most modern routers include a VPN server feature. If so, it is usually the preferred method for private, remote access to your server.

> [!WARNING]
> If your ISP uses [CGNAT](cgnat.md), your router cannot accept inbound connections, so a router-based VPN will not work. Use [Option 2: StartTunnel](#option-2-starttunnel) instead.

1. If you haven't already, assign a static IP address to your server on the LAN. Refer to your router's user manual for detailed instructions.

1. Since home IP addresses can change without warning, we highly recommend setting up [dynamic DNS](https://en.wikipedia.org/wiki/Dynamic_DNS). Many routers offer this as a built-in feature. If not, third-party services are available. Without dynamic DNS, a change to your home IP will disconnect all VPN clients until you re-download configuration files for each one.

1. Enable your router's VPN server. Refer to your router's user manual for detailed instructions.

## Option 2: StartTunnel

By default, StartTunnel exports wireguard config files that are configured for split tunneling, allowing you to use your StartTunnel VPN to access your StartOS server and installed services while also preventing it from being automatically used for all Internet traffic.

There are three reasons to select this option:

1. Your router does not offer a VPN server.
2. Your router's VPN server is not automatically configured for split tunneling.
3. You are already using StartTunnel for clearnet hosting, so most of the work is already done.

To use StartTunnel for private, remote VPN access, see [StartTunnel](/start-tunnel/).

## Connecting Clients (WireGuard)

Once you have successfully enabled a VPN server on your router or added a StartTunnel gateway, follow the instructions below.

1. Obtain a WireGuard config file for your device.
   - **StartTunnel**: Follow instructions [here](/start-tunnel/devices.html)
   - **Router**: Follow your router's instructions.

1. Install WireGuard and import your config file:

{{#tabs global="platform" }}
{{#tab name="Mac" }}

1. Install WireGuard from the [App Store](https://apps.apple.com/us/app/wireguard/id1451685025).

1. Open the WireGuard app, click "Import tunnel(s) from file", and select the config file.

1. MacOS will inform you that WireGuard wants to set up a VPN connection. Click "Allow".

1. Your VPN tunnel will have been created and visible in both your Mac's system settings and in the WireGuard app where you can click to activate it.

   > [!TIP]
   > You may need to edit your newly created tunnel and enable "On-demand" for either ethernet, wifi, or both.

{{#endtab }}
{{#tab name="Windows" }}

1. Install WireGuard from the [official website](https://www.wireguard.com/install/).

1. Click "Import tunnel(s) from file" and select the config file.

1. Your VPN tunnel will have been created and visible in the WireGuard app where you can click to activate it via the "Activate" button.

{{#endtab }}
{{#tab name="iOS" }}

1. Install WireGuard from the [App Store](https://itunes.apple.com/us/app/wireguard/id1441195209?ls=1&mt=8).

1. Click "Add a tunnel".

1. Import the config file to your iOS device. If the configuration file can be displayed as a QR code, that is usually easiest. If not, you can download the file and transfer it to your iOS device.

1. Your VPN tunnel will have been created and visible in the WireGuard app where you can click to activate it.

{{#endtab }}
{{#tab name="Android / Graphene" }}

1. Install WireGuard from the [Play Store](https://play.google.com/store/apps/details?id=com.wireguard.android) or [WireGuard website](https://www.wireguard.com/install/).

1. Click the `+` button to add a new profile/connection.

1. Import the config file to your device. If the configuration file can be displayed as a QR code, that is usually easiest. If not, you can download the file and transfer it to your device.

1. Android will inform you that WireGuard wants to set up a VPN connection. Click "OK".

{{#endtab }}
{{#tab name="Linux" }}

1. Install `wireguard` and `wireguard-tools`:
   - Debian / Ubuntu: `sudo apt update && sudo apt install wireguard`
   - Fedora / RHEL: `sudo dnf update && sudo dnf install wireguard-tools`
   - Arch / Manjaro: `sudo pacman -Syu && sudo pacman -S wireguard-tools wireguard`

1. Copy the config file to `/etc/wireguard/wg0.conf`:

   ```
   sudo mv myconf.conf /etc/wireguard/wg0.conf
   ```

   Replace `myconf.conf` with the name of the file you downloaded.

1. Set the correct permissions:

   ```
   sudo chmod 600 /etc/wireguard/wg0.conf
   ```

1. Bring the interface up:

   ```
   sudo wg-quick up wg0
   ```

1. Verify it worked:

   ```
   sudo wg
   ```

1. (Optional) Enable on boot:

   ```
   sudo systemctl enable wg-quick@wg0
   ```

> [!TIP]
> To disconnect: `sudo wg-quick down wg0`

{{#endtab }}
{{#endtabs }}

## Connecting Clients (OpenVPN)

> [!NOTE]
> OpenVPN is only available when using a router-based VPN server. StartTunnel uses [WireGuard](#connecting-clients-wireguard).

1. Download the configuration file from your router's OpenVPN server.

1. Install OpenVPN and import your config file:

{{#tabs global="platform" }}
{{#tab name="Mac" }}

1. Install the OpenVPN Connect client from the [official website](https://openvpn.net/client-connect-vpn-for-mac-os/).

1. If asked to do so, allow the OpenVPN client to run in the background.

1. Import the configuration file and enter the necessary authentication settings you chose or were default on your OpenVPN server on your router.

1. Depending on how you've configured your OpenVPN server, you may need to add a username and password before you hit Connect.

1. Once set up, click on the name of the profile to connect and disconnect. You can edit the profile from the icon to its right.

{{#endtab }}
{{#tab name="Windows" }}

1. Install the OpenVPN Connect client from the [official website](https://openvpn.net/client/client-connect-vpn-for-windows/).

1. Import the configuration file and enter the necessary authentication settings you chose or were default on your OpenVPN server on your router.

1. Depending on how you've configured your OpenVPN server, you may need to add a username and password before you hit Connect.

1. Once set up, click on the name of the profile to connect and disconnect. You can edit the profile from the icon to its right.

{{#endtab }}
{{#tab name="iOS" }}

1. Install OpenVPN Connect from the [App Store](https://itunes.apple.com/us/app/openvpn-connect/id590379981?mt=8).

1. Transfer the configuration file to your iOS device. If accessing your router UI via a laptop/desktop, you will need to download the file to that device, then send it to yourself via email, message, or other file sharing tool.

1. Import the configuration file and enter the necessary authentication settings you chose or were default on your OpenVPN server on your router.

1. Depending on how you've configured your OpenVPN server, you may need to add a username and password before you hit Connect.

1. Once set up, click on the name of the profile to connect and disconnect. You can edit the profile from the icon to its right.

{{#endtab }}
{{#tab name="Android / Graphene" }}

1. Install **OpenVPN for Android** from [Google Play](https://play.google.com/store/apps/details?id=de.blinkt.openvpn), [F-Droid](https://github.com/schwabe/ics-openvpn), or the APK from [Arne Schwabe's Github](https://github.com/schwabe/ics-openvpn). Alternatively, you can use **OpenVPN Connect** from [OpenVPN, Inc.](https://openvpn.net/client/).

1. Transfer the configuration file to your device. If accessing your router UI via a laptop/desktop, you will need to download the file to that device, then send it to yourself via email, message, or other file sharing tool.

1. Click the `+` button to add a new profile/connection.

1. Import the configuration file. Consider giving the profile a descriptive name.

1. Android will inform you that OpenVPN wants to set up a VPN connection. Click "OK".

1. If you set up your OpenVPN server with username and password authentication, enter those and select to save the password.

1. Once set up, click on the name of the profile to connect and disconnect. You can edit the profile from the icon to its right.

   > [!TIP]
   > If you're not able to browse websites when connected, your router VPN may not be providing valid DNS servers. Edit the profile, visit the IP and DNS tab, and override the DNS settings with your own.

{{#endtab }}
{{#tab name="Linux" }}

1. Install `openvpn`:
   - Debian / Ubuntu: `sudo apt update && sudo apt install openvpn`
   - Fedora / RHEL: `sudo dnf update && sudo dnf install openvpn`
   - Arch / Manjaro: `sudo pacman -Syu && sudo pacman -S openvpn`

1. Copy the config file to `/etc/openvpn/client.conf`:

   ```
   sudo mv myconf.ovpn /etc/openvpn/client.conf
   ```

   Replace `myconf.ovpn` with the name of the file you downloaded.

1. Set the correct permissions:

   ```
   sudo chmod 600 /etc/openvpn/client.conf
   ```

1. Start OpenVPN, entering your username and password when requested:

   ```
   sudo systemctl start openvpn@client
   ```

1. Verify it worked:

   ```
   sudo systemctl status openvpn@client
   ```

1. (Optional) Enable on boot:

   ```
   sudo systemctl enable openvpn@client
   ```

> [!TIP]
> To disconnect: `sudo systemctl stop openvpn@client`

{{#endtab }}
{{#endtabs }}
