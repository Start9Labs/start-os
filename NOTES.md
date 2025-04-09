# Identity PSK

The basic pre-installed packages do not suport wpa_psk_file keyids.

- `opkg remove wpad-basic-mbedtls`
- `opkg install wpad-wolfssl`
- `opkg install hostapd-utils`

## /etc/config/wireless
- under `wifi-iface`
```
        option encryption 'psk2'
        option wpa_psk_file '/etc/hostapd.wpa_psk'
```

## /etc/hostapd.wpa_psk (example)
```
00:00:00:00:00:00 password
keyid=guest 00:00:00:00:00:00 guestpassword
```

## check key of client
- `hostapd_cli all_sta`

# Security Profiles with secprofd

The idea here was to monitor all ip<->mac parings via addrwatch or by
participating in dhcp and also monitor keid<->mac pairings via the hostapd unix
socket (which is used by hostapd_cli under the hood). Then generate nftables
rules to open and close doors between ip addresses as quickly as possible based
on their associated mac addresss and keyids.

It is not possible to use mac addrs in nftables because that is a layer 2
(ethernet), strictly between the device and the router. Routing between devices
is entirely layer 3 (IP). We could somehow tag packets at layer 2, then check
the tag in the nftables rules at layer 3. That is basically a IEEE 802.1Q VLAN, and the
VLAN tagging can be done with hostapd directly!

# Security Profiles with VLANs

The source of truth for all of this is:
https://forum.openwrt.org/t/individual-per-passphrase-wifi-vlans-using-wpa-psk-file-no-radius-required

I have also gotten a lot of good info from:
https://fabianlee.org/2023/01/22/openwrt-bridge-vlan-filtering-for-openwrt-21-x-with-dsa-isolated-guest-wi-fi

# Wifi Modules

- `opkg install pciutils`
- `lspci`

If you see "Qualcomm Atheros AR93XX", then you need:
- `opkg install kmod-ath9k`

Not all wifi drivers support the VLAN features we would like to use. Notably the
ath11k chipset reqiures a driver patch which is not merged as of v24.10.0:
https://forum.openwrt.org/t/individual-per-passphrase-wifi-vlans-using-wpa-psk-file-no-radius-required/161696/204

The Raspberry Pi 4B shows the same "hostapd: Failed to create interface: -95
(Not supported)" error, but does not have a patch that I'm aware of. Probably
the hardware does not support VLAN tagging.

# Compiling OpenWRT

The best way I've found to compile OpenWRT is https://github.com/mwarning/docker-openwrt-build-env

Trying to build it on Arch Linux results in some silent undebuggable errors
