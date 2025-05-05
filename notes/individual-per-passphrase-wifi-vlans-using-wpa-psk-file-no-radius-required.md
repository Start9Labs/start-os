[OpenWrt Forum](/)

# [Individual per-passphrase Wifi VLANs using wpa_psk_file (no RADIUS required)](/t/individual-per-passphrase-wifi-vlans-using-wpa-psk-file-no-radius-required/161696)

[ Installing and Using OpenWrt ](/c/general/6)

[takimata](https://forum.openwrt.org/u/takimata) May 30, 2023, 4:55pm

1

Hello \*,

I'm playing around with the `wpa_psk_file` option for wireless/hostapd
because I found that it supports [vlan assignment using the *vlanid*
tag](https://w1.fi/cgit/hostap/tree/hostapd/hostapd.wpa_psk). This would
be really helpful to create a segmented and client-individualised wifi
network without having to spin up a dedicated RADIUS server.

> Extend wpa_psk_file to allow an optional VLAN ID to be specified with
> "vlanid=" prefix on the line. If VLAN ID is specified and the
> particular wpa_psk_file entry is used for a station, that station is
> bound to the specified VLAN. This can be used to operate a single
> WPA2-Personal BSS with multiple VLANs based on the used
> passphrase/PSK. This is similar to the WPA2-Enterprise case where the
> RADIUS server can assign stations to different VLANs.

Edit: I removed the problem for which I found the solution because I
believe it has not a lot of value to anyone else. This a how-to now.

7 Likes

[Dynamic PSK
Support](https://forum.openwrt.org/t/dynamic-psk-support/171357/2)

[Vlan Tagging via
MAC](https://forum.openwrt.org/t/vlan-tagging-via-mac/166416/3)

[Wifi vlan assignment with
sae_password?](https://forum.openwrt.org/t/wifi-vlan-assignment-with-sae-password/163620/3)

[How does multiple SSID affect wireless
bandwidth?](https://forum.openwrt.org/t/how-does-multiple-ssid-affect-wireless-bandwidth/174873/8)

[Confused about wpad, hostapd and
hostapd-common](https://forum.openwrt.org/t/confused-about-wpad-hostapd-and-hostapd-common/161888)

[Create VLAN with single
SSID](https://forum.openwrt.org/t/create-vlan-with-single-ssid/181064/2)

[Wifi VLANs via wpa_psk_file not adding to
bridge](https://forum.openwrt.org/t/wifi-vlans-via-wpa-psk-file-not-adding-to-bridge/175200)

[Setting Up Automated VLAN Assignment for IoT Devices on TP-Link Archer
C7
(v5)](https://forum.openwrt.org/t/setting-up-automated-vlan-assignment-for-iot-devices-on-tp-link-archer-c7-v5/193943/4)

[How to setup vlans for dynamic PSK on router with 1
SSID?](https://forum.openwrt.org/t/how-to-setup-vlans-for-dynamic-psk-on-router-with-1-ssid/190981)

[Dynamic VLANs without RADIUS only works for 1
SSID](https://forum.openwrt.org/t/dynamic-vlans-without-radius-only-works-for-1-ssid/177977)

[Wifi ppsk mode](https://forum.openwrt.org/t/wifi-ppsk-mode/15483/20)

[WAX202, max number of
SSIDs](https://forum.openwrt.org/t/wax202-max-number-of-ssids/166389/16)

[Temp passwords?](https://forum.openwrt.org/t/temp-passwords/202181/3)

[Multiple VLANs on One SSID (TP-Link Archer C7
Router)](https://forum.openwrt.org/t/multiple-vlans-on-one-ssid-tp-link-archer-c7-router/191929/2)

[What limits VAPs/multi-ssid on
hardware?](https://forum.openwrt.org/t/what-limits-vaps-multi-ssid-on-hardware/160699/12)

[Vlans in non-bridged wifi
networks](https://forum.openwrt.org/t/vlans-in-non-bridged-wifi-networks/175625)

[Possible for one Wi-Fi SSID to have multiple
subnets?](https://forum.openwrt.org/t/possible-for-one-wi-fi-ssid-to-have-multiple-subnets/175847/3)

[freeRADIUS WEB-UI on
openwrt](https://forum.openwrt.org/t/freeradius-web-ui-on-openwrt/173774/9)

[Wpa_psk_file method interface creation not
working](https://forum.openwrt.org/t/wpa-psk-file-method-interface-creation-not-working/204073)

[Matching SSIDs to
radios](https://forum.openwrt.org/t/matching-ssids-to-radios/178512/2)

[Best way to prevent people using my wifi with shared
password?](https://forum.openwrt.org/t/best-way-to-prevent-people-using-my-wifi-with-shared-password/229673/2)

[Wifi scheduler](https://forum.openwrt.org/t/wifi-scheduler/227571/10)

[takimata](https://forum.openwrt.org/u/takimata) June 1, 2023, 6:49am

4

![:information_source:](https://forum.openwrt.org/images/emoji/twitter/information_source.png?v=12 ":information_source:")
**IMPORTANT NOTE:** I wrote this in early 2023, when 22.03 was current
and the default SSL library was *wolfssl*. With 23.05, the default SSL
library has changed to ***mbedtls***. If you're following this topic on
23.05 or later, please substitute any mentions of *wolfssl* with
*mbedtls*, especially in the packages you install.

------------------------------------------------------------------------

After a *lot* of fiddling and trial-and-error I found the problem: The
default *hostapd* provided by *wpad-basic* is not enough, one needs the
fully featured *hostapd* or *wpad*. This is not immediately obvious
because the stripped down default one fails silently for most of the
relevant options, with the exception of `vlan_tagged_interface` which it
rejects violently.

So for posterity, here's my recipe for a dumb AP, and per-passphrase
VLAN segmentation of clients:

First of all, the default `wpad-basic` will not suffice. We need a fully
featured `hostapd`, either installed on its own or as part of the
non-basic `wpad`:

``` lang-plaintext
# remove the default stripped-down wpad containing a stripped-down hostapd:
opkg remove wpad-basic-wolfssl
# removing the package does not kill the process, we need to manually do that:
killall hostapd
# install a fully-featured wpad containing a fully-featured hostapd:
opkg install wpad-wolfssl
```

For a single tagged ethernet port connecting to the router upstream, in
`/etc/config/network` no changes are necessary. Especially it's not
necessary to create custom bridges or interfaces. All of that will be
done by hostapd at startup.

`/etc/config/wireless` needs the following options for the relevant
`wifi-iface`:

``` lang-plaintext
        option wpa_psk_file '/etc/hostapd.wpa_psk'
        option vlan_file '/etc/hostapd.vlan'
        option vlan_tagged_interface 'eth0'
        option vlan_bridge 'br-vlan'
        option dynamic_vlan '1'
```

Of note: `vlan_tagged_interface` and `vlan_bridge` are *prefixes* to
which the VLAN ID will be appended.

`/etc/hostapd.wpa_psk` needs to specify which passphrase should be go
into which VLAN ID (in this example, clients connecting with the
passphrase "supersecret" will go into VLAN ID 101):

``` lang-plaintext
vlanid=101 00:00:00:00:00:00 supersecret
```

`/etc/hostapd.vlan` needs to specify the wifi interface created for each
(possible) VLAN ID. This can be named anything, but for consistency I
choose to name it like a VLAN-tagged interface:

``` lang-plaintext
101 wlan0.101
```

This will result in:

- creation of the wifi interface `wlan0.101` as specified in
  `/etc/hostapd.vlan`
- creation of the `eth0.101` interface as specified by the
  `vlan_tagged_interface` prefix plus a dot and the VLAN ID,
  conveniently automatically tagging VLAN ID 101 on eth0
- creation of the `br-vlan101` bridge as specified by the `vlan_bridge`
  prefix plus the VLAN ID, with the previous two interfaces in it

``` lang-plaintext
root@OpenWrt:~# brctl show
bridge name     bridge id               STP enabled     interfaces
br-vlan101              8000.4018b1eb3c80       no              wlan0.101
                                                        eth0.101
br-lan          7fff.4018b1eb3c80       no              eth0
```

It may get a little bit more complicated if the outgoing interface
should not be a singular tagged ethernet port but a bridge. In this case
one would probably have to, in network config, create a bridge named
something like `br-vlan.101` containing the tagged ethernet ports and
then use `br-vlan` as the `vlan_tagged_interface` prefix option. I
haven't tried that yet because I have no need for that, but I don't see
why it shouldn't work.

17 Likes

[802.1X dynamic VLAN with DSA
config](https://forum.openwrt.org/t/802-1x-dynamic-vlan-with-dsa-config/126040/11)

[Hostapd and 802.1q vlan tagging with DSA
devices](https://forum.openwrt.org/t/hostapd-and-802-1q-vlan-tagging-with-dsa-devices/175437/2)

[Vlans in non-bridged wifi
networks](https://forum.openwrt.org/t/vlans-in-non-bridged-wifi-networks/175625/5)

[MAC-based VLAN
Pass-thru](https://forum.openwrt.org/t/mac-based-vlan-pass-thru/193269/5)

[Vlan attributes in
sae_password](https://forum.openwrt.org/t/vlan-attributes-in-sae-password/166566/2)

[One SSID, multiple
VLANs](https://forum.openwrt.org/t/one-ssid-multiple-vlans/215457)

[VPN Router
recommendations](https://forum.openwrt.org/t/vpn-router-recommendations/181855/2)

[Introduction and Question Fritzbox
7530](https://forum.openwrt.org/t/introduction-and-question-fritzbox-7530/192198/2)

[PPSK/DPSK Native Support In
Luci](https://forum.openwrt.org/t/dpsk-native-support-in-luci/228600/3)

[Edit: How to setup guest wifi on main ssid with network selected by
password
entered](https://forum.openwrt.org/t/how-to-setup-guest-wifi-on-main-ssid-with-network-selected-by-password-entered/228670/2)

[takimata](https://forum.openwrt.org/u/takimata) June 2, 2023, 2:19pm

5

Addendum: using `hostapd_cli` (provided by the *hostapd-utils* package)
the wpa_psk file can be reloaded without restarting the interface:

``` lang-plaintext
hostapd_cli reload_wpa_psk
```

Only clients whose passphrase changed will be booted off the radio,
clients connected with passphrases that didn't change remain untouched.

All in all, the wpa_psk_file opens up a host of new possibilities.
Having all networks -- unrestricted, guest, IOT, etc. -- on one single
radio, distinguished by their passphrases. Rotating passphrases, even in
a staggered manner. Giving individual users their individual passphrases
... I am really impressed, and a little surprised that I seem to be the
first one to actually try and use it.

9 Likes

[mpa](https://forum.openwrt.org/u/mpa) June 3, 2023, 8:08am

6

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> surprised that I seem to be the first one to actually try and use it.

The reason could be that this feature is not documented in the OpenWrt
wiki.  
[https://openwrt.org/docs/guide-user/network/wifi/basic](https://openwrt.org/docs/guide-user/network/wifi/basic)

3 Likes

[takimata](https://forum.openwrt.org/u/takimata) June 3, 2023, 4:31pm

7

![](https://forum.openwrt.org/letter_avatar/mpa/48/5_f40856482fe1e45ac8f1605885140a2d.png)
mpa:

> The reason could be that this feature is not documented in the OpenWrt
> wiki.

Some of the parameters are documented under "WPA Enterprise", which
makes sense if you consider them exclusively related to a RADIUS client
setup, which they obviously don't necessarily have to be.

I'm thinking how those parameters should be described. Maybe a dedicated
"wpa_psk_file" section, even if it duplicates some of the option
descriptions?

Another problem is that there is no support for any of this in LuCI, and
probably for good reason, with the versatility of settings I can't even
imagine how to add LuCI support for any of this would look like. This
may very well remain a CLI only setting.

[\_bernd](https://forum.openwrt.org/u/_bernd) June 3, 2023, 9:04pm

8

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> `option wpa_psk_file`

Is there nowadays a wrapper which translates every UCI option into a
hostapd config or is there explicit support for this option in the
openwrt hostapd config generator script?  
I have walked this path too but I never stumped over WPA psk file and
"just" used WPA and radius. WPA not wpa2 enterprise.  
I like this approach and will check this soon. This way I could avoid
fiddling with radius.
![:+1:](https://forum.openwrt.org/images/emoji/twitter/+1.png?v=12 ":+1:")

[m80](https://forum.openwrt.org/u/m80) June 3, 2023, 11:07pm

9

Hi, what if I have two wifi networks:

- guest psk2 with isolation,
- home psk2 without isolation

"wpa_psk_file" method will not work in this case?  
Full Radius won't help here either or am I wrong?

I need to set the network with wifi parameters the same for all VAPs
(encryption method, isolation, FT, ...)?

[takimata](https://forum.openwrt.org/u/takimata) June 3, 2023, 11:16pm

10

![](https://forum.openwrt.org/letter_avatar/_bernd/48/5_f40856482fe1e45ac8f1605885140a2d.png)
\_bernd:

> Is there nowadays a wrapper which translates every UCI option into a
> hostapd config

No, UCI option to hostapd config translation still has to happen
deliberately and individually. But all of the necessary options are
present and accounted for. I assume that's because the wpa_psk_file
option exists since something like 2015, and the vlan options existed
for RADIUS support, and just by pure chance those two option sets
started inter-operating in 2019.

[takimata](https://forum.openwrt.org/u/takimata) June 3, 2023, 11:19pm

11

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/m80/48/75875_2.png)
m80:

> - guest psk2 with isolation,
> - home psk2 without isolation
>
> "wpa_psk_file" method will not work in this case?

Yes, it will work. You can still mix vlan-less and vlan-tagged networks,
e.g. in hostapd.wpa_psk:

``` lang-plaintext
00:00:00:00:00:00 homepassword
vlanid=123 00:00:00:00:00:00 guestpassword
```

The lines that do not contain a `vlanid` parameter will not use a vlan
and those clients will be connected to the interface you specify via the
regular `network` parameter, just like in a regular radio setup.

Importantly, for mixed vlan-less and vlan-tagged operations you need to
set the `dynamic_vlan` option to `1`, like in my example above (`0`
means no vlan-tagged radios, `2` means *only* vlan-tagged ones).

("without isolation" is not completely correct, though. vlan-less
networks are still separated from vlan-tagged networks.)

1 Like

[m80](https://forum.openwrt.org/u/m80) June 4, 2023, 12:02am

12

Thank You for the tip, one more question if I may..  
I'v read that full hostapd is intended for "hardware with less
resources", so if I have full wpad-openssl/mbedtls installed do I still
have to remove hostapd-common and install full hostapd in its place,
doesn\`t full wpad have such options like hostapd?

[takimata](https://forum.openwrt.org/u/takimata) June 4, 2023, 3:44am

13

The whole hostapd/wpad affair is a bit confusing. I'll try to explain:

- *hostapd-common* does not contain *hostapd/wpad* but scripts and
  configurations that are supporting it ("common" scripts and
  configurations). It is always needed and will be installed by any
  wpad/hostapd package you install.
- *wpad* contains *wpa_supplicant* and *hostapd* in one file. As the
  names suggest, *hostapd* is a wifi "host" or "server",
  *wpa_supplicant* is a wifi "supplicant" or "client", putting both into
  one file saves some space.
- If you never intend to use your device as a wifi client, you don't
  need *wpa_supplicant*, and just a *hostapd* package would be
  sufficient.
- *wpad-basic* is a stripped down version of wpad that comes by default
  with OpenWrt. It contains most functionality that APs use, but not
  *all* of it. As far as wpa_psk_file is concerned, it does contain
  support for wpa_psk_file in general, but not VLANs (those are lumped
  into the "extended" RADIUS/EAP support). So if you want to use VLANs
  with wpa_psk_file, *wpad-basic* is not sufficient.
- All wpad and hostapd packages come in various flavours of SSL library.
  If I'm not completely wrong, the default SSL library that comes with
  OpenWrt is currently *wolfssl*, so it would make sense to "upgrade"
  *wpad-basic-wolfssl* to the full *wpad-wolfssl*. As its name suggests,
  *wpad-openssl* would require/install *libopenssl*. I'm not entirely
  sure what the SSL-variant-suffix-less *wpad* uses, I'm *guessing* it
  uses "whatever SSL is installed in the system", but someone else is
  certainly more qualified than me to answer that.

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/m80/48/75875_2.png)
m80:

> so if I have full wpad-openssl/mbedtls installed do I still have to
> remove hostapd-common and install full hostapd in its place

No. You **never** need to uninstall hostapd-common, it does not contain
hostapd but needed support files and it would be re-installed by any
hostapd/wpad package anyway. At most you need to uninstall
*wpad-basic-wolfssl* (the current default wpad-basic) and install a full
hostapd or wpad, e.g. *wpad-wolfssl*. If you already have full wpad
installed, you don't need to do anything. A "full wpad" contains a "full
hostapd".

11 Likes

[m80](https://forum.openwrt.org/u/m80) June 4, 2023, 10:37am

14

Thank you for this in-depth explanation
![:smiley:](https://forum.openwrt.org/images/emoji/twitter/smiley.png?v=12 ":smiley:")

One more thing that bothers me...

I have a dumb AP to which all vlans are trunked through the WAN port, in
addition, each LAN port is associated with a different VLAN so that I
can conveniently access each network. Part of my **/etc/config/network**
on dumb AP looks like this:

``` lang-plaintext
config interface 'loopback'
    option device 'lo'
    option proto 'static'
    option ipaddr '127.0.0.1'
    option netmask '255.0.0.0'
    
...

config device
        option name 'br0'
        option type 'bridge'
        option igmp_snooping '1'

config bridge-vlan
        option device 'br0'
        option vlan '50'
        option alias 'mgmt'
        list ports 'wan:t'
        list ports 'lan2:u*'

config bridge-vlan
        option device 'br0'
        option vlan '60'
        option alias 'home'
        list ports 'wan:t'
        list ports 'lan3:u*'

config bridge-vlan
        option device 'br0'
        option vlan '70'
        option alias 'iot'
        list ports 'wan:t'
        list ports 'lan4:u*'

config bridge-vlan
        option device 'br0'
        option vlan '80'
        option alias 'guest'
        list ports 'wan:t'

config interface 'mgmt'
        option device 'br0.mgmt'
        option proto 'static'
        option ipaddr '192.168.50.11'
        option netmask '255.255.255.0'
        option gateway '192.168.50.1'
        option dns '192.168.50.1'
        option ip6assign '60'

config interface 'home'
        option device 'br0.home'
        option proto 'none'

config interface 'iot'
        option device 'br0.iot'
        option proto 'none'

config interface 'guest'
        option device 'br0.guest'
        option proto 'none'
```

In the **/etc/config/wireless** I have the VAPs connected to the above
interfaces as usual.

What will these options be in my case? :

``` lang-plaintext
      ...
        option vlan_tagged_interface 'eth0'
        option vlan_bridge 'br-vlan'
        ...
```

1 Like

[takimata](https://forum.openwrt.org/u/takimata) June 4, 2023, 11:33am

15

Edit: After some experimentation I see a bit clearer.

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/m80/48/75875_2.png)
m80:

> What will these options be in my case? :

First of all, since your vlan-tagged bridges already exist you want to
add the new "vlan-dedicated" radio interfaces to those bridges, not have
hostapd create new ones. There is a mechanism for that, ["override the
bridge name"](https://www.spinics.net/lists/hostap/msg04922.html) by
stating the bridge name as the third entry in a line in the `vlan_file`
file.

This should work in hostapd.vlan:

``` lang-plaintext
60 wlan0.60 br0.iot
```

Hostapd will still have to create a vlan-specific radio-specific
interface (I named it "wlan0.60" but it can really be named anything),
and it should insert it into the br0.iot bridge.

As for the options, you need to have a `vlan_bridge` option set, but it
can literally be anything because it is overridden by the above
mechanism. And you don't need `vlan_tagged_interface` at all, the bridge
already has vlan-tagged interfaces.

(This is also helpful if two radios, e.g. 2.4GHz and 5GHz, should put
radio interfaces into the same vlan-tagged bridges.)

1 Like

[Edit: How to setup guest wifi on main ssid with network selected by
password
entered](https://forum.openwrt.org/t/edit-how-to-setup-guest-wifi-on-main-ssid-with-network-selected-by-password-entered/228670/5)

[takimata](https://forum.openwrt.org/u/takimata) June 4, 2023, 1:02pm

16

![](https://forum.openwrt.org/letter_avatar/mpa/48/5_f40856482fe1e45ac8f1605885140a2d.png)
mpa:

> The reason could be that this feature is not documented in the OpenWrt
> wiki.

[I inserted an (at least fundamental)
documentation.](https://openwrt.org/docs/guide-user/network/wifi/basic#wpa_psk_file)
It will probably need a few revisions because it is a challenge to
describe the interconnected features and how interfaces and bridges are
handled. And I'm not entirely sure I fully grasp how hostapd handles
interfaces and bridges, especially when they are individually named. But
it's a start.

8 Likes

[luaraneda](https://forum.openwrt.org/u/luaraneda) June 6, 2023, 4:31am

17

Have you looked at the wifi-station and wifi-vlan sections introduced as
part of
<https://git.openwrt.org/?p=openwrt/openwrt.git;a=commit;h=5aa2ddd0d6b9759c62bbb7bb11b72a7f4269c16b>
?

I'm not fully sure if it's similar to what you are testing or want to
achieve.

4 Likes

[takimata](https://forum.openwrt.org/u/takimata) June 6, 2023, 6:43am

18

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/luaraneda/48/3010_2.png)
luaraneda:

> Have you looked at the wifi-station and wifi-vlan sections introduced
> as part of
> <https://git.openwrt.org/?p=openwrt/openwrt.git;a=commit;h=5aa2ddd0d6b9759c62bbb7bb11b72a7f4269c16b>
> ?

I have not, thanks for the pointer. That is an interesting patch, it
does two things:

The first patch to `mac80211.sh` I "indirectly" knew about, it enables
the *wpa_psk_file* and *vlan_file* options that are necessary to
specify, well, wpa_psk and vlan files. I have been using these options,
I have already documented them, and without these options it would be
very hard to get these files known to hostapd.

The second patch to hostapd.sh was yet unknown to me. It is basically an
abstraction that compiles respective UCI options into *wpa_psk* and
*vlan* files.

The UCI way seems quite a bit more verbose at first, especially if one
were to use it for multiple PSK/passphrase variants. But the abstraction
may help if the options should apply to multiple wifi radios. And I
guess it would be a prerequisite if someone (not me!) would ever extend
LuCI to support the mechanism. Directly editing the files on the other
hand keeps the wireless config a bit more decluttered and allows it to
be manipulated by external scripts (to rotate passwords, for example).
It's a tradeoff, both ways have their use.

But this patch also answers the question *since when* these options are
available in release OpenWrt: 21.02.

(And now I should probably add those new UCI sections to the wireless
config wiki page, shouldn't I?)

[One SSID, multiple
VLANs](https://forum.openwrt.org/t/one-ssid-multiple-vlans/215457/9)

[zekica](https://forum.openwrt.org/u/zekica) June 19, 2023, 3:09pm

19

Has anyone tried to do the following:

- set up multiple APs with different passphrases (using wpa_psk_file) -
  exactly the same as here
- set up 11r (FT-PSK) - both with "generate PMK locally" and without
  (using wildcard addresses in r0 and r1)

It fails for me, but I'm not entirely sure it is related to multiple
PSKs or dynamic_vlan that I was using.

1 Like

[zekica](https://forum.openwrt.org/u/zekica) June 20, 2023, 6:23pm

20

Replying to myself:

It works out of the box on 23.05.0-rc1 and the main branch. The issue
was fixed in [commit: FT: Store PMK-R0/PMK-R1 after EAPOL-Key msg 2/4
MIC
validation](https://w1.fi/cgit/hostap/commit?id=ba150059d1ec964add8f29eb2c92dd6dfde97308)

I have tried porting this back to 19.07.10 (as I have 4/32 device I
would like to test this on), but so far haven't managed. I'll share the
patch if I manage it.

[zekica](https://forum.openwrt.org/u/zekica) June 21, 2023, 1:39pm

21

Another update:

Here is a patch if someone wants it that can be applied on top of
hostapd-2019-08-08-ca8c2bd2 to fix FT with wpa_psk_file on 19.07.10

[openwrt-19.07.10-multi-psk-ft.patch](https://gist.github.com/zekica/bee8df4144d097f2e07bcaaca96de4b8)

Hopefully someone finds it useful.

2 Likes

[Qualcommax NSS
Build](https://forum.openwrt.org/t/qualcommax-nss-build/148529/4244)

[himself](https://forum.openwrt.org/u/himself) June 30, 2023, 11:45am

22

Hi [@takimata](/u/takimata), I was interested in using wpa_psk_file and
I am struggling to get it up and running. Can you please share a demo
network and wireless config file which I could use to setup an BSS with
vlan functionality?  
Following are my wireless and network config files:

``` lang-plaintext
config wifi-device 'radio0'
        option type 'mac80211'
        option path '1e140000.pcie/pci0000:00/0000:00:01.0/0000:02:00.0'
        option channel '1'
        option band '2g'
        option htmode 'HT20'
        option disabled '1'

config wifi-device 'radio1'
        option type 'mac80211'
        option path '1e140000.pcie/pci0000:00/0000:00:00.0/0000:01:00.0'
        option channel '36'
        option band '5g'
        option htmode 'VHT80'
        option country 'IN'
        option cell_density '0'

config wifi-iface 'wifinet0'
        option device 'radio1'
        option mode 'ap'
        option ssid 'HomeWifi'
        option encryption 'psk2'
        option network 'lan'
        option wpa_psk_file '/etc/hostapd.wpa_psk'
        option vlan_file '/etc/hostapd.vlan'
        option vlan_bridge 'br-vlan'
        option dynamic_vlan '1'
```

and

``` lang-plaintext
config interface 'loopback'
        option device 'lo'
        option proto 'static'
        option ipaddr '127.0.0.1'
        option netmask '255.0.0.0'

config globals 'globals'
        option packet_steering '1'
        option ula_prefix 'fd42:2825:343a::/48'

config device
        option type 'bridge'
        option name 'br-switch'
        list ports 'lan1'
        list ports 'lan2'
        list ports 'wan'

config bridge-vlan
        option device 'br-switch'
        option vlan '4'
        list ports 'lan1:t'
        list ports 'lan2:t'
        list ports 'wan:t'

config bridge-vlan
        option device 'br-switch'
        option vlan '8'
        list ports 'lan1:t'
        list ports 'lan2:t'
        list ports 'wan:t'

config bridge-vlan
        option device 'br-switch'
        option vlan '16'
        list ports 'lan1:u*'
        list ports 'lan2:u*'
        list ports 'wan:u*'

config bridge-vlan
        option device 'br-switch'
        option vlan '32'
        list ports 'lan1:t'
        list ports 'lan2:t'
        list ports 'wan:t'

config interface 'iot'
        option proto 'none'
        option device 'br-switch.4'
        option defaultroute '0'
        option delegate '0'

config interface 'guest'
        option proto 'none'
        option device 'br-switch.8'
        option defaultroute '0'
        option delegate '0'

config interface 'mgmt'
        option proto 'dhcp'
        option device 'br-switch.32'
        option delegate '0'

config interface 'lan'
        option proto 'none'
        option device 'br-switch.16'
        option defaultroute '0'
        option delegate '0'
```

And there are the wpa_psk and vlan files:

``` lang-plaintext
vlanid=8 00:00:00:00:00:00 supersecret
vlanid=16 00:00:00:00:00:00 othersecret
```

and

``` lang-plaintext
8 wlan0.8 br-switch.8
16 wlan0.16 br-switch.16
```

EDIT: After some debugging, I could figure out that wpa_psk_file is
being read but the vlan option isn't working. I have installed the full
version of wpad-wolfssl. In the logs it shows this error for passphrases
with vlanid:

> daemon.notice hostapd: wlan1: AP-STA-POSSIBLE-PSK-MISMATCH

Please help me figure this out...
![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:"),
Thanks!

[takimata](https://forum.openwrt.org/u/takimata) June 30, 2023, 6:24pm

23

Look into the syslog when starting up wifi. I can't test it right now
but I suspect the bridges fail to set up correctly and the respective
`vlanid=...` entries are then ignored, leading to what basically amounts
to "wrong password" messages.

Edit: I can't try to reproduce your problem because I don't have any
access points with a switch inside. However, what stands out to me is
your

``` lang-plaintext
        option vlan_bridge 'br-vlan'
```

I'm not entirely sure, but I believe hostapd will try to create the
`br-vlan.xxx` devices then, and fail because the already exist. Since
you actually name all the bridges to which you want to attach the wifi
interfaces, you can state

``` lang-plaintext
        option vlan_bridge 'proforma'
```

or something similarly nonexistant/unneeded. The `vlan_bridge` option
must exist, but it doesn't have to make sense.

I could also be barking up the wrong tree here. Again, the syslog should
tell you a bit more about what exactly fails. And if it doesn't, check
the output of `brctl show` if the wifi interfaces got correctly inserted
into your already existing bridges.

[regae](https://forum.openwrt.org/u/regae) September 22, 2023, 2:27pm

24

[@takimata](/u/takimata) does FT working ?  
i find when i move to another AP, the STA is bind to wrong interface.  
example, wlan0 with wlan-vlan5.  
for the first time it bind correctly to wlan-vlan5, but when i move to
another AP, it bind to wlan0.  
so the STA is in the wrong vlan.

[regae](https://forum.openwrt.org/u/regae) September 22, 2023, 2:29pm

25

perhaps [@nbd](/u/nbd) can help my issue?  
or FT is not supported with this kind of configuration?

[zekica](https://forum.openwrt.org/u/zekica) September 23, 2023, 4:05pm

26

FT is working but only on the 23.05 branch - 23.05.0-rc3. You also need
to disable "Generate PMK locally"

[pompous-stranger](https://forum.openwrt.org/u/pompous-stranger)
September 24, 2023, 5:56am

27

So thanks to the additional UCI functionality adding multiple psk+vlan
to my wpa3 and wpa2 APs was as simple as adding the following to
/etc/config/wireless:

``` lang-plaintext
 config wifi-vlan
         option name 'guest' 
         option vid '33'
         option network 'guest_lan'

config wifi-station
         option vid '33'
         option key 'yournewpassword'
```

**vid** can be any number but must match, **network** is the bridge you
want to attach to (in this case my guest wifi / IOT restricted bridge),
and **name** can be anything.

For me this greatly simplifies adding IOT devices safely without having
to spin up a bunch more virtual APs to separate them from my trusted
wifi clients. Shame this feature isn't more widely known and better
documented.

**EDIT:** As this post is still getting likes, clarifications are in
order:

1.  As many have [noted
    downthread](https://forum.openwrt.org/t/individual-per-passphrase-wifi-vlans-using-wpa-psk-file-no-radius-required/161696/93)
    wpa_psk_file does not work with sae passwords i.e. it does not
    support wpa3, only wpa2.
2.  With that in mind you [can
    limit](https://forum.openwrt.org/t/individual-per-passphrase-wifi-vlans-using-wpa-psk-file-no-radius-required/161696/41)
    what virtual APs (wifi-iface sections) receive multi-psk
    functionality with` option iface ''` in the wifi-vlan and
    wifi-station stanzas. (My example above applies multi-psk to all APs
    on the router.)
3.  Many if not most people will need to add `option dynamic_vlan '1'`
    in their `config wifi-iface` sections, as takimata already [had
    noted](https://forum.openwrt.org/t/individual-per-passphrase-wifi-vlans-using-wpa-psk-file-no-radius-required/161696/11).

9 Likes

[regae](https://forum.openwrt.org/u/regae) September 24, 2023, 12:42pm

28

thanks, my build is the lateset snapshot.  
i did try with pmk local and pmk r1 push, same result,  
always back to master interface after roaming (checked with iwinfo iface
assoclist), instead of designated vlan interface.  
will try 23 branch.

also, can you share your wireless config ?

[zekica](https://forum.openwrt.org/u/zekica) September 24, 2023, 3:55pm

29

Latest snapshot should work fine - it is using even newer code. This is
the
[commit](https://w1.fi/cgit/hostap/commit/?id=ba150059d1ec964add8f29eb2c92dd6dfde97308)
that made it work with 11r FT. There is nothing special in my wifi
config except that it uses . What device are you using?

[regae](https://forum.openwrt.org/u/regae) September 25, 2023, 11:24am

30

is your wireless config for network defined?

[Spacebar](https://forum.openwrt.org/u/Spacebar) October 21, 2023,
4:04pm

31

This is great and something I really wanted to try. I believe this is
what others are calling ePSK, dPSK(Dynamic PSK), mPSK and pPSK.

[\_bernd](https://forum.openwrt.org/u/_bernd) October 21, 2023, 4:28pm

32

I can relate to all the buzzwords but also heard these days that
multiple SOHO products offer now similar features as this setup.

[Spacebar](https://forum.openwrt.org/u/Spacebar) October 21, 2023,
5:31pm

33

Sorry if it's already mentioned (I did not find any information on
this), but is this limited to WPA2 like Unifi? I read that this was not
a Unifi limitation...

[\_bernd](https://forum.openwrt.org/u/_bernd) October 21, 2023, 10:20pm

34

Just guessing but should work with other encryption modes too, because
why not? As far as I know hostapd does not care. Could even work without
a password at all, besides if you ignore the side effects.

[zekica](https://forum.openwrt.org/u/zekica) October 28, 2023, 6:18am

35

It is not limited to WPA2-PSK. In fact it was working fine in hostapd
for WPA3-SAE, WPA2-EAP and WPA3-EAP before it was ever working for
WPA2-PSK.

2 Likes

[Spacebar](https://forum.openwrt.org/u/Spacebar) October 28, 2023,
6:37am

36

That's lovely
![:+1:](https://forum.openwrt.org/images/emoji/twitter/+1.png?v=12 ":+1:")

[DjangoR](https://forum.openwrt.org/u/DjangoR) October 29, 2023, 1:43am

37

There is a very annoying bug when using WPA3 and a wpa_psk_file : if you
choose WPA3-SAE, fill in a key in luci (obviously, you can put only one
key there) and use a wpa_psk_file where each client have his own key,
this wpa_psk_file is completely ignored and the only valid key to
connect to the AP is the one you put in luci (option key 'your key')
Thus, you can't connect with the key in the wpa_psk_file, you may
believe that the AP is not working properly...

If you choose WPA2-PSK/WPA3 SAE mixed mode, then the wpa_psk_file is NOT
IGNORED but the client is forced to connect in WPA2 mode, even if he
support WPA3. If you want to connect in WPA3 mode, you have to use the
key entered in luci (option key 'your key')

tested on OpenWrt 22.03.5

1 Like

[regae](https://forum.openwrt.org/u/regae) October 29, 2023, 2:25am

38

i think for WPA3-SAE need to use sae_password, just a thought.

3 Likes

[grauerfuchs](https://forum.openwrt.org/u/grauerfuchs) October 29, 2023,
2:42pm

39

This certainly appears to be a real issue to me, too. I converted from
WPA2 using the uci wifi-vlan/wifi-station sections and the configuration
which works perfectly on WPA2-PSK does not work at all on WPA3-SAE. As
previously mentioned, only the primary key entered via/recognized by
luci is the one that works. The appropriate network interfaces appear to
exist after a full reload of wpad and no errors exist in the system log,
but it's acting almost as if the declarations aren't present when it
comes to connecting to the AP. I'm wondering if it's not internally
converting these sections into the sae_password declaration as it does
for wpa_psk_file, but I haven't yet been able to track down the
effective config file(s) that wpad is consuming when it loads.

[Catfriend1](https://forum.openwrt.org/u/Catfriend1) November 1, 2023,
1:09pm

40

This doesn't work for me on Xiaomi AX3600. Could you please share a full
config example of /etc/config/network and /etc/config/wireless without
passwords?

``` lang-plaintext
Nov  1 14:08:41 hostapd: nl80211: kernel reports: Attribute failed policy validation
Nov  1 14:08:41 hostapd: Failed to create interface phy1-ap0-test: -22 (Invalid argument)
Nov  1 14:08:41 hostapd: VLAN: Could not add VLAN phy1-ap0-test: No such device
Nov  1 14:08:41 hostapd: VLAN initialization failed.
Nov  1 14:08:41 hostapd: Interface initialization failed
```

I'm using wpad-mesh-openssl and my first SSID is 802.11s mesh. Then the
SSID that should have multiple PSK but fails startup follows. Do I still
need the "config wifi-iface" section? Or is this header replaced by
"config wifi-vlan"? How does OpenWrt know which "config wifi-vlan"
sections corresponds to which SSID?

[grauerfuchs](https://forum.openwrt.org/u/grauerfuchs) November 1, 2023,
2:09pm

41

The wifi-vlan and wifi-station config sections referenced work in
addition to the rest of the configuration sections. Consider it an
add-on/bolt-on feature. By default, the sections refer to any and all
wifi networks present on the device. If you want to limit it to a
specific ssid (and in your case you do), you also need to add a line to
the sections:

     option iface 'wifi-ifacename'

The iface referenced is the named wifi-iface. For instance, 'wifinet0'
is usually the first luci-created wifi interface.

The following example sets up a multi-PSK configuration with the guest
wireless being the primary and the LAN being a secondary. Any additional
radios or SSIDs would be unaffected:

``` lang-plaintext
{radio definitions}
...
config wifi-iface 'wifinet0'
        option device 'radio0'
        option mode 'ap'
        option ssid 'ExampleWiFi'
        option key '*redacted*'
        option network 'GUEST'
        option encryption 'psk2+ccmp'

config wifi-vlan
        option iface 'wifinet0'
        option name 'secondary'
        option vid '3'
        option network 'LAN'

config wifi-station
        option iface 'wifinet0'
        option vid '3'
        option key '*redacted*'

...
```

3 Likes

[Multi psk only on one
device](https://forum.openwrt.org/t/multi-psk-only-on-one-device/178468/2)

[Crect](https://forum.openwrt.org/u/Crect) November 1, 2023, 3:12pm

42

ath11k doesn't support this right now. [@robimarko](/u/robimarko) wanted
to upstream a patch that I fixed and refreshed but didn't find the time
yet.

2 Likes

[robimarko](https://forum.openwrt.org/u/robimarko) November 1, 2023,
4:25pm

43

Can you resend me the fix, I honestly completely forgot about it

3 Likes

[Crect](https://forum.openwrt.org/u/Crect) November 1, 2023, 7:27pm

44

Sure, here you go

![](https://forum.openwrt.org/letter_avatar/crect/48/5_f40856482fe1e45ac8f1605885140a2d.png)
[Dynalink DL-WRX36 Askey RT5010W IPQ8072A technical
discussion](https://forum.openwrt.org/t/dynalink-dl-wrx36-askey-rt5010w-ipq8072a-technical-discussion/110454/1772)
[For Developers](/c/devel/8)

> Rebased and amended the patch but it will just crash the firmware
> after a few seconds, though stations are able to connect before that.
> I only tried with the patch so can't tell if it's definitely it or 6.1
> but I assume the former. From 602272ef6049c81ac2a2bde8fb1095c06ccd74e7
> Mon Sep 17 00:00:00 2001 From: Seevalamuthu Mariappan
> \<quic_seevalam@quicinc.com\> Date: Mon, 10 Jan 2022 09:11:30 +0530
> Subject: \[PATCH\] ath11k: Add support for dynamic vlan Advertise
> AP-VLAN interface type for vlan sup…

3 Likes

[OpenWrt support for Linksys
MX4200](https://forum.openwrt.org/t/openwrt-support-for-linksys-mx4200/86477/1740)

[QFireball](https://forum.openwrt.org/u/QFireball) November 3, 2023,
1:24pm

45

I had a really difficult time with the configuration of my router and I
want to share my experience here. Firstly, it's about a 'covr-x1860'
router, which is supposed to serve as a simple dump access point.

``` lang-plaintext
ubus call system board
{
        "kernel": "5.15.130",
        "hostname": "OpenWrt",
        "system": "MediaTek MT7621 ver:1 eco:3",
        "model": "D-Link COVR-X1860 A1",
        "board_name": "dlink,covr-x1860-a1",
        "rootfs_type": "squashfs",
        "release": {
                "distribution": "OpenWrt",
                "version": "23.05-SNAPSHOT",
                "revision": "r23444-7714fb72be",
                "target": "ramips/mt7621",
                "description": "OpenWrt 23.05-SNAPSHOT r23444-7714fb72be"
        }
}
```

The problem I encountered may be more appropriate to discuss in
[this](https://forum.openwrt.org/t/add-support-for-d-link-covr-x1860/161862/212).
After that, the question arises whether to enable 'bridge VLAN
filtering' or configure it manually. So far, I've only managed to set up
an /etc/network configuration manually through Luci.

Unfortunately, I couldn't enable '11r FT' with 'disabled Generate PMK
locally' nor WPA3 encryption. So, my question is, has anyone managed to
make it work with 'bridge VLAN filtering'? Is there a working wireless
configuration with 11r FT that I can try out? And what about WPA3
encryption? From what I understand here, it seems to require setting it
up through psk2 + file as a separate AP configuration. Am I getting this
right?

I will post my configs later if that's desired.

[fakuivan](https://forum.openwrt.org/u/fakuivan) November 9, 2023,
11:35pm

46

Using and abusing wpa_psk_file I've made a package that generates PSKs
on the fly based on the station MAC address and a master password, with
support for VLANs too, based on the PSK used the station will get
connected to the appropriate VLAN. This "stateless" nature is useful to
me because it means I don't need to maintain a database with credentials
and roaming between APs with the same master passwords configured works
without any issues.

![](https://github.githubassets.com/favicons/favicon.svg)
[GitHub](https://github.com/fakuivan/hostapd-slppsk)

![](https://opengraph.githubassets.com/ad3b131231079a4b22baa8b43910e68c36c3bafb1617067cc768a69eada8d3ed/fakuivan/hostapd-slppsk)

### [GitHub - fakuivan/hostapd-slppsk: Stateless Per-Device PSK for hostapd in...](https://github.com/fakuivan/hostapd-slppsk)

Stateless Per-Device PSK for hostapd in OpenWRT (different wifi password
for each device under the same SSID without WPA Enterprise) - GitHub -
fakuivan/hostapd-slppsk: Stateless Per-Device PSK for...

I'll leave the link here in case anyone is interested, this is a
prototype at best and at worst it can be used to understand any configs
of interest here.

5 Likes

[pluffmud](https://forum.openwrt.org/u/pluffmud) November 17, 2023,
3:55pm

47

Does anyone have this working with 2 SSID's ? It works fine with 1 but
if I add another SSID (either different radio or additional SSID on
original) the login to the new fails with 'incorrect password'

"brctl show" gives this

> bridge name bridge id STP enabled interfaces  
> br-lan 7fff.a42bb0ad6a3c no phy0-ap1  
> wifivlan.1  
> eth1.1  
> phy0-ap0  
> br-vlan3 7fff.a42bb0ad6a3d no wifivlan.3  
> eth0.3

"hostapd.vlan" contains

> 1 wifivlan.1 br-lan  
> 3 wifivlan.3 br-vlan3

"hostapd.wpa_psk" contains

> vlanid=1 00:00:00:00:00:00 phrase1  
> vlanid=3 00:00:00:00:00:00 phrase3

wireless config contains

This is openwrt 23.05.0

[QFireball](https://forum.openwrt.org/u/QFireball) November 19, 2023,
10:42am

48

In my case, it works with the settings of takimata.

``` lang-plaintext
opkg install wpad-wolfssl "OR" opkg install wpad-openssl
/etc/config/wireless 
        option wpa_psk_file '/etc/hostapd.wpa_psk'
        option vlan_file '/etc/hostapd.vlan'
        option vlan_tagged_interface 'eth0' <- "Your device wich should be splittet in vlans OR no line if device is splittet"
        option vlan_bridge 'br-vlan' <- "Nonexisting name"
        option dynamic_vlan '1'
```

[Catfriend1](https://forum.openwrt.org/u/Catfriend1) November 19, 2023,
10:59am

49

Hi robimarko, could you please post here when the fix is merged so I
could try again with my AX3600?

[robimarko](https://forum.openwrt.org/u/robimarko) November 19, 2023,
11:04am

50

I have not even sent it upstream yet, I just dont have the bandwidth to
do currently

3 Likes

[QFireball](https://forum.openwrt.org/u/QFireball) November 21, 2023,
7:30pm

51

I was able to get 'Bridge VLAN Filtering' and '11r FT' to work using
wpa_psk_file.

[pattagghiu](https://forum.openwrt.org/u/pattagghiu) December 4, 2023,
8:01pm

52

stupid question here: what is the advantage of this solution above
having different ssid? i mean, in case of 2/3 ssid (not a ssid for each
client..). Is it relevant in terms of performance? very cool solution,
but i like having a "Iot", a "Guest" and a "Home" ssid with respective
vlans
![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:")

[golialive](https://forum.openwrt.org/u/golialive) December 4, 2023,
9:08pm

53

I guess two or three SSIDs are no problem. You might want to read up on
"wifi beacon" or "beacon frame". There's radio signals going on even if
there's no traffic, even if there's no clients connected to an access
point. As the number of SSIDs on a single radio increases, the number of
beacon frames becomes a significnt portion of your airtime usage. The
wikipedia article about beacon frames states 15 SSIDs as a limit. If you
manage to have a single SSID for a lot of different networks, all those
only need a single beacon frame, hence reduce the required overhead to
make it work.

2 Likes

[xize](https://forum.openwrt.org/u/xize) December 4, 2023, 9:37pm

54

One reason i can think of is to personalize it per invidual for example
if you want to set time restriction on a certain network.

A other reason might be some people don't like a extra ssid for iot
called iot or smart so per convience they can still isolate it via a
vlan and different passphrase.

for me kinda both count heres a example from my topology:

The main reason i want to use multi psk is since i use a windows gaming
handheld, i dont want it to be in iot but neither in the normal wifi so
it gets its own personalized network via vlan, which also mean it can
access some of my local services like a lancache for example.

Per convience the iot wifi which i had, i also put as multi psk.

Now i only got 2 ssids only for 5ghz and 2.4ghz for me its more for
convience and abstraction visibility.

[Harakan](https://forum.openwrt.org/u/Harakan) December 9, 2023, 10:24pm

55

I'd really like to try this out, but I'm unclear on how this interacts
with the wireless client isolation feature?

I currently use three SSIDs each assigned to a VLAN:

- VLAN A - Trusted devices (no client isolation)
- VLAN B - IOT devices (no client isolation)
- VLAN C - External-only guest/IOT devices (client isolation enabled)

Devices in VLAN A/B can see other clients on the same SSID (but not
other SSIDs); devices in VLAN C cannot see any other clients.

Can this be achieved using the wpa_psk_file approach?

[takimata](https://forum.openwrt.org/u/takimata) December 10, 2023,
8:35am

56

![](https://forum.openwrt.org/letter_avatar/harakan/48/5_f40856482fe1e45ac8f1605885140a2d.png)
Harakan:

> I'd really like to try this out, but I'm unclear on how this interacts
> with the wireless client isolation feature?

Wifi client isolation is a per-BSS setting, all clients on the BSS would
be isolated from each other, regardless of VLANs.

(As far as I understand it, the *isolate* option causes packets to be
rejected if both the source and destination MAC are on the same BSS.)

[\_bernd](https://forum.openwrt.org/u/_bernd) December 10, 2023, 11:34am

57

As [@takimata](/u/takimata) explained it already; the client isolation
feature is a function on a single BSS. Therefor using `wpa_psk_file` (or
RADIUS) to assign VLANs dynamically you have to choose to use client
isolation or not.

So if I understand you correctly you have 2 options:

1.  Enable client isolation on the (single) SSID -- which uses
    `wpa_psk_file` to assign VLANs dynamicly -- and isolate *all*
    clients from each other.
2.  (VLANs are already separated layer-2 domains by definition.) Using
    "only" VLANs, and for clients which needs to be truly isolated, give
    them a dedicated VLAN, which could result in heavy config overhead.
3.  Bonus: Don't use client isolation at all.
    ![:person_shrugging:](https://forum.openwrt.org/images/emoji/twitter/person_shrugging.png?v=12 ":person_shrugging:")
    (Besides of public hot-spots I personally don't see a benefit in
    using client isolation, but maybe I'm biased.)

2 Likes

[Harakan](https://forum.openwrt.org/u/Harakan) December 10, 2023,
12:04pm

58

Thanks both. It seems that I still need the multiple SSIDs for my use
case, so there's little benefit in using per-passphrase VLANs as well.

A single SSID with client isolation breaks critical use cases (e.g.
PC-to-PC network shares between trusted wireless devices).

A single SSID without client isolation is a security risk (e.g. a
compromised guest/IOT device could be used to launch attacks against
trusted wireless PCs etc. that would otherwise be totally isolated by
VLAN+firewall rules).

[\_bernd](https://forum.openwrt.org/u/_bernd) December 10, 2023, 12:58pm

59

I'm under the impression you did not understand both vlan and client
isolation.

If your PC is on vlan 78 and you iot shit is on vlan 523 then they are
on separate layer-2 domains!  
If you use client isolation then clients on the same wireless layer 2
are not able to communicate on layer 2 directly.  
If having a single ssid but with dynamic vlans then your PCs are on a
different vlan then your iot shizzle. I do not understand your FUD.

1 Like

[Harakan](https://forum.openwrt.org/u/Harakan) December 10, 2023, 1:39pm

60

Can you explain how this can keep guests isolated from each other? I
don't believe it is possible, but if you believe I do not understand it
properly maybe I'm still missing something.

If client isolation is disabled, all wireless devices on a VLAN can see
each other - so any guests can access each other. With client isolation,
every individual device is isolated.

Using a single SSID without client isolation, I would need to create a
unique passphrase+VLAN for every individual guest device before allowing
them to connect.

Using a single SSID with client isolation, I would need to create a
unique passphrase+VLAN for every trusted device, and set up firewall
zones and rules to allow them to communicate with each other.

Using separate SSIDs, the guest SSID+passphrase can be made known and
still be confident that any/all guests can only access the Internet -
not any other wired/wireless devices, including other guests - without
needing to do any per-client configuration.

[\_bernd](https://forum.openwrt.org/u/_bernd) December 10, 2023, 2:05pm

61

You could setup two ssid. One for general trusted devices which don't
need client isolation but individual vlan (lan, kids, work from home,
etc) . And another ssid where both is required, individual vlan (CCTV,
lights, etc) and client isolation?

[Harakan](https://forum.openwrt.org/u/Harakan) December 10, 2023, 4:20pm

62

Yes - that is my existing setup (one SSID for each group of devices that
need to communicate with one another, plus an SSID for isolated
clients). I was hoping this feature would eliminate the need for
multiple SSIDs, but it doesn't for my use case.

[\_bernd](https://forum.openwrt.org/u/_bernd) December 10, 2023, 4:30pm

63

It still can. In case you need more then 2 vlan or ssid. It will not
scale if you announce like 12 ssid. I think most wireless driver are
limited to like 4 ssid. With wpa psk file, or radius private tunnel
group id, you could, if you like, use all 2^12 - 2 vlan IDs on a single
or even two ssid.

[oxwivi](https://forum.openwrt.org/u/oxwivi) December 10, 2023, 4:46pm

64

You're misunderstanding his goals here. He knows and already isolates
his IoT and guests from his regular LAN. But he also wants IoT and guest
devices isolated from *each other*. It's already established that client
isolation cannot be done with `wpa_psk_file`:

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> Wifi client isolation is a per-BSS setting, all clients on the BSS
> would be isolated from each other, regardless of VLANs.

[\_bernd](https://forum.openwrt.org/u/_bernd) December 10, 2023, 4:49pm

65

I think I understand the request pretty well.  
And I have explained all the various setup options well I would have
thought.  
Yes a single SSID with dynamic vlans can not have isolated clients on a
per vlan basic because it's an SSID setting. So if the user needs both,
the user could use one ssid without client isolation and dynamic vlans,
and another ssid with client isolation and dynamic vlans.

2 Likes

[oxwivi](https://forum.openwrt.org/u/oxwivi) December 10, 2023, 5:01pm

66

I was also looking into it, and similarly wanted client isolation. I see
your point is bunching up LANs that need client isolation to single SSID
to reduce the total number of SSIDs.

But what is that dynamic VLAN thing you mention? Could you point me to
some docs?

[\_bernd](https://forum.openwrt.org/u/_bernd) December 10, 2023, 5:52pm

67

We are talking here about dynamic vlan assignment the whole time, lol
![:wink:](https://forum.openwrt.org/images/emoji/twitter/wink.png?v=12 ":wink:")

Normally it's named 802.1x but this normally required a whole bunch of
stuff, like RADIUS, and proper TLS certificates and yadda yadda yadda.
But, hostapd can simply stuff a wireless client in a vlan by Mac address
or in this case, by a defined PSK, preshared Key aka password on a SSID.

[oxwivi](https://forum.openwrt.org/u/oxwivi) December 10, 2023, 5:54pm

68

No I got that part. I assumed "dynamic VLAN" (from its name) to mean
generating VLANs on the fly somehow. Guess not.

[\_bernd](https://forum.openwrt.org/u/_bernd) December 10, 2023, 6:01pm

69

Yeah, the dynamic is just that you have no static / predefined vlan on a
wireless or a switch port on an Ethernet network, but the client gets
into a vlan either on an access point or switch based on the radius
private tunnel group id with 802.1x and/or wpa2 enterprise.

2 Likes

[roshii](https://forum.openwrt.org/u/roshii) January 2, 2024, 3:23pm

70

`sae_password` or `sae_password_file` option for hostapd are indeed not
converted by openwrt's hostapd script, therefore limiting VLAN ID
assignment to `psk2`.

4 Likes

[webtron](https://forum.openwrt.org/u/webtron) January 18, 2024, 1:51am

71

I'm testing this on a Unifi ac lite(OpenWrt 23.05.0, r23497-6637af95aa)
and I'm finding if I have both radios with the same SSID it works until
my phone decides to roam to the other band.  
I then get a heap of errors in the logs where the vlan connection is
created and then dumped. Has anyone got this working with dual radios
and the same SSID?  
I'm using the wildcard entry only in the vlans file

- vlan#

1 Like

[roshii](https://forum.openwrt.org/u/roshii) January 21, 2024, 12:52pm

72

I just created a [PR](https://github.com/openwrt/openwrt/pull/14448) to
resolve this limitation, I am now able to assign VLAN IDs to WAP3
clients
![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:")  
Hopefully it'll get merge soon
![:crossed_fingers:](https://forum.openwrt.org/images/emoji/twitter/crossed_fingers.png?v=12 ":crossed_fingers:")

7 Likes

[regae](https://forum.openwrt.org/u/regae) January 21, 2024, 1:53pm

73

is there anyone here with mt7621/mt7615 devices using wpa_psk_file with
vlan?  
with re650 v1, ea7500 v2, ea8100 v1, if using vlan configuration
sometimes the STA lost connection at all, no local ping, no internet
connection.  
the connection will be back after several minutes if i take no action.  
reconnect to the wifi, brings back the connection.

tested on 21.02 - 22.03 - 23.05 - SNAPSHOTS

[grauerfuchs](https://forum.openwrt.org/u/grauerfuchs) January 21, 2024,
2:31pm

74

I'm running a Ubiquiti UniFi 6 Lite here (MT7621; WifI MT7603E &
MT7615E) using the wpa_psk_file definitions. The only issue I have is in
reconfiguring the settings via the uci config files. The changes aren't
properly handled during a soft reload, so the networks go haywire until
I do a full restart of wpad. I've had no issues with dropouts outside of
the reconfiguration issues.

[regae](https://forum.openwrt.org/u/regae) January 21, 2024, 2:45pm

75

are you using vlanid param?  
i have this symptom only if using vlan, even with RADIUS ppsk/ipsk.

my goal is using the same SSID for regular and guest user separated by
vlan.

[grauerfuchs](https://forum.openwrt.org/u/grauerfuchs) January 21, 2024,
2:56pm

76

Yes I am, via the OpenWRT uci 'wifi-station' and 'wifi-vlan' config
sections. I'm using a single SSID to provide service across three
separate VLANs including regular, guest, and another untrusted network.

1 Like

[ed8](https://forum.openwrt.org/u/ed8) January 21, 2024, 3:15pm

77

Do you have 802.11r Fast Transition Enabled?  
=\> if so, can you re-verify without this option?

[regae](https://forum.openwrt.org/u/regae) January 21, 2024, 3:54pm

78

no, it's disabled.  
the thing is, this only happened to mt7621/mt7615 as i mentioned
before.  
tried it with google onhub and engenius eap1200h both with ath10k, same
setup, all working normally.  
i am getting frustrated by keep trying.

[@grauerfuchs](/u/grauerfuchs) thankyou for the answer

[grauerfuchs](https://forum.openwrt.org/u/grauerfuchs) January 21, 2024,
4:12pm

79

Try checking the system log and kernel log the next time you notice this
happening. Are there any unusual entries there? This sounds to me like
either a conflict or interference, or possibly something strange going
on with the particular device. The log might show errors, certain types
of interference (some drivers only), or an unexpected reboot.

As for radio-level interference possibilities, if a network on a DFS
frequency detects certain levels and types of interference, it may shut
down to avoid interfering with a priority service. Likewise, on the
2.4GHz band, a network may effectively be shut down by other nearby
devices operating in the band (other WiFi networks? Microwave oven?
Portable telephone?)

The different devices might operate differently based on a number of
things including physical position, gain, antennas, radio
susceptibility, etc.

[regae](https://forum.openwrt.org/u/regae) January 21, 2024, 4:27pm

80

no log indicates anything at all when it happened.  
even with log_level set to debug.  
2.4Ghz tested with 1, 6, 11 channel  
5Ghz tested with 36,40,44,149,153,161 channel

the weird thing is this issue only happened with vlanid set in
wpa_psk_file.  
when only psk is set, without vlanid, all normal.  
so i think it has nothing to do with interference, imho.

[grauerfuchs](https://forum.openwrt.org/u/grauerfuchs) January 21, 2024,
4:48pm

81

Understood. I haven't experienced the issue with a device on the same
base chip, but we still have a relatively small sample size and so
narrowing things down is difficult. Who knows; At this point, my device
simply working may be the outlier and fluke of the whole thing.

The only next steps I could suggest at this point would require a lot of
detailed analysis on inputs and outputs from the related devices. Given
the extreme cost in hardware resources and time involved in tracking it
down that way, I'm hoping we'll have other people chime in with
experiences that could help further narrow down the issue.

[roshii](https://forum.openwrt.org/u/roshii) January 24, 2024, 10:58am

82

After further testing, it appeared that I had seen successfull
connection to separate VLAN due to a
[bug](https://github.com/openwrt/openwrt/issues/14458).

Above mentioned PR should add support for `sae_password_file` bss option
but will not allow VLAN ID assignment for `sae` encryption mode. And
this will likely not happen any time soon, as confirmed on hostap
[mailing
list](http://lists.infradead.org/pipermail/hostap/2024-January/042298.html).

2 Likes

[johnsmith31](https://forum.openwrt.org/u/johnsmith31) February 11,
2024, 12:35am

83

Fantastic thread overall. I've read the whole thing from start to finish
and am a little unclear on a few questions:

- Is there a full How-to document for this functionality posted
  somewhere on the Wiki yet?

- Is it still necessary to replace `wpad-basic-wolfssl` with the full
  `wpad-wolfssl`, or has this been rendered unecessary by later OpenWRT
  versions?

- Is it true/correct that basically this feature won't work with WPA3 /
  SAE, and that there's not likely to be a fix for that due to the way
  SAE itself works?

(Fwiw: my setup is a single Unifi UAP-AC-LITE running OpenWRT 23.05.2,
configured as a pure WAP AP with 3 VLANs, routing handled upstream by a
separate Opnsense router/firewall/gateway.)  
(ath79/generic, kernel 5.15.137)

[takimata](https://forum.openwrt.org/u/takimata) February 11, 2024,
4:41am

84

![](https://forum.openwrt.org/letter_avatar/johnsmith31/48/5_f40856482fe1e45ac8f1605885140a2d.png)
johnsmith31:

> Is there a full How-to document for this functionality posted
> somewhere on the Wiki yet?

No.

> Is it still necessary to replace `wpad-basic-wolfssl` with the full
> `wpad-wolfssl`

Yes.

[\_bernd](https://forum.openwrt.org/u/_bernd) February 11, 2024, 7:51am

85

To answer your third question:

![](https://forum.openwrt.org/letter_avatar/johnsmith31/48/5_f40856482fe1e45ac8f1605885140a2d.png)
johnsmith31:

> Is it true/correct that basically this feature won't work with WPA3 /
> SAE, and that there's not likely to be a fix for that due to the way
> SAE itself works?

As I have understood the rumors, no, wpa3 and dynamic VLAN does not play
well together.

1 Like

[oldium](https://forum.openwrt.org/u/oldium) March 3, 2024, 10:44pm

86

Does using `wifi-vlan` (without `vlan_tagged_interface` and
`vlan_bridge`) work for anybody on Git master? I am not able to get the
wifi interface added to the bridge. It works with `vlan_bridge` option
(I am using `br-vlan<n>` pattern), but I cannot get it working with just
`wifi-vlan` (without `vlan_bridge` option). I read several times in
Forums that it should work, some mentioning I have to add
`none`-protocol interface (which I did), so maybe something is broken on
master, or I am still doing something wrong.

[SkyCrw](https://forum.openwrt.org/u/SkyCrw) March 10, 2024, 1:59pm

87

Hello Guys,

I'm new to this forum but not new to openwrt, although there is alot of
virgin grounds within the domain I've never tried..  
I've recently setup a new linksys ea8300 with 23.05.2 and am trying some
things out before putting this router in my network (my active router,
same linksys ea8300 runs 22.03.5) and I am trying to setup vlans for my
wifi to use what other brands call Dynamic psk but haven't managed to
get it working as explained by [@takimata](/u/takimata) or
[@grauerfuchs](/u/grauerfuchs) , I assume this setup should work too if
router is running DSA switch ?

[slh](https://forum.openwrt.org/u/slh) March 11, 2024, 2:37am

88

![](https://forum.openwrt.org/letter_avatar/skycrw/48/5_f40856482fe1e45ac8f1605885140a2d.png)
SkyCrw:

> I assume this setup should work too if router is running DSA switch ?

ipq40xx (as in the ea8300) isusing DSA since 23.05.x.

[takimata](https://forum.openwrt.org/u/takimata) March 11, 2024, 4:22am

89

![](https://forum.openwrt.org/letter_avatar/skycrw/48/5_f40856482fe1e45ac8f1605885140a2d.png)
SkyCrw:

> I assume this setup should work too if router is running DSA switch ?

I would assume likewise, but I can't offer any actual first-hand
experience, I actually exclusively used this method on switchless
single-port devices I use as access points. What I could imagine is that
DSA requires the bridges to be set up first through the network
configuration, and hostapd being configured to latch onto those bridges
instead of creating new ones.

[NPeca75](https://forum.openwrt.org/u/NPeca75) March 11, 2024, 4:57am

90

Hi [@takimata](/u/takimata)

it is working

76x8 swconfig  
7621 DSA  
ipq 4019 DSA

OWRT v23.05

``` lang-plaintext
7621

bridge vlan sh
port              vlan-id  
lan4              255 PVID Egress Untagged
wan               1 PVID Egress Untagged
                  2
                  100
                  200
                  255
switch            1
                  2
                  100
                  200
                  255
wlan0             2 PVID Egress Untagged
wlan0-vl255       255 PVID Egress Untagged
wlan0-vl200       200 PVID Egress Untagged
wlan0-vl100       100 PVID Egress Untagged
wlan1             2 PVID Egress Untagged
wlan1-vl255       255 PVID Egress Untagged
wlan1-vl200       200 PVID Egress Untagged
wlan1-vl100       100 PVID Egress Untagged
```

``` lang-plaintext
76x8

brctl show
bridge name     bridge id               STP enabled     interfaces
br-vlan100              7fff.0e0001030101       no              eth0.100
                                                        wlan0-vl100
br-vlan2                7fff.0e0001020101       no              eth0.2
                                                        wlan0
br-vlan200              7fff.0e0001040101       no              wlan0-vl200
                                                        eth0.200
br-vlan1                7fff.0e0001010101       no              eth0.1
br-vlan255              7fff.0e0001050101       no              wlan0-vl255
                                                        eth0.255
```

[takimata](https://forum.openwrt.org/u/takimata) March 11, 2024, 5:35am

91

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/npeca75/48/57159_2.png)
NPeca75:

> it is working

That's great. Maybe you would like to share your relevant (and obviously
redacted) wireless/network configuration for posterity?

[NPeca75](https://forum.openwrt.org/u/NPeca75) March 11, 2024, 6:20am

92

already posted in another thread, but why not
![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:")

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/npeca75/48/57159_2.png)
[How to setup vlans for dynamic PSK on router with 1
SSID?](https://forum.openwrt.org/t/how-to-setup-vlans-for-dynamic-psk-on-router-with-1-ssid/190981/2)
[Network and Wireless
Configuration](/c/general/network-and-wireless-configuration/12)

> hi [@SkyCrw](/u/skycrw) if this help you ... i will paste my working
> configs, so you could learn and adapt for your need vlan1 unused
> vlan2,100,200,255 is used and attached to WIFI vlan255 is management
> cat /etc/config/network config interface 'loopback' option device 'lo'
> option proto 'static' option ipaddr '127.0.0.1' option netmask
> '255.0.0.0' config device option type 'bridge' option vlan_filtering
> '1' option name 'switch' list ports…

it is DSA based

1 Like

[yogo1212](https://forum.openwrt.org/u/yogo1212) March 13, 2024, 12:22am

93

Hey there
![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:")

Just in case there's still confusion about PSK2 vs. SAE with regard to
multiple PSKs and VLAN:  
PSK-/MAC-based VLAN assignments work in principle (scroll down to
**Solution** if you don't care about the details).  
There are problems when the assignment is ambiguous. Because both
parties in SAE find out whether they are using the same password **at
the same time**, the AP has only one attempt at checking a password
(when the AP checks a non-matching password, the client will think they
entered the wrong password).

That means that, in theory, no more than one 'default' PSK can exist.  
All other station definitions must include the MAC address.

PSK2 authentication uses `hostapd_wpa_auth_get_psk` from
`src/ap/wpa_auth_glue.c` which can be called multiple times to return
more than one password. It looks for a matching PSK first in
`hostapd_get_psk` (`src/ap/ap_config.c`) and then goes through the list
on the `sta_info` corresponding to the MAC - that doesn't really work
with SAE.

Because everything is so different with SAE, it has different code
paths. For instance `sae_get_password` from `src/ap/ieee802_11.c`: It
first looks for a match in `sae_passwords` (see below), then it tries
`wpa_passphrase`, and - only if none of those yielded a password - will
it check the station-specific PSKs. And it'll take only the first
non-PMK. BUT even if you manage to get a match through there,
`auth_build_sae_commit` only considers VLAN IDs from a
`sae_password_entry`.

# [](#solution-1)Solution

There is a config option `sae_password`.  
From `hostapd.conf`:

``` lang-plaintext
# The last matching (based on peer MAC address and identifier) entry is used to
# select which password to use. Setting sae_password to an empty string has a
# special meaning of removing all previously added entries.
```

There's also an example:

``` lang-plaintext
#sae_password=really secret|mac=ff:ff:ff:ff:ff:ff
#sae_password=example secret|mac=02:03:04:05:06:07|id=pw identifier
#sae_password=example secret|vlanid=3|id=pw identifier
```

Dynamic VLAN assignment is definitely supported with SAE - it's just
done differently than it is with PSK2.  
For it to be practical to use with Openwrt, a lot still needs to be
done.  
A first step could be to convert the stations from UCI to
`sae_password=` config entries and reloading them through ucode.  
A better idea could be to have hostapd read SAE passwords from a file
that can be reloaded separately (more work).

6 Likes

[yogo1212](https://forum.openwrt.org/u/yogo1212) March 13, 2024, 12:29am

94

Oh - nice!

It looks like [@roshii](/u/roshii) has already started working on it
![:partying_face:](https://forum.openwrt.org/images/emoji/twitter/partying_face.png?v=12 ":partying_face:")

(EDIT: me dumb. i was there just a while earlier and even left a
comment. shouldn't have stayed up so late)

[roshii](https://forum.openwrt.org/u/roshii) March 17, 2024, 6:28pm

95

I will try to finalize my PR by end March. My free time is scarce these
days, unfortunately.

[fakuivan](https://forum.openwrt.org/u/fakuivan) March 20, 2024, 12:05pm

96

I've read somewhere that SAE password identifiers could be used to
bypass this limitation. What's the support like for this feature?

[roshii](https://forum.openwrt.org/u/roshii) March 25, 2024, 8:49pm

97

Password indentifiers are not supported on Android or iOS devices afaik

[dannutu](https://forum.openwrt.org/u/dannutu) April 1, 2024, 8:48am

98

This was very useful, thank you!

Is there a way to apply **the same** wifi-vlan and wifi-station stanzas
to more than one virtual AP **but not to all**? E.g. I have 5 APs (2x
2.4Ghz, 2x 5GHz UNII-1 and 1x 5GHz UNII-2c) and 10 VLANs, which I want
applied to just 2 of the APs. I tried adding a second "option iface"
line for wifi-vlan and wifi-station but it didn't work (as expected).

LE: In case you are wondering why I want to set them like this - it's
because I want SAE on one AP on each band and until [@roshii](/u/roshii)
manages to complete the PR for this SAE won't play nice with wifi-vlan
and wifi-station. Hence I added a 2nd AP with just PSK2 on 2 of the 3
bands.

[takimata](https://forum.openwrt.org/u/takimata) April 2, 2024, 5:23am

99

![](https://forum.openwrt.org/letter_avatar/dannutu/48/5_f40856482fe1e45ac8f1605885140a2d.png)
dannutu:

> Is there a way to apply **the same** wifi-vlan and wifi-station
> stanzas to more than one virtual AP **but not to all**? E.g. I have 5
> APs (2x 2.4Ghz, 2x 5GHz UNII-1 and 1x 5GHz UNII-2c) and 10 VLANs,
> which I want applied to just 2 of the APs.

You still need to apply the settings to every AP individually, you can
as well be selective about it.

[simon_lefisch](https://forum.openwrt.org/u/simon_lefisch) April 2,
2024, 8:16pm

100

Hello!

I have a question for everyone....I am trying to implement this feature
on my TP-Link EAP-615v1 AP. However when I try to install the package
(based on the second post in this thread), I get the error below.

``` lang-plaintext
root@OpenWrt:~# opkg install wpad-wolfssl
Installing wpad-wolfssl (2023-09-08-e5ccbfc6-6) to root...
Collected errors:
 * check_conflicts_for: The following packages conflict with wpad-wolfssl:
 * check_conflicts_for:         wpad-basic-mbedtls *
 * opkg_install_cmd: Cannot install package wpad-wolfssl.
```

So when I try to install the full wpad_mbedtls package, it says it can't
(I believe it's due to the dependencies).

Does anyone know if this feature available for my device? I'd love to
implement it if possible. TIA
![:pray:](https://forum.openwrt.org/images/emoji/twitter/pray.png?v=12 ":pray:")

[grauerfuchs](https://forum.openwrt.org/u/grauerfuchs) April 2, 2024,
8:35pm

101

The easiest solve is to replace the firmware with a custom build from
the firmware selector on the OpenWRT site. Just open up the
` Customize installed packages and/or first boot script` option and edit
away. Just make sure you don't delete anything without replacing it with
the alternative, or you could end up with a firmware that doesn't work
properly.

[takimata](https://forum.openwrt.org/u/takimata) April 2, 2024, 9:27pm

102

![](https://forum.openwrt.org/letter_avatar/simon_lefisch/48/5_f40856482fe1e45ac8f1605885140a2d.png)
simon_lefisch:

> However when I try to install the package (based on the second post in
> this thread), I get the error below.

Generally speaking you need to remove the *basic* wpad and install the
non-*basic* wpad.

My post you are referring to was written when the then-current release
version of OpenWrt, 22.03, used *wolfssl* as its SSL library. With
23.05, OpenWrt switched to *mbedtls*.

Consequently you now need to `opkg remove wpad-basic-mbedtls`,
`killall hostapd`, and then `opkg install wpad-mbedtls`.

2 Likes

[simon_lefisch](https://forum.openwrt.org/u/simon_lefisch) April 2,
2024, 9:36pm

103

You, my friend, are awesome! Thank you for that clarification. I will
make sure to do that when I get home later today.

[simon_lefisch](https://forum.openwrt.org/u/simon_lefisch) April 3,
2024, 5:26pm

104

[@takimata](/u/takimata) I was able to get this working after installing
the full *wpad-mbedtls* package. I used the configuration from [this
thread](https://forum.openwrt.org/t/how-to-setup-vlans-for-dynamic-psk-on-router-with-1-ssid/190981/16)
as an example and now have password-based VLANs. Thanks again for
clarifying that.

I am wondering tho, will this ever be able to use *sae-mixed*
encryption? I was only able to get it working with *psk2*, but would
love to use *sae-mixed* if possible.

1 Like

[grauerfuchs](https://forum.openwrt.org/u/grauerfuchs) April 3, 2024,
6:35pm

105

![](https://forum.openwrt.org/letter_avatar/simon_lefisch/48/5_f40856482fe1e45ac8f1605885140a2d.png)
simon_lefisch:

> I am wondering tho, will this ever be able to use *sae-mixed*
> encryption? I was only able to get it working with *psk2*, but would
> love to use *sae-mixed* if possible.

sae-mixed won't give you any benefit even if this particular issue is
resolved. First of all, some (usually older) devices will not respond
kindly to the mixed-mode network. Many WPA2-only devices simply won't
connect no matter what, due to the network's advertisement of and/or
requirement of features that aren't supported by those devices. Even
some devices that officially support both WPA2 and WPA3 still get flaky
and experience issues in the mixed-mode environment.

If you're looking to upgrade to WPA3 for the added security but still
want WPA2 available for compatibility, you should also be aware that the
very nature of the sae-mixed network means any device could be
forcefully and remotely bumped back down to using WPA2. Once that has
been done, the entire security value of using WPA3 is lost anyway.

If that all weren't enough, there are numerous reports out there of the
sae-mixed networks being broken in other ways as well. If those reports
are true, then even with this issue resolved, you may still be blocked
by other problems.

3 Likes

[simon_lefisch](https://forum.openwrt.org/u/simon_lefisch) April 3,
2024, 11:12pm

106

Got it. Thanks for that heads up on that.

[dannutu](https://forum.openwrt.org/u/dannutu) April 11, 2024, 7:38pm

107

Thank you for your answer. I was afraid of this though. Besides the
obvious duplication in the configuration file, if I want to say apply
the same wifi-vlan and wifi-station to two APs, can I use exactly the
same "option name" value for both of them or do I need to choose
different names for each in order to avoid clashes? I'm asking because
with 1 wifi-vlan and 1 wifi-station for 2 APs it is relatively
straightforward but with larger numbers the extra names to avoid
duplication would be just plain silly.

[takimata](https://forum.openwrt.org/u/takimata) April 11, 2024, 10:45pm

108

![](https://forum.openwrt.org/letter_avatar/dannutu/48/5_f40856482fe1e45ac8f1605885140a2d.png)
dannutu:

> Besides the obvious duplication in the configuration file

You will always have duplicate configurations, even with a RADIUS setup
you still have to configure the individual APs. But yes, I see your
point, it may get cumbersome to deploy changes and more elaborate
configurations. Wasn't there someone who created a set of scripts to do
that centralized?

Anyway, noone's stopping you from copy&pasting:

> if I want to say apply the same wifi-vlan and wifi-station to two APs,
> can I use exactly the same "option name" value for both of them

I don't see why not. The other devices don't see that, they only see the
traffic from the AP tagged with the respective numeric VLAN tag, and
sharing that VLAN-tagged traffic between devices is actually kind of the
point.

[nmpu](https://forum.openwrt.org/u/nmpu) April 13, 2024, 3:16pm

109

I currently use a bunch of different SSIDs for segregating traffic. Each
has its own bridge device and VLAN interface. This works fine, but
generates extra 'beacon' traffic. Since I normally keep most of the
SSIDs hidden, client setup typically requires extra steps in the UI.
Using a single SSID with different passwords would eliminate both
issues.

It's my understanding that your setup will automatically create the
bridge device and VLAN interface based on the **hostapd.wpa_psk** and
**hostapd.vlan** files. I have **wpad-mbedtls** installed. I think I've
got everything configured properly, but LuCI shows the SSIDs as
disabled. I don't see any error messages, but maybe I'm not looking in
the right place.

The
[documentation](https://openwrt.org/docs/guide-user/network/wifi/basic)
gives the following example of a minimal config:

``` lang-plaintext
config    wifi-iface
    option  device      'wl0'
    option  network     'lan'
    option  mode        'ap'
    option  ssid        'MyWifiAP'
    option  encryption  'psk2'
    option  key     'secret passphrase'
```

It seems that OpenWrt is not happy without a **network** and **key**,
but the key now has multiple values and the network is supposed to be
generated dynamically/automatically. What am I doing wrong? How can I
tell if my hostapd files are being used?

Here's my **wireless** file. I've actually configured 2 SSIDs on each
band. Since I have 2 APs, I normally use overlapping SSIDs, but I want a
specific SSID for debugging. X is 5GHz overlapped. X2 is 5GHz on AP2. x
is 2.4GHz overlapped. x2 is 2.4GHz on AP2. I've added custom MAC
addresses so that I can use a common **hostapd.wpa_psk**. I could
further simplify if I can reuse the same MAC.

``` lang-plaintext
config wifi-device 'radio0'
    option type 'mac80211'
    option path 'ffe0a000.pcie/pcia000:02/a000:02:00.0/a000:03:00.0'
    option channel '48'
    option band '5g'
    option htmode 'VHT80'
    option cell_density '0'

config wifi-iface 'wifinet1'
    option device 'radio0'
    option ifname 'X'
    option macaddr 'ee:ee:ee:ee:00:22'
    option mode 'ap'
    option ssid 'X'
    option hidden '0'
    option encryption 'psk2'
    option key 'not_used'
    option wpa_psk_file '/etc/hostapd.wpa_psk'
    option vlan_file '/etc/hostapd.vlan'
    option vlan_tagged_interface 'eth0'
    option vlan_bridge 'magic'  
    option dynamic_vlan '2'

config wifi-iface 'wifinet2'
    option device 'radio0'
    option ifname 'X2'
    option macaddr 'ee:ee:ee:ee:02:22'
    option mode 'ap'
    option ssid 'X2'
    option hidden '0'
    option encryption 'psk2'
    option key 'not_used'
    option wpa_psk_file '/etc/hostapd.wpa_psk'
    option vlan_file '/etc/hostapd.vlan'
    option vlan_tagged_interface 'eth0'
    option vlan_bridge 'magic'  
    option dynamic_vlan '2'


config wifi-device 'radio1'
    option type 'mac80211'
    option path 'ffe09000.pcie/pci9000:00/9000:00:00.0/9000:01:00.0'
    option channel '6'
    option band '2g'
    option htmode 'HT20'
    option cell_density '0'

config wifi-iface 'wifinet3'
    option device 'radio1'
    option ifname 'x'
    option macaddr 'ee:ee:ee:ee:00:12'
    option mode 'ap'
    option ssid 'x'
    option hidden '0'
    option encryption 'psk2'
    option key 'not_used'
    option wpa_psk_file '/etc/hostapd.wpa_psk'
    option vlan_file '/etc/hostapd.vlan'
    option vlan_tagged_interface 'eth0'
    option vlan_bridge 'magic'  
    option dynamic_vlan '2'

config wifi-iface 'wifinet4'
    option device 'radio1'
    option ifname 'x2'
    option macaddr 'ee:ee:ee:ee:02:12'
    option mode 'ap'
    option ssid 'x2'
    option hidden '0'
    option encryption 'psk2'
    option key 'not_used'
    option wpa_psk_file '/etc/hostapd.wpa_psk'
    option vlan_file '/etc/hostapd.vlan'
    option vlan_tagged_interface 'eth0'
    option vlan_bridge 'magic'  
    option dynamic_vlan '2'
```

[grauerfuchs](https://forum.openwrt.org/u/grauerfuchs) April 13, 2024,
3:26pm

110

There may be more involved depending on your setup, but I've found in
all of my attempts that the main configuration (effectively the
fallback) must have a network and key associated before the SSID will
come online. In my setup, for instance, I started with a simple
configuration that points the SSID at my guest network and guest network
password. On top of that, I added the separate PSK options and vlan
definitions for the other networks I wanted to extend. Once all was in
place, I performed a complete restart of wpad and everything came online
as intended.

Please note that there's a chance your specified MAC address may be
involved as well, if it wasn't the one you previously successfully used.
For whatever reason, some wireless radios refuse to accept the
assignment certain MAC addresses. I don't know the reasoning or logic
for this.

[nmpu](https://forum.openwrt.org/u/nmpu) April 13, 2024, 3:44pm

111

Thanks for your reply. I already tried commenting out the custom MACs. I
don't think that's the problem. I'll create a dummy network and see if
the magic comes alive.

[nmpu](https://forum.openwrt.org/u/nmpu) April 15, 2024, 10:46pm

112

Just for reference, are you running the latest OpenWrt release? I was
not able to get the **hostapd.wpa_psk** method to work-- even though I
believe my configuration is correct and I could tell something had
changed (i.e. the radio was disabled). Are your interfaces created and
destroyed on-demand or does everything appear at startup?

I decided to try the equally obscure [wifi-vlan and
wifi-station](https://git.openwrt.org/?p=openwrt/openwrt.git;a=commit;h=5aa2ddd0d6b9759c62bbb7bb11b72a7f4269c16b)
sections (which do not get stripped out by LuCI). This actually works as
expected and does *not* require anything more than
**hostapd-basic-mbedtls**. You create the bridges and wrapper interfaces
manually. OpenWrt automatically creates the Wi-Fi interfaces and
attaches each to the corresponding ethernet interface/bridge. This is
all immediately visible under LuCI \> Network \> Interfaces.

Behind the scenes, in **/tmp/run** there's a `hostapd-<name>.psk` and
`hostapd-<name>.vlan` generated for each SSID. Curiously, these files
are formatted exactly as my original **hostapd.wpa_psk**. The only
difference is that I had merged them all into a single file based on
MAC.

There still appears to be a [bug when you disconnect from an
SSID](https://github.com/openwrt/openwrt/issues/12420). I assume this
affects everyone whether you knew it or not. In my case, it does not
crash the AP, so I can limp along until I get a build with the fix.

It's not clear why they bothered to implement `option mac <address>`
when you can use the more generic `option iface <name>` which references
a `config wifi-iface <name>` block. As mentioned above, the password
pools are already separated by SSID. Using a specific MAC filter is
redundant.

Since you manually configure your own interfaces, the **vid** under
`wifi-vlan` and `wifi-station` is really just an arbitrary link. You
could use a different physical VLAN or none at all. If your Wi-Fi lives
inside a router, you could directly configure subnets and DHCP on each
interface.

For anyone else trying to set this up, when attempting a Wi-Fi
connection, if you get stuck on 'authenticating', then there's something
wrong with your password configuration. You'll likely get a prompt to
reenter the password. If you get stuck on 'connecting', then OpenWrt
matched your password, but there's something wrong with your interface.
If you need to test an interface, you could add a spare ethernet port to
the corresponding bridge.

You don't have to waste your base `option key` and `option network`
under `config wifi-iface`. Fill in those values and omit what would be
another `config wifi-station`.

1 Like

[No connection in dumb ap when power
outage](https://forum.openwrt.org/t/no-connection-in-dumb-ap-when-power-outage/213856/9)

[grauerfuchs](https://forum.openwrt.org/u/grauerfuchs) April 15, 2024,
11:07pm

113

I am indeed using the latest release. I didn't directly edit the wpa_psk
file, but instead used the wifi-vlan and wifi-station sections in the
wireless config file, but the bridges and wrapper interfaces are
automatically created for me.

I did note that changes to these sections in settings required a
complete restart of wpad and occasionally a reboot for everything to be
recognized.

The bug does indeed appear to be present on multiple platforms,
including the ones I'm using. However, like you said, it doesn't appear
to be fatal and so it went unnoticed until I checked just now.

[nmpu](https://forum.openwrt.org/u/nmpu) April 16, 2024, 12:28am

114

OK, so our configuration and outcome are similar. What additional
options did you add to your `config wifi-iface` section(s)? Is it
possible that your 'automatic' bridges/interfaces were actually created
some time in the past by an earlier build? I prefer the manual process,
but I'd like to understand the options.

[grauerfuchs](https://forum.openwrt.org/u/grauerfuchs) April 16, 2024,
10:40am

115

![](https://forum.openwrt.org/letter_avatar/nmpu/48/5_f40856482fe1e45ac8f1605885140a2d.png)
nmpu:

> What additional options did you add to your `config wifi-iface`
> section(s)? Is it possible that your 'automatic' bridges/interfaces
> were actually created some time in the past by an earlier build?

The wireless vlan bridges are created and destroyed on-the-fly, even in
23.05.3. I've altered the configuration a few times over multiple
revisions of 23.05.x and have had similar results, and the current
configuration I am using was not in place prior to 23.05.x.

As for the options I used, here's a functional example with the same
parameters that I successfully used in my configuration:

``` lang-plaintext
config wifi-iface 'wifinet0'
    option device 'radio0'
    option mode 'ap'
    option ssid 'YourWifiSSIDGoesHere'
    option key 'GuestNetworkWPAKey'
    option network 'MYGUESTNETWORK'
    option encryption 'psk2+ccmp'
    option proxy_arp '1'
    option wnm_sleep_mode_no_keys '1'
    option wpa_disable_eapol_key_retries '1'
    option ieee80211k '1'
    option bss_transition '1'

config wifi-vlan
    option name 'example'
    option vid '3'
    option network 'MYSECONDNETWORK'
    option iface 'wifinet0'

config wifi-station
    option iface 'wifinet0'
    option vid '3'
    option key '2ndNetworkWPAKey'

config wifi-vlan
    option name 'anotherexample'
    option vid '4'
    option network 'MYTHIRDNETWORK'
    option iface 'wifinet0'

config wifi-station
    option iface 'wifinet0'
    option vid '4'
    option key 'ThirdNetworkWPAKey'
```

2 Likes

[raenye](https://forum.openwrt.org/u/raenye) April 30, 2024, 8:41pm

116

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/grauerfuchs/48/81781_2.png)
grauerfuchs:

> As for the options I used, here's a functional example with the same
> parameters that I successfully used in my configuration

What VLAN id does the base `wifi-iface` get with this? 1?  
(use `bridge vlan` to test)  
Thanks.

[grauerfuchs](https://forum.openwrt.org/u/grauerfuchs) April 30, 2024,
10:22pm

117

When the packets go out via the ethernet port, they will take on
whichever vlan is associated with the network interface to which the
wifi network is connected. For instance, taking my example
configuration, if the wireless client connects with the password
"GuestNetworkWPAKey", the packets will be sent alongside the rest of the
traffic for the uci network configuration interface named
"MYGUESTNETWORK". Said settings include DHCP from that network's range,
the vlan tag(s) associated with that network (if any), and every other
setting configured for that interface will apply.

If the client connects with the password "2ndNetworkWPAKey", the packets
will be sent with the settings applied to the network interface called
"MYSECONDNETWORK" and so on.

[raenye](https://forum.openwrt.org/u/raenye) April 30, 2024, 11:31pm

118

Thanks for the detailed explanation.

So if the definition of `MYGUESTNETWORK` has `list ports 'lan.17'`, the
VLAN id will be 17?

If so, what happens if `MYSECONDNETWORK` has `list ports 'lan.19'`? will
it be VLAN id 19 or 3 (from the `wifi-vlan` named `example`)

Will that work also when the AP is bridged to a 802.11s mesh or a STA
iface instead of an ethernet switch (e.g. wireless repeater)?

[grauerfuchs](https://forum.openwrt.org/u/grauerfuchs) May 1, 2024,
10:27am

119

If `MYSECONDNETWORK` uses `lan.19` then it would be VLAN 19 when the
packets emerge on the cable.

I didn't write the support code and haven't reviewed it to see where all
the data goes, but it looks to me and appears to behave as if the 'vid'
assignments are effectively little more than an ID to ensure the proper
wifi_vlan and wifi_station sections are linked together without getting
mixed up with any others. However, there's nothing wrong with using the
same number that you expect to see for the VLAN, either. At the very
least, it would reduce confusion there.

2 Likes

[Erip](https://forum.openwrt.org/u/Erip) May 1, 2024, 2:30pm

120

Wrote this:

``` lang-plaintext
#!/bin/sh

TEMP_DIRS="/tmp /var/tmp /dev/shm"
SCRIPT_DIR=$(dirname "$0")
IS_TEMP=0

for temp_dir in $TEMP_DIRS; do
    if [ "$SCRIPT_DIR" = "$temp_dir" ]; then
        IS_TEMP=1
        break
    fi
done

if [ $IS_TEMP -eq 0 ]; then
    echo "This script must be run from a temporary directory."
    echo "Please move it to a temporary directory and run it again."
    exit 1
fi

trap 'rm -f "$0"' EXIT

opkg update
opkg install wpa_supplicant

echo "Enter the SSID:"
read SSID

echo "Enter the passphrases (separated by space):"
read -a PASSPHRASES

cat <<EOF >> /etc/wpa_supplicant/wpa_supplicant.conf
ctrl_interface=/var/run/wpa_supplicant
ctrl_interface_group=0
update_config=1
fast_reauth=1
ap_scan=1

EOF

for passphrase in "${PASSPHRASES[@]}"; do
    cat <<EOF >> /etc/wpa_supplicant/wpa_supplicant.conf
network={
    ssid="$SSID"
    psk="$passphrase"
    id_str="$passphrase"
    key_mgmt=WPA-PSK
    priority=1
}

EOF
done

cat <<EOF > /usr/local/bin/assign_permissions.sh
#!/bin/sh

CONNECTED_PASSPHRASE=\$(cat /etc/wpa_supplicant/wpa_supplicant.conf | grep -A 1 "id_str" | tail -n 1 | cut -d '"' -f 2)

case "\$CONNECTED_PASSPHRASE" in
    "Passphrase1")
        echo "Assigning permissions for Passphrase1"
        ;;
    "Passphrase2")
        echo "Assigning permissions for Passphrase2"
        ;;
    *)
        echo "Unknown passphrase"
        ;;
esac
EOF

chmod +x /usr/local/bin/assign_permissions.sh

echo "post-up=/usr/local/bin/assign_permissions.sh" >> /etc/wpa_supplicant/wpa_supplicant.conf

/etc/init.d/wpa_supplicant restart
```

Hope this will make such setup less painless…

[DirkVanDerWalt](https://forum.openwrt.org/u/DirkVanDerWalt) May 7,
2024, 7:09pm

121

Thanks for this summary to get things up and running
![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:")

This helped me to implement the 'without RADIUS' PPSK support in
RADIUSdesk.

[![](https://img.youtube.com/vi/MhlKVulNNCE/maxresdefault.jpg "Easy Private PSK (without RADIUS) deployments for OpenWrt")](https://www.youtube.com/watch?v=MhlKVulNNCE)

'with RADIUS' we added some more advanced features like ability to do
data limits.

[![](https://img.youtube.com/vi/riGV8jNcEWo/maxresdefault.jpg "RADIUSdesk Private PSK Support")](https://www.youtube.com/watch?v=riGV8jNcEWo)

[jmanuelmri](https://forum.openwrt.org/u/jmanuelmri) May 20, 2024,
7:30pm

122

Hey, I'm trying to make it work on my main router (before going to the
ap's).

I don't know what am I missing, but when I add the vlan stuff into
wireless settings, the 5ghz interface is not going up, its enabled but
the ssid is not even appearing looking for wifi devices.

I did remove wpad-basic-mbedtls (which was the one installed on my
router by default) and installed wpad-mbedtls.

Here is my config:

``` lang-plaintext
ubus call system board

{
        "kernel": "5.15.150",
        "hostname": "OpenWrt-Main",
        "system": "ARMv8 Processor rev 4",
        "model": "GL.iNet GL-MT6000",
        "board_name": "glinet,gl-mt6000",
        "rootfs_type": "squashfs",
        "release": {
                "distribution": "OpenWrt",
                "version": "23.05.3",
                "revision": "r23809-234f1a2efa",
                "target": "mediatek/filogic",
                "description": "OpenWrt 23.05.3 r23809-234f1a2efa"
        }
```

``` lang-plaintext
/etc/config/network

config interface 'loopback'
    option device 'lo'
    option proto 'static'
    option ipaddr '127.0.0.1'
    option netmask '255.0.0.0'

config device
    option name 'br-lan'
    option type 'bridge'
    list ports 'lan1'
    list ports 'lan2'
    list ports 'lan3'
    list ports 'lan4'
    list ports 'lan5'
    option ipv6 '0'

config device
    option name 'lan1'
    option macaddr 'censored'
    option ipv6 '0'

config device
    option name 'lan2'
    option macaddr 'censored'
    option ipv6 '0'

config device
    option name 'lan3'
    option macaddr 'censored'
    option ipv6 '0'

config device
    option name 'lan4'
    option macaddr 'censored'

config device
    option name 'lan5'
    option macaddr 'censored'
    option ipv6 '0'

config interface 'lan'
    option device 'br-lan.9'
    option proto 'static'
    option ipaddr '192.168.9.1'
    option netmask '255.255.255.0'
    option delegate '0'

config device
    option name 'eth1'
    option macaddr 'censored'
    option ipv6 '0'

config interface 'wan'
    option device 'eth1'
    option proto 'dhcp'

config bridge-vlan
    option device 'br-lan'
    option vlan '9'
    list ports 'lan1:u*'
    list ports 'lan2:t'
    list ports 'lan4:u*'
    list ports 'lan5'

config bridge-vlan
    option device 'br-lan'
    option vlan '5'
    list ports 'lan2:t'
    list ports 'lan3:t'

config bridge-vlan
    option device 'br-lan'
    option vlan '7'
    list ports 'lan2:t'

config bridge-vlan
    option device 'br-lan'
    option vlan '16'
    list ports 'lan2:t'

config bridge-vlan
    option device 'br-lan'
    option vlan '18'
    list ports 'lan2:t'

config bridge-vlan
    option device 'br-lan'
    option vlan '20'
    list ports 'lan2:t'
    list ports 'lan3:t'

config bridge-vlan
    option device 'br-lan'
    option vlan '30'
    list ports 'lan2:t'
    list ports 'lan3:t'

config interface 'LOCAL'
    option proto 'static'
    option device 'br-lan.5'
    option ipaddr '192.168.5.1'
    option netmask '255.255.255.0'
    option delegate '0'

config interface 'TRABAJO'
    option proto 'static'
    option device 'br-lan.7'
    option ipaddr '192.168.7.1'
    option netmask '255.255.255.0'
    option delegate '0'

config interface 'CAMARAS'
    option proto 'static'
    option device 'br-lan.16'
    option ipaddr '192.168.16.1'
    option netmask '255.255.255.0'
    option delegate '0'

config interface 'ALARMA'
    option proto 'static'
    option device 'br-lan.18'
    option ipaddr '192.168.18.1'
    option netmask '255.255.255.0'
    option delegate '0'

config interface 'IoT'
    option proto 'static'
    option device 'br-lan.20'
    option ipaddr '192.168.20.1'
    option netmask '255.255.255.0'
    option delegate '0'

config interface 'GUEST'
    option proto 'static'
    option device 'br-lan.30'
    option ipaddr '192.168.30.1'
    option netmask '255.255.255.0'
    option delegate '0'

config bridge-vlan
    option device 'br-lan'
    option vlan '10'
    list ports 'lan2:t'
    list ports 'lan3:t'
    list ports 'lan5:u*'

config interface 'MANAGEMENT'
    option proto 'static'
    option device 'br-lan.10'
    option ipaddr '192.168.10.1'
    option netmask '255.255.255.0'
    option delegate '0'
```

``` lang-plaintext
/etc/config/wireless


config wifi-device 'radio0'
    option type 'mac80211'
    option path 'platform/soc/18000000.wifi'
    option band '2g'
    option htmode 'HE20'
    option country 'ES'
    option cell_density '0'
    option channel '1'
    option txpower '10'

config wifi-iface 'default_radio0'
    option device 'radio0'
    option network 'LOCAL'
    option mode 'ap'
    option ssid 'SSID2.4'
    option encryption 'psk2'
    option key 'censored'
    option disabled '1'

config wifi-device 'radio1'
    option type 'mac80211'
    option path 'platform/soc/18000000.wifi+1'
    option channel '40'
    option band '5g'
    option htmode 'HE80'
    option country 'ES'
    option cell_density '0'
    option txpower '12'

config wifi-iface 'default_radio1'
    option device 'radio1'
    option network 'LOCAL'
    option mode 'ap'
    option ssid 'SSID5'
    option encryption 'psk2'
    option key 'censored'

config wifi-vlan
    option name 'wifi-vlan'
    option network 'IoT'
    option vid '20'
    option iface 'default_radio1'

config wifi-station
    option key 'censored'
    option vid '20'
    option iface 'default_radio1'
```

What am I missing?

[takimata](https://forum.openwrt.org/u/takimata) May 21, 2024, 7:47am

123

![](https://forum.openwrt.org/letter_avatar/jmanuelmri/48/5_f40856482fe1e45ac8f1605885140a2d.png)
jmanuelmri:

> the 5ghz interface is not going up, its enabled but the ssid is not
> even appearing looking for wifi devices.

What does the logfile say after you restart the interface? Unlike the
*basic* variant of wpad which fails silently for most options, the
full-blown wpad should give some feedback.

[jmanuelmri](https://forum.openwrt.org/u/jmanuelmri) May 21, 2024,
9:29am

124

Nevermind, seems to be working now without touching anything.

Still have a question, now it's working, but, based on my config, is it
using my existing bridges or creating new ones each time one device
connects to the AP?

I want it to be as optimal as possible, but I don't have those files
previosly named in the thread: /etc/hostapd.wpa_psk or /etc/hostapd.vlan
so, I don't fully understand whats going on.

[takimata](https://forum.openwrt.org/u/takimata) May 21, 2024, 10:23am

125

![](https://forum.openwrt.org/letter_avatar/jmanuelmri/48/5_f40856482fe1e45ac8f1605885140a2d.png)
jmanuelmri:

> Still have a question, now it's working, but, based on my config, is
> it using my existing bridges

Check. `brctl show` shows the bridges and the interfaces they contain.

> or creating new ones each time one device connects to the AP?

That's not happening anyway. Bridges would be created on hostapd
bringup.

[jmanuelmri](https://forum.openwrt.org/u/jmanuelmri) May 21, 2024,
10:54am

126

brctl show only shows one bridge, the main one.

I did not question it good. I wanted to mean interfaces.

I've one interface for each vlan (br-lan.x assigned to each interface),
and I'm using that interfaces names into "config wifi-vlan", so, when I
connect using an specific password, it's connecting directly to that
interface (specified by password and name) and not duplicating or
creating anything not necessary, right?  
Also, AP is not creating another SSID with same name and different
password internally? (because I had one SSID for each vlan, thats why im
doing this, to improve airtime and only have one SSID)

I just don't want to duplicate not necessary things.

Apologyze for my english, and thanks for your response, you're being
very helpful.

[takimata](https://forum.openwrt.org/u/takimata) May 21, 2024, 11:00am

127

![](https://forum.openwrt.org/letter_avatar/jmanuelmri/48/5_f40856482fe1e45ac8f1605885140a2d.png)
jmanuelmri:

> brctl show only shows one bridge, the main one.

That is rather curious since you define multiple bridges in your network
settings even before going into wifi/hostapd, those should show up
there, too.

I must admit, I spent precioius little time with the UCI `wifi-vlan` and
`wifi-station` option driven way of doing it, I feel I should defer your
case to someone who is more versed in that way of doing things. Anything
from here on out is just guesswork on my side, and that's not helpful to
you or anyone else.

[jmanuelmri](https://forum.openwrt.org/u/jmanuelmri) May 21, 2024,
1:30pm

128

Okay, there is only one bridge, but it's indeed creating an interface
for each vlan on wifi side.

``` lang-plaintext
#brctl show
bridge name     bridge id               STP enabled     interfaces
br-lan          7fff.c87f54bxxxx              no              phy1-ap0-vl30
                                                              phy1-ap0-vl9
                                                              phy1-ap0
                                                              lan4
                                                              lan2
                                                              phy1-ap0-vl5
                                                              eth1
                                                              lan5
                                                              lan3
                                                              lan1
                                                              phy0-ap0
```

Now the question is if it's maintaining each vlan on a separate SSID
with same name, or just creating the interfaces but not duplying
everything like with separate SSID's (so then, airtime is improved)

[loiz](https://forum.openwrt.org/u/loiz) May 22, 2024, 5:34am

129

Hello,

I've configure vlan psk successfully. But when I add it to 5ghz network
it is not working. The network is showing and disappear after a while.
On my device, it stuck on get IP address.

5ghz and 2.4ghz are in same vlan.

Any idea?

Thanks.

[simon_lefisch](https://forum.openwrt.org/u/simon_lefisch) May 22, 2024,
8:46pm

130

Hello everyone,

So I managed to get this feature working on my TP-Link EAP615v1 AP.
However there seems to be an issue with AirPlay to my Apple devices when
using this feature on my Netgear R7800 when setup as a Dumb AP per the
OpenWRT article. I have a [thread opened
here](https://forum.openwrt.org/t/apple-airplay-protocol-not-working/198361)
detailing the issue but here is a quick rundown, maybe someone can shed
a little light....

I have my R7800 configured with DPSK but AirPlay doesn't work between my
Apple TV and my iPhone. The R7800 has (2) separate SSIDs, the 2.4GHz
radio is for devices like phones/laptops and such. the 5GHz radio
streaming devices like my Apple TV (bigger pipe since they are not
hardwired yet). I have (3) VLANs...LAN (VLAN1, mainly for wired devices
like servers), WLAN (VLAN20 for laptops, mobile devices, etc), and
Guest-WLAN (VLAN21, segregated for guest devices).

When I have my Apple devices (TV,iPhone) connected to VLAN1, AirPlay
works fine. When I have it connected to VLAN20 AirPlay doesn't work.
Apple devices do not show. This happens regardless of whether the
devices are on the same radio or different radios. The devices are
getting the same subnet IP addresses, so I'm confused as to what could
be the issue.

If anyone can give some insight, that would be great. The latest
configuration for my R7800 can been seen below. NOTE that DHCP, DNS, and
Firewall services have been disabled per the Dumb AP article.

``` lang-plaintext
root@R7800:~# ubus call system board
{
        "kernel": "5.15.137",
        "hostname": "R7800",
        "system": "ARMv7 Processor rev 0 (v7l)",
        "model": "Netgear Nighthawk X4S R7800",
        "board_name": "netgear,r7800",
        "rootfs_type": "squashfs",
        "release": {
                "distribution": "OpenWrt",
                "version": "23.05.2",
                "revision": "r23630-842932a63d",
                "target": "ipq806x/generic",
                "description": "OpenWrt 23.05.2 r23630-842932a63d"
        }
}
```

``` lang-plaintext
root@R7800:~# cat /etc/config/network

config interface 'loopback'
        option device 'lo'
        option proto 'static'
        option ipaddr '127.0.0.1'
        option netmask '255.0.0.0'

config globals 'globals'
        option ula_prefix 'fd60:c227:0684::/48'
        option packet_steering '1'

config switch
        option name 'switch0'
        option reset '1'
        option enable_vlan '1'

config switch_vlan
        option device 'switch0'
        option vlan '1'
        option ports '1 2 3 4 6t'

config switch_vlan
        option device 'switch0'
        option vlan '20'
        option ports '4t 6t'

config switch_vlan
        option device 'switch0'
        option vlan '21'
        option ports '4t 6t'

config device
        option type 'bridge'
        option name 'br-vlan1'
        list ports 'eth1.1'

config device
        option type 'bridge'
        option name 'br-vlan20'
        list ports 'eth1.20'

config device
        option type 'bridge'
        option name 'br-vlan21'
        list ports 'eth1.21'

config interface 'vlan1'
        option device 'br-vlan1'
        option proto 'static'
        option ipaddr '192.168.50.200'
        option netmask '255.255.255.0'
        option gateway '192.168.50.1'
        list dns '192.168.50.1'

config interface 'vlan20'
        option proto 'none'
        option device 'br-vlan20'

config interface 'vlan21'
        option proto 'none'
        option device 'br-vlan21'
```

``` lang-plaintext
root@R7800:~# cat /etc/config/wireless

config wifi-device 'radio0'
        option type 'mac80211'
        option path 'soc/1b500000.pci/pci0000:00/0000:00:00.0/0000:01:00.0'
        option channel '36'
        option band '5g'
        option htmode 'VHT160'
        option cell_density '0'
        option country 'US'

config wifi-iface 'default_radio0'
        option device 'radio0'
        option network 'vlan1'
        option mode 'ap'
        option ssid 'SSID1'
        option encryption 'psk2'
        option key '********'

config wifi-device 'radio1'
        option type 'mac80211'
        option path 'soc/1b700000.pci/pci0001:00/0001:00:00.0/0001:01:00.0'
        option channel '2'
        option band '2g'
        option htmode 'HT40'
        option cell_density '0'
        option country 'US'

config wifi-iface 'default_radio1'
        option device 'radio1'
        option network 'vlan1'
        option mode 'ap'
        option ssid 'SSID2'
        option encryption 'psk2'
        option key '*********'

config wifi-vlan
        option name 'vl20'
        option network 'vlan20'
        option vid '20'

config wifi-station
        option key '********'
        option vid '20'

config wifi-vlan
        option name 'vl21'
        option network 'vlan21'
        option vid '21'

config wifi-station
        option key '*********'
        option vid '21'
```

``` lang-plaintext
root@R7800:~# cat /etc/config/dhcp

config dnsmasq
        option domainneeded '1'
        option boguspriv '1'
        option filterwin2k '0'
        option localise_queries '1'
        option rebind_protection '1'
        option rebind_localhost '1'
        option local '/lan/'
        option domain 'lan'
        option expandhosts '1'
        option nonegcache '0'
        option cachesize '1000'
        option authoritative '1'
        option readethers '1'
        option leasefile '/tmp/dhcp.leases'
        option resolvfile '/tmp/resolv.conf.d/resolv.conf.auto'
        option nonwildcard '1'
        option localservice '1'
        option ednspacket_max '1232'
        option filter_aaaa '0'
        option filter_a '0'

config dhcp 'lan'
        option interface 'lan'
        option start '100'
        option limit '150'
        option leasetime '12h'
        option dhcpv4 'server'
        option ignore '1'

config odhcpd 'odhcpd'
        option maindhcp '0'
        option leasefile '/tmp/hosts/odhcpd'
        option leasetrigger '/usr/sbin/odhcpd-update'
        option loglevel '4'
```

``` lang-plaintext
root@R7800:~# cat /etc/config/firewall

config defaults
        option syn_flood '1'
        option input 'REJECT'
        option output 'ACCEPT'
        option forward 'REJECT'

config zone
        option name 'lan'
        option input 'ACCEPT'
        option output 'ACCEPT'
        option forward 'ACCEPT'

config zone
        option name 'wan'
        option input 'REJECT'
        option output 'ACCEPT'
        option forward 'REJECT'
        option masq '1'
        option mtu_fix '1'

config forwarding
        option src 'lan'
        option dest 'wan'

config rule
        option name 'Allow-DHCP-Renew'
        option src 'wan'
        option proto 'udp'
        option dest_port '68'
        option target 'ACCEPT'
        option family 'ipv4'

config rule
        option name 'Allow-Ping'
        option src 'wan'
        option proto 'icmp'
        option icmp_type 'echo-request'
        option family 'ipv4'
        option target 'ACCEPT'

config rule
        option name 'Allow-IGMP'
        option src 'wan'
        option proto 'igmp'
        option family 'ipv4'
        option target 'ACCEPT'

config rule
        option name 'Allow-DHCPv6'
        option src 'wan'
        option proto 'udp'
        option dest_port '546'
        option family 'ipv6'
        option target 'ACCEPT'

config rule
        option name 'Allow-MLD'
        option src 'wan'
        option proto 'icmp'
        option src_ip 'fe80::/10'
        list icmp_type '130/0'
        list icmp_type '131/0'
        list icmp_type '132/0'
        list icmp_type '143/0'
        option family 'ipv6'
        option target 'ACCEPT'

config rule
        option name 'Allow-ICMPv6-Input'
        option src 'wan'
        option proto 'icmp'
        list icmp_type 'echo-request'
        list icmp_type 'echo-reply'
        list icmp_type 'destination-unreachable'
        list icmp_type 'packet-too-big'
        list icmp_type 'time-exceeded'
        list icmp_type 'bad-header'
        list icmp_type 'unknown-header-type'
        list icmp_type 'router-solicitation'
        list icmp_type 'neighbour-solicitation'
        list icmp_type 'router-advertisement'
        list icmp_type 'neighbour-advertisement'
        option limit '1000/sec'
        option family 'ipv6'
        option target 'ACCEPT'

config rule
        option name 'Allow-ICMPv6-Forward'
        option src 'wan'
        option dest '*'
        option proto 'icmp'
        list icmp_type 'echo-request'
        list icmp_type 'echo-reply'
        list icmp_type 'destination-unreachable'
        list icmp_type 'packet-too-big'
        list icmp_type 'time-exceeded'
        list icmp_type 'bad-header'
        list icmp_type 'unknown-header-type'
        option limit '1000/sec'
        option family 'ipv6'
        option target 'ACCEPT'

config rule
        option name 'Allow-IPSec-ESP'
        option src 'wan'
        option dest 'lan'
        option proto 'esp'
        option target 'ACCEPT'

config rule
        option name 'Allow-ISAKMP'
        option src 'wan'
        option dest 'lan'
        option dest_port '500'
        option proto 'udp'
        option target 'ACCEPT'
```

[MastermindPanda](https://forum.openwrt.org/u/MastermindPanda) August 3,
2024, 11:44am

131

Hi [@roshii](/u/roshii),

did I understand correctly, that there is a solution for multiple VLANs
with SAE (WPA3 personal) and that you started a pull request for it?

Can you update us on you success so far?

Thank you very much!

[remlei](https://forum.openwrt.org/u/remlei) August 7, 2024, 11:59pm

132

Anyone experiencing issues with Wifi roaming issues? Specifically Fast
Transition (802.11r) causing wifi clients to basically VLAN hop to the
default config wifi-iface network.

It also throws this errors

``` lang-plaintext
Tue Aug  6 05:21:55 2024 daemon.err hostapd: nl80211: kernel reports: key addition failed
Tue Aug  6 05:21:55 2024 daemon.info hostapd: phy1-ap0: STA xx:xx:xx:xx:xx:xx IEEE 802.11: associated (aid 1)
Tue Aug  6 05:21:55 2024 daemon.notice hostapd: phy1-ap0: AP-STA-CONNECTED xx:xx:xx:xx:xx:xx auth_alg=ft
```

For now I workaround this by disabling Generate PMK Locally and this
basically force client to do the 4way handshake and properly assigned to
their VLANs.

[regae](https://forum.openwrt.org/u/regae) August 8, 2024, 12:53am

133

try
[this](https://github.com/regae/openwrt/blob/openwrt-23.05-chromium/package/network/services/hostapd/patches/999-00-fix-vlan-ft-wpa_psk_file-with-ft_psk_generate_local.patch)
workaround

[roshii](https://forum.openwrt.org/u/roshii) August 11, 2024, 8:57am

134

Multiple VLANs with SAE (WPA3 personal) will not work. The only option
would be for clients to support `pw_identifier` which isn't the case for
any common platform (android, ios, windows, etc...)

My PR is only meant at adding an `sae_password_file` option, but unless
`pw_identifier` is implemented in clients, it won't help.

1 Like

[mattimat](https://forum.openwrt.org/u/mattimat) October 6, 2024, 5:32pm

135

Hi is anyone using this with ath11k ? I't was broken nov. last year
waiting for some patching. Might be handy.

[gtxent](https://forum.openwrt.org/u/gtxent) October 28, 2024, 8:32pm

136

The Ath11k driver still requires some patches that haven't been merged
yet, but I've managed to get it running smoothly on my custom build for
the MX4300. You can check out my working version here:

[github.com/gtxaspec/openwrt-mx4300](https://github.com/gtxaspec/openwrt-mx4300/commit/b0fb8508f099a1339e87f8ccc1b5fdd59b0347fb)

![](data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iNjAiIGhlaWdodD0iNjAiIGNsYXNzPSJnaXRodWItaWNvbiIgdmlld2JveD0iMCAwIDE0IDE2IiBhcmlhLWhpZGRlbj0idHJ1ZSI+PHBhdGggZmlsbC1ydWxlPSJldmVub2RkIiBkPSJNMTAuODYgN2MtLjQ1LTEuNzItMi0zLTMuODYtMy0xLjg2IDAtMy40MSAxLjI4LTMuODYgM0gwdjJoMy4xNGMuNDUgMS43MiAyIDMgMy44NiAzIDEuODYgMCAzLjQxLTEuMjggMy44Ni0zSDE0VjdoLTMuMTR6TTcgMTAuMmMtMS4yMiAwLTIuMi0uOTgtMi4yLTIuMiAwLTEuMjIuOTgtMi4yIDIuMi0yLjIgMS4yMiAwIDIuMi45OCAyLjIgMi4yIDAgMS4yMi0uOTggMi4yLTIuMiAyLjJ6IiAvPjwvc3ZnPg==)

#### [add ath11k patch for dynamic vlan](https://github.com/gtxaspec/openwrt-mx4300/commit/b0fb8508f099a1339e87f8ccc1b5fdd59b0347fb)

committed 07:31AM - 21 Oct 24 UTC

[![gtxaspec](https://avatars.githubusercontent.com/u/12115272?v=4)
gtxaspec](https://github.com/gtxaspec)

[+242
-0](https://github.com/gtxaspec/openwrt-mx4300/commit/b0fb8508f099a1339e87f8ccc1b5fdd59b0347fb)

With this patch, it's possible to enable on Ath11k on your own devices
as well.

3 Likes

[systemcrash](https://forum.openwrt.org/u/systemcrash) October 29, 2024,
8:33pm

137

Did you send this to the dev list? And/or open a PR to openwrt?

[gtxent](https://forum.openwrt.org/u/gtxent) October 30, 2024, 12:59am

138

No, I had found the patch posted here in the forums,
[@robimarko](/u/robimarko) had asked for it, but I hadn't seen any
follow up on it since then.

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/robimarko/48/617_2.png)
[Individual per-passphrase Wifi VLANs using wpa_psk_file (no RADIUS
required)](https://forum.openwrt.org/t/individual-per-passphrase-wifi-vlans-using-wpa-psk-file-no-radius-required/161696/43)
[Installing and Using OpenWrt](/c/general/6)

> Can you resend me the fix, I honestly completely forgot about it

[robimarko](https://forum.openwrt.org/u/robimarko) October 31, 2024,
9:37am

139

Sorry, but this is so low on the TODO pile that I doubt that I will get
to it anytime soon

[Mijzelf](https://forum.openwrt.org/u/Mijzelf) December 27, 2024,
12:15pm

140

It seems this doesn't work anymore on 24.10? I had a working config on
23.05, but on 24.10 it seems wpad/hostapd fails to add the wlan-vlan to
the bridge:  
logread after 'wifi up':

``` lang-plaintext
Fri Dec 27 12:48:14 2024 daemon.notice netifd: radio0 (15365): WARNING: Variable 'data' does not exist or is not an array/object
Fri Dec 27 12:48:14 2024 daemon.notice hostapd: Set new config for phy phy0:
Fri Dec 27 12:48:15 2024 daemon.notice wpa_supplicant[1469]: Set new config for phy phy0
Fri Dec 27 12:48:16 2024 daemon.notice wpa_supplicant[1469]: Set new config for phy phy0
Fri Dec 27 12:48:16 2024 daemon.notice hostapd: Set new config for phy phy0: /var/run/hostapd-phy0.conf
Fri Dec 27 12:48:16 2024 daemon.notice hostapd: Restart interface for phy phy0
Fri Dec 27 12:48:16 2024 daemon.notice hostapd: Configuration file: data: driver=nl80211 logger_syslog=127 logger_syslog_level=2 logger_stdout=127 logger_stdout_level=2 country_code=NL ieee80211d=1 hw_mode=g supported_rates=60 90 120 180 240 360 480 540 basic_rates=60 120 240 beacon_int=100 stationary_ap=1 chanlist=1 #num_global_macaddr=1 ieee80211n=1 ht_coex=0 ht_capab=[SHORT-GI-20][SHORT-GI-40][TX-STBC][RX-STBC12] channel=1  interface=phy0-ap0 bssid=fc:f5:28:93:b5:d0 ctrl_interface=/var/run/hostapd bss_load_update_period=60 chan_util_avg_period=600 disassoc_low_ack=0 skip_inactivity_poll=0 preamble=1 wmm_enabled=1 ignore_broadcast_ssid=0 uapsd_advertisement_enabled=1 utf8_ssid=1 multi_ap=0 wpa_group_rekey=86400 wpa_psk_file=/etc/hostapd.wpa_psk auth_algs=1 wpa=2 wpa_pairwise=CCMP ssid=MY_SSID wpa_disable_eapol_key_retries=0 wpa_key_mgmt=WPA-PSK okc=0 disable_pmksa_caching=1 dynamic_vlan=0 vlan_naming=1 vlan_no_bridge=1 vlan_file=/etc/hostapd.vlan qos_map_set=0,0,2,16,1,1,255,255,18,22,24,38,40,40,44,46,48,56 #default_macad
Fri Dec 27 12:48:17 2024 daemon.notice hostapd: phy0-ap0: interface state UNINITIALIZED->COUNTRY_UPDATE
Fri Dec 27 12:48:18 2024 daemon.notice hostapd: phy0-ap0: interface state COUNTRY_UPDATE->ENABLED
Fri Dec 27 12:48:18 2024 daemon.notice hostapd: phy0-ap0: AP-ENABLED
Fri Dec 27 12:48:18 2024 daemon.notice netifd: Wireless device 'radio0' is now up
Fri Dec 27 12:48:23 2024 daemon.info hostapd: phy0-ap0: STA 80:19:34:ac:a1:cf IEEE 802.11: authenticated
Fri Dec 27 12:48:23 2024 daemon.info hostapd: phy0-ap0: STA 80:19:34:ac:a1:cf IEEE 802.11: associated (aid 1)
Fri Dec 27 12:48:23 2024 daemon.notice hostapd: Assigned VLAN ID 10 from wpa_psk_file to 80:19:34:ac:a1:cf
Fri Dec 27 12:48:23 2024 daemon.notice hostapd: phy0-ap0: AP-STA-CONNECTED 80:19:34:ac:a1:cf auth_alg=open
Fri Dec 27 12:48:23 2024 daemon.info hostapd: phy0-ap0: STA 80:19:34:ac:a1:cf RADIUS: starting accounting session 5FEC7A665B090EBD
Fri Dec 27 12:48:23 2024 daemon.info hostapd: phy0-ap0: STA 80:19:34:ac:a1:cf WPA: pairwise key handshake completed (RSN)
Fri Dec 27 12:48:23 2024 daemon.notice hostapd: phy0-ap0: EAPOL-4WAY-HS-COMPLETED 80:19:34:ac:a1:cf
Fri Dec 27 12:48:24 2024 daemon.warn dnsmasq-dhcp[1]: DHCP packet received on wlan0.10 which has no address
Fri Dec 27 12:48:28 2024 daemon.warn dnsmasq-dhcp[1]: DHCP packet received on wlan0.10 which has no address
```

The client connects, but doesn't get an IP address.

/etc/config/wireless

``` lang-plaintext
config wifi-device 'radio0'
    option type 'mac80211'
    option hwmode '11g'
    option path 'pci0000:00/0000:00:0e.0'
    option txpower '20'
    option country 'NL'
    option cell_density '0'
    option htmode 'HT20'
    option channel '1'

config wifi-iface 'wifinet0'
    option device 'radio0'
    option mode 'ap'
    option encryption 'psk2'
    option wpa_psk_file '/etc/hostapd.wpa_psk'
    option vlan_file '/etc/hostapd.vlan'
    option disassoc_low_ack '0'
    option dtim_period '2'
    option wpa_group_rekey '86400'
    option wds '0'
    option ssid 'MY_SSID'
```

/etc/hostapd.wpa_psk

``` lang-plaintext
vlanid=10 00:00:00:00:00:00 Secret 1
vlanid=20 00:00:00:00:00:00 Secret 2
vlanid=30 00:00:00:00:00:00 Secret 3
```

/etc/hostapd.vlan

``` lang-plaintext
10 wlan0.10 br-lan
20 wlan0.20 br-guest
30 wlan0.30 br-iot
```

wpad hostapd -v

``` lang-plaintext
hostapd v2.12-devel
User space daemon for IEEE 802.11 AP management,
IEEE 802.1X/WPA/WPA2/EAP/RADIUS Authenticator
Copyright (c) 2002-2024, Jouni Malinen <j@w1.fi> and contributors
```

brctl show br-lan

``` lang-plaintext
bridge name   bridge id       STP enabled interfaces
br-lan      7fff.66e12ff96390   no      lan4
                            lan3
                            lan1
```

Hostapd did create the wlan-vlans:

ip a \| grep wlan

``` lang-plaintext
30: wlan0.30: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default qlen 1000
31: wlan0.20: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default qlen 1000
32: wlan0.10: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default qlen 1000
```

[Mijzelf](https://forum.openwrt.org/u/Mijzelf) December 27, 2024,
12:35pm

141

Solved, I think. I was re-reading my post, and saw in the logging of
'hostapd: Configuration file' the argument 'vlan_no_bridge=1', which
exactly describes the problem. So I added an

``` lang-plaintext
    option vlan_no_bridge '0'
```

and voilá, it works.

3 Likes

[Using custom files in Image
Builder](https://forum.openwrt.org/t/using-custom-files-in-image-builder/229750/12)

[\_bernd](https://forum.openwrt.org/u/_bernd) January 14, 2025, 8:24pm

142

Hey [@takimata](/u/takimata)

Sorry if I've missed that: But; Is it possible with a `wpa_psk`-file to
do a password-less, but macaddr-specific, VLAN assignment? Or *things*
like with radius, where the provided password is the macaddr of the
client?

I've finally found the time to move my old setup and config, and this
setup feels like to most zen. Thanks for showing us.

1 Like

[takimata](https://forum.openwrt.org/u/takimata) January 14, 2025,
10:32pm

143

![](https://forum.openwrt.org/letter_avatar/_bernd/48/5_f40856482fe1e45ac8f1605885140a2d.png)
\_bernd:

> Is it possible with a `wpa_psk`-file to do a password-less, but
> macaddr-specific, VLAN assignment?

It's possible to have the same password for everyone (or multiple
clients) and then assign VLANs based on the MAC.

IIRC, and someone CMIIW: It's generally not possible to have WPA-PSK
entirely without, well, a PSK. "Passwordless" refers to WPA-Enterprise
with certificate-based authentication, and for that you actually need
RADIUS.

1 Like

[dannutu](https://forum.openwrt.org/u/dannutu) January 25, 2025, 6:03am

144

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> The other devices don't see that, they only see the traffic from the
> AP tagged with the respective numeric VLAN tag, and sharing that
> VLAN-tagged traffic between devices is actually kind of the point.

I bumped into an issue and I wonder if this "*sharing that VLAN-tagged
traffic between devices is actually kind of the point*" is the
explanation.

In `/etc/config/wireless` I have this section:

``` lang-plaintext
config wifi-iface 'radio1-ap1
        option device 'radio1'
        option ifname 'wlan11'
        option mode 'ap'
        option ssid 'My SSID'
        option encryption 'psk2'
        option key 'my passwd'
        option isolate '1'
        option network 'lan'
        option dynamic_vlan '1'
        option ieee80211w '1'
        option multicast_to_unicast_all '1'
        option disassoc_low_ack '0'
```

and this section, too:

``` lang-plaintext
config wifi-vlan
        option name 'vlan90'
        option network 'guest'
        option vid '90'
        option iface 'radio1-ap1'

config wifi-station
        option key 'guest passwd'
        option vid '90'
        option iface 'radio1-ap1'
```

Clients can successfully attach to both:

- the 'lan' network via 'My SSID'/'my passwd'
- the 'guest' network via vlan90 via 'My SSID'/'guest passwd'

However, clients are isolated only when connected to the 'lan' network
via 'My SSID'.  
When connected to the 'guest' network via vlan90 clients can "see" and
ping each other, there is no isolation.

Am I missing a trick here? How can I configure the 'access to the guest'
network via vlan90 so that clients can't see each other?

Thanks a lot for any ideas!

[raenye](https://forum.openwrt.org/u/raenye) January 25, 2025, 3:54pm

145

![](https://forum.openwrt.org/letter_avatar/dannutu/48/5_f40856482fe1e45ac8f1605885140a2d.png)
dannutu:

> Am I missing a trick here? How can I configure the 'access to the
> guest' network via vlan90 so that clients can't see each other?

How about setting `guest` network in the `wifi-iface` and `lan` in the
wifi-vlan? (switch `guest passwd` and `my passwd` too, obviously)

[dannutu](https://forum.openwrt.org/u/dannutu) January 25, 2025, 5:57pm

146

Thanks. I should have mentioned - I want them isolated on *both* `lan`
and `guest`, so swapping them won't solve the issue.

[takimata](https://forum.openwrt.org/u/takimata) January 26, 2025,
2:45am

147

![](https://forum.openwrt.org/letter_avatar/dannutu/48/5_f40856482fe1e45ac8f1605885140a2d.png)
dannutu:

> clients are isolated only when connected to the 'lan' network via 'My
> SSID'.  
> When connected to the 'guest' network via vlan90 clients can "see" and
> ping each other

It's possible that there's a weird interaction within hostapd, but let's
rule out the obvious first: Wireless client isolation happens per-radio
within the radio -- but *only* within the radio. Are those "guest"
clients connected to the same (physical) radio, or do they come into the
guest network by other means (wired, different AP)?

[trevormcooper](https://forum.openwrt.org/u/trevormcooper) January 26,
2025, 3:57am

148

I'm very interested in the idea of this,

would it be plausible to set this up on my flint 2(gl-mt6000)  
port1 wan, ports 2-5 would be vlan10 and port 6 would be vlan30

ideally i just want to name both the 2.4 ghz and 5ghz networks the same
name, and have 2 passwords

password 1 -\> vlan10 / would allow wireless devices full access to
everything similar to a default lan

password 2 vlan20 would allow all wireless devices to talk to each other
but not to the router or other vlans

[takimata](https://forum.openwrt.org/u/takimata) January 26, 2025,
4:09am

149

Absolutely, that's the purpose of this exercise. In your case you would
first introduce individual bridges for your VLANs for your physical
ports. You then have hostapd *not* create new bridges but rather attach
to existing ones, by announcing the preexisting bridges either [via a
third parameter in the vlan_file
entries](https://forum.openwrt.org/t/individual-per-passphrase-wifi-vlans-using-wpa-psk-file-no-radius-required/161696/15)
or the appropriate UCI sections (which I don't know off the top of my
head).

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 9:51am

150

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> Wireless client isolation happens per-radio within the radio -- but
> *only* within the radio. Are those "guest" clients connected to the
> same (physical) radio, or do they come into the guest network by other
> means (wired, different AP)?

Thank you for your help. Yes, you are right, I can confirm that I am
talking about *wireless* `guest` clients connected to the same SSID
(`My SSID`), on the same radio device (`radio1`). The password they
enter selects whether they attach "directly" to network `lan` via ssid
`My SSID` or to network `guest` via `vid '90'` / `'vlan90'` over ssid
`My SSID`.

I was aware that the "isolation" only works between *wireless* clients
on the same AP on the same radio (and not between wireless and wired, or
between wired and wired clients) and I checked when I set up the router
(April 2024) that it was in fact working like that, but I checked only
on the "native" (i.e. no vlan) access to the SSID/AP, and since I knew
that it worked on the radio part and that the `guest` vlan was set-up
over the same SSID/AP and radio, it didn't occur to me to also check on
access via a vlan. I assumed that the "isolation" between *wireless*
clients on the same vlan would be "inherited" so-to-speak from the
underlying SSID/AP/radio.

This week I found out by accident (I was troubleshooting something on
that `guest` vlan and a nmap ARP scan from my laptop, connected via
wi-fi, showed all the other wireless clients on the same vlan / SSID /
AP / radio. Surprised, I also checked from my mobile and indeed, I was
able to reach the other wireless clients. When I moved them all to the
"native" SSID/AP (on the same radio) by changing their wi-fi passwords,
the isolation worked as expected and they couldn't see each other, there
was no ARP answer. Once I changed back their passwords and they were
back on the `guest` vlan they could again see each other.

I also tried adding `option isolate '1'` to both `config wifi-vlan` and
`config wifi-station` "stanzas" above, and while it didn't throw any
error when I restarted the radio with `wifi down` + `wifi up`, and
`"isolate": true` was subsequently showing *twice* (!) for each SSID in
the output of `wifi status`, it didn't make isolation work on the
`guest` vlan.

That's why I came here, looking for ideas to explain this behaviour and
hopefully also suggestions for how to fix it. Thank you again for your
time and attention to this, I wouldn't know where else to go with such
an issue!

[takimata](https://forum.openwrt.org/u/takimata) January 26, 2025,
10:08am

151

I believe it's possible that, contrary to a "regular" wifi BSS that has
an internal briddge, for a wifi-vlan, hostapd does not run an internal
bridge but rather forwards the frames to the "external" bridge where
there is no isolation anymore. Someone with deeper knowledge will have
to confirm or deny that, though.

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 10:40am

152

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/trevormcooper/48/89616_2.png)
trevormcooper:

> on my flint 2(gl-mt6000)

I was intrigued by your description mentioning six Ethernet ports so I
looked it up - it turns out that it's a pretty nice router! I wish I had
this option last year when I went looking for current hardware to
upgrade my setup (I ended up with "separates", i.e. a NanoPi R4S for the
router and a Linksys MR-8300 for the AP, since it has enough RAM and
flash for the AP role and it also has 3 different radios, allowing me to
have multiple SSIDs over three radio bands, while also minimising
bandwidth loss due to beacons etc.

Your router seems good enough that you could get away with doing
everything on a single box. I especially liked the two 2.5Gbps Ethernet
interfaces! Those would be very sweet to connect to a managed switch (my
R4S has only two built-in 1Gbps Ethernet interfaces, I had to add an
external USB3.0 1Gbps interface for WAN and bond the two built-in
interfaces into a 2Gbps vlan trunk to the managed switch, but it is a
complete kludge). Your router would allow a much cleaner setup. Yes, it
seems the speeds and the CPU load would be affected as soon as you
enable SQM but I think it could still do a pretty decent job and be a
lot "tidier" setup at the same time.

Sorry for the off-topic, I'll stop here!
![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:")

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 10:56am

153

Thanks, I really appreciate your suggestion and honesty! There is no
expectation on my part that anybody in particular would or should
already have an answer for this, I am just hoping that I could find at
least some ideas and suggestions for how to go about "isolating" this
issue and hopefully fixing it, too.

Speaking of ideas - would you have any suggestion for how I could try to
identify those "internal" / "external" bridges that you mentioned? I
have to admit that my own knowledge about bridges and the linux commands
to manage them is very limited. I literally just tried a
`bridge -d vlan` command now and I can see that the `guest` vlan 90
appears three times:

- in the `br-lan` interface, as `state forwarding mcast_router 1`
- in the `bond-bond0` interface (used for the vlan trunk to the router),
  as `state forwarding mcast_router 1`
- in the `wlan11-vlan90` (this is the `guest` vlan over `My SSID`), as
  `90 PVID Egress Untagged state forwarding mcast_router 1`

I have no idea though how to interpret these... Any hints where I could
go dig further?

Thanks again, much appreciated!

[takimata](https://forum.openwrt.org/u/takimata) January 26, 2025,
11:25am

154

![](https://forum.openwrt.org/letter_avatar/dannutu/48/5_f40856482fe1e45ac8f1605885140a2d.png)
dannutu:

> would you have any suggestion for how I could try to identify those
> "internal" / "external" bridges that you mentioned?

I believe it is possible to run tcpdump on the bridge interface to see
if packets between the not-isolated clients reach it in the first place,
or if they are forwarded inside of hostapd and don't even show up on the
bridge.

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 11:26am

155

I am wondering (and this is pure speculation): could it be that, since
these are bridges, all packets on a vlan are reaching all "clients"
attached to that vlan? And therefore all wireless clients on the guest
vlan would get all the packets on that guest vlan, and so see each
other?

Is there a way to inspect traffic on a bridge, similar to tcpdump? I
could try watching the ARP queries and replies on the guest vlan.

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 11:27am

156

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> I believe it is possible to run tcpdump on the bridge interface

hehehe, I was just typing the above message about the same thing!
![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:")

Let me go figure out how I could run tcpdump on the bridge interface...
I'll come back and report!

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 11:31am

157

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> if they are forwarded inside of hostapd and don't even show up on the
> bridge

So, in the `bridge -d vlan` output above, I take it that the "internal"
hostapd bridge is the one listed under `wlan11-vlan90`? Or is that one
also "external" to hostapd?

LE:  
Just looking for bridge related info on openwrt, I found this:  
[https://openwrt.org/docs/guide-user/network/wifi/basic](https://openwrt.org/docs/guide-user/network/wifi/basic)

- **isolate**\|boolean\|no\|0\|Isolates wireless clients from each
  other, **only applicable in ap mode**.
- **bridge_isolate**\|boolean\|no\|0\|Isolates wireless clients from
  each other **on the AP's bridge**. (i.e. 2.4ghz and 5ghz radios on the
  same AP.)\|

It looks like I would need to add this bridge_isolate somehow, somewhere

[takimata](https://forum.openwrt.org/u/takimata) January 26, 2025,
11:38am

158

Not what I meant by "internal" bridge. Set by hostapd, it's the wifi
driver that does the internal forwarding and as such also the isolation.
Packets between clients on the same BSS should actually never reach the
bridge.

But, again, I'm seriously out of my depth here. I think I will really
have to shut up and sit down and let someone more knowledgeable take
over from here.

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 11:47am

159

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> I'm seriously out of my depth here

That makes two of us!
![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:")
But it's literally the very first time I'm looking at this, so there's
no way that you (or anybody else) would know *less* than me about this
topic
![:smiley:](https://forum.openwrt.org/images/emoji/twitter/smiley.png?v=12 ":smiley:")
So here goes my Sunday (again)... well, it's stormy outside anyway...

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 12:10pm

160

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> Packets between clients on the same BSS should actually never reach
> the bridge.

I "hear" what you're "saying" (more like "I read what you're typing"),
however it turns out that this is not entirely the case here:

`brctl show` lists only one bridge (`br-lan`) and `wlan11-vlan90` as
part of it (as expected).

`tcpdump -i br-lan arp` shows ARP *requests* from one wireless client on
that guest vlan to another wireless client on the same guest vlan...

But strangely it does **not** show any ARP **reply to *those* ARP
requests**! And yet the first wireless client can somehow successfully
ping the second wireless client! From where / how does the first client
get the MAC address of the second client?
![:confused:](https://forum.openwrt.org/images/emoji/twitter/confused.png?v=12 ":confused:")

And at the same time I can see both ARP requests *and replies* on other
vlans that are configured over br-lan

LE: Here's what I see:

`tcpdump -i br-lan` shows **only** ARP requests from the first wireless
client on the guest vlan (for the MAC address of the second wireless
client on the same guest vlan), **and nothing else** related to these
two clients.

`tcpdump -i wlan11-vlan90` shows **both** ARP requests from, **and
replies to**, the first wireless client on the guest vlan **and it also
shows the actual traffic between the two clients** (ICMP echo requests
and replies in my test).

Is there a meaningful conclusion I could draw from this? As in, is this
at least how it's expected to work, or is there anything unusual so far?

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 12:36pm

161

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> Not what I meant by "internal" bridge. Set by hostapd, it's the wifi
> driver that does the internal forwarding and as such also the
> isolation. Packets between clients on the same BSS should actually
> never reach the bridge

So I reckon that this bridge set by hostapd doesn't even show in brctl
show, right?

Anyway, it's clear that (*some* of) the vlan traffic on that SSID is
clearly making it to `br-lan` (i.e. the ARP requests), while **all** of
it is making it to `wlan11-vlan90`.

[takimata](https://forum.openwrt.org/u/takimata) January 26, 2025,
12:38pm

162

![](https://forum.openwrt.org/letter_avatar/dannutu/48/5_f40856482fe1e45ac8f1605885140a2d.png)
dannutu:

> `brctl show` lists only one bridge (`br-lan`) and `wlan11-vlan90` as
> part of it (as expected).

I'm a bit confused. Shouldn't be your VLAN 90 be on the *guest* bridge,
not on the *br-lan* bridge?

The point is to compare traffic on your respective bridges between the
clients on your regular lan bridge (where the isolation works) and the
guest bridge (where it doesn't).

> So I reckon that this bridge set by hostapd doesn't even show in brctl
> show, right?

Yes. If the documentation is to believed, this is not an actual
"bridge", but an in-driver forwarding of packets (or lack thereof), a
low-level shortcut between wifi clients. E.g., the driver sees both the
source and the target handled by itself and just directly hurls the
packets over (or, if isolated ... throws them away).

(As such, the failure to isolate the clients on the VLAN may be a
shortcoming in the driver.)

Edit: I feel like I'm bumbling.

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 1:02pm

163

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> Shouldn't be your VLAN 90 be on the *guest* bridge, not on the
> *br-lan* bridge?
>
> The point is to compare traffic on your respective bridges between the
> clients on your regular lan bridge (where the isolation works) and the
> guest bridge (where it doesn't).

Allow me to explain my setup, as best as I can:

I use this router (a Linksys MR8300) as a "dumb" AP (ok, with VLANs, but
no routing).  
I "took out" one LAN port (lan4) from the default `br-lan` bridge
(leaving only lan1, lan2 and lan3 ports in `br-lan`).  
I used the "freed" lan4 port together with the wan port, to create a LAG
interface (`bond-bond0`) between the AP and the router.  
I added this `bond-bond0` to `br-lan` (which also still had lan1, lan2
and lan3) so I can configure VLANs using the new DSA "bridge filtering"
method (which, as far as I can tell, only works on `br-lan`)  
I then configured a number of VLANs over `br-lan` (including the `guest`
VLAN 90).  
Only one of these VLANs also has an interface with an IP address
assigned to it on the AP (so I can manage it via ssh & LUCI), the other
VLANs are simply trunked back to the router over `br-lan` / `bond-bond0`
/ `lan4` & `wan`.  
I hope this could give you a better idea of the setup.

This resulted in one single bridge listed by `brctl show`, i.e. the
`br-lan` one. It has the following "interfaces":

- lan1, lan2 and lan3 (the physical ports on the router)
- bond-bond0 (the LAG between the AP and the router)
- wlan00 (first AP on the first radio)
- wlan10 (first AP on the second radio)
- wlan11 (second AP on the second radio)
- wlan20 (first AP on the third radio)
- wlan21 (second AP on the third radio)
- a number of wlan11-vlan*XY*, with different *XY* numbers, including 90
  for `guest` (the VLANs which are configured over wlan11)

I hope this explains it - this is what I currently have. I'm not quite
sure I understood exactly what you meant by "*Shouldn't be your VLAN 90
be on the *guest* bridge, not on the *br-lan* bridge?*", could you
please help me see what you meant? Thanks

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 1:05pm

164

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> I feel like I'm bumbling

No, it makes sense, I'm with you on this. It's just that I'm trying to
figure out if the current setup resembles anything like the *intended*
setup, and this is difficult without *really* understanding how it
*should* work. Which is difficult to figure from bits and pieces.

[takimata](https://forum.openwrt.org/u/takimata) January 26, 2025,
1:15pm

165

![](https://forum.openwrt.org/letter_avatar/dannutu/48/5_f40856482fe1e45ac8f1605885140a2d.png)
dannutu:

> I'm not quite sure I understood exactly what you meant by "*Shouldn't
> be your VLAN 90 be on the *guest* bridge, not on the *br-lan*
> bridge?*", could you please help me see what you meant?

Never mind then, your setup at first glance looked like your router and
your AP are one and the same, and you're doing it *very* differently.

In my mind (and in my setup) the simplest way is the prototypical
"router with guest network on dumb AP" approach: Bridges for each
network on the router, one port to the AP tagged with VLANs for each
bridge, have the AP pick them up for the wifi VLANs (separate SSIDs or
no). I'm not at all familiar with how bridge filtering works, since I
never had any purpose for it, and after seeing your setup I feel it's
overcomplicating things quite a bit.

> Which is difficult to figure from bits and pieces.

I feel you. When I created this thread, I had to piece it all together
for myself, too.

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 1:19pm

166

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> the failure to isolate the clients on the VLAN may be a shortcoming in
> the driver

Let me try to plug that `bridge_isolate` option into the `wifi-iface`
and/or `wifi-vlan` section and see if it improves things. I hope it
won't break everything!
![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:")

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 1:37pm

167

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> your setup at first glance looked like your router and your AP are one
> and the same, and you're doing it *very* differently

Yeah, what can I say, I completely geeked out while doing it :)) And it
seems that now I'm paying the price for it - complexity is the enemy of
reliability (and security).

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> the simplest way is the prototypical "router with guest network on
> dumb AP" approach

Yes, this was more or less what I was trying to do, with the addition
of:

- the LAG in-between the router and the AP,
- quite a few more VLANs (not just the guest one),
- and the VLAN setup explained in this thread (i.e. individual
  per-passphrase Wi-Fi VLANs - no RADIUS required).

Otherwise I wasn't seeking to reinvent the wheel. This thread was
immensely useful last year, when I set it up. I just hope now that I
didn't interpret something the wrong way!

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> Bridges for each network on the router \[...\] after seeing your setup
> I feel it's overcomplicating things quite a bit

I feel you could be unto something here. Now, I don't have a *need* for
bridges **on the router** (quite the opposite in fact, as I want the
router to inspect the traffic between the different VLANs and *only*
forward packets allowed by explicit ACLs).

However, I can see how this setup with multiple bridges could *possibly*
aid the AP to isolate wireless clients. The only question I have is: how
would I create these multiple bridges though? IIRC when I set everything
up last year I found out that this new "bridge filtering" method for
setting VLANs was only available on br-lan. Do you think I missed
something or did something wrong? How do you create multiple bridges and
VLANs on an openwrt box with DSA?

[dannutu](https://forum.openwrt.org/u/dannutu) January 26, 2025, 1:53pm

168

![](https://forum.openwrt.org/letter_avatar/dannutu/48/5_f40856482fe1e45ac8f1605885140a2d.png)
dannutu:

> Let me try to plug that `bridge_isolate` option into the `wifi-iface`
> and/or `wifi-vlan` section and see if it improves things.

Ok, sorry to say that I added it and it didn't improve things.

FWIW, I added `option bridge_isolate '1'` to all five APs (i.e.
`radio0-ap0`, `radio1-ap0`, `radio1-ap1`, `radio2-ap0` and `radio2-ap1`)
and also to all VLANs configured on `radio1-ap1` via `config wifi-vlan`
and `config wifi-station` "stanzas".

And they show up as `"bridge_isolate": true` in the output of
`wifi status` in each of the section for the 5 APs and all VLANs (twice
for each AP). But nothing has improved otherwise...

BTW, I also checked on the router... it has only one bridge (`br-lan`),
with only one interface (`bond-bond0`).

`tcpdump -i bond-bond0` (on the router) shows the initial ARP request
from the first wireless client on the `guest` VLAN asking for the MAC
address of the target wireless client on the same `guest` VLAN, but
nothing else (while the ping succeeds between the two `guest` clients).

`tcpdump -i br-lan.90` (on the router) does the same, although it
captures both ARP requests (there are also two ARP requests on the AP
bridge, before the target wakes up and answers them)

This leads me to believe that the "non isolation leak" is happening only
on the AP.

[trevormcooper](https://forum.openwrt.org/u/trevormcooper) January 26,
2025, 2:58pm

169

No totally fine, I love this box,

I have tested sqm and offloading, and it can handle both well over
1gbps, not 100% certain sqm can hit 2.5gb, but now that we’re talking
about it I’ll go set it up on the lan and run some internal testing to
see the cpu load with sqm at higher values

My setup document for this router is pretty short because you don’t need
to do much to make it work really nicely.

Bufferbloat doesn’t scare me because it’s exceedingly rare for us to max
out our connections to the point that it would trigger any real buffer
load, that said I get naturally lower buffer load through this guy than
my last one

[trevormcooper](https://forum.openwrt.org/u/trevormcooper) January 26,
2025, 3:00pm

170

Accidentally replied the the entire post instead of your comment so I
deleted and am replying lol (adding text because it didn’t like that my
post was so similar)

No totally fine, I love this box,

I have tested sqm and offloading, and it can handle both well over
1gbps, not 100% certain sqm can hit 2.5gb, but now that we’re talking
about it I’ll go set it up on the lan and run some internal testing to
see the cpu load with sqm at higher values

My setup document for this router is pretty short because you don’t need
to do much to make it work really nicely.

Bufferbloat doesn’t scare me because it’s exceedingly rare for us to max
out our connections to the point that it would trigger any real buffer
load, that said I get naturally lower buffer load through this guy than
my last one

[saxy](https://forum.openwrt.org/u/saxy) February 7, 2025, 10:37pm

171

hy,  
i have an ap3805 from extreme with 24.10.0  
it has correct package  
root@OpenWrt:~# opkg list-installed \|grep wpad  
wpad-mbedtls - 2024.09.15~5ace39b0-r2  
config is correct.  
assignment of vlans per psk works on 2.4ghz wifi but not on 5ghz wifi  
how can that be? same config!

when associating on 5ghz i see this message

Mon Feb 3 23:22:58 2025 daemon.err hostapd: nl80211:
NL80211_ATTR_STA_VLAN (addr=9e:5e:87:36:f0:d8 ifname=wlan0.22
vlan_id=22) failed: -22 (Invalid argument)

this is the full log

``` lang-plaintext
Fri Feb  7 22:26:06 2025 user.info kernel: [   24.945765] kmodloader: done loading kernel modules from /etc/modules.d/*
Fri Feb  7 22:26:08 2025 user.notice dnsmasq: DNS rebinding protection is active, will discard upstream RFC1918 responses!
Fri Feb  7 22:26:08 2025 user.notice dnsmasq: Allowing 127.0.0.0/8 responses
Fri Feb  7 22:26:09 2025 daemon.info dnsmasq[1]: started, version 2.90 cachesize 1000
Fri Feb  7 22:26:09 2025 daemon.info dnsmasq[1]: DNS service limited to local subnets
Fri Feb  7 22:26:09 2025 daemon.info dnsmasq[1]: compile time options: IPv6 GNU-getopt no-DBus UBus no-i18n no-IDN DHCP no-DHCPv6 no-Lua TFTP no-conntrack no-ipset no-nftset no-auth no-cryptohash no-DNSSEC no-ID loop-detect inotify dumpfile
Fri Feb  7 22:26:09 2025 daemon.info dnsmasq[1]: UBus support enabled: connected to system bus
Fri Feb  7 22:26:09 2025 daemon.info dnsmasq[1]: using only locally-known addresses for test
Fri Feb  7 22:26:09 2025 daemon.info dnsmasq[1]: using only locally-known addresses for onion
Fri Feb  7 22:26:09 2025 daemon.info dnsmasq[1]: using only locally-known addresses for localhost
Fri Feb  7 22:26:09 2025 daemon.info dnsmasq[1]: using only locally-known addresses for local
Fri Feb  7 22:26:09 2025 daemon.info dnsmasq[1]: using only locally-known addresses for invalid
Fri Feb  7 22:26:09 2025 daemon.info dnsmasq[1]: using only locally-known addresses for bind
Fri Feb  7 22:26:09 2025 daemon.info dnsmasq[1]: using only locally-known addresses for lan
Fri Feb  7 22:26:09 2025 daemon.warn dnsmasq[1]: no servers found in /tmp/resolv.conf.d/resolv.conf.auto, will retry
Fri Feb  7 22:26:09 2025 daemon.info dnsmasq[1]: read /etc/hosts - 12 names
Fri Feb  7 22:26:09 2025 daemon.info dnsmasq[1]: read /tmp/hosts/dhcp.cfg01411c - 0 names
Fri Feb  7 22:26:09 2025 authpriv.info dropbear[1638]: Not backgrounding
Fri Feb  7 22:26:13 2025 daemon.notice wpa_supplicant[1749]: Successfully initialized wpa_supplicant
Fri Feb  7 22:26:13 2025 user.notice : Added device handler type: bonding
Fri Feb  7 22:26:13 2025 user.notice : Added device handler type: 8021ad
Fri Feb  7 22:26:13 2025 user.notice : Added device handler type: 8021q
Fri Feb  7 22:26:13 2025 user.notice : Added device handler type: macvlan
Fri Feb  7 22:26:13 2025 user.notice : Added device handler type: veth
Fri Feb  7 22:26:13 2025 user.notice : Added device handler type: bridge
Fri Feb  7 22:26:13 2025 user.notice : Added device handler type: Network device
Fri Feb  7 22:26:13 2025 user.notice : Added device handler type: tunnel
Fri Feb  7 22:26:18 2025 user.notice ucitrack: Setting up /etc/config/luci-splash reload dependency on /etc/config/firewall
Fri Feb  7 22:26:18 2025 user.notice ucitrack: Setting up /etc/config/qos reload dependency on /etc/config/firewall
Fri Feb  7 22:26:18 2025 user.notice ucitrack: Setting up /etc/config/miniupnpd reload dependency on /etc/config/firewall
Fri Feb  7 22:26:18 2025 user.notice ucitrack: Setting up /etc/config/odhcpd reload dependency on /etc/config/dhcp
Fri Feb  7 22:26:19 2025 user.notice ucitrack: Setting up /etc/config/dhcp reload dependency on /etc/config/network
Fri Feb  7 22:26:19 2025 user.notice ucitrack: Setting up /etc/config/network reload dependency on /etc/config/wireless
Fri Feb  7 22:26:19 2025 user.notice ucitrack: Setting up non-init /etc/config/fstab reload handler: /sbin/block mount
Fri Feb  7 22:26:20 2025 user.notice ucitrack: Setting up /etc/config/system reload trigger for non-procd /etc/init.d/led
Fri Feb  7 22:26:20 2025 user.notice ucitrack: Setting up /etc/config/luci_statistics reload dependency on /etc/config/system
Fri Feb  7 22:26:20 2025 user.notice ucitrack: Setting up /etc/config/dhcp reload dependency on /etc/config/system
Fri Feb  7 22:26:22 2025 kern.info kernel: [   44.445784] br-lan: port 1(eth0) entered blocking state
Fri Feb  7 22:26:22 2025 kern.info kernel: [   44.451106] br-lan: port 1(eth0) entered disabled state
Fri Feb  7 22:26:22 2025 kern.info kernel: [   44.456491] ag71xx-legacy 19000000.eth eth0: entered allmulticast mode
Fri Feb  7 22:26:22 2025 kern.info kernel: [   44.463368] ag71xx-legacy 19000000.eth eth0: entered promiscuous mode
Fri Feb  7 22:26:22 2025 daemon.notice netifd: Interface 'lan' is enabled
Fri Feb  7 22:26:22 2025 daemon.notice netifd: Interface 'lan' is setting up now
Fri Feb  7 22:26:22 2025 daemon.notice netifd: Interface 'lan' is now up
Fri Feb  7 22:26:22 2025 daemon.notice netifd: Interface 'VLAN11' is enabled
Fri Feb  7 22:26:22 2025 daemon.notice netifd: Interface 'VLAN11' is setting up now
Fri Feb  7 22:26:22 2025 daemon.notice netifd: Interface 'VLAN11' is now up
Fri Feb  7 22:26:22 2025 daemon.info dnsmasq[1]: reading /tmp/resolv.conf.d/resolv.conf.auto
Fri Feb  7 22:26:22 2025 daemon.info dnsmasq[1]: using nameserver 192.168.11.1#53
Fri Feb  7 22:26:22 2025 daemon.info dnsmasq[1]: using only locally-known addresses for test
Fri Feb  7 22:26:22 2025 daemon.info dnsmasq[1]: using only locally-known addresses for onion
Fri Feb  7 22:26:22 2025 daemon.info dnsmasq[1]: using only locally-known addresses for localhost
Fri Feb  7 22:26:22 2025 daemon.info dnsmasq[1]: using only locally-known addresses for local
Fri Feb  7 22:26:22 2025 daemon.info dnsmasq[1]: using only locally-known addresses for invalid
Fri Feb  7 22:26:22 2025 daemon.info dnsmasq[1]: using only locally-known addresses for bind
Fri Feb  7 22:26:22 2025 daemon.info dnsmasq[1]: using only locally-known addresses for lan
Fri Feb  7 22:26:22 2025 daemon.notice netifd: Interface 'VLAN22' is enabled
Fri Feb  7 22:26:22 2025 daemon.notice netifd: Interface 'VLAN33' is enabled
Fri Feb  7 22:26:22 2025 daemon.notice netifd: Interface 'loopback' is enabled
Fri Feb  7 22:26:22 2025 daemon.notice netifd: Interface 'loopback' is setting up now
Fri Feb  7 22:26:22 2025 daemon.notice netifd: Interface 'loopback' is now up
Fri Feb  7 22:26:22 2025 daemon.notice netifd: Network device 'lo' link is up
Fri Feb  7 22:26:22 2025 daemon.notice netifd: Interface 'loopback' has link connectivity
Fri Feb  7 22:26:24 2025 daemon.notice netifd: radio0 (2473): WARNING: Variable 'data' does not exist or is not an array/object
Fri Feb  7 22:26:24 2025 daemon.notice netifd: radio1 (2474): WARNING: Variable 'data' does not exist or is not an array/object
Fri Feb  7 22:26:25 2025 user.notice firewall: Reloading firewall due to ifup of lan (br-lan.1)
Fri Feb  7 22:26:25 2025 daemon.info procd: - init complete -
Fri Feb  7 22:26:25 2025 daemon.notice hostapd: Set new config for phy phy1:
Fri Feb  7 22:26:25 2025 kern.info kernel: [   47.566833] eth0: link up (1000Mbps/Full duplex)
Fri Feb  7 22:26:25 2025 kern.info kernel: [   47.571714] br-lan: port 1(eth0) entered blocking state
Fri Feb  7 22:26:25 2025 kern.info kernel: [   47.577076] br-lan: port 1(eth0) entered forwarding state
Fri Feb  7 22:26:25 2025 daemon.notice netifd: Network device 'eth0' link is up
Fri Feb  7 22:26:25 2025 daemon.notice netifd: bridge 'br-lan' link is up
Fri Feb  7 22:26:25 2025 daemon.notice netifd: VLAN 'br-lan.1' link is up
Fri Feb  7 22:26:25 2025 daemon.notice netifd: Interface 'lan' has link connectivity
Fri Feb  7 22:26:25 2025 daemon.notice netifd: VLAN 'br-lan.11' link is up
Fri Feb  7 22:26:25 2025 daemon.notice netifd: Interface 'VLAN11' has link connectivity
Fri Feb  7 22:26:25 2025 daemon.notice netifd: VLAN 'br-lan.22' link is up
Fri Feb  7 22:26:25 2025 daemon.notice netifd: Interface 'VLAN22' has link connectivity
Fri Feb  7 22:26:25 2025 daemon.notice netifd: Interface 'VLAN22' is setting up now
Fri Feb  7 22:26:25 2025 daemon.notice netifd: Interface 'VLAN22' is now up
Fri Feb  7 22:26:25 2025 daemon.notice netifd: VLAN 'br-lan.33' link is up
Fri Feb  7 22:26:25 2025 daemon.notice netifd: Interface 'VLAN33' has link connectivity
Fri Feb  7 22:26:25 2025 daemon.notice netifd: Interface 'VLAN33' is setting up now
Fri Feb  7 22:26:25 2025 daemon.notice netifd: Interface 'VLAN33' is now up
Fri Feb  7 22:26:25 2025 daemon.notice wpa_supplicant[1749]: Set new config for phy phy1
Fri Feb  7 22:26:25 2025 daemon.notice hostapd: Set new config for phy phy0:
Fri Feb  7 22:26:26 2025 daemon.notice wpa_supplicant[1749]: Set new config for phy phy0
Fri Feb  7 22:26:59 2025 daemon.notice wpa_supplicant[1749]: Set new config for phy phy1
Fri Feb  7 22:26:59 2025 daemon.notice wpa_supplicant[1749]: Set new config for phy phy0
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: exiting on receipt of SIGTERM
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: started, version 2.90 cachesize 1000
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: DNS service limited to local subnets
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: compile time options: IPv6 GNU-getopt no-DBus UBus no-i18n no-IDN DHCP no-DHCPv6 no-Lua TFTP no-conntrack no-ipset no-nftset no-auth no-cryptohash no-DNSSEC no-ID loop-detect inotify dumpfile
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: UBus support enabled: connected to system bus
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for test
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for onion
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for localhost
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for local
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for invalid
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for bind
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for lan
Fri Feb  7 22:26:59 2025 daemon.notice hostapd: Set new config for phy phy1: /var/run/hostapd-phy1.conf
Fri Feb  7 22:26:59 2025 daemon.notice hostapd: Restart interface for phy phy1
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: reading /tmp/resolv.conf.d/resolv.conf.auto
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using nameserver 192.168.11.1#53
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for test
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for onion
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for localhost
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for local
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for invalid
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for bind
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: using only locally-known addresses for lan
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: read /etc/hosts - 12 names
Fri Feb  7 22:26:59 2025 daemon.info dnsmasq[1]: read /tmp/hosts/dhcp.cfg01411c - 0 names
Fri Feb  7 22:27:00 2025 daemon.notice hostapd: Configuration file: data: driver=nl80211 logger_syslog=127 logger_syslog_level=2 logger_stdout=127 logger_stdout_level=2 hw_mode=g supported_rates=60 90 120 180 240 360 480 540 basic_rates=60 120 240 beacon_int=100 stationary_ap=1 chanlist=1 #num_global_macaddr=1 #macaddr_base= ieee80211n=1 ht_coex=0 ht_capab=[LDPC][SHORT-GI-20][SHORT-GI-40][TX-STBC][RX-STBC1][DSSS_CCK-40] channel=1  interface=phy1-ap0 bssid=d8:84:66:54:14:d8 ctrl_interface=/var/run/hostapd ap_isolate=1 bss_load_update_period=60 chan_util_avg_period=600 disassoc_low_ack=1 skip_inactivity_poll=0 preamble=1 wmm_enabled=1 ignore_broadcast_ssid=0 uapsd_advertisement_enabled=1 utf8_ssid=1 multi_ap=0 wpa_passphrase=SECRET wpa_psk_file=/etc/hostapd.wpa_psk auth_algs=1 wpa=2 wpa_pairwise=CCMP ssid=127-O-O-2-X bridge=br-lan wds_bridge= snoop_iface=br-lan.1 wpa_disable_eapol_key_retries=0 wpa_key_mgmt=WPA-PSK okc=0 disable_pmksa_caching=1 dynamic_vlan=1 vlan_naming=1 vlan_bridge=br-vlan vlan_no_bridge= vlan
Fri Feb  7 22:27:00 2025 kern.info kernel: [   51.976831] br-lan: port 2(phy1-ap0) entered blocking state
Fri Feb  7 22:27:00 2025 kern.info kernel: [   51.982506] br-lan: port 2(phy1-ap0) entered disabled state
Fri Feb  7 22:27:00 2025 kern.info kernel: [   51.988245] ath9k 18100000.wmac phy1-ap0: entered allmulticast mode
Fri Feb  7 22:27:00 2025 kern.info kernel: [   51.994877] ath9k 18100000.wmac phy1-ap0: entered promiscuous mode
Fri Feb  7 22:27:02 2025 user.notice firewall: Reloading firewall due to ifup of VLAN11 (br-lan.11)
Fri Feb  7 22:27:03 2025 kern.info kernel: [   54.876632] br-lan.iot: port 1(eth0.33) entered blocking state
Fri Feb  7 22:27:03 2025 kern.info kernel: [   54.882573] br-lan.iot: port 1(eth0.33) entered disabled state
Fri Feb  7 22:27:03 2025 kern.info kernel: [   54.888573] eth0.33: entered allmulticast mode
Fri Feb  7 22:27:03 2025 kern.info kernel: [   54.893348] eth0.33: entered promiscuous mode
Fri Feb  7 22:27:03 2025 kern.info kernel: [   54.966706] br-lan.iot: port 1(eth0.33) entered blocking state
Fri Feb  7 22:27:03 2025 kern.info kernel: [   54.972651] br-lan.iot: port 1(eth0.33) entered forwarding state
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.022214] br-lan.iot: port 2(wlan0.33) entered blocking state
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.028288] br-lan.iot: port 2(wlan0.33) entered disabled state
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.034459] ath9k 18100000.wmac wlan0.33: entered allmulticast mode
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.041147] ath9k 18100000.wmac wlan0.33: entered promiscuous mode
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.445618] br-lan.guest: port 1(eth0.22) entered blocking state
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.451739] br-lan.guest: port 1(eth0.22) entered disabled state
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.458017] eth0.22: entered allmulticast mode
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.505408] eth0.22: entered promiscuous mode
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.536986] br-lan.guest: port 1(eth0.22) entered blocking state
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.543121] br-lan.guest: port 1(eth0.22) entered forwarding state
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.586746] br-lan.guest: port 2(wlan0.22) entered blocking state
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.592953] br-lan.guest: port 2(wlan0.22) entered disabled state
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.599219] ath9k 18100000.wmac wlan0.22: entered allmulticast mode
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.666555] ath9k 18100000.wmac wlan0.22: entered promiscuous mode
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.673035] br-lan.guest: port 2(wlan0.22) entered blocking state
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.679286] br-lan.guest: port 2(wlan0.22) entered forwarding state
Fri Feb  7 22:27:04 2025 kern.info kernel: [   55.876095] br-lan.guest: port 2(wlan0.22) entered disabled state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.085568] br-lan.lan: port 1(eth0.11) entered blocking state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.091511] br-lan.lan: port 1(eth0.11) entered disabled state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.097604] eth0.11: entered allmulticast mode
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.143837] eth0.11: entered promiscuous mode
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.176748] br-lan.lan: port 1(eth0.11) entered blocking state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.182694] br-lan.lan: port 1(eth0.11) entered forwarding state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.230154] br-lan.lan: port 2(wlan0.11) entered blocking state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.236372] br-lan.lan: port 2(wlan0.11) entered disabled state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.242424] ath9k 18100000.wmac wlan0.11: entered allmulticast mode
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.249213] ath9k 18100000.wmac wlan0.11: entered promiscuous mode
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.255687] br-lan.lan: port 2(wlan0.11) entered blocking state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.261724] br-lan.lan: port 2(wlan0.11) entered forwarding state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.559223] br-lan: port 2(phy1-ap0) entered blocking state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.564924] br-lan: port 2(phy1-ap0) entered forwarding state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.571313] br-lan.guest: port 2(wlan0.22) entered blocking state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.577551] br-lan.guest: port 2(wlan0.22) entered forwarding state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.584311] br-lan.iot: port 2(wlan0.33) entered blocking state
Fri Feb  7 22:27:05 2025 kern.info kernel: [   56.590382] br-lan.iot: port 2(wlan0.33) entered forwarding state
Fri Feb  7 22:27:05 2025 daemon.notice netifd: Network device 'phy1-ap0' link is up
Fri Feb  7 22:27:05 2025 daemon.info dnsmasq[1]: read /etc/hosts - 12 names
Fri Feb  7 22:27:05 2025 daemon.info dnsmasq[1]: read /tmp/hosts/dhcp.cfg01411c - 0 names
Fri Feb  7 22:27:05 2025 daemon.notice hostapd: phy1-ap0: interface state UNINITIALIZED->ENABLED
Fri Feb  7 22:27:05 2025 daemon.notice hostapd: phy1-ap0: AP-ENABLED
Fri Feb  7 22:27:05 2025 daemon.notice hostapd: Set new config for phy phy0: /var/run/hostapd-phy0.conf
Fri Feb  7 22:27:05 2025 daemon.notice hostapd: Restart interface for phy phy0
Fri Feb  7 22:27:06 2025 daemon.notice hostapd: netlink: recvfrom failed: No buffer space available
Fri Feb  7 22:27:06 2025 daemon.err hostapd: VLAN: vlan_event_receive: recvfrom failed: No buffer space available
Fri Feb  7 22:27:06 2025 daemon.notice hostapd: Configuration file: data: driver=nl80211 logger_syslog=127 logger_syslog_level=2 logger_stdout=127 logger_stdout_level=2 hw_mode=a beacon_int=100 stationary_ap=1 chanlist=36 tx_queue_data2_burst=2.0 #num_global_macaddr=1 #macaddr_base= ieee80211n=1 ht_coex=0 ht_capab=[HT40+][LDPC][SHORT-GI-20][SHORT-GI-40][TX-STBC][RX-STBC1][MAX-AMSDU-7935][DSSS_CCK-40] ieee80211ac=1 vht_oper_chwidth=1 vht_oper_centr_freq_seg0_idx=42 vht_capab=[RXLDPC][SHORT-GI-80][TX-STBC-2BY1][RX-ANTENNA-PATTERN][TX-ANTENNA-PATTERN][RX-STBC-1][MAX-MPDU-11454][MAX-A-MPDU-LEN-EXP7] channel=36  interface=phy0-ap0 bssid=d8:84:66:54:14:d0 ctrl_interface=/var/run/hostapd ap_isolate=1 bss_load_update_period=60 chan_util_avg_period=600 disassoc_low_ack=1 skip_inactivity_poll=0 preamble=1 wmm_enabled=1 ignore_broadcast_ssid=0 uapsd_advertisement_enabled=1 utf8_ssid=1 multi_ap=0 wpa_passphrase=SECRET wpa_psk_file=/etc/hostapd.wpa_psk auth_algs=1 wpa=2 wpa_pairwise=CCMP ssid=127-O-O-5-X bridge=br-lan wds_b
Fri Feb  7 22:27:06 2025 daemon.notice netifd: Wireless device 'radio1' is now up
Fri Feb  7 22:27:08 2025 kern.warn kernel: [   59.241927] ath10k_pci 0000:00:00.0: 10.1 wmi init: vdevs: 16  peers: 127  tid: 256
Fri Feb  7 22:27:08 2025 kern.info kernel: [   59.259782] ath10k_pci 0000:00:00.0: wmi print 'P 128 V 8 T 410'
Fri Feb  7 22:27:08 2025 kern.info kernel: [   59.266164] ath10k_pci 0000:00:00.0: wmi print 'msdu-desc: 1424  sw-crypt: 0 ct-sta: 0'
Fri Feb  7 22:27:08 2025 kern.info kernel: [   59.274302] ath10k_pci 0000:00:00.0: wmi print 'alloc rem: 24984 iram: 38672'
Fri Feb  7 22:27:08 2025 kern.warn kernel: [   59.347166] ath10k_pci 0000:00:00.0: pdev param 0 not supported by firmware
Fri Feb  7 22:27:08 2025 kern.info kernel: [   59.375573] ath10k_pci 0000:00:00.0: rts threshold -1
Fri Feb  7 22:27:08 2025 kern.info kernel: [   59.395683] br-lan: port 3(phy0-ap0) entered blocking state
Fri Feb  7 22:27:08 2025 kern.info kernel: [   59.401366] br-lan: port 3(phy0-ap0) entered disabled state
Fri Feb  7 22:27:08 2025 kern.info kernel: [   59.407290] ath10k_pci 0000:00:00.0 phy0-ap0: entered allmulticast mode
Fri Feb  7 22:27:08 2025 kern.info kernel: [   59.414325] ath10k_pci 0000:00:00.0 phy0-ap0: entered promiscuous mode
Fri Feb  7 22:27:08 2025 daemon.notice hostapd: phy0-ap0: interface state UNINITIALIZED->HT_SCAN
Fri Feb  7 22:27:08 2025 daemon.notice netifd: Wireless device 'radio0' is now up
Fri Feb  7 22:27:11 2025 daemon.err hostapd: VLAN: vlan_add: ADD_VLAN_CMD failed for eth0: File exists
Fri Feb  7 22:27:11 2025 daemon.err hostapd: VLAN: vlan_add: ADD_VLAN_CMD failed for eth0: File exists
Fri Feb  7 22:27:11 2025 daemon.err hostapd: VLAN: vlan_add: ADD_VLAN_CMD failed for eth0: File exists
Fri Feb  7 22:27:11 2025 kern.info kernel: [   62.895070] br-lan: port 3(phy0-ap0) entered blocking state
Fri Feb  7 22:27:11 2025 kern.info kernel: [   62.900807] br-lan: port 3(phy0-ap0) entered forwarding state
Fri Feb  7 22:27:11 2025 daemon.notice netifd: Network device 'phy0-ap0' link is up
Fri Feb  7 22:27:11 2025 daemon.notice hostapd: phy0-ap0: interface state HT_SCAN->ENABLED
Fri Feb  7 22:27:11 2025 daemon.notice hostapd: phy0-ap0: AP-ENABLED
Fri Feb  7 22:27:13 2025 daemon.warn odhcpd[1977]: No default route present, overriding ra_lifetime to 0!
Fri Feb  7 22:27:29 2025 daemon.warn odhcpd[1977]: No default route present, overriding ra_lifetime to 0!
Fri Feb  7 22:27:45 2025 daemon.warn odhcpd[1977]: No default route present, overriding ra_lifetime to 0!
Fri Feb  7 22:27:57 2025 authpriv.info dropbear[3563]: Child connection from 192.168.1.3:53644
Fri Feb  7 22:28:02 2025 authpriv.notice dropbear[3563]: Password auth succeeded for 'root' from 192.168.1.3:53644
Fri Feb  7 22:28:33 2025 daemon.err uhttpd[2095]: [info] luci: accepted login on /admin/network/wireless for root from 192.168.1.3
Fri Feb  7 22:31:03 2025 daemon.info hostapd: phy1-ap0: STA fa:0e:3e:18:92:1f IEEE 802.11: authenticated
Fri Feb  7 22:31:03 2025 daemon.info hostapd: phy1-ap0: STA fa:0e:3e:18:92:1f IEEE 802.11: associated (aid 1)
Fri Feb  7 22:31:03 2025 daemon.notice hostapd: Assigned VLAN ID 22 from wpa_psk_file to fa:0e:3e:18:92:1f
Fri Feb  7 22:31:03 2025 daemon.notice hostapd: phy1-ap0: AP-STA-CONNECTED fa:0e:3e:18:92:1f auth_alg=open
Fri Feb  7 22:31:03 2025 daemon.info hostapd: phy1-ap0: STA fa:0e:3e:18:92:1f RADIUS: starting accounting session 15B9630079BCAADC
Fri Feb  7 22:31:03 2025 daemon.info hostapd: phy1-ap0: STA fa:0e:3e:18:92:1f WPA: pairwise key handshake completed (RSN)
Fri Feb  7 22:31:03 2025 daemon.notice hostapd: phy1-ap0: EAPOL-4WAY-HS-COMPLETED fa:0e:3e:18:92:1f
Fri Feb  7 22:31:13 2025 daemon.notice hostapd: phy1-ap0: AP-STA-DISCONNECTED fa:0e:3e:18:92:1f
Fri Feb  7 22:31:13 2025 daemon.info hostapd: phy0-ap0: STA 9e:5e:87:36:f0:d8 IEEE 802.11: authenticated
Fri Feb  7 22:31:13 2025 daemon.info hostapd: phy0-ap0: STA 9e:5e:87:36:f0:d8 IEEE 802.11: associated (aid 1)
Fri Feb  7 22:31:13 2025 daemon.notice hostapd: Assigned VLAN ID 22 from wpa_psk_file to 9e:5e:87:36:f0:d8
Fri Feb  7 22:31:13 2025 daemon.err hostapd: nl80211: NL80211_ATTR_STA_VLAN (addr=9e:5e:87:36:f0:d8 ifname=wlan0.22 vlan_id=22) failed: -22 (Invalid argument)
Fri Feb  7 22:31:16 2025 daemon.info hostapd: phy1-ap0: STA fa:0e:3e:18:92:1f IEEE 802.11: authenticated
Fri Feb  7 22:31:16 2025 daemon.info hostapd: phy1-ap0: STA fa:0e:3e:18:92:1f IEEE 802.11: associated (aid 1)
Fri Feb  7 22:31:16 2025 daemon.notice hostapd: Assigned VLAN ID 22 from wpa_psk_file to fa:0e:3e:18:92:1f
Fri Feb  7 22:31:16 2025 daemon.notice hostapd: phy1-ap0: AP-STA-CONNECTED fa:0e:3e:18:92:1f auth_alg=open
Fri Feb  7 22:31:16 2025 daemon.info hostapd: phy1-ap0: STA fa:0e:3e:18:92:1f RADIUS: starting accounting session 562B30DB9898A6A6
Fri Feb  7 22:31:16 2025 daemon.info hostapd: phy1-ap0: STA fa:0e:3e:18:92:1f WPA: pairwise key handshake completed (RSN)
Fri Feb  7 22:31:16 2025 daemon.notice hostapd: phy1-ap0: EAPOL-4WAY-HS-COMPLETED fa:0e:3e:18:92:1f
Fri Feb  7 22:31:18 2025 daemon.info hostapd: phy0-ap0: STA 9e:5e:87:36:f0:d8 IEEE 802.11: deauthenticated due to local deauth request
Fri Feb  7 22:31:22 2025 daemon.notice hostapd: phy1-ap0: AP-STA-DISCONNECTED fa:0e:3e:18:92:1f
Fri Feb  7 22:31:44 2025 daemon.info hostapd: phy1-ap0: STA fa:0e:3e:18:92:1f IEEE 802.11: authenticated
Fri Feb  7 22:31:44 2025 daemon.info hostapd: phy1-ap0: STA fa:0e:3e:18:92:1f IEEE 802.11: associated (aid 1)
Fri Feb  7 22:31:44 2025 daemon.notice hostapd: Assigned VLAN ID 11 from wpa_psk_file to fa:0e:3e:18:92:1f
Fri Feb  7 22:31:44 2025 daemon.notice hostapd: phy1-ap0: AP-STA-CONNECTED fa:0e:3e:18:92:1f auth_alg=open
Fri Feb  7 22:31:44 2025 daemon.info hostapd: phy1-ap0: STA fa:0e:3e:18:92:1f RADIUS: starting accounting session 81223F7835805F3B
Fri Feb  7 22:31:44 2025 daemon.info hostapd: phy1-ap0: STA fa:0e:3e:18:92:1f WPA: pairwise key handshake completed (RSN)
Fri Feb  7 22:31:44 2025 daemon.notice hostapd: phy1-ap0: EAPOL-4WAY-HS-COMPLETED fa:0e:3e:18:92:1f
```

[monotux](https://forum.openwrt.org/u/monotux) February 13, 2025, 8:41pm

172

I just got this working on my Acer Vero W6m running 24.10.0, this is
amazing! Thanks to everyone posting in this thread
![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:")

In case anyone comes here looking for a semi-complete setup, mine is
below.

My use case is this:

- One interface with both untagged and tagged frames, called `internet`
  (so `internet.100`, `internet.200` are the interfaces for VLAN 100 &
  200). Untagged is used for the AP, while user and iot traffic goes on
  the vlans
- One bridge per vlan, `br-100` and `br-200`
- I'm manually creating the bridges and vlan interfaces

``` lang-plaintext
# /etc/config/network
# This isn't a complete file but should cover enough
config device
        option name 'internet'
        option macaddr 'xx:xx:xx:xx:xx:xx'

# management interface for ap
config interface 'uplink'
        option proto 'dhcp'
        option device 'internet'

config device
        option type '8021q'
        option ifname 'internet'
        option vid '200'
        option name 'internet.200'

config device
        option type '8021q'
        option ifname 'internet'
        option vid '100'
        option name 'internet.100'

# super specific for me: I'm adding the other switch ports to vlan100
config device
        option type 'bridge'
        option name 'br-100'
        option bridge_empty '1'
        list ports 'internet.100'
        list ports 'lan1'
        list ports 'lan2'
        list ports 'lan3'

config device
        option type 'bridge'
        option name 'br-200'
        option bridge_empty '1'
        list ports 'internet.200'

config interface 'wifi'
        option proto 'none'
        option device 'br-100'
        option defaultroute '0'

config interface 'iot'
        option proto 'none'
        option device 'br-200'
        option defaultroute '0'
```

Next up is my wireless configuration. In this case I have three radios
(2.4, 5 & 6 GHz) and I want this multi-psk on all frequencies.

``` lang-plaintext
# /etc/config/wireless
config wifi-device 'radio0'
        option type 'mac80211'
        option path 'soc/11280000.pcie/pci0000:00/0000:00:00.0/0000:01:00.0'
        option band '2g'
        option channel '1'
        option htmode 'HE20'
        option cell_density '0'
        option country 'SE'

config wifi-iface 'default_radio0'
        option device 'radio0'
        option mode 'ap'
        option ssid 'OpenWrt'
        option encryption 'psk2'
        # This is the important part!
        option dynamic_vlan '1'
        option vlan_file '/etc/config/wpa_vlan.radio0'
        option wpa_psk_file '/etc/config/wpa_psk'
        option vlan_tagged_interface 'internet'
        option vlan_bridge 'br-'

config wifi-device 'radio1'
        option type 'mac80211'
        option path 'soc/11280000.pcie/pci0000:00/0000:00:00.0/0000:01:00.0+1'
        option htmode 'HE80'
        option cell_density '0'
        option country 'SE'

config wifi-iface 'default_radio1'
        option device 'radio1'
        option mode 'ap'
        option ssid 'OpenWrt'
        option encryption 'psk2'
        # Notice that the vlan_file has the suffix of radio1!
        option dynamic_vlan '1'
        option vlan_file '/etc/config/wpa_vlan.radio1'
        option wpa_psk_file '/etc/config/wpa_psk'
        option vlan_tagged_interface 'internet'
        option vlan_bridge 'br-'

config wifi-device 'radio2'
        option type 'mac80211'
        option path 'platform/soc/18000000.wifi'
        option band '5g'
        option channel '36'
        option htmode 'HE80'
        option cell_density '0'
        option country 'SE'

config wifi-iface 'default_radio2'
        option device 'radio2'
        option mode 'ap'
        option ssid 'OpenWrt'
        option encryption 'psk2'
        option dynamic_vlan '1'
        # notice that the suffix here is radio2!
        option vlan_file '/etc/config/wpa_vlan.radio2'
        option wpa_psk_file '/etc/config/wpa_psk'
        option vlan_tagged_interface 'internet'
        option vlan_bridge 'br-'
```

To make this multi-psk work on multiple interfaces I figured I had to
create a `vlan_file` for each radio:

``` lang-plaintext
# /etc/config/wpa_vlan.radio0
100 wlan0.100
200 wlan0.200

# /etc/config/wpa_vlan.radio1
100 wlan1.100
200 wlan1.200

# /etc/config/wpa_vlan.radio2
100 wlan2.100
200 wlan2.200
```

And finally, my `wpa_psk`:

``` lang-plaintext
vlanid=100 00:00:00:00:00:00 my-really-secure-psk
vlanid=200 00:00:00:00:00:00 my-other-really-secure-psk
```

Now my hostapd will create wireless interfaces and add them to my
existing bridges, and clients are mapped to the correct VLAN no matter
what band they connect to.

```
# brctl show
bridge name     bridge id               STP enabled     interfaces
br-100          7fff.000123131000       no              internet.100
                                                        wlan0.100
                                                        lan2
                                                        wlan2.100
                                                        lan3
                                                        lan1
br-200          7fff.000123131000       no              internet.200
                                                        wlan0.200
                                                        wlan2.200
```

6 Likes

[raenye](https://forum.openwrt.org/u/raenye) February 14, 2025, 12:22pm

173

Thanks for the detailed info!

May I ask what is the use of the interfaces `wifi` and `iot`?  
Aren't `br-x00` enough?  
Also, `internet` is just a switch port, right? (same as `wan` on most
devices)

[monotux](https://forum.openwrt.org/u/monotux) February 14, 2025, 8:48pm

174

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/raenye/48/72867_2.png)
raenye:

> May I ask what is the use of the interfaces `wifi` and `iot`?
>
> Aren't `br-x00` enough?

I don't know
![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:")
I'm used to just treating bridges as plain bridges, and plugging in
subinterfaces to them to provide a path to the rest of the subnet.

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/raenye/48/72867_2.png)
raenye:

> Also, `internet` is just a switch port, right? (same as `wan` on most
> devices)

Yes, exactly! No switch device or so.

[raenye](https://forum.openwrt.org/u/raenye) February 14, 2025, 11:13pm

175

![](https://forum.openwrt.org/letter_avatar/monotux/48/5_047764fa20c941da5a8a2a620d7322fb.png)
monotux:

> I don't know
> ![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:")
> I'm used to just treating bridges as plain bridges, and plugging in
> subinterfaces to them to provide a path to the rest of the subnet.

Hmm, perhaps these interfaces are mentioned in `/etc/config/firewall` or
`/etc/config/dhcp`?

[takimata](https://forum.openwrt.org/u/takimata) February 15, 2025,
3:46am

176

![](https://forum.openwrt.org/letter_avatar/monotux/48/5_047764fa20c941da5a8a2a620d7322fb.png)
monotux:

> I have three radios (2.4, 5 & 6 GHz) and I want this multi-psk on all
> frequencies. (...) To make this multi-psk work on multiple interfaces
> I figured I had to create a `vlan_file` for each radio:

While you can do that, by using [the UCI
abstraction](https://git.openwrt.org/?p=openwrt/openwrt.git;a=commit;h=5aa2ddd0d6b9759c62bbb7bb11b72a7f4269c16b)
you don't have to. `wifi-vlan` sections should apply to all radios at
once (unless you limit them to a particular station using the optional
`iface` option), and then you can just add them to your existing
bridges. (I believe it's still possible to mix in the
`wpa_psk_file option` if you want its benefits over the UCI
abstractions.)

[raenye](https://forum.openwrt.org/u/raenye) February 15, 2025, 10:19am

177

IIRC the UCI abstraction forces you to have `option network` in the
non-vlan WiFi interface, which doesn't happen in monotux's manual
config.

[monotux](https://forum.openwrt.org/u/monotux) February 16, 2025, 7:07am

178

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/raenye/48/72867_2.png)
raenye:

> Hmm, perhaps these interfaces are mentioned in `/etc/config/firewall`
> or `/etc/config/dhcp`?

Yes, they are defined at the end of my `/etc/config/network` example.
They are just unmanaged without addresses.

[monotux](https://forum.openwrt.org/u/monotux) February 16, 2025, 7:09am

179

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> While you can do that, by using [the UCI
> abstraction](https://git.openwrt.org/?p=openwrt/openwrt.git;a=commit;h=5aa2ddd0d6b9759c62bbb7bb11b72a7f4269c16b)
> you don't have to. `wifi-vlan` sections should apply to all radios at
> once (unless you limit them to a particular station using the optional
> `iface` option), and then you can just add them to your existing
> bridges. (I believe it's still possible to mix in the
> `wpa_psk_file option` if you want its benefits over the UCI
> abstractions.)

Thanks, I'll have a look!

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/raenye/48/72867_2.png)
raenye:

> IIRC the UCI abstraction forces you to have `option network` in the
> non-vlan WiFi interface, which doesn't happen in monotux's manual
> config.

No non-vlan wifi interface in my configuration
![:slight_smile:](https://forum.openwrt.org/images/emoji/twitter/slight_smile.png?v=12 ":slight_smile:")

[Rafal9184](https://forum.openwrt.org/u/Rafal9184) February 21, 2025,
12:41pm

180

Hi,  
I'm using OpenWRT 24.10.0 on Xiaomi AX3000T.

Currently I've one, single bridge `br-lan`, which contains all, 4
physical network interfaces:

- wan (uplink - vlan101 untagged, others tagged)
- lan2
- lan3
- lan4

Please find content of `/etc/config/network` below:

/etc/config/network

root@ap01:~# cat /etc/config/network

config interface 'loopback'  
option device 'lo'  
option proto 'static'  
option ipaddr '127.0.0.1'  
option netmask '255.0.0.0'

config globals 'globals'  
option ula_prefix 'fdee:66b1:4d2b::/48'

config device  
option name 'br-lan'  
option type 'bridge'  
list ports 'lan2'  
list ports 'lan3'  
list ports 'lan4'  
list ports 'wan'  
option ipv6 '0'

config device  
option type '8021q'  
option ifname 'br-lan'  
option vid '102'  
option name 'br-lan.102'  
option ipv6 '0'

config device  
option type '8021q'  
option ifname 'br-lan'  
option vid '103'  
option name 'br-lan.103'

config device  
option type '8021q'  
option ifname 'br-lan'  
option vid '104'  
option name 'br-lan.104'

config device  
option type '8021q'  
option ifname 'br-lan'  
option vid '200'  
option name 'br-lan.200'

config device  
option type '8021q'  
option ifname 'br-lan'  
option vid '101'  
option name 'br-lan.101'

config bridge-vlan  
option device 'br-lan'  
option vlan '101'  
list ports 'lan2:u\*'  
list ports 'lan3:u\*'  
list ports 'lan4:u\*'  
list ports 'wan:u\*'

config bridge-vlan  
option device 'br-lan'  
option vlan '102'  
list ports 'lan2:t'  
list ports 'lan3:t'  
list ports 'lan4:t'  
list ports 'wan:t'

config bridge-vlan  
option device 'br-lan'  
option vlan '103'  
list ports 'lan2:t'  
list ports 'lan3:t'  
list ports 'lan4:t'  
list ports 'wan:t'

config bridge-vlan  
option device 'br-lan'  
option vlan '104'  
list ports 'lan2:t'  
list ports 'lan3:t'  
list ports 'lan4:t'  
list ports 'wan:t'

config bridge-vlan  
option device 'br-lan'  
option vlan '200'  
list ports 'lan2:t'  
list ports 'lan3:t'  
list ports 'lan4:t'  
list ports 'wan:t'

config interface 'guest_vlan104'  
option proto 'none'  
option device 'br-lan.104'  
option delegate '0'  
option defaultroute '0'

config interface 'lan_vlan101'  
option proto 'none'  
option device 'br-lan.101'  
option delegate '0'  
option defaultroute '0'

config interface 'mgmt_vlan200'  
option proto 'static'  
option device 'br-lan.200'  
option ipaddr '192.168.200.3'  
option netmask '255.255.255.0'  
option delegate '0'  
option gateway '192.168.200.1'  
option dns '192.168.1.185'

config interface 'monit_vlan102'  
option proto 'none'  
option device 'br-lan.102'  
option delegate '0'  
option defaultroute '0'

config interface 'iot_vlan103'  
option proto 'none'  
option device 'br-lan.103'  
option delegate '0'  
option defaultroute '0'

AX3000T has two radios - one for 2.4 GHz and another for 5 GHz.

Below you can find /etc/config/wireless content:

/etc/config/wireless

root@ap01:~# cat /etc/config/wireless

config wifi-device 'radio0'  
option type 'mac80211'  
option path 'platform/soc/18000000.wifi'  
option band '2g'  
option channel '11'  
option htmode 'HE40'  
option txpower '26'  
option country 'MY'  
option cell_density '0'  
option noscan '1'

\[...\]

config wifi-device 'radio1'  
option type 'mac80211'  
option path 'platform/soc/18000000.wifi+1'  
option band '5g'  
option channel '100'  
option htmode 'HE160'  
option country 'MY'  
option cell_density '0'  
option txpower '28'

\[...\]

config wifi-iface 'wifinet2'  
option device 'radio0'  
option mode 'ap'  
option ssid 'test'  
option hidden '0'  
option encryption 'psk2'  
option dynamic_vlan '1'  
option isolate '0'  
option vlan_file '/etc/hostapd.vlan'  
option wpa_psk_file '/etc/hostapd.wpa_psk'  
option vlan_interface 'wan'  
option vlan_bridge 'br-lan.'

Then, my `/etc/hostapd.vlan` file:

/etc/hostapd.vlan

root@ap01:~# cat /etc/hostapd.vlan  
101 br-lan.101  
102 br-lan.102  
103 br-lan.103  
104 br-lan.104  
200 br-lan.200

And finally, `/etc/hostapd.wpa_psk` file:

/etc/hostapd.wpa_psk

root@ap01:~# cat /etc/hostapd.wpa_psk  
vlanid=101 00:00:00:00:00:00 passphrase1  
vlanid=102 00:00:00:00:00:00 passphrase2  
vlanid=103 00:00:00:00:00:00 passphrase3  
vlanid=104 00:00:00:00:00:00 passphrase4  
vlanid=200 00:00:00:00:00:00 passphrase5

When I'm restarting 2.4GHz radio, I can see following logs:

logs

Fri Feb 21 13:00:55 2025 daemon.notice netifd: Network device 'phy0-ap0'
link is up  
Fri Feb 21 13:00:55 2025 daemon.err hostapd: WPA initialization for VLAN
200 failed (-1)  
Fri Feb 21 13:00:55 2025 daemon.err hostapd: WPA deinit of br-lan.200
failed  
Fri Feb 21 13:00:55 2025 daemon.err hostapd: Failed to remove interface
(ifidx=12)  
Fri Feb 21 13:00:55 2025 daemon.err hostapd: VLAN: Could not add VLAN
br-lan.200: Resource temporarily unava ilable  
Fri Feb 21 13:00:55 2025 daemon.err hostapd: VLAN initialization
failed.  
Fri Feb 21 13:00:55 2025 daemon.err hostapd: Failed to remove interface
(ifidx=12)  
Fri Feb 21 13:00:55 2025 daemon.err hostapd: Failed to remove interface
(ifidx=11)  
Fri Feb 21 13:00:55 2025 daemon.err hostapd: Failed to remove interface
(ifidx=10)  
Fri Feb 21 13:00:55 2025 daemon.err hostapd: Failed to remove interface
(ifidx=9)  
Fri Feb 21 13:00:55 2025 daemon.err hostapd: Failed to remove interface
(ifidx=8)  
Fri Feb 21 13:00:55 2025 daemon.err hostapd: Interface initialization
failed  
Fri Feb 21 13:00:55 2025 daemon.notice hostapd: phy0-ap0: interface
state COUNTRY_UPDATE-\>DISABLED  
Fri Feb 21 13:00:55 2025 daemon.notice hostapd: phy0-ap0: AP-DISABLED  
Fri Feb 21 13:00:55 2025 daemon.err hostapd: phy0-ap0: Unable to setup
interface.

Can someone please take a look on my config and help me resolve the
issue?

[takimata](https://forum.openwrt.org/u/takimata) February 21, 2025,
12:57pm

181

You have two errors. First of all, by using `dynamic_vlan` and
`vlan_bridge 'br-lan.'` you are asking hostapd to *create* VLAN-tagged
and VLAN-numbered bridges for the VLANs you list in `hostapd.vlan`, e.g.
`br-lan.101` for VLAN 101, and so on. Your bridges, however, already
exist, and that's why hostapd fails in creating them and tearing them
down again.

Secondly, in `hostapd.vlan`, the second parameter in every line is the
name of the interface hostapd creates to attach to the vlan-tagged
bridge. It can be named pretty much anything *except* an already
existing interface. If you want to attach a vlan-tagged wifi to an
existing bridge, use the existing bridge as a *third* parameter in the
line, e.g.:

``` lang-plaintext
101 wifi.101 br-lan.101
```

(You don't *have to* name it `wifi.101`, you can also name it
`refrigerator`, but it's a bit less confusing if you keep to a
consistent naming scheme.)

[Rafal9184](https://forum.openwrt.org/u/Rafal9184) February 21, 2025,
2:11pm

182

Thank you for your help.  
I've deleted `dynamic_vlan` and `vlan_bridge` parameters in
`/etc/config/wireless`:

``` lang-plaintext
config wifi-iface 'wifinet2'
        option device 'radio0'
        option mode 'ap'
        option ssid 'test'
        option hidden '0'
        option encryption 'psk2'
        option isolate '0'
        option vlan_file '/etc/hostapd.vlan'
        option wpa_psk_file '/etc/hostapd.wpa_psk'
        option vlan_interface 'wan'
```

Also, I added `wifi.XXX` as second parameter in `hostapd.vlan` in each
line:

``` lang-plaintext
root@ap01:~# cat /etc/hostapd.vlan
101 wifi.101 br-lan.101
102 wifi.102 br-lan.102
103 wifi.103 br-lan.103
104 wifi.104 br-lan.104
200 wifi.200 br-lan.200
```

Now, after 2.4GHz radio restart, everything starts correctly, even
clients are assigned to proper VLANs based on passphrase:

``` lang-plaintext
Fri Feb 21 15:07:03 2025 daemon.info hostapd: phy0-ap1: STA XX:XX:XX:XX:XX:XX IEEE 802.11: authenticated
Fri Feb 21 15:07:03 2025 daemon.info hostapd: phy0-ap1: STA XX:XX:XX:XX:XX:XX IEEE 802.11: associated (aid 1)
Fri Feb 21 15:07:03 2025 daemon.notice hostapd: Assigned VLAN ID 104 from wpa_psk_file to XX:XX:XX:XX:XX:XX
Fri Feb 21 15:07:03 2025 daemon.notice hostapd: phy0-ap1: AP-STA-CONNECTED XX:XX:XX:XX:XX:XX auth_alg=open
Fri Feb 21 15:07:03 2025 daemon.info hostapd: phy0-ap1: STA XX:XX:XX:XX:XX:XX WPA: pairwise key handshake completed (RSN)
Fri Feb 21 15:07:03 2025 daemon.notice hostapd: phy0-ap1: EAPOL-4WAY-HS-COMPLETED XX:XX:XX:XX:XX:XX
```

But client has no connection to network, VLAN's gateway is not
reachable.

I've checked using `brctl show` command that `wifi.XXX` interfaces are
not listed, thus not attachted to the bridge:

``` lang-plaintext
root@ap01:~# brctl show
bridge name     bridge id               STP enabled     interfaces
br-lan          7fff.44f7702b3c58       no              phy1-ap0
                                                        lan4
                                                        lan2
                                                        wan
                                                        lan3
                                                        phy0-ap0
```

Maybe, it's worth mentioning that in `/etc/config/wireless` I can't use
`vlan_tagged_interface` parameter, because then in logs I can see:

``` lang-plaintext
Fri Feb 21 14:49:51 2025 daemon.err hostapd: Line 120: unknown configuration item 'vlan_tagged_interface'
```

so I used `vlan_interface` parameter, if that matters.

Is there something more that should be modified?

[takimata](https://forum.openwrt.org/u/takimata) February 21, 2025,
3:38pm

183

![](https://forum.openwrt.org/letter_avatar/rafal9184/48/5_047764fa20c941da5a8a2a620d7322fb.png)
Rafal9184:

> I can't use `vlan_tagged_interface`

You don't need to, in fact you shouldn't. This and `vlan_bridge` are
convenience functions; If you don't have a bridge and don't have
vlan-tagged ethernet interfaces within, hostapd will basically create
the whole shebang for you. This is convenient for dumb access points
with only one ethernet interface.

You already have both, you only need hostapd to create the vlan-tagged
wifi and attach it to the existing bridge. Remove
`vlan_tagged_interface`. (And also `vlan_interface`, that's not a real
option and will be ignored.)

That being said:

> `unknown configuration item 'vlan_tagged_interface'`

is a clear indication that you are using the stripped-down default
*wpad-basic* which is missing the features for this purpose. That also
explains the missing wifi interface in your bridge. [You need the full
*wpad*.](https://forum.openwrt.org/t/individual-per-passphrase-wifi-vlans-using-wpa-psk-file-no-radius-required/161696/4)

[Rafal9184](https://forum.openwrt.org/u/Rafal9184) February 21, 2025,
5:25pm

184

Yes, you're right. I had `wpad-basic-mbedtls` package installed, so I
removed it, killed `hostpad`, and then installed `wpad-mbedtls` package.

In logs now I can see the following:

``` lang-plaintext
Fri Feb 21 18:22:22 2025 daemon.info hostapd: phy0-ap1: STA XX:XX:XX:XX:XX:XX IEEE 802.11: authenticated
Fri Feb 21 18:22:22 2025 daemon.info hostapd: phy0-ap1: STA XX:XX:XX:XX:XX:XX IEEE 802.11: associated (aid 1)
Fri Feb 21 18:22:22 2025 daemon.notice hostapd: Assigned VLAN ID 104 from wpa_psk_file to XX:XX:XX:XX:XX:XX
Fri Feb 21 18:22:22 2025 daemon.notice hostapd: phy0-ap1: AP-STA-CONNECTED XX:XX:XX:XX:XX:XX auth_alg=open
Fri Feb 21 18:22:22 2025 daemon.info hostapd: phy0-ap1: STA XX:XX:XX:XX:XX:XX RADIUS: starting accounting session YYYYYYYYYYYYYY
Fri Feb 21 18:22:22 2025 daemon.info hostapd: phy0-ap1: STA XX:XX:XX:XX:XX:XX WPA: pairwise key handshake completed (RSN)
Fri Feb 21 18:22:22 2025 daemon.notice hostapd: phy0-ap1: EAPOL-4WAY-HS-COMPLETED XX:XX:XX:XX:XX:XX
```

So RADIUS is getting involved.  
But it doesn't change the fact, that still, output from `brctl show` is
the same:

``` lang-plaintext
root@ap01:~# brctl show
bridge name     bridge id               STP enabled     interfaces
br-lan          7fff.44f7702b3c58       no              phy1-ap0
                                                        lan4
                                                        lan2
                                                        wan
                                                        lan3
                                                        phy0-ap0
```

No new interfaces are listed. And, of course, gateway is still
unreachable.

Do you have any clue why is that?

[takimata](https://forum.openwrt.org/u/takimata) February 21, 2025,
5:35pm

185

![](https://forum.openwrt.org/letter_avatar/rafal9184/48/5_047764fa20c941da5a8a2a620d7322fb.png)
Rafal9184:

> Do you have any clue why is that?

Not at the moment, mainly because I don't have a similar test case set
up. Sorry. Did you try rebooting the device, to give it a clean bringup?
What does the logfile say when wifi is initialized?

[Rafal9184](https://forum.openwrt.org/u/Rafal9184) February 21, 2025,
5:54pm

186

Yes, I rebooted the device, no changes.

Logs produced by using `wifi down` command:

``` lang-plaintext
Fri Feb 21 18:36:29 2025 daemon.notice hostapd: Set new config for phy phy1:
Fri Feb 21 18:36:29 2025 daemon.notice hostapd: Remove interface 'phy1'
Fri Feb 21 18:36:29 2025 daemon.notice hostapd: phy1-ap0: interface state ENABLED->DISABLED
Fri Feb 21 18:36:29 2025 daemon.notice hostapd: phy1-ap0: AP-STA-DISCONNECTED 9a:dc:e2:84:46:d3
Fri Feb 21 18:36:29 2025 daemon.notice hostapd: phy1-ap0: AP-STA-DISCONNECTED 80:8a:bd:50:77:78
Fri Feb 21 18:36:29 2025 daemon.notice hostapd: phy1-ap0: AP-DISABLED
Fri Feb 21 18:36:29 2025 daemon.notice hostapd: phy1-ap0: CTRL-EVENT-TERMINATING
Fri Feb 21 18:36:29 2025 daemon.err hostapd: rmdir[ctrl_interface=/var/run/hostapd]: Permission denied
Fri Feb 21 18:36:29 2025 daemon.notice hostapd: nl80211: deinit ifname=phy1-ap0 disabled_11b_rates=0
Fri Feb 21 18:36:29 2025 kern.info kernel: [ 5294.038590] mt798x-wmac 18000000.wifi phy1-ap0: left allmulticast mode
Fri Feb 21 18:36:29 2025 kern.info kernel: [ 5294.045205] mt798x-wmac 18000000.wifi phy1-ap0: left promiscuous mode
Fri Feb 21 18:36:29 2025 kern.info kernel: [ 5294.051746] br-lan: port 5(phy1-ap0) entered disabled state
Fri Feb 21 18:36:29 2025 daemon.notice netifd: Network device 'phy1-ap0' link is down
Fri Feb 21 18:36:29 2025 daemon.notice hostapd: Set new config for phy phy0:
Fri Feb 21 18:36:29 2025 daemon.notice hostapd: Remove interface 'phy0'
Fri Feb 21 18:36:29 2025 daemon.notice hostapd: phy0-ap0: interface state ENABLED->DISABLED
Fri Feb 21 18:36:29 2025 daemon.notice hostapd: phy0-ap1: AP-DISABLED
Fri Feb 21 18:36:29 2025 daemon.notice hostapd: phy0-ap1: CTRL-EVENT-TERMINATING
Fri Feb 21 18:36:29 2025 daemon.err hostapd: rmdir[ctrl_interface=/var/run/hostapd]: Permission denied
Fri Feb 21 18:36:30 2025 daemon.notice hostapd: phy0-ap0: AP-STA-DISCONNECTED 58:b6:23:02:f9:59
Fri Feb 21 18:36:30 2025 daemon.notice hostapd: phy0-ap0: AP-STA-DISCONNECTED 24:d7:eb:04:2e:8c
Fri Feb 21 18:36:30 2025 daemon.notice hostapd: phy0-ap0: AP-STA-DISCONNECTED d8:f1:5b:92:29:b8
Fri Feb 21 18:36:30 2025 daemon.notice hostapd: phy0-ap0: AP-STA-DISCONNECTED dc:4f:22:be:c2:c8
Fri Feb 21 18:36:30 2025 daemon.notice hostapd: phy0-ap0: AP-STA-DISCONNECTED 00:80:92:a6:a5:69
Fri Feb 21 18:36:30 2025 daemon.notice hostapd: phy0-ap0: AP-DISABLED
Fri Feb 21 18:36:30 2025 daemon.notice hostapd: phy0-ap0: CTRL-EVENT-TERMINATING
Fri Feb 21 18:36:30 2025 daemon.err hostapd: rmdir[ctrl_interface=/var/run/hostapd]: Permission denied
Fri Feb 21 18:36:30 2025 daemon.notice hostapd: nl80211: deinit ifname=phy0-ap0 disabled_11b_rates=0
Fri Feb 21 18:36:30 2025 kern.info kernel: [ 5295.537934] mt798x-wmac 18000000.wifi phy0-ap0: left allmulticast mode
Fri Feb 21 18:36:30 2025 kern.info kernel: [ 5295.544496] mt798x-wmac 18000000.wifi phy0-ap0: left promiscuous mode
Fri Feb 21 18:36:30 2025 kern.info kernel: [ 5295.551026] br-lan: port 6(phy0-ap0) entered disabled state
Fri Feb 21 18:36:31 2025 daemon.notice netifd: Network device 'phy0-ap0' link is down
Fri Feb 21 18:36:31 2025 daemon.notice wpa_supplicant[5857]: Set new config for phy phy1
Fri Feb 21 18:36:31 2025 daemon.notice wpa_supplicant[5857]: Set new config for phy phy0
Fri Feb 21 18:36:31 2025 daemon.notice netifd: Wireless device 'radio1' is now down
Fri Feb 21 18:36:31 2025 daemon.notice netifd: Wireless device 'radio0' is now down
```

And logs produced by using `wifi up` command:

``` lang-plaintext
Fri Feb 21 18:38:08 2025 daemon.notice netifd: radio1 (6888): WARNING: Variable 'data' does not exist or is not an array/object
Fri Feb 21 18:38:08 2025 daemon.notice netifd: radio0 (6887): WARNING: Variable 'data' does not exist or is not an array/object
Fri Feb 21 18:38:08 2025 daemon.notice hostapd: Set new config for phy phy1:
Fri Feb 21 18:38:08 2025 daemon.notice hostapd: Set new config for phy phy0:
Fri Feb 21 18:38:08 2025 daemon.notice wpa_supplicant[5857]: Set new config for phy phy1
Fri Feb 21 18:38:08 2025 daemon.notice wpa_supplicant[5857]: Set new config for phy phy0
Fri Feb 21 18:38:08 2025 daemon.notice wpa_supplicant[5857]: Set new config for phy phy1
Fri Feb 21 18:38:09 2025 daemon.notice hostapd: Set new config for phy phy1: /var/run/hostapd-phy1.conf
Fri Feb 21 18:38:09 2025 daemon.notice hostapd: Restart interface for phy phy1
Fri Feb 21 18:38:09 2025 daemon.notice hostapd: Configuration file: data: driver=nl80211 logger_syslog=127 logger_syslog_level=2 logger_stdout=127 logger_stdout_level=2 country_code=MY ieee80211d=1 ieee80211h=1 hw_mode=a beacon_int=100 stationary_ap=1 chanlist=100 tx_queue_data2_burst=2.0 #num_global_macaddr=1 #macaddr_base= ieee80211n=1 ht_coex=0 ht_capab=[HT40+][LDPC][SHORT-GI-20][SHORT-GI-40][TX-STBC][RX-STBC1][MAX-AMSDU-7935] ieee80211ac=1 vht_oper_chwidth=2 vht_oper_centr_freq_seg0_idx=114 vht_capab=[RXLDPC][SHORT-GI-80][SHORT-GI-160][TX-STBC-2BY1][SU-BEAMFORMER][SU-BEAMFORMEE][MU-BEAMFORMER][MU-BEAMFORMEE][RX-ANTENNA-PATTERN][TX-ANTENNA-PATTERN][RX-STBC-1][SOUNDING-DIMENSION-3][BF-ANTENNA-3][VHT160][MAX-MPDU-11454][MAX-A-MPDU-LEN-EXP7] ieee80211ax=1 he_oper_chwidth=2 he_oper_centr_freq_seg0_idx=114 he_su_beamformer=1 he_su_beamformee=1 he_mu_beamformer=1 he_bss_color=128 he_spr_sr_control=3 he_default_pe_duration=4 he_rts_threshold=1023 he_mu_edca_qos_info_param_count=0 he_mu_edca_qos_info_q_ack=0 he_mu_edca_qos_info_que
Fri Feb 21 18:38:09 2025 daemon.notice wpa_supplicant[5857]: Set new config for phy phy0
Fri Feb 21 18:38:09 2025 kern.info kernel: [ 5394.075769] br-lan: port 5(phy1-ap0) entered blocking state
Fri Feb 21 18:38:09 2025 kern.info kernel: [ 5394.081424] br-lan: port 5(phy1-ap0) entered disabled state
Fri Feb 21 18:38:09 2025 kern.info kernel: [ 5394.087035] mt798x-wmac 18000000.wifi phy1-ap0: entered allmulticast mode
Fri Feb 21 18:38:09 2025 kern.info kernel: [ 5394.094075] mt798x-wmac 18000000.wifi phy1-ap0: entered promiscuous mode
Fri Feb 21 18:38:09 2025 kern.info kernel: [ 5394.104975] br-lan: port 5(phy1-ap0) entered blocking state
Fri Feb 21 18:38:09 2025 kern.info kernel: [ 5394.110559] br-lan: port 5(phy1-ap0) entered forwarding state
Fri Feb 21 18:38:09 2025 kern.info kernel: [ 5394.116528] br-lan: port 5(phy1-ap0) entered disabled state
Fri Feb 21 18:38:09 2025 daemon.notice hostapd: phy1-ap0: interface state UNINITIALIZED->COUNTRY_UPDATE
Fri Feb 21 18:38:09 2025 daemon.notice hostapd: phy1-ap0: interface state COUNTRY_UPDATE->HT_SCAN
Fri Feb 21 18:38:09 2025 daemon.notice hostapd: Set new config for phy phy0: /var/run/hostapd-phy0.conf
Fri Feb 21 18:38:09 2025 daemon.notice hostapd: Restart interface for phy phy0
Fri Feb 21 18:38:09 2025 daemon.notice hostapd: Configuration file: data: driver=nl80211 logger_syslog=127 logger_syslog_level=2 logger_stdout=127 logger_stdout_level=2 country_code=MY ieee80211d=1 hw_mode=g supported_rates=60 90 120 180 240 360 480 540 basic_rates=60 120 240 beacon_int=100 stationary_ap=1 chanlist=11 noscan=1 #num_global_macaddr=1 #macaddr_base= ieee80211n=1 ht_coex=0 ht_capab=[HT40-][LDPC][SHORT-GI-20][SHORT-GI-40][TX-STBC][RX-STBC1][MAX-AMSDU-7935] ieee80211ax=1 he_su_beamformer=1 he_su_beamformee=1 he_mu_beamformer=1 he_bss_color=128 he_spr_sr_control=3 he_default_pe_duration=4 he_rts_threshold=1023 he_mu_edca_qos_info_param_count=0 he_mu_edca_qos_info_q_ack=0 he_mu_edca_qos_info_queue_request=0 he_mu_edca_qos_info_txop_request=0 he_mu_edca_ac_be_aifsn=8 he_mu_edca_ac_be_aci=0 he_mu_edca_ac_be_ecwmin=9 he_mu_edca_ac_be_ecwmax=10 he_mu_edca_ac_be_timer=255 he_mu_edca_ac_bk_aifsn=15 he_mu_edca_ac_bk_aci=1 he_mu_edca_ac_bk_ecwmin=9 he_mu_edca_ac_bk_ecwmax=10 he_mu_edca_ac_bk_timer=255 he_mu_edca_ac_vi_ecwmin=
Fri Feb 21 18:38:09 2025 kern.info kernel: [ 5394.406834] br-lan: port 6(phy0-ap0) entered blocking state
Fri Feb 21 18:38:09 2025 kern.info kernel: [ 5394.412460] br-lan: port 6(phy0-ap0) entered disabled state
Fri Feb 21 18:38:09 2025 kern.info kernel: [ 5394.418056] mt798x-wmac 18000000.wifi phy0-ap0: entered allmulticast mode
Fri Feb 21 18:38:09 2025 kern.info kernel: [ 5394.425081] mt798x-wmac 18000000.wifi phy0-ap0: entered promiscuous mode
Fri Feb 21 18:38:09 2025 kern.info kernel: [ 5394.442867] br-lan: port 6(phy0-ap0) entered blocking state
Fri Feb 21 18:38:09 2025 kern.info kernel: [ 5394.448447] br-lan: port 6(phy0-ap0) entered forwarding state
Fri Feb 21 18:38:09 2025 daemon.notice hostapd: phy0-ap0: interface state UNINITIALIZED->COUNTRY_UPDATE
Fri Feb 21 18:38:09 2025 daemon.notice netifd: Wireless device 'radio1' is now up
Fri Feb 21 18:38:09 2025 daemon.notice netifd: Network device 'phy0-ap0' link is up
Fri Feb 21 18:38:10 2025 daemon.notice hostapd: phy0-ap0: interface state COUNTRY_UPDATE->ENABLED
Fri Feb 21 18:38:10 2025 daemon.notice hostapd: phy0-ap0: AP-ENABLED
Fri Feb 21 18:38:10 2025 daemon.notice hostapd: phy1-ap0: interface state HT_SCAN->DFS
Fri Feb 21 18:38:10 2025 daemon.notice hostapd: phy1-ap0: DFS-CAC-START freq=5500 chan=100 sec_chan=1, width=2, seg0=114, seg1=0, cac_time=60s
Fri Feb 21 18:38:10 2025 daemon.notice netifd: Wireless device 'radio0' is now up
[clients getting connected]
Fri Feb 21 18:39:14 2025 daemon.notice hostapd: phy1-ap0: DFS-CAC-COMPLETED success=1 freq=5500 ht_enabled=0 chan_offset=0 chan_width=5 cf1=5570 cf2=0 radar_detected=0
Fri Feb 21 18:39:14 2025 daemon.notice netifd: Network device 'phy1-ap0' link is up
Fri Feb 21 18:39:15 2025 kern.info kernel: [ 5459.859135] br-lan: port 5(phy1-ap0) entered blocking state
Fri Feb 21 18:39:15 2025 kern.info kernel: [ 5459.864727] br-lan: port 5(phy1-ap0) entered forwarding state
Fri Feb 21 18:39:15 2025 daemon.notice hostapd: phy1-ap0: interface state DFS->ENABLED
Fri Feb 21 18:39:15 2025 daemon.notice hostapd: phy1-ap0: AP-ENABLED
```

Only error that I can see is
`daemon.err hostapd: rmdir[ctrl_interface=/var/run/hostapd]: Permission denied`
![:thinking:](https://forum.openwrt.org/images/emoji/twitter/thinking.png?v=12 ":thinking:")

What's interesting - I can't see `phy0-ap1` network device in `wifi up`
logs.

[monotux](https://forum.openwrt.org/u/monotux) February 24, 2025, 8:46am

187

I got around to update my configuration and use `wifi-vlan` &
`wifi-station`. Below is my `/etc/config/wireless` in full.

``` lang-plaintext
config wifi-device 'radio0'
    option type 'mac80211'
    option path 'soc/11280000.pcie/pci0000:00/0000:00:00.0/0000:01:00.0'
    option band '2g'
    option channel '1'
    option country 'SE'
    option htmode 'HE20'
    option cell_density '0'

config wifi-device 'radio1'
    option type 'mac80211'
    option path 'oc/11280000.pcie/pci0000:00/0000:00:00.0/0000:01:00.0+1'
    option htmode 'HE80'
    option country 'SE'
    option cell_density '0'

config wifi-device 'radio2'
    option type 'mac80211'
    option path 'platform/soc/18000000.wifi'
    option channel '64'
    option band '5g'
    option htmode 'HE80'
    option cell_density '0'
    option country 'SE'

config wifi-iface 'default_radio0'
    option device 'radio0'
    option mode 'ap'
    option ssid 'OpenWrt'
    option encryption 'psk2'
    option dynamic_vlan '1'
    option vlan_tagged_interface 'internet'
    option vlan_bridge 'br-'

config wifi-iface 'default_radio1'
    option device 'radio1'
    option mode 'ap'
    option ssid 'OpenWrt'
    option encryption 'psk2'
    option dynamic_vlan '1'
    option vlan_tagged_interface 'internet'
    option vlan_bridge 'br-'

config wifi-iface 'default_radio2'
    option device 'radio2'
    option mode 'ap'
    option ssid 'OpenWrt'
    option encryption 'psk2'
    option dynamic_vlan '1'
    option vlan_tagged_interface 'internet'
    option vlan_bridge 'br-'

config wifi-vlan
    option vid '100'
    option network 'wifi'
    option name 'wifi'

config wifi-vlan
    option vid '200'
    option network 'iot'
    option name 'iot'

config wifi-station
    option vid '100'
    option key 'foobar'

config wifi-station
    option vid '200'
    option key 'bazfoo'
```

The only thing I was unsure of was `name` and `network` for `wifi-vlan`.
In this case I just used the interface names from my
`/etc/config/network`.

``` lang-plaintext
# from my network configuration
config interface 'wifi'
        option proto 'none'
        option device 'br-100'
        option defaultroute '0'

config interface 'iot'
        option proto 'none'
        option device 'br-200'
        option defaultroute '0'
```

1 Like

[kawog45017](https://forum.openwrt.org/u/kawog45017) February 25, 2025,
11:19am

188

I'm in the process of implementing this on my home routers (running
OpenWrt 24.10.0 (r28427-6df0e3d02a)) and would like to be able to add
keys without `reload_config` or `wifi reload`, but sadly the command
mentioned for this above `hostapd_cli reload_wpa_psk` only prints this
error:

> Selected interface 'global'  
> FAIL

Can somebody help me with that?

[raenye](https://forum.openwrt.org/u/raenye) February 25, 2025, 12:35pm

189

![](https://forum.openwrt.org/letter_avatar/monotux/48/5_047764fa20c941da5a8a2a620d7322fb.png)
monotux:

> The only thing I was unsure of was `name` and `network` for
> `wifi-vlan`. In this case I just used the interface names from my
> `/etc/config/network`.

Neat!  
Do you still need to create bridges (`br-X`) manually in
`/etc/config/network`? I guess so, because you have to add `internet.X`
to them...

[takimata](https://forum.openwrt.org/u/takimata) February 25, 2025,
12:45pm

190

![](https://forum.openwrt.org/letter_avatar/kawog45017/48/5_047764fa20c941da5a8a2a620d7322fb.png)
kawog45017:

> `hostapd_cli reload_wpa_psk` only prints this error

Let's see your `/etc/config/wireless` (with sensitive information
redacted of course)

[kawog45017](https://forum.openwrt.org/u/kawog45017) February 25, 2025,
1:09pm

191

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/takimata/48/114647_2.png)
takimata:

> Let's see your `/etc/config/wireless`

``` lang-plaintext
config wifi-device 'radio0'
        option type 'mac80211'
        option hwmode '11a'
        option path 'pci0000:00/0000:00:00.0'
        option htmode 'VHT80'
        option channel 'auto'
        option country 'AT'
        option cell_density '0'

config wifi-iface 'default_radio0'
        option device 'radio0'
        option mode 'ap'
        option ssid 'ssid5'
        option encryption 'psk2'
        option network 'lan'
        option ieee80211r '1'
        option key '***********'
        option ft_over_ds '1'
        option wpa_disable_eapol_key_retries '1'
        option ft_psk_generate_local '1'

config wifi-device 'radio1'
        option type 'mac80211'
        option hwmode '11g'
        option channel 'auto'
        option htmode 'HT40'
        option country 'US'
        option path 'platform/ahb/18100000.wmac'
        option cell_density '0'

config wifi-iface 'default_radio1'
        option device 'radio1'
        option mode 'ap'
        option ssid 'ssid2'
        option encryption 'psk2'
        option ieee80211r '1'
        option pmk_r1_push '1'
        option key '***************'
        option ft_over_ds '1'
        option wpa_disable_eapol_key_retries '1'
        option ft_psk_generate_local '1'
        option network 'lan'

config wifi-iface 'wifinet3'
        option ssid 'ssid3'
        option encryption 'psk2'
        option device 'radio0'
        option ieee80211r '1'
        option ft_over_ds '1'
        option key '***************'
        option network 'lan'
        option mode 'ap'
        option wpa_disable_eapol_key_retries '1'
        option ft_psk_generate_local '1'
        option mobility_domain 'fa33'
        option ieee80211w '2'

config wifi-iface 'wifinet4'
        option ssid 'ssid3'
        option encryption 'psk2'
        option device 'radio1'
        option ieee80211r '1'
        option ft_over_ds '1'
        option key '****************'
        option mode 'ap'
        option wpa_disable_eapol_key_retries '1'
        option ft_psk_generate_local '1'
        option mobility_domain 'fa33'
        option ieee80211w '2'
        option network 'lan'

config wifi-iface 'wifinet5'
        option ssid 'guest_ssid'
        option encryption 'psk2'
        option device 'radio0'
        option ieee80211r '1'
        option ft_over_ds '1'
        option key '*************'
        option mode 'ap'
        option wpa_disable_eapol_key_retries '1'
        option ft_psk_generate_local '1'
        option network 'lan'

config wifi-iface 'wifinet6'
        option ssid 'guest_ssid'
        option encryption 'psk2'
        option device 'radio1'
        option ieee80211r '1'
        option ft_over_ds '1'
        option key '**********'
        option mode 'ap'
        option wpa_disable_eapol_key_retries '1'
        option ft_psk_generate_local '1'
        option mobility_domain 'a25a'
        option network 'lan'

config wifi-iface 'new_network_for_this0'
        option device 'radio0'
        option mode 'ap'
        option ssid 'new_network_for_this'
        option encryption 'psk2'
        option network 'lan'
        option wpa_psk_file '/etc/hostapd.wpa_psk'
        option vlan_file '/etc/hostapd.vlan'
        option vlan_tagged_interface 'eth0'
        option vlan_bridge 'br-vlan'
        option dynamic_vlan '1'

config wifi-iface 'new_network_for_this1'
        option device 'radio1'
        option mode 'ap'
        option ssid 'new_network_for_this'
        option encryption 'psk2'
        option network 'lan'
        option wpa_psk_file '/etc/hostapd.wpa_psk'
        option vlan_file '/etc/hostapd.vlan'
        option vlan_tagged_interface 'eth0'
        option vlan_bridge 'br-vlan'
        option dynamic_vlan '1'

config wifi-iface 'new_network_for_this5'
        option device 'radio0'
        option mode 'ap'
        option ssid 'new_network_for_this5'
        option encryption 'psk2'
        option network 'lan LAN6'
        option wpa_psk_file '/etc/hostapd.wpa_psk'
        option vlan_file '/etc/hostapd.vlan'
        option vlan_tagged_interface 'eth0'
        option vlan_bridge 'br-vlan'
        option dynamic_vlan '1'

config wifi-iface 'new_network_for_this2'
        option device 'radio1'
        option mode 'ap'
        option ssid 'new_network_for_this2'
        option encryption 'psk2'
        option network 'lan LAN6'
        option wpa_psk_file '/etc/hostapd.wpa_psk'
        option vlan_file '/etc/hostapd.vlan'
        option vlan_tagged_interface 'eth0'
        option vlan_bridge 'br-vlan'
        option dynamic_vlan '1'
```

[raenye](https://forum.openwrt.org/u/raenye) February 25, 2025, 1:27pm

192

> `option network 'lan LAN6'`

Hmm, can network names have whitespace?

[kawog45017](https://forum.openwrt.org/u/kawog45017) February 25, 2025,
1:31pm

193

no, it's two networks. I don't know from which sample this came from,
but never got that ipv6 stuff to work.. but that's besides the point of
this thread, the network itself works, though I'm struggling with the
`/etc/hostapd.wpa_psk` part. One device works, all others say wrong
password, even though I rebooted the router in between...  
And the OpenWRT router prints many repeats of this line for all but that
one working client device...

> daemon.notice hostapd: phy0-ap4: AP-STA-POSSIBLE-PSK-MISMATCH

UPDATE: Now I found that none of the below actually reloads the
`/etc/hostapd.wpa_psk` file, only rebooting the router makes changes
take effect..:

```
reload_config
wifi reload
hostapd_cli interface phy1-ap3 reload_wpa_psk
hostapd_cli interface phy1-ap4 reload_wpa_psk
hostapd_cli interface phy0-ap3 reload_wpa_psk
hostapd_cli interface phy0-ap4 reload_wpa_psk
```

[takimata](https://forum.openwrt.org/u/takimata) February 25, 2025,
1:47pm

194

Phew, there's an anthill's worth of configuration going on there. I know
it sounds tedious, but my approach would be to tear it all down and
rebuild the wifi interfaces from scratch, one iface at a time, testing
and confirming operation of every step before the next.

[kawog45017](https://forum.openwrt.org/u/kawog45017) February 25, 2025,
2:14pm

195

my plan was to remove all the old networks once the new ones work as
intended and I've brought over all the clients to that new ssid with the
unique per client keys.

[takimata](https://forum.openwrt.org/u/takimata) February 25, 2025,
2:50pm

196

I realize that. I can't shake the feeling, though, that the old networks
are interfering with the new ones. I actually never tried mixing
"regular" wifi networks with wpa_psk_key ones, maybe that's the problem,
they don't work together? I really don't know.

1 Like

[kawog45017](https://forum.openwrt.org/u/kawog45017) February 25, 2025,
3:55pm

197

Well. it does work, at least the non-vlan stuff. It's only that changes
to `/etc/hostapd.wpa_psk` don't take effect until I reboot the router.
That's annoying, but at the moment for me this doesn't justify the
effort it would take to start my home router configs fresh from a blank
state...

[\_bernd](https://forum.openwrt.org/u/_bernd) February 25, 2025, 5:40pm

198

How do you restart or reload the wireless config? (Which did not worked
for you.)

[kawog45017](https://forum.openwrt.org/u/kawog45017) February 25, 2025,
7:02pm

199

I tried lots of things. I tried via ssh = cli:

``` lang-plaintext
reload_config
wifi reload
hostapd_cli interface phy1-ap3 reload_wpa_psk
hostapd_cli interface phy1-ap4 reload_wpa_psk
hostapd_cli interface phy0-ap3 reload_wpa_psk
hostapd_cli interface phy0-ap4 reload_wpa_psk
```

And via the LuCI website I tried:

- disabling and re-enabling the wifi network
- using the "restart" button for the wifi device hosting the network

but nothing but rebooting works.

[\_bernd](https://forum.openwrt.org/u/_bernd) February 25, 2025, 7:29pm

200

Afaik reload only reloads. You need `wifi` to restart wireless. Or just
restart networking and wifi gets restarted too...

1 Like

[kawog45017](https://forum.openwrt.org/u/kawog45017) March 2, 2025,
9:14am

201

I'm trying to replace my separate guest wifi with a
`vlanid=4 00:00:00:00:00:00 myguestpassword` entry in
`/etc/hostapd.wpa_psk`. My `/etc/hostapd.vlan` reads `4 wan.4`. Trying
to connect a client with that password fails with those lines in the
log:

``` lang-plaintext
Sun Mar  2 10:06:13 2025 daemon.info hostapd: phy1-ap1: STA xx:xx:xx:xx:xx:xx IEEE 802.11: authenticated
Sun Mar  2 10:06:13 2025 daemon.info hostapd: phy1-ap1: STA xx:xx:xx:xx:xx:xx IEEE 802.11: associated (aid 1)
Sun Mar  2 10:06:13 2025 daemon.notice hostapd: Assigned VLAN ID 4 from wpa_psk_file to xx:xx:xx:xx:xx:xx
Sun Mar  2 10:06:13 2025 daemon.err hostapd: nl80211: NL80211_ATTR_STA_VLAN (addr=xx:xx:xx:xx:xx:xx ifname=wan.4 vlan_id=4) failed: -22 (Invalid argument)
Sun Mar  2 10:06:18 2025 daemon.info hostapd: phy1-ap1: STA xx:xx:xx:xx:xx:xx IEEE 802.11: deauthenticated due to local deauth request
```

Can somebody help me? What do I need to do, to make this work?

UPDATE: just found that [@saxy](/u/saxy) had the same error message
[last
month](https://forum.openwrt.org/t/individual-per-passphrase-wifi-vlans-using-wpa-psk-file-no-radius-required/161696/171)
but says it works on 2ghz for them. I'm gonna force my client to 2GHz to
verify if it's the same for me. Did you solve your issue with 5GHz
[@saxy](/u/saxy)?

[saxy](https://forum.openwrt.org/u/saxy) March 3, 2025, 8:09am

202

hy,  
i needed to create 2 seperate vlan files, because they create virtual
interfaces.  
on a dual radio ap, it can't create two times the same interface.  
the problem with 5ghz is solved for me

I have a new problem, the management IP on VLAN 11 replys very short
during boot up. after 1 minute of operation it does not replay
anymore.  
192.168.11.14  
it has todo with individual psk/vlans and bridges. but I don't know how
to solve it. I have access over 192.168.1.1 untagged but this is no
solution only a fix.

``` lang-plaintext
root@OpenWrt:~# cat /etc/hostapd.vlan2
11 wlan0.11 br-lan.vlan11
22 wlan0.22 br-lan.vlan22
33 wlan0.33 br-lan.vlan33

root@OpenWrt:~# cat /etc/hostapd.vlan5
11 wlan1.11 br-lan.vlan11
22 wlan1.22 br-lan.vlan22
33 wlan1.33 br-lan.vlan33
root@OpenWrt:~# cat /etc/hostapd.wpa_psk
vlanid=11 00:00:00:00:00:00 secret11
vlanid=22 00:00:00:00:00:00 secret22
vlanid=33 00:00:00:00:00:00 secret33
root@OpenWrt:~# cat /etc/config/network

config interface 'loopback'
        option device 'lo'
        option proto 'static'
        option ipaddr '127.0.0.1'
        option netmask '255.0.0.0'

config globals 'globals'
        option ula_prefix 'fd70:1574:e9fe::/48'
        option packet_steering '1'

config device
        option name 'br-lan'
        option type 'bridge'
        list ports 'eth0'

config interface 'lan'
        option device 'br-lan.1'
        option proto 'static'
        option ipaddr '192.168.1.1'
        option netmask '255.255.255.0'
        option ip6assign '60'

config interface 'mgmt'
        option proto 'static'
        option device 'eth1'
        option ipaddr '192.168.95.1'
        option netmask '255.255.255.0'

config bridge-vlan
        option device 'br-lan'
        option vlan '1'
        list ports 'eth0'

config bridge-vlan
        option device 'br-lan'
        option vlan '11'
        list ports 'eth0:t'

config bridge-vlan
        option device 'br-lan'
        option vlan '22'
        list ports 'eth0:t'

config bridge-vlan
        option device 'br-lan'
        option vlan '33'
        list ports 'eth0:t'

config interface 'vlan11'
        option proto 'static'
        option device 'br-lan.11'
        option ipaddr '192.168.11.14'
        option netmask '255.255.255.0'
        option gateway '192.168.11.1'
        list dns '192.168.11.1'

config interface 'vlan22'
        option proto 'none'
        option device 'br-lan.22'

config interface 'vlan33'
        option proto 'none'
        option device 'br-lan.33'

root@OpenWrt:~# cat /etc/config/wireless

config wifi-device 'radio0'
        option type 'mac80211'
        option path 'soc/1b700000.pci/pci0001:00/0001:00:00.0/0001:01:00.0'
        option band '2g'
        option channel '1'
        option htmode 'VHT20'
        option disabled '0'

config wifi-device 'radio1'
        option type 'mac80211'
        option path 'soc/1b500000.pci/pci0000:00/0000:00:00.0/0000:01:00.0'
        option band '5g'
        option channel '36'
        option htmode 'VHT80'
        option disabled '0'

config wifi-iface 'default_radio0'
        option device 'radio0'
        option network 'lan'
        option mode 'ap'
        option ssid '127-O-O-5-X'
        option encryption 'psk2'
        option key 'secret11'
        option wpa_psk_file '/etc/hostapd.wpa_psk'
        option vlan_file '/etc/hostapd.vlan'
        option vlan_tagged_interface 'eth0'
        option vlan_bridge 'br-vlan'
        option dynamic_vlan '1'

config wifi-iface 'default_radio1'
        option device 'radio1'
        option network 'lan'
        option mode 'ap'
        option ssid '127-O-O-2-X'
        option encryption 'psk2'
        option key 'secret11'
        option wpa_psk_file '/etc/hostapd.wpa_psk'
        option vlan_file '/etc/hostapd.vlan'
        option vlan_tagged_interface 'eth0'
        option vlan_bridge 'br-vlan'
        option dynamic_vlan '1'
root@OpenWrt:~#```
```

1 Like

[kawog45017](https://forum.openwrt.org/u/kawog45017) March 4, 2025,
7:34am

203

Thank you for your reply! I don't know the solution to your problem, but
maybe you could try the `option bridge_empty '1'` in your
`config device` sections of `/etc/config/network` in the
`option type 'bridge'` blocks.

I've made it work for my by removing all the vlan config from the
individual ssid configs in `/etc/config/wireless` (only using the
`option wpa_psk_file` option) & not using `/etc/hostapd.vlan` files, but
instead using a separate vlan block (in `/etc/config/wireless`):

``` lang-plaintext
config wifi-vlan
        option vid '4'
        option network 'guest4'
        option name 'guest4'
```

while also having this in `/etc/config/network`:

``` lang-plaintext
config device
        option type '8021q'
        option ifname 'eth0'
        option vid '4'
        option name 'eth0.4'

config device
        option type 'bridge'
        option name 'br-4'
        option bridge_empty '1'
        list ports 'eth0.4'

config interface 'guest4'
        option proto 'dhcp'
        option device 'br-4'
        option defaultroute '0'
```

[Sylvan](https://forum.openwrt.org/u/Sylvan) March 4, 2025, 12:54pm

204

Has anyone ever managed to get the password VLAN mapping to work on a
Xiaomi Redmi AX3600?

It works fine on a Xiaomi Redmi AC2100. I simply used the variant via
UCI with the “*wifi-vlan*” and “*wifi-station*” and it works straight
away.

When I did the similar configuration on an AX3600, no client could
connect and I found the following entries in logread:

```
Tue Mar 4 13:20:14 2025 daemon.err hostapd: Failed to create interface vlan3: -95 (Not supported)
Tue Mar 4 13:20:14 2025 daemon.err hostapd: VLAN: Could not add VLAN vlan3: No such device
Tue Mar 4 13:20:14 2025 daemon.err hostapd: VLAN initialization failed.
Tue Mar 4 13:20:15 2025 daemon.err hostapd: Interface initialization failed
```

So I thought to myself, obviously it doesn't seem to be able to create
interfaces itself. So I tried the manual method via hostapd.vlan and
hostapd.wpa_psk. But this also failed with similar entries in logread.

I have brought my configuration files with me. Maybe someone will see
the mistake I made.

Specifically, I would like to achieve the following: If the client logs
on with "*myLANPassword*", its packets should be forwarded as untagged
(i.e. implicitly VLAN 1). The client should therefore end up in the
"*lan*" network.  
However, if a client logs on with "*myguestpassword*", it should end up
in the "*guest*" network and its packets should therefore be tagged with
VLAN ID 3. The access point is a dumb AP. This means no DNS, DHCP,
firewall etc.

/etc/config/network

```
config interface 'loopback'
    option device 'lo'
    option proto 'static'
    option ipaddr '127.0.0.1'
    option netmask '255.0.0.0'

config device
    option name 'br-lan'
    option type 'bridge'
    option vlan_filtering '1'
    list ports 'lan1'
    list ports 'lan2'
    list ports 'lan3'
    list ports 'wan'

config bridge-vlan
    option device 'br-lan'
    option vlan '1'
    list ports 'lan1:u*'
    list ports 'lan2:u*'
    list ports 'lan3:u*'
    list ports 'wan:u*'

config bridge-vlan
    option device 'br-lan'
    option vlan '3'
    list ports 'lan1:t'
    list ports 'lan2:t'
    list ports 'lan3:t'
    list ports 'wan:t'

config interface 'lan'
    option broadcast '192.168.0.255'
    option device 'br-lan.1'
    option gateway '192.168.0.2'
    option ip6assign '60'
    option ip6ifaceid '::B'
    option ipaddr '192.168.0.11'
    option metric '5'
    option netmask '255.255.255.0'
    option proto 'static'
    list dns '192.168.0.2'
    list dns_search 'lan'

config interface 'lan6'
    option delegate '0'
    option proto 'dhcpv6'
    option ifname '@lan'
    option reqprefix 'no'
    option reqaddress 'try'
    option metric '5'

config interface 'guest'
    list dns '192.168.3.2'
    option delegate '0'
    option device 'br-lan.3'
    option force_link '0'
    option gateway '192.168.3.2'
    option ip6assign '64'
    option ipaddr '192.168.3.11'
    option netmask '255.255.255.0'
    option proto 'static'
    option metric '20'
```

/etc/config/wireless

```
# Another problem: additional Wifi interfaces, which come from the standard configuration. These are not used by me.
config wifi-device 'radio0'
    option type 'mac80211'
    option path 'soc@0/20000000.pci/pci0000:00/0000:00:00.0/0000:01:00.0'
    option band '5g'
    option channel '36'
    option htmode 'VHT80'
    option disabled '1'

# Another problem: additional Wifi interfaces, which come from the standard configuration. These are not used by me.
config wifi-device 'radio1'
    option type 'mac80211'
    option path 'platform/soc@0/c000000.wifi'
    option band '5g'
    option channel '36'
    option htmode 'HE80'
    option disabled '1'

# IoT device: i don`t use it
config wifi-device 'dev_24_iot'
    option type 'mac80211'
    option path 'platform/soc@0/c000000.wifi+1'
    option band '2g'
    option channel '1'
    option htmode 'HE20'
    option disabled '1'

# Another problem: additional Wifi interfaces, which come from the standard configuration. These are not used by me.
config wifi-device 'radio3'
    option type 'mac80211'
    option path 'soc@0/20000000.pci/pci0000:00/0000:00:00.0/0000:01:00.0'
    option channel '36'
    option band '5g'
    option htmode 'VHT80'
    option disabled '1'

# my wifi 5 Ghz device
config wifi-device 'dev_wifi_5Ghz'
    option type 'mac80211'
    option path 'platform/soc@0/c000000.wifi'
    option channel '60'
    option band '5g'
    option htmode 'HE80'
    option cell_density '2'
    option country 'DE'

# my wifi 2.4 Ghz device
config wifi-device 'dev_wifi_2_4Ghz'
    option type 'mac80211'
    option path 'platform/soc@0/c000000.wifi+1'
    option channel '1'
    option band '2g'
    option htmode 'HE20'
    option cell_density '2'
    option country 'DE'

config wifi-iface 'If_Wifi_5'
    option bss_transition '1'
    option device 'dev_wifi_5Ghz'
    option encryption 'psk2+ccmp'
    option key 'myLANPassword'
    option mode 'ap'
    option network 'lan'
    option ssid 'MyLAN'
    option wpa_psk_file '/etc/hostapd.wpa_psk'
    option vlan_file '/etc/hostapd.vlan'
    option dynamic_vlan '1'
    option vlan_tagged_interface 'lan3' # my physical uplink port (br-lan also don`t work here)
    option vlan_bridge 'br-vlan' # don`t know what to insert here, from my unterstanding it`s an prefix for created interfaces

config wifi-iface 'If_Wifi_24'
    option bss_transition '1'
    option device 'dev_wifi_2_4Ghz'
    option encryption 'psk2+ccmp'
    option key 'myLANPassword'
    option mode 'ap'
    option network 'lan'
    option ssid 'MyLAN'
    option wpa_psk_file '/etc/hostapd.wpa_psk'
    option vlan_file '/etc/hostapd.vlan'
    option dynamic_vlan '1'
    option vlan_tagged_interface 'lan3' # my physical uplink port (br-lan also don`t work here)
    option vlan_bridge 'br-vlan' # don`t know what to insert here, from my unterstanding it`s an prefix for created interfaces
```

/etc/hostapd.vlan

``` lang-plaintext
3 vlan3 br-lan.3
```

/etc/hostapd.wpa_psk

``` lang-plaintext
vlanid=3 00:00:00:00:00:00 myguestpassword
```

Both - the AX3600 and the AC2100 runs on v24.10.0 and use wpad-mbedtls.

**Edit:** I have now discovered that there is a patch for ath11k which
has not yet been included in OpenWRT (see
[\>\>here\<\<](https://forum.openwrt.org/t/individual-per-passphrase-wifi-vlans-using-wpa-psk-file-no-radius-required/161696/136)).  
This means you have to build your own image.  
I did that and integrated the patch.  
Now it works wonderfully even with the simple version with "*wifi-vlan*"
and "*wifi-station*".

So that others can understand this and since I am still untrained in
this, I will briefly explain the command sequence with which I created
the image:

```
# before: set up an OpenWRT build environment
git clone https://git.openwrt.org/openwrt/openwrt.git
cd openwrt
git branch -a
git tag
# use the current release
git checkout v24.10.0
# add the patch to the specific folder
cd package/kernel/mac80211/patches/ath11k
curl -O https://raw.githubusercontent.com/gtxaspec/openwrt-mx4300/b0fb8508f099a1339e87f8ccc1b5fdd59b0347fb/package/kernel/mac80211/patches/ath11k/906-ath11k-add-support-for-dynamic-vlan.patch
cd -
./scripts/feeds update -a
./scripts/feeds install -a
# download the standard configuration for that target
curl -o .config https://downloads.openwrt.org/releases/24.10.0/targets/qualcommax/ipq807x/config.buildinfo
# configure as you wish. Select packages in particular - here in particular switch to wpad-mbedtls (instead of basic)
make menuconfig
# build
make -j4
```

If someone gives me a hint where I could upload the file best, then I
could also share the built image.

[mattimat](https://forum.openwrt.org/u/mattimat) March 10, 2025, 10:44pm

205

Let's be calm, maybe somreone like marco takes the bite

[monotux](https://forum.openwrt.org/u/monotux) March 21, 2025, 9:46am

206

Yes, but that's mainly because I want to assign ports to one of the
bridges manually. I guess I could try something else but if I mess
something up the UART is a hassle to reach...

[Rafal9184](https://forum.openwrt.org/u/Rafal9184) March 25, 2025,
11:28pm

207

Does anyone have a working configuration of WLAN Roaming setup related
to our use case?  
And can share which options should be enabled to allow fast
transitioning?

In previous posts, I found that "Generate PMK Locally" (or
"ft_psk_generate local" in CLI) should be disabled, but there is no more
information beyond that. Is it still correct?

Thanks in advance.

[trevormcooper](https://forum.openwrt.org/u/trevormcooper) March 28,
2025, 8:54pm

208

does anyone have a simple setup with just the main lan and guest lan
setup that would be willing to share config files,

im running a MT6000 which has a 2.4 and 5ghz radio, i want to have one
wifi ssid named waypoint with a primary lan and a guest lan, guest lan
would not have access to primary lan, i think im starting to understand
the config but im not sure how to setup the guest portion to include the
wifi psk portion.

[remlei](https://forum.openwrt.org/u/remlei) March 30, 2025, 1:37am

209

unfortunately, PPSK/DPSK/WPA_PSK_FILE in nature dont work well with fast
roaming (802.11r/k/v). This issue is not only found on openwrt but on
other system as well like OMADA, Unifi and Ruckus with all 3 of them
just outright deauth you when you roam hop to one AP to another and
force a 4-way handshake.

With that, disabling the Generate PMK locally basically do the same
thing as other systems dealing with wifi roaming and PPSK/DPSK.

1 Like

[ayanna](https://forum.openwrt.org/u/ayanna) April 1, 2025, 6:06am

210

Hi, on two APs with psk2 keys, I left "ft_psk_generate local" enabled
and just directly attached the wifi interface to my main lan network,
while the other (like work, iot, guest) are APVLANs. By doing this it
seems FT works correctly even if only on one network. On the other
(APVLAN netoworks) the stations try FT but get attached firstly to the
main phy0-ap0 device, after that they immediately do a complete 4 way
handshake again to be "routed" to the correct VLAN. One could go on with
this setup if can accept broken FT on the other VLANs or ... wait for a
better solution.

[bahtsiz_bedevi](https://forum.openwrt.org/u/bahtsiz_bedevi) April 9,
2025, 11:00pm

211

There's no way to limit concurrent use of a psk key, right?

[takimata](https://forum.openwrt.org/u/takimata) April 9, 2025, 11:05pm

212

![](https://forum.openwrt.org/user_avatar/forum.openwrt.org/bahtsiz_bedevi/48/2862_2.png)
bahtsiz_bedevi:

> There's no way to limit concurrent use of a psk key, right?

Right. This is not a captive portal management mechanism. It is an
elaboration on the regular PSK authentication mechanism, which also does
not limit concurrent use(r)s of a single key.

[Best way to prevent people using my wifi with shared
password?](https://forum.openwrt.org/t/best-way-to-prevent-people-using-my-wifi-with-shared-password/229673/3)

-  [Home](/)
-  [Categories](/categories)
-  [Guidelines](/guidelines)
-  [Terms of Service](/tos)
-  [Privacy Policy](/privacy)

Powered by [Discourse](https://www.discourse.org), best viewed with
JavaScript enabled
