# Design Decision Log

* 1/4/20 - Switching from HTTPS to HTTP over local LAN. Due to eventual Tor support/default, this gives
us the neatest slot for the Tor support
	* This means it is possible to snoop on traffic between the companion app and the server if you
	have a LAN presence.
	* This also makes it possible to masquerade as the server if you have a LAN presence