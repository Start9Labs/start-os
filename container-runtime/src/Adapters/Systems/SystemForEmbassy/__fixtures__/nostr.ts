export default {
  "tor-address": {
    name: "Tor Address",
    description: "The Tor address of the network interface",
    type: "pointer",
    subtype: "package",
    "package-id": "nostr-wallet-connect",
    target: "tor-address",
    interface: "main",
  },
  "lan-address": {
    name: "LAN Address",
    description: "The LAN address of the network interface",
    type: "pointer",
    subtype: "package",
    "package-id": "nostr-wallet-connect",
    target: "lan-address",
    interface: "main",
  },
  "nostr-relay": {
    type: "string",
    name: "Nostr Relay",
    default: "wss://relay.getalby.com/v1",
    description: "The Nostr Relay to use for Nostr Wallet Connect connections",
    copyable: true,
    nullable: false,
  },
}
