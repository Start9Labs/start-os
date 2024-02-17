import { oldSpecToBuilder } from "../../scripts/oldSpecToBuilder"

oldSpecToBuilder(
  // Make the location
  "./lib/test/output.ts",
  // Put the config here
  {
    mediasources: {
      type: "list",
      subtype: "enum",
      name: "Media Sources",
      description: "List of Media Sources to use with Jellyfin",
      range: "[1,*)",
      default: ["nextcloud"],
      spec: {
        values: ["nextcloud", "filebrowser"],
        "value-names": {
          nextcloud: "NextCloud",
          filebrowser: "File Browser",
        },
      },
    },
    testListUnion: {
      type: "list",
      subtype: "union",
      name: "Lightning Nodes",
      description: "List of Lightning Network node instances to manage",
      range: "[1,*)",
      default: ["lnd"],
      spec: {
        type: "string",
        "display-as": "{{name}}",
        "unique-by": "name",
        name: "Node Implementation",
        tag: {
          id: "type",
          name: "Type",
          description:
            "- LND: Lightning Network Daemon from Lightning Labs\n- CLN: Core Lightning from Blockstream\n",
          "variant-names": {
            lnd: "Lightning Network Daemon (LND)",
            "c-lightning": "Core Lightning (CLN)",
          },
        },
        default: "lnd",
        variants: {
          lnd: {
            name: {
              type: "string",
              name: "Node Name",
              description: "Name of this node in the list",
              default: "LND Wrapper",
              nullable: false,
            },
          },
        },
      },
    },
    rpc: {
      type: "object",
      name: "RPC Settings",
      description: "RPC configuration options.",
      spec: {
        enable: {
          type: "boolean",
          name: "Enable",
          description: "Allow remote RPC requests.",
          default: true,
        },
        username: {
          type: "string",
          nullable: false,
          name: "Username",
          description: "The username for connecting to Bitcoin over RPC.",
          default: "bitcoin",
          masked: true,
          pattern: "^[a-zA-Z0-9_]+$",
          "pattern-description":
            "Must be alphanumeric (can contain underscore).",
        },
        password: {
          type: "string",
          nullable: false,
          name: "RPC Password",
          description: "The password for connecting to Bitcoin over RPC.",
          default: {
            charset: "a-z,2-7",
            len: 20,
          },
          pattern: '^[^\\n"]*$',
          "pattern-description":
            "Must not contain newline or quote characters.",
          copyable: true,
          masked: true,
        },
        bio: {
          type: "string",
          nullable: false,
          name: "Username",
          description: "The username for connecting to Bitcoin over RPC.",
          default: "bitcoin",
          masked: true,
          pattern: "^[a-zA-Z0-9_]+$",
          "pattern-description":
            "Must be alphanumeric (can contain underscore).",
          textarea: true,
        },
        advanced: {
          type: "object",
          name: "Advanced",
          description: "Advanced RPC Settings",
          spec: {
            auth: {
              name: "Authorization",
              description:
                "Username and hashed password for JSON-RPC connections. RPC clients connect using the usual http basic authentication.",
              type: "list",
              subtype: "string",
              default: [],
              spec: {
                pattern:
                  "^[a-zA-Z0-9_-]+:([0-9a-fA-F]{2})+\\$([0-9a-fA-F]{2})+$",
                "pattern-description":
                  'Each item must be of the form "<USERNAME>:<SALT>$<HASH>".',
                masked: false,
              },
              range: "[0,*)",
            },
            serialversion: {
              name: "Serialization Version",
              description:
                "Return raw transaction or block hex with Segwit or non-SegWit serialization.",
              type: "enum",
              values: ["non-segwit", "segwit"],
              "value-names": {},
              default: "segwit",
            },
            servertimeout: {
              name: "Rpc Server Timeout",
              description:
                "Number of seconds after which an uncompleted RPC call will time out.",
              type: "number",
              nullable: false,
              range: "[5,300]",
              integral: true,
              units: "seconds",
              default: 30,
            },
            threads: {
              name: "Threads",
              description:
                "Set the number of threads for handling RPC calls. You may wish to increase this if you are making lots of calls via an integration.",
              type: "number",
              nullable: false,
              default: 16,
              range: "[1,64]",
              integral: true,
            },
            workqueue: {
              name: "Work Queue",
              description:
                "Set the depth of the work queue to service RPC calls. Determines how long the backlog of RPC requests can get before it just rejects new ones.",
              type: "number",
              nullable: false,
              default: 128,
              range: "[8,256]",
              integral: true,
              units: "requests",
            },
          },
        },
      },
    },
    "zmq-enabled": {
      type: "boolean",
      name: "ZeroMQ Enabled",
      description: "Enable the ZeroMQ interface",
      default: true,
    },
    txindex: {
      type: "boolean",
      name: "Transaction Index",
      description: "Enable the Transaction Index (txindex)",
      default: true,
    },
    wallet: {
      type: "object",
      name: "Wallet",
      description: "Wallet Settings",
      spec: {
        enable: {
          name: "Enable Wallet",
          description: "Load the wallet and enable wallet RPC calls.",
          type: "boolean",
          default: true,
        },
        avoidpartialspends: {
          name: "Avoid Partial Spends",
          description:
            "Group outputs by address, selecting all or none, instead of selecting on a per-output basis. This improves privacy at the expense of higher transaction fees.",
          type: "boolean",
          default: true,
        },
        discardfee: {
          name: "Discard Change Tolerance",
          description:
            "The fee rate (in BTC/kB) that indicates your tolerance for discarding change by adding it to the fee.",
          type: "number",
          nullable: false,
          default: 0.0001,
          range: "[0,.01]",
          integral: false,
          units: "BTC/kB",
        },
      },
    },
    advanced: {
      type: "object",
      name: "Advanced",
      description: "Advanced Settings",
      spec: {
        mempool: {
          type: "object",
          name: "Mempool",
          description: "Mempool Settings",
          spec: {
            mempoolfullrbf: {
              name: "Enable Full RBF",
              description:
                "Policy for your node to use for relaying and mining unconfirmed transactions.  For details, see https://github.com/bitcoin/bitcoin/blob/master/doc/release-notes/release-notes-24.0.md#notice-of-new-option-for-transaction-replacement-policies",
              type: "boolean",
              default: false,
            },
            persistmempool: {
              type: "boolean",
              name: "Persist Mempool",
              description: "Save the mempool on shutdown and load on restart.",
              default: true,
            },
            maxmempool: {
              type: "number",
              nullable: false,
              name: "Max Mempool Size",
              description:
                "Keep the transaction memory pool below <n> megabytes.",
              range: "[1,*)",
              integral: true,
              units: "MiB",
              default: 300,
            },
            mempoolexpiry: {
              type: "number",
              nullable: false,
              name: "Mempool Expiration",
              description:
                "Do not keep transactions in the mempool longer than <n> hours.",
              range: "[1,*)",
              integral: true,
              units: "Hr",
              default: 336,
            },
          },
        },
        peers: {
          type: "object",
          name: "Peers",
          description: "Peer Connection Settings",
          spec: {
            listen: {
              type: "boolean",
              name: "Make Public",
              description:
                "Allow other nodes to find your server on the network.",
              default: true,
            },
            onlyconnect: {
              type: "boolean",
              name: "Disable Peer Discovery",
              description: "Only connect to specified peers.",
              default: false,
            },
            onlyonion: {
              type: "boolean",
              name: "Disable Clearnet",
              description: "Only connect to peers over Tor.",
              default: false,
            },
            addnode: {
              name: "Add Nodes",
              description: "Add addresses of nodes to connect to.",
              type: "list",
              subtype: "object",
              range: "[0,*)",
              default: [],
              spec: {
                "unique-by": null,
                spec: {
                  hostname: {
                    type: "string",
                    nullable: true,
                    name: "Hostname",
                    description: "Domain or IP address of bitcoin peer",
                    pattern:
                      "(^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$)|((^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$)|(^[a-z2-7]{16}\\.onion$)|(^([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?\\.)+[a-z0-9][a-z0-9-]{0,61}[a-z0-9]$))",
                    "pattern-description":
                      "Must be either a domain name, or an IPv4 or IPv6 address. Do not include protocol scheme (eg 'http://') or port.",
                    masked: false,
                  },
                  port: {
                    type: "number",
                    nullable: true,
                    name: "Port",
                    description:
                      "Port that peer is listening on for inbound p2p connections",
                    range: "[0,65535]",
                    integral: true,
                  },
                },
              },
            },
          },
        },
        dbcache: {
          type: "number",
          nullable: true,
          name: "Database Cache",
          description:
            "How much RAM to allocate for caching the TXO set. Higher values improve syncing performance, but increase your chance of using up all your system's memory or corrupting your database in the event of an ungraceful shutdown. Set this high but comfortably below your system's total RAM during IBD, then turn down to 450 (or leave blank) once the sync completes.",
          warning:
            "WARNING: Increasing this value results in a higher chance of ungraceful shutdowns, which can leave your node unusable if it happens during the initial block download. Use this setting with caution. Be sure to set this back to the default (450 or leave blank) once your node is synced. DO NOT press the STOP button if your dbcache is large. Instead, set this number back to the default, hit save, and wait for bitcoind to restart on its own.",
          range: "(0,*)",
          integral: true,
          units: "MiB",
        },
        pruning: {
          type: "union",
          name: "Pruning Settings",
          description:
            "Blockchain Pruning Options\nReduce the blockchain size on disk\n",
          warning:
            "If you set pruning to Manual and your disk is smaller than the total size of the blockchain, you MUST have something running that prunes these blocks or you may overfill your disk!\nDisabling pruning will convert your node into a full archival node. This requires a resync of the entire blockchain, a process that may take several days. Make sure you have enough free disk space or you may fill up your disk.\n",
          tag: {
            id: "mode",
            name: "Pruning Mode",
            description:
              '- Disabled: Disable pruning\n- Automatic: Limit blockchain size on disk to a certain number of megabytes\n- Manual: Prune blockchain with the "pruneblockchain" RPC\n',
            "variant-names": {
              disabled: "Disabled",
              automatic: "Automatic",
              manual: "Manual",
            },
          },
          variants: {
            disabled: {},
            automatic: {
              size: {
                type: "number",
                nullable: false,
                name: "Max Chain Size",
                description: "Limit of blockchain size on disk.",
                warning:
                  "Increasing this value will require re-syncing your node.",
                default: 550,
                range: "[550,1000000)",
                integral: true,
                units: "MiB",
              },
            },
            manual: {
              size: {
                type: "number",
                nullable: false,
                name: "Failsafe Chain Size",
                description: "Prune blockchain if size expands beyond this.",
                default: 65536,
                range: "[550,1000000)",
                integral: true,
                units: "MiB",
              },
            },
          },
          default: "disabled",
        },
        blockfilters: {
          type: "object",
          name: "Block Filters",
          description: "Settings for storing and serving compact block filters",
          spec: {
            blockfilterindex: {
              type: "boolean",
              name: "Compute Compact Block Filters (BIP158)",
              description:
                "Generate Compact Block Filters during initial sync (IBD) to enable 'getblockfilter' RPC. This is useful if dependent services need block filters to efficiently scan for addresses/transactions etc.",
              default: true,
            },
            peerblockfilters: {
              type: "boolean",
              name: "Serve Compact Block Filters to Peers (BIP157)",
              description:
                "Serve Compact Block Filters as a peer service to other nodes on the network. This is useful if you wish to connect an SPV client to your node to make it efficient to scan transactions without having to download all block data.  'Compute Compact Block Filters (BIP158)' is required.",
              default: false,
            },
          },
        },
        bloomfilters: {
          type: "object",
          name: "Bloom Filters (BIP37)",
          description: "Setting for serving Bloom Filters",
          spec: {
            peerbloomfilters: {
              type: "boolean",
              name: "Serve Bloom Filters to Peers",
              description:
                "Peers have the option of setting filters on each connection they make after the version handshake has completed. Bloom filters are for clients implementing SPV (Simplified Payment Verification) that want to check that block headers  connect together correctly, without needing to verify the full blockchain.  The client must trust that the transactions in the chain are in fact valid.  It is highly recommended AGAINST using for anything except Bisq integration.",
              warning:
                "This is ONLY for use with Bisq integration, please use Block Filters for all other applications.",
              default: false,
            },
          },
        },
      },
    },
  },
  {
    // convert this to `start-sdk/lib` for conversions
    StartSdk: "./output.sdk",
  },
)
