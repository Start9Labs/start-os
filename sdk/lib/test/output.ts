
import { sdk } from "./output.sdk"
const {Config, List, Value, Variants} = sdk

export const configSpec = Config.of({"mediasources": Value.multiselect({
  "name": "Media Sources",
  "minLength": null,
  "maxLength": null,
  "default": [
    "nextcloud"
  ],
  "description": "List of Media Sources to use with Jellyfin",
  "warning": null,
  "values": {
    "nextcloud": "NextCloud",
    "filebrowser": "File Browser"
  }
}),"testListUnion": Value.list(/* TODO: Convert range for this value ([1,*))*/List.obj({
          name:"Lightning Nodes",
          minLength:null,
          maxLength:null,
          default: [],
          description: "List of Lightning Network node instances to manage",
          warning: null,
        }, {
          spec: 
          Config.of({
            "union": /* TODO: Convert range for this value ([1,*))*/
          Value.union({
            name: "Type",
            description: "- LND: Lightning Network Daemon from Lightning Labs\n- CLN: Core Lightning from Blockstream\n",
            warning: null,
            required: {"default":"lnd"},
          }, Variants.of({"lnd": {name: "lnd", spec: Config.of({"name": Value.text({
  "name": "Node Name",
  "required": {
    "default": "LND Wrapper"
  },
  "description": "Name of this node in the list",
  "warning": null,
  "masked": false,
  "placeholder": null,
  "inputmode": "text",
  "patterns": [],
  "minLength": null,
  "maxLength": null
}),})},}))
        
          })
        ,
          displayAs: "{{name}}",
          uniqueBy: "name",
        })),"rpc": Value.object({
        name: "RPC Settings",
        description: "RPC configuration options.",
        warning: null,
      }, Config.of({"enable": Value.toggle({
  "name": "Enable",
  "default": true,
  "description": "Allow remote RPC requests.",
  "warning": null
}),"username": Value.text({
  "name": "Username",
  "required": {
    "default": "bitcoin"
  },
  "description": "The username for connecting to Bitcoin over RPC.",
  "warning": null,
  "masked": true,
  "placeholder": null,
  "inputmode": "text",
  "patterns": [
    {
      "regex": "^[a-zA-Z0-9_]+$",
      "description": "Must be alphanumeric (can contain underscore)."
    }
  ],
  "minLength": null,
  "maxLength": null
}),"password": Value.text({
  "name": "RPC Password",
  "required": {
    "default": {
      "charset": "a-z,2-7",
      "len": 20
    }
  },
  "description": "The password for connecting to Bitcoin over RPC.",
  "warning": null,
  "masked": true,
  "placeholder": null,
  "inputmode": "text",
  "patterns": [
    {
      "regex": "^[^\\n\"]*$",
      "description": "Must not contain newline or quote characters."
    }
  ],
  "minLength": null,
  "maxLength": null
}),"bio": Value.textarea({
  "name": "Username",
  "description": "The username for connecting to Bitcoin over RPC.",
  "warning": null,
  "required": true,
  "placeholder": null,
  "maxLength": null,
  "minLength": null
}),"advanced": Value.object({
        name: "Advanced",
        description: "Advanced RPC Settings",
        warning: null,
      }, Config.of({"auth": Value.list(/* TODO: Convert range for this value ([0,*))*/List.text({
  "name": "Authorization",
  "minLength": null,
  "maxLength": null,
  "default": [],
  "description": "Username and hashed password for JSON-RPC connections. RPC clients connect using the usual http basic authentication.",
  "warning": null
}, {"masked":false,"placeholder":null,"patterns":[{"regex":"^[a-zA-Z0-9_-]+:([0-9a-fA-F]{2})+\\$([0-9a-fA-F]{2})+$","description":"Each item must be of the form \"<USERNAME>:<SALT>$<HASH>\"."}],"minLength":null,"maxLength":null})),"serialversion": Value.select({
  "name": "Serialization Version",
  "description": "Return raw transaction or block hex with Segwit or non-SegWit serialization.",
  "warning": null,
  "required": {
    "default": "segwit"
  },
  "values": {
    "non-segwit": "non-segwit",
    "segwit": "segwit"
  }
} as const),"servertimeout": /* TODO: Convert range for this value ([5,300])*/Value.number({
  "name": "Rpc Server Timeout",
  "description": "Number of seconds after which an uncompleted RPC call will time out.",
  "warning": null,
  "required": {
    "default": 30
  },
  "min": null,
  "max": null,
  "step": null,
  "integer": true,
  "units": "seconds",
  "placeholder": null
}),"threads": /* TODO: Convert range for this value ([1,64])*/Value.number({
  "name": "Threads",
  "description": "Set the number of threads for handling RPC calls. You may wish to increase this if you are making lots of calls via an integration.",
  "warning": null,
  "required": {
    "default": 16
  },
  "min": null,
  "max": null,
  "step": null,
  "integer": true,
  "units": null,
  "placeholder": null
}),"workqueue": /* TODO: Convert range for this value ([8,256])*/Value.number({
  "name": "Work Queue",
  "description": "Set the depth of the work queue to service RPC calls. Determines how long the backlog of RPC requests can get before it just rejects new ones.",
  "warning": null,
  "required": {
    "default": 128
  },
  "min": null,
  "max": null,
  "step": null,
  "integer": true,
  "units": "requests",
  "placeholder": null
}),})),})),"zmq-enabled": Value.toggle({
  "name": "ZeroMQ Enabled",
  "default": true,
  "description": "Enable the ZeroMQ interface",
  "warning": null
}),"txindex": Value.toggle({
  "name": "Transaction Index",
  "default": true,
  "description": "Enable the Transaction Index (txindex)",
  "warning": null
}),"wallet": Value.object({
        name: "Wallet",
        description: "Wallet Settings",
        warning: null,
      }, Config.of({"enable": Value.toggle({
  "name": "Enable Wallet",
  "default": true,
  "description": "Load the wallet and enable wallet RPC calls.",
  "warning": null
}),"avoidpartialspends": Value.toggle({
  "name": "Avoid Partial Spends",
  "default": true,
  "description": "Group outputs by address, selecting all or none, instead of selecting on a per-output basis. This improves privacy at the expense of higher transaction fees.",
  "warning": null
}),"discardfee": /* TODO: Convert range for this value ([0,.01])*/Value.number({
  "name": "Discard Change Tolerance",
  "description": "The fee rate (in BTC/kB) that indicates your tolerance for discarding change by adding it to the fee.",
  "warning": null,
  "required": {
    "default": 0.0001
  },
  "min": null,
  "max": null,
  "step": null,
  "integer": false,
  "units": "BTC/kB",
  "placeholder": null
}),})),"advanced": Value.object({
        name: "Advanced",
        description: "Advanced Settings",
        warning: null,
      }, Config.of({"mempool": Value.object({
        name: "Mempool",
        description: "Mempool Settings",
        warning: null,
      }, Config.of({"mempoolfullrbf": Value.toggle({
  "name": "Enable Full RBF",
  "default": false,
  "description": "Policy for your node to use for relaying and mining unconfirmed transactions.  For details, see https://github.com/bitcoin/bitcoin/blob/master/doc/release-notes/release-notes-24.0.md#notice-of-new-option-for-transaction-replacement-policies",
  "warning": null
}),"persistmempool": Value.toggle({
  "name": "Persist Mempool",
  "default": true,
  "description": "Save the mempool on shutdown and load on restart.",
  "warning": null
}),"maxmempool": /* TODO: Convert range for this value ([1,*))*/Value.number({
  "name": "Max Mempool Size",
  "description": "Keep the transaction memory pool below <n> megabytes.",
  "warning": null,
  "required": {
    "default": 300
  },
  "min": null,
  "max": null,
  "step": null,
  "integer": true,
  "units": "MiB",
  "placeholder": null
}),"mempoolexpiry": /* TODO: Convert range for this value ([1,*))*/Value.number({
  "name": "Mempool Expiration",
  "description": "Do not keep transactions in the mempool longer than <n> hours.",
  "warning": null,
  "required": {
    "default": 336
  },
  "min": null,
  "max": null,
  "step": null,
  "integer": true,
  "units": "Hr",
  "placeholder": null
}),})),"peers": Value.object({
        name: "Peers",
        description: "Peer Connection Settings",
        warning: null,
      }, Config.of({"listen": Value.toggle({
  "name": "Make Public",
  "default": true,
  "description": "Allow other nodes to find your server on the network.",
  "warning": null
}),"onlyconnect": Value.toggle({
  "name": "Disable Peer Discovery",
  "default": false,
  "description": "Only connect to specified peers.",
  "warning": null
}),"onlyonion": Value.toggle({
  "name": "Disable Clearnet",
  "default": false,
  "description": "Only connect to peers over Tor.",
  "warning": null
}),"addnode": Value.list(/* TODO: Convert range for this value ([0,*))*/List.obj({
          name: "Add Nodes",
          minLength: null,
          maxLength: null,
          default: [],
          description: "Add addresses of nodes to connect to.",
          warning: null,
        }, {
          spec: Config.of({"hostname": Value.text({
  "name": "Hostname",
  "required": false,
  "description": "Domain or IP address of bitcoin peer",
  "warning": null,
  "masked": false,
  "placeholder": null,
  "inputmode": "text",
  "patterns": [
    {
      "regex": "(^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$)|((^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$)|(^[a-z2-7]{16}\\.onion$)|(^([a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?\\.)+[a-z0-9][a-z0-9-]{0,61}[a-z0-9]$))",
      "description": "Must be either a domain name, or an IPv4 or IPv6 address. Do not include protocol scheme (eg 'http://') or port."
    }
  ],
  "minLength": null,
  "maxLength": null
}),"port": /* TODO: Convert range for this value ([0,65535])*/Value.number({
  "name": "Port",
  "description": "Port that peer is listening on for inbound p2p connections",
  "warning": null,
  "required": false,
  "min": null,
  "max": null,
  "step": null,
  "integer": true,
  "units": null,
  "placeholder": null
}),}),
          displayAs: null,
          uniqueBy: null,
        })),})),"dbcache": /* TODO: Convert range for this value ((0,*))*/Value.number({
  "name": "Database Cache",
  "description": "How much RAM to allocate for caching the TXO set. Higher values improve syncing performance, but increase your chance of using up all your system's memory or corrupting your database in the event of an ungraceful shutdown. Set this high but comfortably below your system's total RAM during IBD, then turn down to 450 (or leave blank) once the sync completes.",
  "warning": "WARNING: Increasing this value results in a higher chance of ungraceful shutdowns, which can leave your node unusable if it happens during the initial block download. Use this setting with caution. Be sure to set this back to the default (450 or leave blank) once your node is synced. DO NOT press the STOP button if your dbcache is large. Instead, set this number back to the default, hit save, and wait for bitcoind to restart on its own.",
  "required": false,
  "min": null,
  "max": null,
  "step": null,
  "integer": true,
  "units": "MiB",
  "placeholder": null
}),"pruning": Value.union({
        name: "Pruning Settings",
        description: "- Disabled: Disable pruning\n- Automatic: Limit blockchain size on disk to a certain number of megabytes\n- Manual: Prune blockchain with the \"pruneblockchain\" RPC\n",
        warning: null,
        
        // prettier-ignore
        required: {"default":"disabled"},
      }, Variants.of({"disabled": {name: "Disabled", spec: Config.of({})},"automatic": {name: "Automatic", spec: Config.of({"size": /* TODO: Convert range for this value ([550,1000000))*/Value.number({
  "name": "Max Chain Size",
  "description": "Limit of blockchain size on disk.",
  "warning": "Increasing this value will require re-syncing your node.",
  "required": {
    "default": 550
  },
  "min": null,
  "max": null,
  "step": null,
  "integer": true,
  "units": "MiB",
  "placeholder": null
}),})},"manual": {name: "Manual", spec: Config.of({"size": /* TODO: Convert range for this value ([550,1000000))*/Value.number({
  "name": "Failsafe Chain Size",
  "description": "Prune blockchain if size expands beyond this.",
  "warning": null,
  "required": {
    "default": 65536
  },
  "min": null,
  "max": null,
  "step": null,
  "integer": true,
  "units": "MiB",
  "placeholder": null
}),})},})),"blockfilters": Value.object({
        name: "Block Filters",
        description: "Settings for storing and serving compact block filters",
        warning: null,
      }, Config.of({"blockfilterindex": Value.toggle({
  "name": "Compute Compact Block Filters (BIP158)",
  "default": true,
  "description": "Generate Compact Block Filters during initial sync (IBD) to enable 'getblockfilter' RPC. This is useful if dependent services need block filters to efficiently scan for addresses/transactions etc.",
  "warning": null
}),"peerblockfilters": Value.toggle({
  "name": "Serve Compact Block Filters to Peers (BIP157)",
  "default": false,
  "description": "Serve Compact Block Filters as a peer service to other nodes on the network. This is useful if you wish to connect an SPV client to your node to make it efficient to scan transactions without having to download all block data.  'Compute Compact Block Filters (BIP158)' is required.",
  "warning": null
}),})),"bloomfilters": Value.object({
        name: "Bloom Filters (BIP37)",
        description: "Setting for serving Bloom Filters",
        warning: null,
      }, Config.of({"peerbloomfilters": Value.toggle({
  "name": "Serve Bloom Filters to Peers",
  "default": false,
  "description": "Peers have the option of setting filters on each connection they make after the version handshake has completed. Bloom filters are for clients implementing SPV (Simplified Payment Verification) that want to check that block headers  connect together correctly, without needing to verify the full blockchain.  The client must trust that the transactions in the chain are in fact valid.  It is highly recommended AGAINST using for anything except Bisq integration.",
  "warning": "This is ONLY for use with Bisq integration, please use Block Filters for all other applications."
}),})),})),});
export const matchConfigSpec = configSpec.validator;
export type ConfigSpec = typeof matchConfigSpec._TYPE;