export function properties() {
  return "Anything here";
}

export async function getConfig(effects) {
  try {
    await effects.writeFile({
      path: "../test.log",
      toWrite: "This is a test",
      volumeId: "main",
    });
    throw new Error(
      "Expecting that the ../test.log should not be a valid path since we are breaking out of the parent"
    );
  } catch (e) {}
  try {
    await effects.writeFile({
      path: "./hack_back/broken.log",
      toWrite: "This is a test",
      volumeId: "main",
    });
    throw new Error(
      "Expecting that using a symlink to break out of parent still fails for writing"
    );
  } catch (e) {}
  try {
    await effects.createDir({
      path: "./hack_back/broken_dir",
      volumeId: "main",
    });
    throw new Error(
      "Expecting that using a symlink to break out of parent still fails for writing dir"
    );
  } catch (e) {}
  try {
    await effects.readFile({
      path: "./hack_back/data/bad_file.txt",
      volumeId: "main",
    });
    throw new Error(
      "Expecting that using a symlink to break out of parent still fails for reading"
    );
  } catch (e) {}

  // Testing dir, create + delete
  await effects.createDir({
    path: "./testing",
    volumeId: "main",
  });
  await effects.writeJsonFile({
    path: "./testing/test2.log",
    toWrite: { value: "This is a test" },
    volumeId: "main",
  });

  (
    await effects.readJsonFile({
      path: "./testing/test2.log",
      volumeId: "main",
      // @ts-ignore
    })
  ).value;

  await effects.removeFile({
    path: "./testing/test2.log",
    volumeId: "main",
  });
  await effects.removeDir({
    path: "./testing",
    volumeId: "main",
  });

  // Testing reading + writing
  await effects.writeFile({
    path: "./test.log",
    toWrite: "This is a test",
    volumeId: "main",
  });

  effects.debug(
    `Read results are ${await effects.readFile({
      path: "./test.log",
      volumeId: "main",
    })}`
  );
  // Testing loging
  effects.trace("trace");
  effects.debug("debug");
  effects.warn("warn");
  effects.error("error");
  effects.info("info");

  {
    const metadata = await effects.metadata({
      path: "./test.log",
      volumeId: "main",
    });

    if (typeof metadata.fileType !== "string") {
      throw new TypeError("File type is not a string");
    }
    if (typeof metadata.isDir !== "boolean") {
      throw new TypeError("isDir is not a boolean");
    }
    if (typeof metadata.isFile !== "boolean") {
      throw new TypeError("isFile is not a boolean");
    }
    if (typeof metadata.isSymlink !== "boolean") {
      throw new TypeError("isSymlink is not a boolean");
    }
    if (typeof metadata.len !== "number") {
      throw new TypeError("len is not a number");
    }
    if (typeof metadata.gid !== "number") {
      throw new TypeError("gid is not a number");
    }
    if (typeof metadata.uid !== "number") {
      throw new TypeError("uid is not a number");
    }
    if (typeof metadata.mode !== "number") {
      throw new TypeError("mode is not a number");
    }
    if (!(metadata.modified instanceof Date)) {
      throw new TypeError("modified is not a Date");
    }
    if (!(metadata.accessed instanceof Date)) {
      throw new TypeError("accessed is not a Date");
    }
    if (!(metadata.created instanceof Date)) {
      throw new TypeError("created is not a Date");
    }
    if (typeof metadata.readonly !== "boolean") {
      throw new TypeError("readonly is not a boolean");
    }
    effects.error(JSON.stringify(metadata));
  }
  return {
    result: {
      spec: {
        "control-tor-address": {
          name: "Control Tor Address",
          description: "The Tor address for the control interface.",
          type: "pointer",
          subtype: "package",
          "package-id": "lnd",
          target: "tor-address",
          interface: "control",
        },
        "peer-tor-address": {
          name: "Peer Tor Address",
          description: "The Tor address for the peer interface.",
          type: "pointer",
          subtype: "package",
          "package-id": "lnd",
          target: "tor-address",
          interface: "peer",
        },
        "watchtower-tor-address": {
          name: "Watchtower Tor Address",
          description: "The Tor address for the watchtower interface.",
          type: "pointer",
          subtype: "package",
          "package-id": "lnd",
          target: "tor-address",
          interface: "watchtower",
        },
        alias: {
          type: "string",
          name: "Alias",
          description: "The public, human-readable name of your Lightning node",
          nullable: true,
          pattern: ".{1,32}",
          "pattern-description":
            "Must be at least 1 character and no more than 32 characters",
        },
        color: {
          type: "string",
          name: "Color",
          description: "The public color dot of your Lightning node",
          nullable: false,
          pattern: "[0-9a-fA-F]{6}",
          "pattern-description":
            "Must be a valid 6 digit hexadecimal RGB value. The first two digits are red, middle two are green, and final two are\nblue\n",
          default: {
            charset: "a-f,0-9",
            len: 6,
          },
        },
        "accept-keysend": {
          type: "boolean",
          name: "Accept Keysend",
          description:
            "Allow others to send payments directly to your public key through keysend instead of having to get a new invoice\n",
          default: false,
        },
        "accept-amp": {
          type: "boolean",
          name: "Accept Spontaneous AMPs",
          description:
            "If enabled, spontaneous payments through AMP will be accepted. Payments to AMP\ninvoices will be accepted regardless of this setting.\n",
          default: false,
        },
        "reject-htlc": {
          type: "boolean",
          name: "Reject Routing Requests",
          description:
            "If true, LND will not forward any HTLCs that are meant as onward payments. This option will still allow LND to send\nHTLCs and receive HTLCs but lnd won't be used as a hop.\n",
          default: false,
        },
        "min-chan-size": {
          type: "number",
          name: "Minimum Channel Size",
          description:
            "The smallest channel size that we should accept. Incoming channels smaller than this will be rejected.\n",
          nullable: true,
          range: "[1,16777215]",
          integral: true,
          units: "satoshis",
        },
        "max-chan-size": {
          type: "number",
          name: "Maximum Channel Size",
          description:
            "The largest channel size that we should accept. Incoming channels larger than this will be rejected.\nFor non-Wumbo channels this limit remains 16777215 satoshis by default as specified in BOLT-0002. For wumbo\nchannels this limit is 1,000,000,000 satoshis (10 BTC). Set this config option explicitly to restrict your maximum\nchannel size to better align with your risk tolerance.  Don't forget to enable Wumbo channels under 'Advanced,' if desired.\n",
          nullable: true,
          range: "[1,1000000000]",
          integral: true,
          units: "satoshis",
        },
        tor: {
          type: "object",
          name: "Tor Config",
          spec: {
            "use-tor-only": {
              type: "boolean",
              name: "Use Tor for all traffic",
              description:
                "Use the tor proxy even for connections that are reachable on clearnet. This will hide your node's public IP address, but will slow down your node's performance",
              default: false,
            },
            "stream-isolation": {
              type: "boolean",
              name: "Stream Isolation",
              description:
                "Enable Tor stream isolation by randomizing user credentials for each connection. With this mode active, each connection will use a new circuit. This means that multiple applications (other than lnd) using Tor won't be mixed in with lnd's traffic.\nThis option may not be used when 'Use Tor for all traffic' is disabled, since direct connections compromise source IP privacy by default.",
              default: false,
            },
          },
        },
        bitcoind: {
          type: "union",
          name: "Bitcoin Core",
          description:
            "The Bitcoin Core node to connect to:\n  - internal: The Bitcoin Core and Proxy services installed to your Embassy\n  - external: An unpruned Bitcoin Core node running on a different device\n",
          tag: {
            id: "type",
            name: "Type",
            "variant-names": {
              internal: "Internal (Bitcoin Core)",
              "internal-proxy": "Internal (Bitcoin Proxy)",
              external: "External",
            },
            description:
              "The Bitcoin Core node to connect to:\n  - internal: The Bitcoin Core and Proxy services installed to your Embassy\n  - external: An unpruned Bitcoin Core node running on a different device\n",
          },
          default: "internal",
          variants: {
            internal: {
              user: {
                type: "pointer",
                name: "RPC Username",
                description: "The username for Bitcoin Core's RPC interface",
                subtype: "package",
                "package-id": "bitcoind",
                target: "config",
                multi: false,
                selector: "$.rpc.username",
              },
              password: {
                type: "pointer",
                name: "RPC Password",
                description: "The password for Bitcoin Core's RPC interface",
                subtype: "package",
                "package-id": "bitcoind",
                target: "config",
                multi: false,
                selector: "$.rpc.password",
              },
            },
            "internal-proxy": {
              user: {
                type: "pointer",
                name: "RPC Username",
                description: "The username for the RPC user allocated to lnd",
                subtype: "package",
                "package-id": "btc-rpc-proxy",
                target: "config",
                multi: false,
                selector: '$.users[?(@.name == "lnd")].name',
              },
              password: {
                type: "pointer",
                name: "RPC Password",
                description: "The password for the RPC user allocated to lnd",
                subtype: "package",
                "package-id": "btc-rpc-proxy",
                target: "config",
                multi: false,
                selector: '$.users[?(@.name == "lnd")].password',
              },
            },
            external: {
              "connection-settings": {
                type: "union",
                name: "Connection Settings",
                description:
                  "Information to connect to an external unpruned Bitcoin Core node",
                tag: {
                  id: "type",
                  name: "Type",
                  description:
                    "- Manual: Raw information for finding a Bitcoin Core node\n- Quick Connect: A Quick Connect URL for a Bitcoin Core node\n",
                  "variant-names": {
                    manual: "Manual",
                    "quick-connect": "Quick Connect",
                  },
                },
                default: "quick-connect",
                variants: {
                  manual: {
                    host: {
                      type: "string",
                      name: "Public Address",
                      description:
                        "The public address of your Bitcoin Core server",
                      nullable: false,
                    },
                    "rpc-user": {
                      type: "string",
                      name: "RPC Username",
                      description:
                        "The username for the RPC user on your Bitcoin Core RPC server",
                      nullable: false,
                    },
                    "rpc-password": {
                      type: "string",
                      name: "RPC Password",
                      description:
                        "The password for the RPC user on your Bitcoin Core RPC server",
                      nullable: false,
                    },
                    "rpc-port": {
                      type: "number",
                      name: "RPC Port",
                      description:
                        "The port that your Bitcoin Core RPC server is bound to",
                      nullable: false,
                      range: "[0,65535]",
                      integral: true,
                      default: 8332,
                    },
                    "zmq-block-port": {
                      type: "number",
                      name: "ZeroMQ Block Port",
                      description:
                        "The port that your Bitcoin Core ZeroMQ server is bound to for raw blocks",
                      nullable: false,
                      range: "[0,65535]",
                      integral: true,
                      default: 28332,
                    },
                    "zmq-tx-port": {
                      type: "number",
                      name: "ZeroMQ Transaction Port",
                      description:
                        "The port that your Bitcoin Core ZeroMQ server is bound to for raw transactions",
                      nullable: false,
                      range: "[0,65535]",
                      integral: true,
                      default: 28333,
                    },
                  },
                  "quick-connect": {
                    "quick-connect-url": {
                      type: "string",
                      name: "Quick Connect URL",
                      description:
                        "The Quick Connect URL for your Bitcoin Core RPC server\nNOTE: LND will not accept a .onion url for this option\n",
                      nullable: false,
                      pattern:
                        "btcstandup://[^:]*:[^@]*@[a-zA-Z0-9.-]+:[0-9]+(/(\\?(label=.+)?)?)?",
                      "pattern-description":
                        "Must be a valid Quick Connect URL. For help, check out https://github.com/BlockchainCommons/Gordian/blob/master/Docs/Quick-Connect-API.md",
                    },
                    "zmq-block-port": {
                      type: "number",
                      name: "ZeroMQ Block Port",
                      description:
                        "The port that your Bitcoin Core ZeroMQ server is bound to for raw blocks",
                      nullable: false,
                      range: "[0,65535]",
                      integral: true,
                      default: 28332,
                    },
                    "zmq-tx-port": {
                      type: "number",
                      name: "ZeroMQ Transaction Port",
                      description:
                        "The port that your Bitcoin Core ZeroMQ server is bound to for raw transactions",
                      nullable: false,
                      range: "[0,65535]",
                      integral: true,
                      default: 28333,
                    },
                  },
                },
              },
            },
          },
        },
        autopilot: {
          type: "object",
          name: "Autopilot",
          description: "Autopilot Settings",
          spec: {
            enabled: {
              type: "boolean",
              name: "Enabled",
              description:
                "If the autopilot agent should be active or not. The autopilot agent will\nattempt to AUTOMATICALLY OPEN CHANNELS to put your node in an advantageous\nposition within the network graph.  DO NOT ENABLE THIS IF YOU WANT TO MANAGE \nCHANNELS MANUALLY OR DO NOT UNDERSTAND IT.\n",
              default: false,
            },
            private: {
              type: "boolean",
              name: "Private",
              description:
                "Whether the channels created by the autopilot agent should be private or not.\nPrivate channels won't be announced to the network.\n",
              default: false,
            },
            maxchannels: {
              type: "number",
              name: "Maximum Channels",
              description:
                "The maximum number of channels that should be created.",
              nullable: false,
              range: "[1,*)",
              integral: true,
              default: 5,
            },
            allocation: {
              type: "number",
              name: "Allocation",
              description:
                'The fraction of total funds that should be committed to automatic channel\nestablishment. For example 60% means that 60% of the total funds available\nwithin the wallet should be used to automatically establish channels. The total\namount of attempted channels will still respect the "Maximum Channels" parameter.\n',
              nullable: false,
              range: "[0,100]",
              integral: false,
              default: 60,
              units: "%",
            },
            "min-channel-size": {
              type: "number",
              name: "Minimum Channel Size",
              description:
                "The smallest channel that the autopilot agent should create.",
              nullable: false,
              range: "[0,*)",
              integral: true,
              default: 20000,
              units: "satoshis",
            },
            "max-channel-size": {
              type: "number",
              name: "Maximum Channel Size",
              description:
                "The largest channel that the autopilot agent should create.",
              nullable: false,
              range: "[0,*)",
              integral: true,
              default: 16777215,
              units: "satoshis",
            },
            advanced: {
              type: "object",
              name: "Advanced",
              description: "Advanced Options",
              spec: {
                "min-confirmations": {
                  type: "number",
                  name: "Minimum Confirmations",
                  description:
                    "The minimum number of confirmations each of your inputs in funding transactions\ncreated by the autopilot agent must have.\n",
                  nullable: false,
                  range: "[0,*)",
                  integral: true,
                  default: 1,
                  units: "blocks",
                },
                "confirmation-target": {
                  type: "number",
                  name: "Confirmation Target",
                  description:
                    "The confirmation target (in blocks) for channels opened by autopilot.",
                  nullable: false,
                  range: "[0,*)",
                  integral: true,
                  default: 1,
                  units: "blocks",
                },
              },
            },
          },
        },
        advanced: {
          type: "object",
          name: "Advanced",
          description: "Advanced Options",
          spec: {
            "debug-level": {
              type: "enum",
              name: "Log Verbosity",
              values: ["trace", "debug", "info", "warn", "error", "critical"],
              description:
                "Sets the level of log filtration. Trace is the most verbose, Critical is the least.\n",
              default: "info",
              "value-names": {},
            },
            "db-bolt-no-freelist-sync": {
              type: "boolean",
              name: "Disallow Bolt DB Freelist Sync",
              description:
                "If true, prevents the database from syncing its freelist to disk.\n",
              default: false,
            },
            "db-bolt-auto-compact": {
              type: "boolean",
              name: "Compact Database on Startup",
              description:
                "Performs database compaction on startup. This is necessary to keep disk usage down over time at the cost of\nhaving longer startup times.\n",
              default: true,
            },
            "db-bolt-auto-compact-min-age": {
              type: "number",
              name: "Minimum Autocompaction Age for Bolt DB",
              description:
                "How long ago (in hours) the last compaction of a database file must be for it to be considered for auto\ncompaction again. Can be set to 0 to compact on every startup.\n",
              nullable: false,
              range: "[0, *)",
              integral: true,
              default: 168,
              units: "hours",
            },
            "db-bolt-db-timeout": {
              type: "number",
              name: "Bolt DB Timeout",
              description:
                "How long should LND try to open the database before giving up?",
              nullable: false,
              range: "[1, 86400]",
              integral: true,
              default: 60,
              units: "seconds",
            },
            "recovery-window": {
              type: "number",
              name: "Recovery Window",
              description:
                "Number of blocks in the past that LND should scan for unknown transactions",
              nullable: true,
              range: "[1,*)",
              integral: true,
              units: "blocks",
            },
            "payments-expiration-grace-period": {
              type: "number",
              name: "Payments Expiration Grace Period",
              description:
                "A period to wait before for closing channels with outgoing htlcs that have timed out and are a result of this\nnodes instead payment. In addition to our current block based deadline, is specified this grace period will\nalso be taken into account.\n",
              nullable: false,
              range: "[1,*)",
              integral: true,
              default: 30,
              units: "seconds",
            },
            "default-remote-max-htlcs": {
              type: "number",
              name: "Maximum Remote HTLCs",
              description:
                "The default max_htlc applied when opening or accepting channels. This value limits the number of concurrent\nHTLCs that the remote party can add to the commitment. The maximum possible value is 483.\n",
              nullable: false,
              range: "[1,483]",
              integral: true,
              default: 483,
              units: "htlcs",
            },
            "max-channel-fee-allocation": {
              type: "number",
              name: "Maximum Channel Fee Allocation",
              description:
                "The maximum percentage of total funds that can be allocated to a channel's commitment fee. This only applies for\nthe initiator of the channel.\n",
              nullable: false,
              range: "[0.1, 1]",
              integral: false,
              default: 0.5,
            },
            "max-commit-fee-rate-anchors": {
              type: "number",
              name: "Maximum Commitment Fee for Anchor Channels",
              description:
                "The maximum fee rate in sat/vbyte that will be used for commitments of channels of the anchors type. Must be\nlarge enough to ensure transaction propagation.\n",
              nullable: false,
              range: "[1,*)",
              integral: true,
              default: 10,
            },
            "protocol-wumbo-channels": {
              type: "boolean",
              name: "Enable Wumbo Channels",
              description:
                "If set, then lnd will create and accept requests for channels larger than 0.16 BTC\n",
              default: false,
            },
            "protocol-no-anchors": {
              type: "boolean",
              name: "Disable Anchor Channels",
              description:
                "Set to disable support for anchor commitments. Anchor channels allow you to determine your fees at close time by\nusing a Child Pays For Parent transaction.\n",
              default: false,
            },
            "protocol-disable-script-enforced-lease": {
              type: "boolean",
              name: "Disable Script Enforced Channel Leases",
              description:
                "Set to disable support for script enforced lease channel commitments. If not set, lnd will accept these channels by default if the remote channel party proposes them. Note that lnd will require 1 UTXO to be reserved for this channel type if it is enabled.\nNote: This may cause you to be unable to close a channel and your wallets may not understand why",
              default: false,
            },
            "gc-canceled-invoices-on-startup": {
              type: "boolean",
              name: "Cleanup Canceled Invoices on Startup",
              description:
                "If true, LND will attempt to garbage collect canceled invoices upon start.\n",
              default: false,
            },
            bitcoin: {
              type: "object",
              name: "Bitcoin Channel Configuration",
              description:
                "Configuration options for lightning network channel management operating over the Bitcoin network",
              spec: {
                "default-channel-confirmations": {
                  type: "number",
                  name: "Default Channel Confirmations",
                  description:
                    "The default number of confirmations a channel must have before it's considered\nopen. LND will require any incoming channel requests to wait this many\nconfirmations before it considers the channel active.\n",
                  nullable: false,
                  range: "[1,6]",
                  integral: true,
                  default: 3,
                  units: "blocks",
                },
                "min-htlc": {
                  type: "number",
                  name: "Minimum Incoming HTLC Size",
                  description:
                    "The smallest HTLC LND will to accept on your channels, in millisatoshis.\n",
                  nullable: false,
                  range: "[1,*)",
                  integral: true,
                  default: 1,
                  units: "millisatoshis",
                },
                "min-htlc-out": {
                  type: "number",
                  name: "Minimum Outgoing HTLC Size",
                  description:
                    "The smallest HTLC LND will send out on your channels, in millisatoshis.\n",
                  nullable: false,
                  range: "[1,*)",
                  integral: true,
                  default: 1000,
                  units: "millisatoshis",
                },
                "base-fee": {
                  type: "number",
                  name: "Routing Base Fee",
                  description:
                    "The base fee in millisatoshi you will charge for forwarding payments on your\nchannels.\n",
                  nullable: false,
                  range: "[0,*)",
                  integral: true,
                  default: 1000,
                  units: "millisatoshi",
                },
                "fee-rate": {
                  type: "number",
                  name: "Routing Fee Rate",
                  description:
                    "The fee rate used when forwarding payments on your channels. The total fee\ncharged is the Base Fee + (amount * Fee Rate / 1000000), where amount is the\nforwarded amount. Measured in sats per million\n",
                  nullable: false,
                  range: "[1,1000000)",
                  integral: true,
                  default: 1,
                  units: "sats per million",
                },
                "time-lock-delta": {
                  type: "number",
                  name: "Time Lock Delta",
                  description:
                    "The CLTV delta we will subtract from a forwarded HTLC's timelock value.",
                  nullable: false,
                  range: "[6, 144]",
                  integral: true,
                  default: 40,
                  units: "blocks",
                },
              },
            },
          },
        },
      },
    },
  };
}

export async function setConfig(effects) {
  return {
    error: "Not setup",
  };
}

const assert = (condition, message) => {
  if (!condition) {
    throw new Error(message);
  }
};

export const action = {
  async fetch(effects, _input) {
    const example = await effects.fetch(
      "https://postman-echo.com/get?foo1=bar1&foo2=bar2"
    );
    assert(
      Number(example.headers["content-length"]) > 0 &&
        Number(example.headers["content-length"]) <= 1000000,
      "Should have content length"
    );
    assert(
      example.text() instanceof Promise,
      "example.text() should be a promise"
    );
    assert(example.body === undefined, "example.body should not be defined");
    assert(
      JSON.parse(await example.text()).args.foo1 === "bar1",
      "Body should be parsed"
    );
    const message = `This worked @ ${new Date().toISOString()}`;
    const secondResponse = await effects.fetch(
      "https://postman-echo.com/post",
      {
        method: "POST",
        body: JSON.stringify({ message }),
        headers: {
          test: "1234",
        },
      }
    );
    assert(
      (await secondResponse.json()).json.message === message,
      "Body should be parsed from response"
    );
    return {
      result: {
        copyable: false,
        message: "Done",
        version: "0",
        qr: false,
      },
    };
  },

  async "js-action-var-arg"(_effects, _input, testInput) {
    assert(testInput == 42, "Input should be passed in");
    return {
      result: {
        copyable: false,
        message: "Done",
        version: "0",
        qr: false,
      },
    };
  },
  async "test-rename"(effects, _input) {
    let failed = false;
    await effects.writeFile({
      volumeId: "main",
      path: "test-rename.txt",
      toWrite: "Hello World",
    });
    await effects.rename({
      srcVolume: "main",
      srcPath: "test-rename.txt",
      dstVolume: "main",
      dstPath: "test-rename-2.txt",
    });

    const readIn = await effects.readFile({
      volumeId: "main",
      path: "test-rename-2.txt",
    });
    assert(readIn === "Hello World", "Contents should be the same");

    await effects.removeFile({
      path: "test-rename-2.txt",
      volumeId: "main",
    });

    failed = false;
    try {
      await effects.removeFile({
        path: "test-rename.txt",
        volumeId: "main",
      });
    } catch (_) {
      failed = true;
    }
    assert(failed, "Should not be able to remove file that doesn't exist");
    

    return {
      result: {
        copyable: false,
        message: "Done",
        version: "0",
        qr: false,
      },
    };
  },
  /**
   * Created this test because of issue
   * https://github.com/Start9Labs/embassy-os/issues/1737 
   * which that we couldn't create a dir that was deeply nested, and the parents where
   * not created yet. Found this out during the migrations, where the parent would die.
   * @param {*} effects 
   * @param {*} _input 
   * @returns 
   */
  async "test-deep-dir"(effects, _input) {
    await effects
      .removeDir({
        volumeId: "main",
        path: "test-deep-dir",
      })
      .catch(() => {});
    await effects.createDir({
      volumeId: "main",
      path: "test-deep-dir/deep/123",
    });
    await effects.removeDir({
      volumeId: "main",
      path: "test-deep-dir",
    });
    return {
      result: {
        copyable: false,
        message: "Done",
        version: "0",
        qr: false,
      },
    };
  },
  /**
   * Found case where we could escape with the new deeper dir fix.
   * @param {*} effects 
   * @param {*} _input 
   * @returns 
   */
  async "test-deep-dir-escape"(effects, _input) {
    await effects
      .removeDir({
        volumeId: "main",
        path: "test-deep-dir",
      })
      .catch(() => {});
    await effects.createDir({
      volumeId: "main",
      path: "test-deep-dir/../../test",
    }).then(_ => {throw new Error("Should not be able to create sub")}, _ => {});

    return {
      result: {
        copyable: false,
        message: "Done",
        version: "0",
        qr: false,
      },
    };
  },
};
