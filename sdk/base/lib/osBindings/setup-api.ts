export type SetupApi = {
  _CHILDREN: {
    auth: { _CHILDREN: { session: { _CHILDREN: {} } & { _PARAMS: {} } } } & {
      _PARAMS: {}
    }
    backup: {
      _CHILDREN: {
        target: { _CHILDREN: { cifs: { _CHILDREN: {} } & { _PARAMS: {} } } } & {
          _PARAMS: {}
        }
      }
    } & { _PARAMS: {} }
    db: { _CHILDREN: { put: { _CHILDREN: {} } & { _PARAMS: {} } } } & {
      _PARAMS: {}
    }
    diagnostic: {
      _CHILDREN: {
        disk: {
          _CHILDREN: {
            forget: { _PARAMS: {}; _RETURN: null }
            repair: { _PARAMS: {}; _RETURN: null }
          }
        } & { _PARAMS: {} }
      }
    } & { _PARAMS: {} }
    disk: { _CHILDREN: { repair: { _PARAMS: {}; _RETURN: null } } } & {
      _PARAMS: {}
    }
    "git-info": { _PARAMS: {}; _RETURN: string }
    init: { _CHILDREN: {} } & { _PARAMS: {} }
    install: { _CHILDREN: { disk: { _CHILDREN: {} } & { _PARAMS: {} } } } & {
      _PARAMS: {}
    }
    kiosk: { _CHILDREN: {} } & { _PARAMS: {} }
    net: {
      _CHILDREN: {
        acme: { _CHILDREN: {} } & { _PARAMS: {} }
        dns: {
          _CHILDREN: {
            query: { _PARAMS: { fqdn: string }; _RETURN: string | null }
          }
        } & { _PARAMS: {} }
        forward: { _CHILDREN: {} } & { _PARAMS: {} }
        gateway: { _CHILDREN: {} } & { _PARAMS: {} }
        tor: { _CHILDREN: { key: { _CHILDREN: {} } & { _PARAMS: {} } } } & {
          _PARAMS: {}
        }
        tunnel: { _CHILDREN: {} } & { _PARAMS: {} }
        vhost: { _CHILDREN: {} } & { _PARAMS: {} }
      }
    } & { _PARAMS: {} }
    notification: { _CHILDREN: {} } & { _PARAMS: {} }
    package: {
      _CHILDREN: {
        action: { _CHILDREN: {} } & { _PARAMS: {} }
        backup: { _CHILDREN: {} } & { _PARAMS: {} }
        host: {
          _CHILDREN: {
            address: {
              _CHILDREN: {
                domain: {
                  _CHILDREN: {
                    private: { _CHILDREN: {} } & { _PARAMS: {} }
                    public: { _CHILDREN: {} } & { _PARAMS: {} }
                  }
                } & { _PARAMS: {} }
                onion: { _CHILDREN: {} } & { _PARAMS: {} }
              }
            } & { _PARAMS: { host: HostId } }
            binding: { _CHILDREN: {} } & { _PARAMS: { host: HostId } }
          }
        } & { _PARAMS: { package: PackageId } }
      }
    } & { _PARAMS: {} }
    server: {
      _CHILDREN: {
        experimental: { _CHILDREN: {} } & { _PARAMS: {} }
        host: {
          _CHILDREN: {
            address: {
              _CHILDREN: {
                domain: {
                  _CHILDREN: {
                    private: { _CHILDREN: {} } & { _PARAMS: {} }
                    public: { _CHILDREN: {} } & { _PARAMS: {} }
                  }
                } & { _PARAMS: {} }
                onion: { _CHILDREN: {} } & { _PARAMS: {} }
              }
            } & { _PARAMS: {} }
            binding: { _CHILDREN: {} } & { _PARAMS: {} }
          }
        } & { _PARAMS: {} }
        metrics: { _CHILDREN: {} } & { _PARAMS: {} }
      }
    } & { _PARAMS: {} }
    setup: {
      _CHILDREN: {
        attach: {
          _PARAMS: {
            startOsPassword: EncryptedWire | null
            guid: string
            kiosk?: boolean
          }
          _RETURN: { progress: FullProgress; guid: Guid }
        }
        cifs: {
          _CHILDREN: {
            verify: {
              _PARAMS: {
                hostname: string
                path: string
                username: string
                password: EncryptedWire | null
              }
              _RETURN: {
                [key: string]: {
                  hostname: Hostname
                  version: string
                  timestamp: string
                  passwordHash: string | null
                  wrappedKey: string | null
                }
              }
            }
          }
        } & { _PARAMS: {} }
        complete: {
          _PARAMS: {}
          _RETURN: {
            torAddresses: Array<string>
            hostname: string
            lanAddress: string
            rootCa: string
          }
        }
        disk: {
          _CHILDREN: {
            list: {
              _PARAMS: {}
              _RETURN: Array<{
                logicalname: string
                partitionTable: PartitionTable | null
                vendor: string | null
                model: string | null
                partitions: Array<PartitionInfo>
                capacity: bigint
                guid: string | null
              }>
            }
          }
        } & { _PARAMS: {} }
        execute: {
          _PARAMS: {
            startOsLogicalname: string
            startOsPassword: EncryptedWire
            recoverySource: RecoverySource<EncryptedWire> | null
            kiosk?: boolean
          }
          _RETURN: { progress: FullProgress; guid: Guid }
        }
        exit: { _PARAMS: {}; _RETURN: null }
        "get-pubkey": { _PARAMS: {}; _RETURN: unknown }
        restart: { _PARAMS: {}; _RETURN: null }
        status: {
          _PARAMS: {}
          _RETURN:
            | ({ status: "complete" } & SetupResult)
            | ({ status: "running" } & SetupProgress)
            | null
        }
      }
    } & { _PARAMS: {} }
    ssh: { _CHILDREN: {} } & { _PARAMS: {} }
    util: { _CHILDREN: {} } & { _PARAMS: {} }
    wifi: {
      _CHILDREN: {
        available: { _CHILDREN: {} } & { _PARAMS: {} }
        country: { _CHILDREN: {} } & { _PARAMS: {} }
      }
    } & { _PARAMS: {} }
  }
} & { _PARAMS: {} }
