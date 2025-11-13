export type InitApi = {
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
    init: {
      _CHILDREN: {
        "kernel-logs": {
          _CHILDREN: {
            follow: {
              _PARAMS: {} & {
                limit: number | null
                cursor: string | null
                boot: string | number | null
                before: boolean
              }
              _RETURN: { startCursor: string | null; guid: Guid }
            }
          }
        } & {
          _PARAMS: {} & {
            limit: number | null
            cursor: string | null
            boot: string | number | null
            before: boolean
          }
          _RETURN: {
            entries: Array<LogEntry>
            startCursor: string | null
            endCursor: string | null
          }
        }
        logs: {
          _CHILDREN: {
            follow: {
              _PARAMS: {} & {
                limit: number | null
                cursor: string | null
                boot: string | number | null
                before: boolean
              }
              _RETURN: { startCursor: string | null; guid: Guid }
            }
          }
        } & {
          _PARAMS: {} & {
            limit: number | null
            cursor: string | null
            boot: string | number | null
            before: boolean
          }
          _RETURN: {
            entries: Array<LogEntry>
            startCursor: string | null
            endCursor: string | null
          }
        }
        subscribe: {
          _PARAMS: {}
          _RETURN: { progress: FullProgress; guid: Guid }
        }
      }
    } & { _PARAMS: {} }
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
        cifs: { _CHILDREN: {} } & { _PARAMS: {} }
        disk: { _CHILDREN: {} } & { _PARAMS: {} }
      }
    } & { _PARAMS: {} }
    ssh: { _CHILDREN: {} } & { _PARAMS: {} }
    state: { _PARAMS: {}; _RETURN: "error" | "initializing" | "running" }
    util: { _CHILDREN: {} } & { _PARAMS: {} }
    wifi: {
      _CHILDREN: {
        available: { _CHILDREN: {} } & { _PARAMS: {} }
        country: { _CHILDREN: {} } & { _PARAMS: {} }
      }
    } & { _PARAMS: {} }
  }
} & { _PARAMS: {} }
