export type MainApi = {
  _CHILDREN: {
    auth: {
      _CHILDREN: {
        login: {
          _PARAMS: { password: string; ephemeral: boolean }
          _RETURN: { session: string }
        }
        logout: { _PARAMS: {}; _RETURN: null | null }
        "reset-password": {
          _PARAMS: {
            oldPassword: PasswordType | null
            newPassword: PasswordType | null
          }
          _RETURN: null
        }
        session: {
          _CHILDREN: {
            kill: { _PARAMS: { ids: Array<string> }; _RETURN: null }
            list: {
              _PARAMS: {}
              _RETURN: { current: string | null; sessions: Sessions }
            }
          }
        } & { _PARAMS: {} }
      }
    } & { _PARAMS: {} }
    backup: {
      _CHILDREN: {
        create: {
          _PARAMS: {
            targetId: BackupTargetId
            oldPassword: PasswordType | null
            packageIds: Array<PackageId> | null
            password: PasswordType
          }
          _RETURN: null
        }
        target: {
          _CHILDREN: {
            cifs: {
              _CHILDREN: {
                add: {
                  _PARAMS: {
                    hostname: string
                    path: string
                    username: string
                    password: string | null
                  }
                  _RETURN: {
                    [T in string]:
                      | {
                          type: "disk"
                          vendor: string | null
                          model: string | null
                          logicalname: string
                          label: string | null
                          capacity: bigint
                          used: bigint | null
                          startOs: { [key: string]: StartOsRecoveryInfo }
                          guid: string | null
                        }
                      | ({ type: "cifs" } & CifsBackupTarget)
                  }
                }
                remove: { _PARAMS: { id: BackupTargetId }; _RETURN: null }
                update: {
                  _PARAMS: {
                    id: BackupTargetId
                    hostname: string
                    path: string
                    username: string
                    password: string | null
                  }
                  _RETURN: {
                    [T in string]:
                      | {
                          type: "disk"
                          vendor: string | null
                          model: string | null
                          logicalname: string
                          label: string | null
                          capacity: bigint
                          used: bigint | null
                          startOs: { [key: string]: StartOsRecoveryInfo }
                          guid: string | null
                        }
                      | ({ type: "cifs" } & CifsBackupTarget)
                  }
                }
              }
            } & { _PARAMS: {} }
            info: {
              _PARAMS: {
                targetId: BackupTargetId
                serverId: string
                password: string
              }
              _RETURN: {
                version: string
                timestamp: string | null
                packageBackups: { [key: PackageId]: PackageBackupInfo }
              }
            }
            list: {
              _PARAMS: {}
              _RETURN: {
                [key: string]:
                  | {
                      type: "disk"
                      vendor: string | null
                      model: string | null
                      logicalname: string
                      label: string | null
                      capacity: bigint
                      used: bigint | null
                      startOs: { [key: string]: StartOsRecoveryInfo }
                      guid: string | null
                    }
                  | ({ type: "cifs" } & CifsBackupTarget)
              }
            }
            mount: {
              _PARAMS: {
                targetId: BackupTargetId
                serverId: string | null
                password: string
                allowPartial: boolean
              }
              _RETURN: string
            }
            umount: {
              _PARAMS: { targetId: BackupTargetId | null }
              _RETURN: null
            }
          }
        } & { _PARAMS: {} }
      }
    } & { _PARAMS: {} }
    db: {
      _CHILDREN: {
        apply: { _PARAMS: { expr: string }; _RETURN: null }
        dump: {
          _PARAMS: { pointer: string | null }
          _RETURN: { id: number; value: unknown }
        }
        put: {
          _CHILDREN: {
            ui: { _PARAMS: { pointer: string; value: any }; _RETURN: null }
          }
        } & { _PARAMS: {} }
        subscribe: {
          _PARAMS: { pointer: string | null }
          _RETURN: { dump: { id: number; value: unknown }; guid: Guid }
        }
      }
    } & { _PARAMS: {} }
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
        repair: { _PARAMS: {}; _RETURN: null }
      }
    } & { _PARAMS: {} }
    echo: { _PARAMS: { message: string }; _RETURN: string }
    "git-info": { _PARAMS: {}; _RETURN: string }
    init: { _CHILDREN: {} } & { _PARAMS: {} }
    install: { _CHILDREN: { disk: { _CHILDREN: {} } & { _PARAMS: {} } } } & {
      _PARAMS: {}
    }
    kiosk: {
      _CHILDREN: {
        disable: { _PARAMS: {}; _RETURN: null }
        enable: { _PARAMS: {}; _RETURN: null }
      }
    } & { _PARAMS: {} }
    net: {
      _CHILDREN: {
        acme: {
          _CHILDREN: {
            init: {
              _PARAMS: { provider: AcmeProvider; contact: Array<string> }
              _RETURN: null
            }
            remove: { _PARAMS: { provider: AcmeProvider }; _RETURN: null }
          }
        } & { _PARAMS: {} }
        dns: {
          _CHILDREN: {
            "dump-table": {
              _PARAMS: {}
              _RETURN: { [key: string]: string | null }
            }
            query: { _PARAMS: { fqdn: string }; _RETURN: string | null }
            "set-static": {
              _PARAMS: { servers: Array<string> | null }
              _RETURN: null
            }
          }
        } & { _PARAMS: {} }
        forward: {
          _CHILDREN: {
            "dump-table": {
              _PARAMS: {}
              _RETURN: { [key: number]: ForwardTarget }
            }
          }
        } & { _PARAMS: {} }
        gateway: {
          _CHILDREN: {
            forget: { _PARAMS: { gateway: GatewayId }; _RETURN: null }
            list: {
              _PARAMS: {}
              _RETURN: {
                [key: string]: {
                  name: string | null
                  public: boolean | null
                  secure: boolean | null
                  ipInfo: IpInfo | null
                }
              }
            }
            "set-name": {
              _PARAMS: { id: GatewayId; name: string }
              _RETURN: null
            }
            "set-public": {
              _PARAMS: { gateway: GatewayId; public: boolean | null }
              _RETURN: null
            }
            "unset-public": { _PARAMS: { gateway: GatewayId }; _RETURN: null }
          }
        } & { _PARAMS: {} }
        tor: {
          _CHILDREN: {
            key: {
              _CHILDREN: {
                add: { _PARAMS: { key: Base64 }; _RETURN: string }
                generate: { _PARAMS: {}; _RETURN: string }
                list: { _PARAMS: {}; _RETURN: Array<string> }
              }
            } & { _PARAMS: {} }
            "list-services": { _PARAMS: {}; _RETURN: Array<string> }
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
            reset: {
              _PARAMS: { wipeState: boolean; reason: string }
              _RETURN: null
            }
          }
        } & { _PARAMS: {} }
        tunnel: {
          _CHILDREN: {
            add: {
              _PARAMS: { name: string; config: string; public: boolean }
              _RETURN: string
            }
            remove: { _PARAMS: { id: GatewayId }; _RETURN: null }
          }
        } & { _PARAMS: {} }
        vhost: {
          _CHILDREN: {
            "dump-table": {
              _PARAMS: {}
              _RETURN: { [key: string]: { [key: string]: Array<string> } }
            }
          }
        } & { _PARAMS: {} }
      }
    } & { _PARAMS: {} }
    notification: {
      _CHILDREN: {
        create: {
          _PARAMS: {
            package: PackageId | null
            level: NotificationLevel
            title: string
            message: string
          }
          _RETURN: null
        }
        list: {
          _PARAMS: { before: number | null; limit: number | null }
          _RETURN: Array<{
            id: number
            packageId: PackageId | null
            createdAt: string
            code: number
            level: NotificationLevel
            title: string
            message: string
            data: unknown
            seen: boolean
          }>
        }
        "mark-seen": { _PARAMS: { ids: number[] }; _RETURN: null }
        "mark-seen-before": { _PARAMS: { before: number }; _RETURN: null }
        "mark-unseen": { _PARAMS: { ids: number[] }; _RETURN: null }
        remove: { _PARAMS: { ids: number[] }; _RETURN: null }
        "remove-before": { _PARAMS: { before: number }; _RETURN: null }
      }
    } & { _PARAMS: {} }
    package: {
      _CHILDREN: {
        action: {
          _CHILDREN: {
            "clear-task": {
              _PARAMS: {
                packageId: PackageId
                replayId: ReplayId
                force: boolean
              }
              _RETURN: null
            }
            "get-input": {
              _PARAMS: { packageId: PackageId; actionId: ActionId }
              _RETURN: {
                eventId: Guid
                spec: Record<string, unknown>
                value: Record<string, unknown> | null
              } | null
            }
            run: {
              _PARAMS: {
                packageId: PackageId
                eventId: Guid | null
                actionId: ActionId
                input?: any
              }
              _RETURN:
                | ({ version: "0" } & ActionResultV0)
                | ({ version: "1" } & ActionResultV1)
                | null
            }
          }
        } & { _PARAMS: {} }
        attach: {
          _PARAMS: {
            id: PackageId
            command: string[]
            tty: boolean
            stderrTty: boolean
            ptySize: TermSize | null
            subcontainer: string | null
            name: string | null
            imageId: string | null
            user: string | null
          }
          _RETURN: string
        }
        backup: {
          _CHILDREN: {
            restore: {
              _PARAMS: {
                ids: Array<PackageId>
                targetId: BackupTargetId
                password: string
              }
              _RETURN: null
            }
          }
        } & { _PARAMS: {} }
        "cancel-install": { _PARAMS: { id: PackageId }; _RETURN: null }
        host: {
          _CHILDREN: {
            address: {
              _CHILDREN: {
                domain: {
                  _CHILDREN: {
                    private: {
                      _CHILDREN: {
                        add: { _PARAMS: { fqdn: string }; _RETURN: null }
                        remove: { _PARAMS: { fqdn: string }; _RETURN: null }
                      }
                    } & { _PARAMS: {} }
                    public: {
                      _CHILDREN: {
                        add: {
                          _PARAMS: {
                            fqdn: string
                            acme: AcmeProvider | null
                            gateway: GatewayId
                          }
                          _RETURN: string | null
                        }
                        remove: { _PARAMS: { fqdn: string }; _RETURN: null }
                      }
                    } & { _PARAMS: {} }
                  }
                } & { _PARAMS: {} }
                list: {
                  _PARAMS: {}
                  _RETURN: Array<
                    | { kind: "onion"; address: OnionAddress }
                    | {
                        kind: "domain"
                        address: string
                        public: PublicDomainConfig | null
                        private: boolean
                      }
                  >
                }
                onion: {
                  _CHILDREN: {
                    add: { _PARAMS: { onion: string }; _RETURN: null }
                    remove: { _PARAMS: { onion: string }; _RETURN: null }
                  }
                } & { _PARAMS: {} }
              }
            } & { _PARAMS: { host: HostId } }
            binding: {
              _CHILDREN: {
                list: {
                  _PARAMS: {}
                  _RETURN: {
                    [key: number]: {
                      enabled: boolean
                      options: BindOptions
                      net: NetInfo
                    }
                  }
                }
                "set-gateway-enabled": {
                  _PARAMS: {
                    internalPort: number
                    gateway: GatewayId
                    enabled: boolean | null
                  }
                  _RETURN: null
                }
              }
            } & { _PARAMS: { host: HostId } }
            list: { _PARAMS: {}; _RETURN: Array<string> }
          }
        } & { _PARAMS: { package: PackageId } }
        install: {
          _PARAMS: { registry: string; id: PackageId; version: Version }
          _RETURN: null
        }
        "installed-version": {
          _PARAMS: { id: PackageId }
          _RETURN: string | null
        }
        list: { _PARAMS: {}; _RETURN: Array<unknown> }
        "list-subcontainers": {
          _PARAMS: { id: PackageId }
          _RETURN: { [key: string]: { name: string; imageId: ImageId } }
        }
        logs: {
          _CHILDREN: {
            follow: {
              _PARAMS: { id: PackageId } & {
                limit: number | null
                cursor: string | null
                boot: string | number | null
                before: boolean
              }
              _RETURN: { startCursor: string | null; guid: Guid }
            }
          }
        } & {
          _PARAMS: { id: PackageId } & {
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
        rebuild: { _PARAMS: { id: PackageId }; _RETURN: null }
        restart: { _PARAMS: { id: PackageId }; _RETURN: null }
        sideload: { _PARAMS: {}; _RETURN: { upload: Guid; progress: Guid } }
        start: { _PARAMS: { id: PackageId }; _RETURN: null }
        stats: {
          _PARAMS: {}
          _RETURN: {
            [key: string]: {
              container_id: ContainerId
              memory_usage: MiB
              memory_limit: MiB
            } | null
          }
        }
        stop: { _PARAMS: { id: PackageId }; _RETURN: null }
        uninstall: {
          _PARAMS: { id: PackageId; soft: boolean; force: boolean }
          _RETURN: null
        }
      }
    } & { _PARAMS: {} }
    registry: {
      _CHILDREN: {
        admin: {
          _CHILDREN: {
            add: { _PARAMS: { signer: Guid }; _RETURN: null }
            list: {
              _PARAMS: {}
              _RETURN: {
                [key: string]: {
                  name: string
                  contact: Array<ContactInfo>
                  keys: Array<AnyVerifyingKey>
                }
              }
            }
            remove: { _PARAMS: { signer: Guid }; _RETURN: null }
            signer: {
              _CHILDREN: {
                add: {
                  _PARAMS: {
                    name: string
                    contact: Array<ContactInfo>
                    keys: Array<AnyVerifyingKey>
                  }
                  _RETURN: string
                }
                edit: {
                  _PARAMS: {
                    id: Guid
                    setName: string | null
                    addContact: Array<ContactInfo>
                    addKey: Array<AnyVerifyingKey>
                    removeContact: Array<ContactInfo>
                    removeKey: Array<AnyVerifyingKey>
                  }
                  _RETURN: null
                }
                list: {
                  _PARAMS: {}
                  _RETURN: {
                    [key: string]: {
                      name: string
                      contact: Array<ContactInfo>
                      keys: Array<AnyVerifyingKey>
                    }
                  }
                }
              }
            } & { _PARAMS: {} }
          }
        } & { _PARAMS: {} }
        db: {
          _CHILDREN: {
            apply: {
              _PARAMS: { expr: string; path: string | null }
              _RETURN: null
            }
            dump: {
              _PARAMS: { pointer: string | null }
              _RETURN: { id: number; value: unknown }
            }
          }
        } & { _PARAMS: {} }
        index: {
          _PARAMS: {}
          _RETURN: {
            name: string | null
            icon: DataUrl | null
            package: PackageIndex
            os: OsIndex
            signers: { [key: Guid]: SignerInfo }
          }
        }
        info: {
          _CHILDREN: {
            "set-icon": { _PARAMS: { icon: DataUrl }; _RETURN: null }
            "set-name": { _PARAMS: { name: string }; _RETURN: null }
          }
        } & {
          _PARAMS: {}
          _RETURN: {
            name: string | null
            icon: DataUrl | null
            categories: { [key: string]: Category }
          }
        }
        os: {
          _CHILDREN: {
            asset: {
              _CHILDREN: {
                add: {
                  _CHILDREN: {
                    img: {
                      _PARAMS: {
                        version: string
                        platform: string
                        url: string
                        signature: AnySignature
                        commitment: Blake3Commitment
                      }
                      _RETURN: null
                    }
                    iso: {
                      _PARAMS: {
                        version: string
                        platform: string
                        url: string
                        signature: AnySignature
                        commitment: Blake3Commitment
                      }
                      _RETURN: null
                    }
                    squashfs: {
                      _PARAMS: {
                        version: string
                        platform: string
                        url: string
                        signature: AnySignature
                        commitment: Blake3Commitment
                      }
                      _RETURN: null
                    }
                  }
                } & { _PARAMS: {} }
                get: {
                  _CHILDREN: {
                    img: {
                      _PARAMS: { version: string; platform: string }
                      _RETURN: {
                        publishedAt: string
                        url: string
                        commitment: Blake3Commitment
                        signatures: { [key: AnyVerifyingKey]: AnySignature }
                      }
                    }
                    iso: {
                      _PARAMS: { version: string; platform: string }
                      _RETURN: {
                        publishedAt: string
                        url: string
                        commitment: Blake3Commitment
                        signatures: { [key: AnyVerifyingKey]: AnySignature }
                      }
                    }
                    squashfs: {
                      _PARAMS: { version: string; platform: string }
                      _RETURN: {
                        publishedAt: string
                        url: string
                        commitment: Blake3Commitment
                        signatures: { [key: AnyVerifyingKey]: AnySignature }
                      }
                    }
                  }
                } & { _PARAMS: {} }
                remove: {
                  _CHILDREN: {
                    img: {
                      _PARAMS: { version: string; platform: string }
                      _RETURN: null
                    }
                    iso: {
                      _PARAMS: { version: string; platform: string }
                      _RETURN: null
                    }
                    squashfs: {
                      _PARAMS: { version: string; platform: string }
                      _RETURN: null
                    }
                  }
                } & { _PARAMS: {} }
                sign: {
                  _CHILDREN: {
                    img: {
                      _PARAMS: {
                        version: string
                        platform: string
                        signature: AnySignature
                      }
                      _RETURN: null
                    }
                    iso: {
                      _PARAMS: {
                        version: string
                        platform: string
                        signature: AnySignature
                      }
                      _RETURN: null
                    }
                    squashfs: {
                      _PARAMS: {
                        version: string
                        platform: string
                        signature: AnySignature
                      }
                      _RETURN: null
                    }
                  }
                } & { _PARAMS: {} }
              }
            } & { _PARAMS: {} }
            index: { _PARAMS: {}; _RETURN: { versions: OsVersionInfoMap } }
            version: {
              _CHILDREN: {
                add: {
                  _PARAMS: {
                    version: string
                    headline: string
                    releaseNotes: string
                    sourceVersion: string
                  }
                  _RETURN: null
                }
                get: {
                  _PARAMS: {
                    sourceVersion: string | null
                    targetVersion: string | null
                    serverId: string | null
                    platform: string | null
                  }
                  _RETURN: {
                    [key: string]: {
                      headline: string
                      releaseNotes: string
                      sourceVersion: string
                      authorized: Array<Guid>
                      iso: { [key: string]: RegistryAsset<Blake3Commitment> }
                      squashfs: {
                        [key: string]: RegistryAsset<Blake3Commitment>
                      }
                      img: { [key: string]: RegistryAsset<Blake3Commitment> }
                    }
                  }
                }
                remove: { _PARAMS: { version: string }; _RETURN: null }
                signer: {
                  _CHILDREN: {
                    add: {
                      _PARAMS: { version: string; signer: Guid }
                      _RETURN: null
                    }
                    list: {
                      _PARAMS: { version: string }
                      _RETURN: {
                        [key: string]: {
                          name: string
                          contact: Array<ContactInfo>
                          keys: Array<AnyVerifyingKey>
                        }
                      }
                    }
                    remove: {
                      _PARAMS: { version: string; signer: Guid }
                      _RETURN: null
                    }
                  }
                } & { _PARAMS: {} }
              }
            } & { _PARAMS: {} }
          }
        } & { _PARAMS: {} }
        package: {
          _CHILDREN: {
            add: {
              _PARAMS: {
                url: string
                commitment: MerkleArchiveCommitment
                signature: AnySignature
              }
              _RETURN: null
            }
            category: {
              _CHILDREN: {
                add: { _PARAMS: { id: string; name: string }; _RETURN: null }
                "add-package": {
                  _PARAMS: { id: string; package: PackageId }
                  _RETURN: null
                }
                list: {
                  _PARAMS: {}
                  _RETURN: { [key: string]: { name: string } }
                }
                remove: { _PARAMS: { id: string }; _RETURN: null }
                "remove-package": {
                  _PARAMS: { id: string; package: PackageId }
                  _RETURN: null
                }
              }
            } & { _PARAMS: {} }
            get: {
              _PARAMS: {
                id: PackageId | null
                targetVersion: string | null
                sourceVersion: Version | null
                otherVersions: PackageDetailLevel
              }
              _RETURN: unknown
            }
            index: {
              _PARAMS: {}
              _RETURN: {
                categories: { [key: string]: Category }
                packages: { [key: PackageId]: PackageInfo }
              }
            }
            remove: {
              _PARAMS: { id: PackageId; version: Version }
              _RETURN: null
            }
            signer: {
              _CHILDREN: {
                add: { _PARAMS: { id: PackageId; signer: Guid }; _RETURN: null }
                list: {
                  _PARAMS: { id: PackageId }
                  _RETURN: {
                    [key: string]: {
                      name: string
                      contact: Array<ContactInfo>
                      keys: Array<AnyVerifyingKey>
                    }
                  }
                }
                remove: {
                  _PARAMS: { id: PackageId; signer: Guid }
                  _RETURN: null
                }
              }
            } & { _PARAMS: {} }
          }
        } & { _PARAMS: {} }
      }
    } & { _PARAMS: {} }
    server: {
      _CHILDREN: {
        "clear-smtp": { _PARAMS: {}; _RETURN: null }
        experimental: {
          _CHILDREN: {
            governor: {
              _PARAMS: { set: Governor | null }
              _RETURN: { current: Governor | null; available: Array<Governor> }
            }
            zram: { _PARAMS: { enable: boolean }; _RETURN: null }
          }
        } & { _PARAMS: {} }
        host: {
          _CHILDREN: {
            address: {
              _CHILDREN: {
                domain: {
                  _CHILDREN: {
                    private: {
                      _CHILDREN: {
                        add: { _PARAMS: { fqdn: string }; _RETURN: null }
                        remove: { _PARAMS: { fqdn: string }; _RETURN: null }
                      }
                    } & { _PARAMS: {} }
                    public: {
                      _CHILDREN: {
                        add: {
                          _PARAMS: {
                            fqdn: string
                            acme: AcmeProvider | null
                            gateway: GatewayId
                          }
                          _RETURN: string | null
                        }
                        remove: { _PARAMS: { fqdn: string }; _RETURN: null }
                      }
                    } & { _PARAMS: {} }
                  }
                } & { _PARAMS: {} }
                list: {
                  _PARAMS: {}
                  _RETURN: Array<
                    | { kind: "onion"; address: OnionAddress }
                    | {
                        kind: "domain"
                        address: string
                        public: PublicDomainConfig | null
                        private: boolean
                      }
                  >
                }
                onion: {
                  _CHILDREN: {
                    add: { _PARAMS: { onion: string }; _RETURN: null }
                    remove: { _PARAMS: { onion: string }; _RETURN: null }
                  }
                } & { _PARAMS: {} }
              }
            } & { _PARAMS: {} }
            binding: {
              _CHILDREN: {
                list: {
                  _PARAMS: {}
                  _RETURN: {
                    [key: number]: {
                      enabled: boolean
                      options: BindOptions
                      net: NetInfo
                    }
                  }
                }
                "set-gateway-enabled": {
                  _PARAMS: {
                    internalPort: number
                    gateway: GatewayId
                    enabled: boolean | null
                  }
                  _RETURN: null
                }
              }
            } & { _PARAMS: {} }
          }
        } & { _PARAMS: {} }
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
        metrics: {
          _CHILDREN: {
            follow: { _PARAMS: {}; _RETURN: { guid: Guid; metrics: Metrics } }
          }
        } & {
          _PARAMS: {}
          _RETURN: {
            general: MetricsGeneral
            memory: MetricsMemory
            cpu: MetricsCpu
            disk: MetricsDisk
          }
        }
        rebuild: { _PARAMS: {}; _RETURN: null }
        restart: { _PARAMS: {}; _RETURN: null }
        "set-smtp": {
          _PARAMS: {
            server: string
            port: number
            from: string
            login: string
            password: string | null
          }
          _RETURN: null
        }
        shutdown: { _PARAMS: {}; _RETURN: null }
        "test-smtp": {
          _PARAMS: {
            server: string
            port: number
            from: string
            to: string
            login: string
            password: string
          }
          _RETURN: null
        }
        time: { _PARAMS: {}; _RETURN: { now: string; uptime: bigint } }
        update: {
          _PARAMS: {
            registry: string
            target: string | null
            progress: boolean
          }
          _RETURN: { target: string | null; progress: string | null }
        }
        "update-firmware": { _PARAMS: {}; _RETURN: boolean }
      }
    } & { _PARAMS: {} }
    setup: {
      _CHILDREN: {
        cifs: { _CHILDREN: {} } & { _PARAMS: {} }
        disk: { _CHILDREN: {} } & { _PARAMS: {} }
      }
    } & { _PARAMS: {} }
    ssh: {
      _CHILDREN: {
        add: {
          _PARAMS: { key: SshPubKey }
          _RETURN: {
            alg: string
            fingerprint: string
            hostname: string
            createdAt: string
          }
        }
        list: {
          _PARAMS: {}
          _RETURN: Array<{
            alg: string
            fingerprint: string
            hostname: string
            createdAt: string
          }>
        }
        remove: { _PARAMS: { fingerprint: string }; _RETURN: null }
      }
    } & { _PARAMS: {} }
    state: { _PARAMS: {}; _RETURN: "error" | "initializing" | "running" }
    tunnel: {
      _CHILDREN: {
        auth: {
          _CHILDREN: {
            key: {
              _CHILDREN: {
                add: {
                  _PARAMS: { name: string; key: AnyVerifyingKey }
                  _RETURN: null
                }
                list: {
                  _PARAMS: {}
                  _RETURN: { [key: string]: { name: string } }
                }
                remove: { _PARAMS: { key: AnyVerifyingKey }; _RETURN: null }
              }
            } & { _PARAMS: {} }
            login: {
              _PARAMS: { password: string; ephemeral: boolean }
              _RETURN: { session: string }
            }
            logout: { _PARAMS: {}; _RETURN: null | null }
            "set-password": { _PARAMS: { password: string }; _RETURN: null }
          }
        } & { _PARAMS: {} }
        db: {
          _CHILDREN: {
            apply: {
              _PARAMS: { expr: string; path: string | null }
              _RETURN: null
            }
            dump: {
              _PARAMS: { pointer: string | null }
              _RETURN: { id: number; value: unknown }
            }
            subscribe: {
              _PARAMS: { pointer: string | null }
              _RETURN: { dump: { id: number; value: unknown }; guid: Guid }
            }
          }
        } & { _PARAMS: {} }
        device: {
          _CHILDREN: {
            add: {
              _PARAMS: { subnet: string; name: string; ip: string | null }
              _RETURN: null
            }
            list: {
              _PARAMS: { subnet: string }
              _RETURN: { name: string; clients: WgSubnetClients }
            }
            remove: { _PARAMS: { subnet: string; ip: string }; _RETURN: null }
            "show-config": {
              _PARAMS: { subnet: string; ip: string; wanAddr: string | null }
              _RETURN: string
            }
          }
        } & { _PARAMS: {} }
        "port-forward": {
          _CHILDREN: {
            add: { _PARAMS: { source: string; target: string }; _RETURN: null }
            remove: { _PARAMS: { source: string }; _RETURN: null }
          }
        } & { _PARAMS: {} }
        subnet: {
          _CHILDREN: {
            add: { _PARAMS: { name: string }; _RETURN: null }
            remove: { _PARAMS: {}; _RETURN: null }
          }
        } & { _PARAMS: { subnet: string } }
        web: {
          _CHILDREN: {
            disable: { _PARAMS: {}; _RETURN: null }
            enable: { _PARAMS: {}; _RETURN: null }
            "generate-certificate": {
              _PARAMS: { subject: Array<string> }
              _RETURN: string
            }
            "get-available-ips": { _PARAMS: {}; _RETURN: Array<string> }
            "get-certificate": { _PARAMS: {}; _RETURN: string | null }
            "get-listen": { _PARAMS: {}; _RETURN: string | null }
            "import-certificate": {
              _PARAMS: { key: Pem; cert: Pem }
              _RETURN: null
            }
            reset: { _PARAMS: {}; _RETURN: null }
            "set-listen": { _PARAMS: { listen: string }; _RETURN: null }
          }
        } & { _PARAMS: {} }
      }
    } & { _PARAMS: {} }
    util: { _CHILDREN: {} } & { _PARAMS: {} }
    wifi: {
      _CHILDREN: {
        add: { _PARAMS: { ssid: string; password: string }; _RETURN: null }
        available: {
          _CHILDREN: {
            get: {
              _PARAMS: {}
              _RETURN: Array<{
                ssid: Ssid
                strength: SignalStrength
                security: Array<string>
              }>
            }
          }
        } & { _PARAMS: {} }
        connect: { _PARAMS: { ssid: string }; _RETURN: null }
        country: {
          _CHILDREN: { set: { _PARAMS: { country: string }; _RETURN: null } }
        } & { _PARAMS: {} }
        get: {
          _PARAMS: {}
          _RETURN: {
            ssids: { [key: Ssid]: SignalStrength }
            connected: Ssid | null
            country: sttring | null
            ethernet: boolean
            availableWifi: Array<WifiListOut>
          }
        }
        remove: { _PARAMS: { ssid: string }; _RETURN: null }
        "set-enabled": { _PARAMS: { enabled: boolean }; _RETURN: null }
      }
    } & { _PARAMS: {} }
  }
} & { _PARAMS: {} }
