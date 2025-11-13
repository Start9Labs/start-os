export type ServiceEffectsApi = {
  _CHILDREN: {
    action: {
      _CHILDREN: {
        clear: { _PARAMS: { except: Array<ActionId> }; _RETURN: null }
        "clear-tasks": {
          _PARAMS: { only: string[] } | { except: string[] }
          _RETURN: null
        }
        "create-task": {
          _PARAMS: {
            replayId: ReplayId
            packageId: PackageId
            actionId: ActionId
            severity: TaskSeverity
            reason?: string
            when?: TaskTrigger
            input?: TaskInput
          }
          _RETURN: null
        }
        export: {
          _PARAMS: { id: ActionId; metadata: ActionMetadata }
          _RETURN: null
        }
        "get-input": {
          _PARAMS: { packageId?: PackageId; actionId: ActionId }
          _RETURN: {
            eventId: Guid
            spec: Record<string, unknown>
            value: Record<string, unknown> | null
          } | null
        }
        run: {
          _PARAMS: {
            packageId?: PackageId
            actionId: ActionId
            input?: unknown
          }
          _RETURN:
            | ({ version: "0" } & ActionResultV0)
            | ({ version: "1" } & ActionResultV1)
            | null
        }
      }
    } & { _PARAMS: {} }
    bind: {
      _PARAMS: {
        id: HostId
        internalPort: number
        preferredExternalPort: number
        addSsl: AddSslOptions | null
        secure: Security | null
      }
      _RETURN: null
    }
    "check-dependencies": {
      _PARAMS: { packageIds?: Array<PackageId> }
      _RETURN: Array<{
        packageId: PackageId
        title: string | null
        installedVersion: Version | null
        satisfies: Array<Version>
        isRunning: boolean
        tasks: { [key: ReplayId]: TaskEntry }
        healthChecks: { [key: HealthCheckId]: NamedHealthCheckResult }
      }>
    }
    "clear-bindings": { _PARAMS: { except: Array<BindId> }; _RETURN: null }
    "clear-callbacks": {
      _PARAMS: { only: number[] } | { except: number[] }
      _RETURN: null
    }
    "clear-service-interfaces": {
      _PARAMS: { except: Array<ServiceInterfaceId> }
      _RETURN: null
    }
    echo: { _PARAMS: { message: string }; _RETURN: string }
    "export-service-interface": {
      _PARAMS: {
        id: ServiceInterfaceId
        name: string
        description: string
        masked: boolean
        addressInfo: AddressInfo
        type: ServiceInterfaceType
      }
      _RETURN: null
    }
    "get-container-ip": {
      _PARAMS: { packageId?: PackageId; callback?: CallbackId }
      _RETURN: string | null
    }
    "get-data-version": { _PARAMS: {}; _RETURN: string | null }
    "get-dependencies": {
      _PARAMS: {}
      _RETURN: Array<
        | {
            kind: "running"
            id: PackageId
            healthChecks: Array<HealthCheckId>
            versionRange: string
          }
        | { kind: "exists"; id: PackageId; versionRange: string }
      >
    }
    "get-host-info": {
      _PARAMS: { hostId: HostId; packageId?: PackageId; callback?: CallbackId }
      _RETURN: {
        bindings: { [key: number]: BindInfo }
        onions: string[]
        publicDomains: { [key: string]: PublicDomainConfig }
        privateDomains: Array<string>
        /**
         * COMPUTED: NetService::update
         */
        hostnameInfo: { [key: number]: Array<HostnameInfo> }
      } | null
    }
    "get-installed-packages": { _PARAMS: {}; _RETURN: Array<string> }
    "get-os-ip": { _PARAMS: {}; _RETURN: string }
    "get-service-interface": {
      _PARAMS: {
        packageId?: PackageId
        serviceInterfaceId: ServiceInterfaceId
        callback?: CallbackId
      }
      _RETURN: {
        id: ServiceInterfaceId
        name: string
        description: string
        masked: boolean
        addressInfo: AddressInfo
        type: ServiceInterfaceType
      } | null
    }
    "get-service-port-forward": {
      _PARAMS: { packageId?: PackageId; hostId: HostId; internalPort: number }
      _RETURN: {
        privateDisabled: Array<GatewayId>
        publicEnabled: Array<GatewayId>
        assignedPort: number | null
        assignedSslPort: number | null
      }
    }
    "get-ssl-certificate": {
      _PARAMS: {
        hostnames: string[]
        algorithm?: Algorithm
        callback?: CallbackId
      }
      _RETURN: Array<string>
    }
    "get-ssl-key": {
      _PARAMS: { hostnames: string[]; algorithm?: Algorithm }
      _RETURN: string
    }
    "get-status": {
      _PARAMS: { packageId?: PackageId; callback?: CallbackId }
      _RETURN:
        | {
            main: "error"
            onRebuild: StartStop
            message: string
            debug: string | null
          }
        | { main: "stopped" }
        | { main: "restarting" }
        | { main: "stopping" }
        | {
            main: "starting"
            health: { [key: HealthCheckId]: NamedHealthCheckResult }
          }
        | {
            main: "running"
            started: string
            health: { [key: HealthCheckId]: NamedHealthCheckResult }
          }
        | { main: "backingUp"; onComplete: StartStop }
    }
    "get-system-smtp": {
      _PARAMS: { callback: CallbackId | null }
      _RETURN: {
        server: string
        port: number
        from: string
        login: string
        password: string | null
      } | null
    }
    "git-info": { _PARAMS: {}; _RETURN: string }
    "list-service-interfaces": {
      _PARAMS: { packageId?: PackageId; callback?: CallbackId }
      _RETURN: {
        [key: string]: {
          id: ServiceInterfaceId
          name: string
          description: string
          masked: boolean
          addressInfo: AddressInfo
          type: ServiceInterfaceType
        }
      }
    }
    mount: { _PARAMS: { location: string; target: MountTarget }; _RETURN: null }
    rebuild: { _PARAMS: {}; _RETURN: null }
    restart: { _PARAMS: { eventId: Guid }; _RETURN: null }
    "set-data-version": { _PARAMS: { version: string }; _RETURN: null }
    "set-dependencies": {
      _PARAMS: { dependencies: Array<DependencyRequirement> }
      _RETURN: null
    }
    "set-health": {
      _PARAMS: { id: HealthCheckId; name: string } & (
        | { result: "success"; message: string | null }
        | { result: "disabled"; message: string | null }
        | { result: "starting"; message: string | null }
        | { result: "loading"; message: string }
        | { result: "failure"; message: string }
      )
      _RETURN: null
    }
    "set-main-status": {
      _PARAMS: { status: SetMainStatusStatus }
      _RETURN: null
    }
    shutdown: { _PARAMS: { eventId: Guid }; _RETURN: null }
    subcontainer: {
      _CHILDREN: {
        "create-fs": {
          _PARAMS: { imageId: ImageId; name: string | null }
          _RETURN: [string, string]
        }
        "destroy-fs": { _PARAMS: { guid: Guid }; _RETURN: null }
      }
    } & { _PARAMS: {} }
  }
} & { _PARAMS: {} }
