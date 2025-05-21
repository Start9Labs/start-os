import { ExtendedVersion, VersionRange } from "./exver"
import {
  ActionId,
  ActionInput,
  ActionMetadata,
  SetMainStatus,
  DependencyRequirement,
  CheckDependenciesResult,
  SetHealth,
  BindParams,
  HostId,
  NetInfo,
  Host,
  ExportServiceInterfaceParams,
  ServiceInterface,
  CreateTaskParams,
  MainStatus,
} from "./osBindings"
import {
  PackageId,
  Dependencies,
  ServiceInterfaceId,
  SmtpValue,
  ActionResult,
} from "./types"
import { UrlString } from "./util/getServiceInterface"

/** Used to reach out from the pure js runtime */

export type Effects = {
  child: (name: string) => Effects
  constRetry?: () => void
  isInContext: boolean
  onLeaveContext: (fn: () => void | null | undefined) => void
  clearCallbacks: (
    options: { only: number[] } | { except: number[] },
  ) => Promise<null>

  // action
  action: {
    /** Define an action that can be invoked by a user or service */
    export(options: { id: ActionId; metadata: ActionMetadata }): Promise<null>
    /** Remove all exported actions */
    clear(options: { except: ActionId[] }): Promise<null>
    getInput(options: {
      packageId?: PackageId
      actionId: ActionId
    }): Promise<ActionInput | null>
    run<Input extends Record<string, unknown>>(options: {
      packageId?: PackageId
      actionId: ActionId
      input?: Input
    }): Promise<ActionResult | null>
    createTask(options: CreateTaskParams): Promise<null>
    clearTasks(
      options: { only: string[] } | { except: string[] },
    ): Promise<null>
  }

  // control
  /** restart this service's main function */
  restart(): Promise<null>
  /** stop this service's main function */
  shutdown(): Promise<null>
  /** ask the host os what the service's current status is */
  getStatus(options: {
    packageId?: PackageId
    callback?: () => void
  }): Promise<MainStatus>
  /** indicate to the host os what runstate the service is in */
  setMainStatus(options: SetMainStatus): Promise<null>

  // dependency
  /** Set the dependencies of what the service needs, usually run during the inputSpec action as a best practice */
  setDependencies(options: { dependencies: Dependencies }): Promise<null>
  /** Get the list of the dependencies, both the dynamic set by the effect of setDependencies and the end result any required in the manifest  */
  getDependencies(): Promise<DependencyRequirement[]>
  /** Test whether current dependency requirements are satisfied */
  checkDependencies(options: {
    packageIds?: PackageId[]
  }): Promise<CheckDependenciesResult[]>
  /** mount a volume of a dependency */
  mount(options: {
    location: string
    target: {
      packageId: string
      volumeId: string
      subpath: string | null
      readonly: boolean
    }
  }): Promise<string>
  /** Returns a list of the ids of all installed packages */
  getInstalledPackages(): Promise<string[]>

  // health
  /** sets the result of a health check */
  setHealth(o: SetHealth): Promise<null>

  // subcontainer
  subcontainer: {
    /** A low level api used by SubContainer */
    createFs(options: {
      imageId: string
      name: string | null
    }): Promise<[string, string]>
    /** A low level api used by SubContainer */
    destroyFs(options: { guid: string }): Promise<null>
  }

  // net
  // bind
  /** Creates a host connected to the specified port with the provided options */
  bind(options: BindParams): Promise<null>
  /** Get the port address for a service */
  getServicePortForward(options: {
    packageId?: PackageId
    hostId: HostId
    internalPort: number
  }): Promise<NetInfo>
  /** Removes all network bindings, called in the setupInputSpec */
  clearBindings(options: {
    except: { id: HostId; internalPort: number }[]
  }): Promise<null>
  // host
  /** Returns information about the specified host, if it exists */
  getHostInfo(options: {
    packageId?: PackageId
    hostId: HostId
    callback?: () => void
  }): Promise<Host | null>
  /** Returns the IP address of the container */
  getContainerIp(options: {
    packageId?: PackageId
    callback?: () => void
  }): Promise<string>
  /** Returns the IP address of StartOS */
  getOsIp(): Promise<string>
  // interface
  /** Creates an interface bound to a specific host and port to show to the user */
  exportServiceInterface(options: ExportServiceInterfaceParams): Promise<null>
  /** Returns an exported service interface */
  getServiceInterface(options: {
    packageId?: PackageId
    serviceInterfaceId: ServiceInterfaceId
    callback?: () => void
  }): Promise<ServiceInterface | null>
  /** Returns all exported service interfaces for a package */
  listServiceInterfaces(options: {
    packageId?: PackageId
    callback?: () => void
  }): Promise<Record<ServiceInterfaceId, ServiceInterface>>
  /** Removes all service interfaces */
  clearServiceInterfaces(options: {
    except: ServiceInterfaceId[]
  }): Promise<null>
  // ssl
  /** Returns a PEM encoded fullchain for the hostnames specified */
  getSslCertificate: (options: {
    hostnames: string[]
    algorithm?: "ecdsa" | "ed25519"
    callback?: () => void
  }) => Promise<[string, string, string]>
  /** Returns a PEM encoded private key corresponding to the certificate for the hostnames specified */
  getSslKey: (options: {
    hostnames: string[]
    algorithm?: "ecdsa" | "ed25519"
  }) => Promise<string>

  /** sets the version that this service's data has been migrated to */
  setDataVersion(options: { version: string | null }): Promise<null>
  /** returns the version that this service's data has been migrated to */
  getDataVersion(): Promise<string | null>

  // system
  /** Returns globally configured SMTP settings, if they exist */
  getSystemSmtp(options: { callback?: () => void }): Promise<SmtpValue | null>
}
