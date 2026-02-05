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
  MountParams,
  StatusInfo,
  Manifest,
} from "./osBindings"
import {
  PackageId,
  Dependencies,
  ServiceInterfaceId,
  SmtpValue,
  ActionResult,
} from "./types"

/**
 * The Effects interface is the primary mechanism for a StartOS service to interact
 * with the host operating system. All system operations—file I/O, networking,
 * health reporting, dependency management, and more—are performed through Effects.
 *
 * Effects are passed to all lifecycle functions (main, init, uninit, actions, etc.)
 * and provide a controlled, sandboxed API for service operations.
 *
 * @example
 * ```typescript
 * export const main = sdk.setupMain(async ({ effects }) => {
 *   // Use effects to interact with the system
 *   const smtp = await effects.getSystemSmtp({})
 *   await effects.setHealth({ name: 'server', result: { result: 'success' } })
 * })
 * ```
 */
export type Effects = {
  /**
   * A unique identifier for the current event/request context.
   * Returns null when not executing within an event context.
   * Useful for correlating logs and tracking request flows.
   */
  readonly eventId: string | null

  /**
   * Creates a child Effects context with a namespaced identifier.
   * Child contexts inherit the parent's capabilities but have their own
   * event tracking namespace, useful for organizing complex operations.
   *
   * @param name - The name to append to the context namespace
   * @returns A new Effects instance scoped to the child context
   */
  child: (name: string) => Effects

  /**
   * Internal retry mechanism for `.const()` operations.
   * Called automatically when a const operation needs to be retried
   * due to dependency changes. Not typically used directly by service developers.
   */
  constRetry?: () => void

  /**
   * Indicates whether the Effects instance is currently within a valid execution context.
   * Returns false if the context has been destroyed or left.
   */
  isInContext: boolean

  /**
   * Registers a cleanup callback to be invoked when leaving the current context.
   * Use this to clean up resources, close connections, or perform other teardown
   * operations when the service or action completes.
   *
   * @param fn - Cleanup function to execute on context exit. Can return void, null, or undefined.
   *
   * @example
   * ```typescript
   * effects.onLeaveContext(() => {
   *   socket.close()
   *   clearInterval(healthCheckInterval)
   * })
   * ```
   */
  onLeaveContext: (fn: () => void | null | undefined) => void

  /**
   * Clears registered callbacks by their internal IDs.
   * Used for cleanup when callbacks are no longer needed.
   *
   * @param options - Either `{ only: number[] }` to clear specific callbacks,
   *                  or `{ except: number[] }` to clear all except the specified ones
   * @returns Promise resolving to null on completion
   */
  clearCallbacks: (
    options: { only: number[] } | { except: number[] },
  ) => Promise<null>

  /**
   * Action-related methods for defining, invoking, and managing user-callable operations.
   * Actions appear in the StartOS UI and can be triggered by users or other services.
   */
  action: {
    /**
     * Exports an action to make it available in the StartOS UI.
     * Call this during initialization to register actions that users can invoke.
     *
     * @param options.id - Unique identifier for the action
     * @param options.metadata - Action configuration including name, description, input spec, and visibility
     * @returns Promise resolving to null on success
     */
    export(options: { id: ActionId; metadata: ActionMetadata }): Promise<null>

    /**
     * Removes all exported actions except those specified.
     * Typically called during initialization before re-registering current actions.
     *
     * @param options.except - Array of action IDs to keep (not remove)
     * @returns Promise resolving to null on success
     */
    clear(options: { except: ActionId[] }): Promise<null>

    /**
     * Retrieves the previously submitted input for an action.
     * Useful for pre-filling forms with the last-used values.
     *
     * @param options.packageId - Package ID (defaults to current package if omitted)
     * @param options.actionId - The action whose input to retrieve
     * @returns Promise resolving to the stored input, or null if none exists
     */
    getInput(options: {
      packageId?: PackageId
      actionId: ActionId
    }): Promise<ActionInput | null>

    /**
     * Programmatically invokes an action on this or another service.
     * Enables service-to-service communication and automation.
     *
     * @param options.packageId - Target package ID (defaults to current package if omitted)
     * @param options.actionId - The action to invoke
     * @param options.input - Input data matching the action's input specification
     * @returns Promise resolving to the action result, or null if the action doesn't exist
     */
    run<Input extends Record<string, unknown>>(options: {
      packageId?: PackageId
      actionId: ActionId
      input?: Input
    }): Promise<ActionResult | null>

    /**
     * Creates a task that appears in the StartOS UI task list.
     * Tasks are used for long-running operations or required setup steps
     * that need user attention (e.g., "Create admin user", "Configure backup").
     *
     * @param options - Task configuration including ID, name, description, and completion criteria
     * @returns Promise resolving to null on success
     */
    createTask(options: CreateTaskParams): Promise<null>

    /**
     * Removes tasks from the UI task list.
     *
     * @param options - Either `{ only: string[] }` to remove specific tasks,
     *                  or `{ except: string[] }` to remove all except specified tasks
     * @returns Promise resolving to null on success
     */
    clearTasks(
      options: { only: string[] } | { except: string[] },
    ): Promise<null>
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // Control Methods - Manage service lifecycle
  // ─────────────────────────────────────────────────────────────────────────────

  /**
   * Restarts this service's main function.
   * The current main process will be terminated and a new one started.
   * Use this after configuration changes that require a restart to take effect.
   *
   * @returns Promise resolving to null when the restart has been initiated
   */
  restart(): Promise<null>

  /**
   * Gracefully stops this service's main function.
   * The daemon will receive a termination signal and be given time to clean up.
   *
   * @returns Promise resolving to null when shutdown has been initiated
   */
  shutdown(): Promise<null>

  /**
   * Queries the current status of a service from the host OS.
   *
   * @param options.packageId - Package to query (defaults to current package if omitted)
   * @param options.callback - Optional callback invoked when status changes (for reactive updates)
   * @returns Promise resolving to the service's current status information
   */
  getStatus(options: {
    packageId?: PackageId
    callback?: () => void
  }): Promise<StatusInfo>

  /**
   * @deprecated This method is deprecated and should not be used.
   * Health status is now managed through the health check system.
   *
   * Previously used to manually indicate the service's run state to the host OS.
   */
  setMainStatus(options: SetMainStatus): Promise<null>

  // ─────────────────────────────────────────────────────────────────────────────
  // Dependency Methods - Manage service dependencies and inter-service communication
  // ─────────────────────────────────────────────────────────────────────────────

  /**
   * Declares the runtime dependencies this service requires.
   * Dependencies can be marked as "running" (must be actively running) or "exists" (must be installed).
   * Call this during initialization to ensure dependencies are satisfied before the service starts.
   *
   * @param options.dependencies - Array of dependency requirements with package IDs, version ranges, and dependency kind
   * @returns Promise resolving to null on success
   *
   * @example
   * ```typescript
   * await effects.setDependencies({
   *   dependencies: [
   *     { packageId: 'bitcoind', versionRange: '>=25.0.0', kind: 'running' },
   *     { packageId: 'lnd', versionRange: '>=0.16.0', kind: 'exists' }
   *   ]
   * })
   * ```
   */
  setDependencies(options: { dependencies: Dependencies }): Promise<null>

  /**
   * Retrieves the complete list of dependencies for this service.
   * Includes both statically declared dependencies from the manifest
   * and dynamically set dependencies from setDependencies().
   *
   * @returns Promise resolving to array of all dependency requirements
   */
  getDependencies(): Promise<DependencyRequirement[]>

  /**
   * Tests whether the specified or all dependencies are currently satisfied.
   * Use this to verify dependencies are met before performing operations that require them.
   *
   * @param options.packageIds - Specific packages to check (checks all dependencies if omitted)
   * @returns Promise resolving to array of check results indicating satisfaction status
   */
  checkDependencies(options: {
    packageIds?: PackageId[]
  }): Promise<CheckDependenciesResult[]>

  /**
   * Mounts a volume from a dependency service into this service's filesystem.
   * Enables read-only or read-write access to another service's data.
   *
   * @param options - Mount configuration including dependency ID, volume ID, mountpoint, and access mode
   * @returns Promise resolving to the mount path
   *
   * @example
   * ```typescript
   * // Mount bitcoind's data directory for read access
   * const mountPath = await effects.mount({
   *   dependencyId: 'bitcoind',
   *   volumeId: 'main',
   *   mountpoint: '/mnt/bitcoin',
   *   readonly: true
   * })
   * ```
   */
  mount(options: MountParams): Promise<string>

  /**
   * Returns the package IDs of all services currently installed on the system.
   * Useful for discovering available services for optional integrations.
   *
   * @returns Promise resolving to array of installed package IDs
   */
  getInstalledPackages(): Promise<string[]>

  /**
   * Retrieves the manifest of another installed service.
   * Use this to inspect another service's metadata, version, or capabilities.
   *
   * @param options.packageId - The package ID to retrieve the manifest for
   * @param options.callback - Optional callback invoked when the manifest changes (for reactive updates)
   * @returns Promise resolving to the service's manifest
   */
  getServiceManifest(options: {
    packageId: PackageId
    callback?: () => void
  }): Promise<Manifest>

  // ─────────────────────────────────────────────────────────────────────────────
  // Health Methods - Report service health status
  // ─────────────────────────────────────────────────────────────────────────────

  /**
   * Reports the result of a health check to the StartOS UI.
   * Health checks appear in the service's status panel and indicate operational status.
   *
   * @param o - Health check result including the check name and result status (success/failure/starting)
   * @returns Promise resolving to null on success
   *
   * @example
   * ```typescript
   * await effects.setHealth({
   *   name: 'web-interface',
   *   result: { result: 'success', message: 'Web UI is accessible' }
   * })
   * ```
   */
  setHealth(o: SetHealth): Promise<null>

  // ─────────────────────────────────────────────────────────────────────────────
  // Subcontainer Methods - Low-level container filesystem management
  // ─────────────────────────────────────────────────────────────────────────────

  /**
   * Low-level APIs for managing subcontainer filesystems.
   * These are typically used internally by the SubContainer class.
   * Service developers should use `sdk.SubContainer.of()` instead.
   */
  subcontainer: {
    /**
     * Creates a new container filesystem from a Docker image.
     * This is a low-level API - prefer using `sdk.SubContainer.of()` for most use cases.
     *
     * @param options.imageId - The Docker image ID to create the filesystem from
     * @param options.name - Optional name for the container (null for anonymous)
     * @returns Promise resolving to a tuple of [guid, rootPath] for the created filesystem
     */
    createFs(options: {
      imageId: string
      name: string | null
    }): Promise<[string, string]>

    /**
     * Destroys a container filesystem and cleans up its resources.
     * This is a low-level API - SubContainer handles cleanup automatically.
     *
     * @param options.guid - The unique identifier of the filesystem to destroy
     * @returns Promise resolving to null on success
     */
    destroyFs(options: { guid: string }): Promise<null>
  }

  // ─────────────────────────────────────────────────────────────────────────────
  // Network Methods - Port binding, host info, and network configuration
  // ─────────────────────────────────────────────────────────────────────────────

  /**
   * Binds a network port and creates a host entry for the service.
   * This makes the port accessible via the StartOS networking layer (Tor, LAN, etc.).
   *
   * @param options - Binding configuration including host ID, port, protocol, and network options
   * @returns Promise resolving to null on success
   *
   * @example
   * ```typescript
   * await effects.bind({
   *   id: 'webui',
   *   internalPort: 8080,
   *   protocol: 'http'
   * })
   * ```
   */
  bind(options: BindParams): Promise<null>

  /**
   * Gets the network address information for accessing a service's port.
   * Use this to discover how to connect to another service's exposed port.
   *
   * @param options.packageId - Target package (defaults to current package if omitted)
   * @param options.hostId - The host identifier for the binding
   * @param options.internalPort - The internal port number
   * @returns Promise resolving to network info including addresses and ports
   */
  getServicePortForward(options: {
    packageId?: PackageId
    hostId: HostId
    internalPort: number
  }): Promise<NetInfo>

  /**
   * Removes all network bindings except those specified.
   * Typically called during initialization to clean up stale bindings before re-registering.
   *
   * @param options.except - Array of bindings to preserve (by host ID and port)
   * @returns Promise resolving to null on success
   */
  clearBindings(options: {
    except: { id: HostId; internalPort: number }[]
  }): Promise<null>

  // ─────────────────────────────────────────────────────────────────────────────
  // Host Info Methods - Query network host and address information
  // ─────────────────────────────────────────────────────────────────────────────

  /**
   * Retrieves detailed information about a network host binding.
   *
   * @param options.packageId - Target package (defaults to current package if omitted)
   * @param options.hostId - The host identifier to query
   * @param options.callback - Optional callback invoked when host info changes (for reactive updates)
   * @returns Promise resolving to host information, or null if the host doesn't exist
   */
  getHostInfo(options: {
    packageId?: PackageId
    hostId: HostId
    callback?: () => void
  }): Promise<Host | null>

  /**
   * Returns the internal IP address of the service's container.
   * Useful for configuring services that need to know their own network address.
   *
   * @param options.packageId - Target package (defaults to current package if omitted)
   * @param options.callback - Optional callback invoked when the IP changes
   * @returns Promise resolving to the container's IP address string
   */
  getContainerIp(options: {
    packageId?: PackageId
    callback?: () => void
  }): Promise<string>

  /**
   * Returns the IP address of the StartOS host system.
   * Useful for services that need to communicate with the host or other system services.
   *
   * @returns Promise resolving to the StartOS IP address string
   */
  getOsIp(): Promise<string>

  // ─────────────────────────────────────────────────────────────────────────────
  // Service Interface Methods - Expose and discover service endpoints
  // ─────────────────────────────────────────────────────────────────────────────

  /**
   * Exports a service interface that appears in the StartOS UI.
   * Service interfaces are the user-visible endpoints (web UIs, APIs, etc.) that users
   * can click to access the service.
   *
   * @param options - Interface configuration including ID, name, description, type, and associated host/port
   * @returns Promise resolving to null on success
   *
   * @example
   * ```typescript
   * await effects.exportServiceInterface({
   *   id: 'webui',
   *   name: 'Web Interface',
   *   description: 'Access the web dashboard',
   *   type: 'ui',
   *   hostId: 'main',
   *   internalPort: 8080
   * })
   * ```
   */
  exportServiceInterface(options: ExportServiceInterfaceParams): Promise<null>

  /**
   * Retrieves information about an exported service interface.
   *
   * @param options.packageId - Target package (defaults to current package if omitted)
   * @param options.serviceInterfaceId - The interface identifier to query
   * @param options.callback - Optional callback invoked when the interface changes
   * @returns Promise resolving to the interface info, or null if it doesn't exist
   */
  getServiceInterface(options: {
    packageId?: PackageId
    serviceInterfaceId: ServiceInterfaceId
    callback?: () => void
  }): Promise<ServiceInterface | null>

  /**
   * Lists all exported service interfaces for a package.
   * Useful for discovering what endpoints another service exposes.
   *
   * @param options.packageId - Target package (defaults to current package if omitted)
   * @param options.callback - Optional callback invoked when any interface changes
   * @returns Promise resolving to a record mapping interface IDs to their configurations
   */
  listServiceInterfaces(options: {
    packageId?: PackageId
    callback?: () => void
  }): Promise<Record<ServiceInterfaceId, ServiceInterface>>

  /**
   * Removes all service interfaces except those specified.
   * Typically called during initialization to clean up stale interfaces before re-registering.
   *
   * @param options.except - Array of interface IDs to preserve
   * @returns Promise resolving to null on success
   */
  clearServiceInterfaces(options: {
    except: ServiceInterfaceId[]
  }): Promise<null>

  // ─────────────────────────────────────────────────────────────────────────────
  // SSL Methods - Manage TLS certificates
  // ─────────────────────────────────────────────────────────────────────────────

  /**
   * Retrieves a PEM-encoded SSL certificate chain for the specified hostnames.
   * StartOS automatically manages certificate generation and renewal.
   *
   * @param options.hostnames - Array of hostnames the certificate should cover
   * @param options.algorithm - Signing algorithm: "ecdsa" (default) or "ed25519"
   * @param options.callback - Optional callback invoked when the certificate is renewed
   * @returns Promise resolving to a tuple of [certificate, chain, fullchain] PEM strings
   */
  getSslCertificate: (options: {
    hostnames: string[]
    algorithm?: "ecdsa" | "ed25519"
    callback?: () => void
  }) => Promise<[string, string, string]>

  /**
   * Retrieves the PEM-encoded private key corresponding to the SSL certificate.
   *
   * @param options.hostnames - Array of hostnames (must match a previous getSslCertificate call)
   * @param options.algorithm - Signing algorithm: "ecdsa" (default) or "ed25519"
   * @returns Promise resolving to the private key PEM string
   */
  getSslKey: (options: {
    hostnames: string[]
    algorithm?: "ecdsa" | "ed25519"
  }) => Promise<string>

  // ─────────────────────────────────────────────────────────────────────────────
  // Data Version Methods - Track data migration state
  // ─────────────────────────────────────────────────────────────────────────────

  /**
   * Sets the version that this service's data has been migrated to.
   * Used by the version migration system to track which migrations have been applied.
   * Service developers typically don't call this directly - it's managed by the version graph.
   *
   * @param options.version - The version string to record, or null to clear
   * @returns Promise resolving to null on success
   */
  setDataVersion(options: { version: string | null }): Promise<null>

  /**
   * Returns the version that this service's data has been migrated to.
   * Used to determine which migrations need to be applied during updates.
   *
   * @returns Promise resolving to the current data version string, or null if not set
   */
  getDataVersion(): Promise<string | null>

  // ─────────────────────────────────────────────────────────────────────────────
  // System Methods - Access system-wide configuration
  // ─────────────────────────────────────────────────────────────────────────────

  /**
   * Retrieves the globally configured SMTP settings from StartOS.
   * Users can configure SMTP in the StartOS settings for services to use for sending emails.
   *
   * @param options.callback - Optional callback invoked when SMTP settings change (for reactive updates)
   * @returns Promise resolving to SMTP configuration, or null if not configured
   *
   * @example
   * ```typescript
   * const smtp = await effects.getSystemSmtp({})
   * if (smtp) {
   *   console.log(`SMTP server: ${smtp.server}:${smtp.port}`)
   *   console.log(`From address: ${smtp.from}`)
   * }
   * ```
   */
  getSystemSmtp(options: { callback?: () => void }): Promise<SmtpValue | null>
}
