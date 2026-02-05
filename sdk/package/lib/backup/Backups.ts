/**
 * @module Backups
 *
 * Provides backup and restore functionality for StartOS services.
 * The Backups class uses rsync to efficiently synchronize service data
 * to and from backup destinations.
 *
 * @example
 * ```typescript
 * // Simple backup of all volumes
 * export const createBackup = Backups.ofVolumes<Manifest>('main', 'config')
 *
 * // Advanced backup with hooks
 * export const createBackup = Backups.ofVolumes<Manifest>('main')
 *   .setPreBackup(async (effects) => {
 *     // Stop accepting writes before backup
 *     await stopService()
 *   })
 *   .setPostBackup(async (effects) => {
 *     // Resume after backup
 *     await startService()
 *   })
 * ```
 */
import * as T from "../../../base/lib/types"
import * as child_process from "child_process"
import * as fs from "fs/promises"
import { Affine, asError } from "../util"
import { ExtendedVersion, VersionRange } from "../../../base/lib"
import { InitKind, InitScript } from "../../../base/lib/inits"

/**
 * Default sync options for backup/restore operations.
 * - `delete: true` - Remove files in destination that don't exist in source
 * - `exclude: []` - No exclusions by default
 */
export const DEFAULT_OPTIONS: T.SyncOptions = {
  delete: true,
  exclude: [],
}

/**
 * Configuration for a single backup synchronization operation.
 * Maps a source data path to a backup destination path.
 *
 * @typeParam Volumes - The volume ID type from the manifest
 */
export type BackupSync<Volumes extends string> = {
  /** Source path on the data volume (e.g., "/media/startos/volumes/main/data") */
  dataPath: `/media/startos/volumes/${Volumes}/${string}`
  /** Destination path in the backup (e.g., "/media/startos/backup/volumes/main/") */
  backupPath: `/media/startos/backup/${string}`
  /** Sync options applied to both backup and restore */
  options?: Partial<T.SyncOptions>
  /** Sync options applied only during backup (merged with options) */
  backupOptions?: Partial<T.SyncOptions>
  /** Sync options applied only during restore (merged with options) */
  restoreOptions?: Partial<T.SyncOptions>
}

/**
 * Effects type with backup context marker.
 * Provides type safety to prevent backup operations in non-backup contexts.
 */
export type BackupEffects = T.Effects & Affine<"Backups">

/**
 * Manages backup and restore operations for a StartOS service.
 *
 * Exposed via `sdk.Backups`. The Backups class provides a fluent API for
 * configuring which volumes to back up and optional hooks for pre/post
 * backup/restore operations. It uses rsync for efficient incremental backups.
 *
 * Common usage patterns:
 * - Simple: `sdk.Backups.ofVolumes('main')` - Back up the main volume
 * - Multiple volumes: `sdk.Backups.ofVolumes('main', 'config', 'logs')`
 * - With hooks: Add pre/post callbacks for database dumps, service stops, etc.
 * - Custom paths: Use `addSync()` for non-standard backup mappings
 *
 * @typeParam M - The service manifest type for type-safe volume names
 *
 * @example
 * ```typescript
 * // In your package's exports:
 * export const createBackup = Backups.ofVolumes<Manifest>('main', 'config')
 *
 * // With database dump before backup
 * export const createBackup = Backups.ofVolumes<Manifest>('main')
 *   .setPreBackup(async (effects) => {
 *     // Create a database dump before backing up files
 *     await subcontainer.exec(['pg_dump', '-f', '/data/backup.sql'])
 *   })
 *
 * // Exclude temporary files
 * export const createBackup = Backups.withOptions({ exclude: ['*.tmp', 'cache/'] })
 *   .addVolume('main')
 * ```
 */
export class Backups<M extends T.SDKManifest> implements InitScript {
  private constructor(
    /** @internal Default sync options */
    private options = DEFAULT_OPTIONS,
    /** @internal Options specific to restore operations */
    private restoreOptions: Partial<T.SyncOptions> = {},
    /** @internal Options specific to backup operations */
    private backupOptions: Partial<T.SyncOptions> = {},
    /** @internal Set of sync configurations */
    private backupSet = [] as BackupSync<M["volumes"][number]>[],
    /** @internal Hook called before backup starts */
    private preBackup = async (effects: BackupEffects) => {},
    /** @internal Hook called after backup completes */
    private postBackup = async (effects: BackupEffects) => {},
    /** @internal Hook called before restore starts */
    private preRestore = async (effects: BackupEffects) => {},
    /** @internal Hook called after restore completes */
    private postRestore = async (effects: BackupEffects) => {},
  ) {}

  /**
   * Creates a Backups instance configured to back up the specified volumes.
   * This is the most common way to create a backup configuration.
   *
   * Each volume is backed up to a corresponding path in the backup destination
   * using the volume's name as the subdirectory.
   *
   * @typeParam M - The manifest type (inferred from volume names)
   * @param volumeNames - Volume IDs to include in backups (from manifest.volumes)
   * @returns A configured Backups instance
   *
   * @example
   * ```typescript
   * // Back up a single volume
   * export const createBackup = Backups.ofVolumes<Manifest>('main')
   *
   * // Back up multiple volumes
   * export const createBackup = Backups.ofVolumes<Manifest>('main', 'config', 'logs')
   * ```
   */
  static ofVolumes<M extends T.SDKManifest = never>(
    ...volumeNames: Array<M["volumes"][number]>
  ): Backups<M> {
    return Backups.ofSyncs(
      ...volumeNames.map((srcVolume) => ({
        dataPath: `/media/startos/volumes/${srcVolume}/` as const,
        backupPath: `/media/startos/backup/volumes/${srcVolume}/` as const,
      })),
    )
  }

  /**
   * Creates a Backups instance from explicit sync configurations.
   * Use this for custom source/destination path mappings.
   *
   * @typeParam M - The manifest type
   * @param syncs - Array of sync configurations
   * @returns A configured Backups instance
   *
   * @example
   * ```typescript
   * const backups = Backups.ofSyncs<Manifest>(
   *   { dataPath: '/media/startos/volumes/main/data', backupPath: '/media/startos/backup/data' },
   *   { dataPath: '/media/startos/volumes/main/config', backupPath: '/media/startos/backup/config' }
   * )
   * ```
   */
  static ofSyncs<M extends T.SDKManifest = never>(
    ...syncs: BackupSync<M["volumes"][number]>[]
  ) {
    return syncs.reduce((acc, x) => acc.addSync(x), new Backups<M>())
  }

  /**
   * Creates a Backups instance with custom default sync options.
   * Call `addVolume()` or `addSync()` to add volumes after setting options.
   *
   * @typeParam M - The manifest type
   * @param options - Default sync options (merged with DEFAULT_OPTIONS)
   * @returns An empty Backups instance with the specified options
   *
   * @example
   * ```typescript
   * // Exclude cache and temp files from all backups
   * export const createBackup = Backups.withOptions<Manifest>({
   *   exclude: ['cache/', '*.tmp', '*.log']
   * }).addVolume('main')
   * ```
   */
  static withOptions<M extends T.SDKManifest = never>(
    options?: Partial<T.SyncOptions>,
  ) {
    return new Backups<M>({ ...DEFAULT_OPTIONS, ...options })
  }

  /**
   * Sets default sync options for both backup and restore operations.
   *
   * @param options - Sync options to merge with current defaults
   * @returns This instance for chaining
   */
  setOptions(options?: Partial<T.SyncOptions>) {
    this.options = {
      ...this.options,
      ...options,
    }
    return this
  }

  /**
   * Sets sync options applied only during backup operations.
   * These are merged with the default options.
   *
   * @param options - Backup-specific sync options
   * @returns This instance for chaining
   */
  setBackupOptions(options?: Partial<T.SyncOptions>) {
    this.backupOptions = {
      ...this.backupOptions,
      ...options,
    }
    return this
  }

  /**
   * Sets sync options applied only during restore operations.
   * These are merged with the default options.
   *
   * @param options - Restore-specific sync options
   * @returns This instance for chaining
   */
  setRestoreOptions(options?: Partial<T.SyncOptions>) {
    this.restoreOptions = {
      ...this.restoreOptions,
      ...options,
    }
    return this
  }

  /**
   * Registers a function to run before backup starts.
   * Use this to prepare the service for backup (e.g., flush caches,
   * create database dumps, pause writes).
   *
   * @param fn - Async function to run before backup
   * @returns This instance for chaining
   *
   * @example
   * ```typescript
   * Backups.ofVolumes<Manifest>('main')
   *   .setPreBackup(async (effects) => {
   *     // Flush database to disk
   *     await db.checkpoint()
   *   })
   * ```
   */
  setPreBackup(fn: (effects: BackupEffects) => Promise<void>) {
    this.preBackup = fn
    return this
  }

  /**
   * Registers a function to run after backup completes.
   * Use this to resume normal operations after backup.
   *
   * @param fn - Async function to run after backup
   * @returns This instance for chaining
   */
  setPostBackup(fn: (effects: BackupEffects) => Promise<void>) {
    this.postBackup = fn
    return this
  }

  /**
   * Registers a function to run before restore starts.
   * Use this to prepare for incoming data (e.g., stop services,
   * clear existing data).
   *
   * @param fn - Async function to run before restore
   * @returns This instance for chaining
   */
  setPreRestore(fn: (effects: BackupEffects) => Promise<void>) {
    this.preRestore = fn
    return this
  }

  /**
   * Registers a function to run after restore completes.
   * Use this to finalize restore (e.g., run migrations, rebuild indexes).
   *
   * @param fn - Async function to run after restore
   * @returns This instance for chaining
   *
   * @example
   * ```typescript
   * Backups.ofVolumes<Manifest>('main')
   *   .setPostRestore(async (effects) => {
   *     // Rebuild search indexes after restore
   *     await rebuildIndexes()
   *   })
   * ```
   */
  setPostRestore(fn: (effects: BackupEffects) => Promise<void>) {
    this.postRestore = fn
    return this
  }

  /**
   * Adds a volume to the backup configuration.
   *
   * @param volume - Volume ID from the manifest
   * @param options - Optional sync options for this specific volume
   * @returns This instance for chaining
   *
   * @example
   * ```typescript
   * Backups.withOptions<Manifest>({ exclude: ['*.tmp'] })
   *   .addVolume('main')
   *   .addVolume('logs', { backupOptions: { exclude: ['*.log'] } })
   * ```
   */
  addVolume(
    volume: M["volumes"][number],
    options?: Partial<{
      options: T.SyncOptions
      backupOptions: T.SyncOptions
      restoreOptions: T.SyncOptions
    }>,
  ) {
    return this.addSync({
      dataPath: `/media/startos/volumes/${volume}/` as const,
      backupPath: `/media/startos/backup/volumes/${volume}/` as const,
      ...options,
    })
  }

  /**
   * Adds a custom sync configuration to the backup.
   * Use this for non-standard path mappings.
   *
   * @param sync - Sync configuration with source and destination paths
   * @returns This instance for chaining
   */
  addSync(sync: BackupSync<M["volumes"][0]>) {
    this.backupSet.push(sync)
    return this
  }

  /**
   * Creates a backup by syncing all configured volumes to the backup destination.
   * Called by StartOS when the user initiates a backup.
   *
   * Execution order:
   * 1. Runs preBackup hook
   * 2. Syncs each volume using rsync
   * 3. Saves the data version to the backup
   * 4. Runs postBackup hook
   *
   * @param effects - Effects instance for system operations
   */
  async createBackup(effects: T.Effects) {
    await this.preBackup(effects as BackupEffects)
    for (const item of this.backupSet) {
      const rsyncResults = await runRsync({
        srcPath: item.dataPath,
        dstPath: item.backupPath,
        options: {
          ...this.options,
          ...this.backupOptions,
          ...item.options,
          ...item.backupOptions,
        },
      })
      await rsyncResults.wait()
    }

    const dataVersion = await effects.getDataVersion()
    if (dataVersion)
      await fs.writeFile("/media/startos/backup/dataVersion.txt", dataVersion, {
        encoding: "utf-8",
      })
    await this.postBackup(effects as BackupEffects)
    return
  }

  /**
   * InitScript implementation - handles restore during initialization.
   * Called automatically during the init phase when kind is "restore".
   *
   * @param effects - Effects instance
   * @param kind - The initialization kind (only acts on "restore")
   */
  async init(effects: T.Effects, kind: InitKind): Promise<void> {
    if (kind === "restore") {
      await this.restoreBackup(effects)
    }
  }

  /**
   * Restores data from a backup by syncing from backup destination to volumes.
   *
   * Execution order:
   * 1. Runs preRestore hook
   * 2. Syncs each volume from backup using rsync
   * 3. Restores the data version from the backup
   * 4. Runs postRestore hook
   *
   * @param effects - Effects instance for system operations
   */
  async restoreBackup(effects: T.Effects) {
    this.preRestore(effects as BackupEffects)

    for (const item of this.backupSet) {
      const rsyncResults = await runRsync({
        srcPath: item.backupPath,
        dstPath: item.dataPath,
        options: {
          ...this.options,
          ...this.backupOptions,
          ...item.options,
          ...item.backupOptions,
        },
      })
      await rsyncResults.wait()
    }
    const dataVersion = await fs
      .readFile("/media/startos/backup/dataVersion.txt", {
        encoding: "utf-8",
      })
      .catch((_) => null)
    if (dataVersion) await effects.setDataVersion({ version: dataVersion })
    this.postRestore(effects as BackupEffects)
    return
  }
}

/**
 * Executes rsync to synchronize files between source and destination.
 *
 * @param rsyncOptions - Configuration for the rsync operation
 * @returns Object with methods to get process ID, wait for completion, and check progress
 * @internal
 */
async function runRsync(rsyncOptions: {
  srcPath: string
  dstPath: string
  options: T.SyncOptions
}): Promise<{
  id: () => Promise<string>
  wait: () => Promise<null>
  progress: () => Promise<number>
}> {
  const { srcPath, dstPath, options } = rsyncOptions

  await fs.mkdir(dstPath, { recursive: true })

  const command = "rsync"
  const args: string[] = []
  if (options.delete) {
    args.push("--delete")
  }
  for (const exclude of options.exclude) {
    args.push(`--exclude=${exclude}`)
  }
  args.push("-rlptgocAXH")
  args.push("--info=progress2")
  args.push("--no-inc-recursive")
  args.push(srcPath)
  args.push(dstPath)
  const spawned = child_process.spawn(command, args, { detached: true })
  let percentage = 0.0
  spawned.stdout.on("data", (data: unknown) => {
    const lines = String(data).replace(/\r/g, "\n").split("\n")
    for (const line of lines) {
      const parsed = /$([0-9.]+)%/.exec(line)?.[1]
      if (!parsed) {
        console.log(line)
        continue
      }
      percentage = Number.parseFloat(parsed)
    }
  })

  let stderr = ""

  spawned.stderr.on("data", (data: string | Buffer) => {
    const errString = data.toString("utf-8")
    stderr += errString
    console.error(`Backups.runAsync`, asError(errString))
  })

  const id = async () => {
    const pid = spawned.pid
    if (pid === undefined) {
      throw new Error("rsync process has no pid")
    }
    return String(pid)
  }
  const waitPromise = new Promise<null>((resolve, reject) => {
    spawned.on("exit", (code: any) => {
      if (code === 0) {
        resolve(null)
      } else {
        reject(new Error(`rsync exited with code ${code}\n${stderr}`))
      }
    })
  })
  const wait = () => waitPromise
  const progress = () => Promise.resolve(percentage)
  return { id, wait, progress }
}
