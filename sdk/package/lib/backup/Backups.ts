import * as T from '../../../base/lib/types'
import * as child_process from 'child_process'
import * as fs from 'fs/promises'
import { Affine, asError } from '../util'
import { InitKind, InitScript } from '../../../base/lib/inits'
import { SubContainerRc, execFile } from '../util/SubContainer'
import { Mounts } from '../mainFn/Mounts'

const BACKUP_HOST_PATH = '/media/startos/backup'
const BACKUP_CONTAINER_MOUNT = '/backup-target'

/** A password value, or a function that returns one. Functions are resolved lazily (only during restore). */
export type LazyPassword = string | (() => string | Promise<string>)

async function resolvePassword(pw: LazyPassword): Promise<string> {
  return typeof pw === 'function' ? pw() : pw
}

/** Configuration for PostgreSQL dump-based backup */
export type PgDumpConfig<M extends T.SDKManifest> = {
  /** Image ID of the PostgreSQL container (e.g. 'postgres') */
  imageId: keyof M['images'] & T.ImageId
  /** Volume ID containing the PostgreSQL data directory */
  dbVolume: M['volumes'][number]
  /** Path to PGDATA within the container (e.g. '/var/lib/postgresql/data') */
  pgdata: string
  /** PostgreSQL database name to dump */
  database: string
  /** PostgreSQL user */
  user: string
  /** PostgreSQL password (for restore). Can be a string or a function that returns one — functions are resolved lazily after volumes are restored. */
  password: LazyPassword
  /** Additional initdb arguments (e.g. ['--data-checksums']) */
  initdbArgs?: string[]
}

/** Configuration for MySQL/MariaDB dump-based backup */
export type MysqlDumpConfig<M extends T.SDKManifest> = {
  /** Image ID of the MySQL/MariaDB container (e.g. 'mysql', 'mariadb') */
  imageId: keyof M['images'] & T.ImageId
  /** Volume ID containing the MySQL data directory */
  dbVolume: M['volumes'][number]
  /** Path to MySQL data directory within the container (typically '/var/lib/mysql') */
  datadir: string
  /** MySQL database name to dump */
  database: string
  /** MySQL user for dump operations */
  user: string
  /** MySQL password. Can be a string or a function that returns one — functions are resolved lazily after volumes are restored. */
  password: LazyPassword
  /** Database engine: 'mysql' uses --initialize-insecure, 'mariadb' uses mysql_install_db */
  engine: 'mysql' | 'mariadb'
  /** Custom readiness check command (default: ['mysqladmin', 'ping', ...]) */
  readyCommand?: string[]
}

/** Bind-mount the backup target into a SubContainer's rootfs */
async function mountBackupTarget(rootfs: string) {
  const target = `${rootfs}${BACKUP_CONTAINER_MOUNT}`
  await fs.mkdir(target, { recursive: true })
  await execFile('mount', ['--rbind', BACKUP_HOST_PATH, target])
}

/** Default rsync options used for backup and restore operations */
export const DEFAULT_OPTIONS: T.SyncOptions = {
  delete: true,
  exclude: [],
}
/** A single source-to-destination sync pair for backup and restore */
export type BackupSync<Volumes extends string> = {
  dataPath: `/media/startos/volumes/${Volumes}/${string}`
  backupPath: `/media/startos/backup/${string}`
  options?: Partial<T.SyncOptions>
  backupOptions?: Partial<T.SyncOptions>
  restoreOptions?: Partial<T.SyncOptions>
}

/** Effects type narrowed for backup/restore contexts, preventing reuse outside that scope */
export type BackupEffects = T.Effects & Affine<'Backups'>

/**
 * Configures backup and restore operations using rsync.
 *
 * Supports syncing entire volumes or custom path pairs, with optional pre/post hooks
 * for both backup and restore phases. Implements {@link InitScript} so it can be used
 * as a restore-init step in `setupInit`.
 *
 * @typeParam M - The service manifest type
 */
export class Backups<M extends T.SDKManifest> implements InitScript {
  private constructor(
    private options = DEFAULT_OPTIONS,
    private restoreOptions: Partial<T.SyncOptions> = {},
    private backupOptions: Partial<T.SyncOptions> = {},
    private backupSet = [] as BackupSync<M['volumes'][number]>[],
    private preBackup = async (effects: BackupEffects) => {},
    private postBackup = async (effects: BackupEffects) => {},
    private preRestore = async (effects: BackupEffects) => {},
    private postRestore = async (effects: BackupEffects) => {},
  ) {}

  /**
   * Create a Backups configuration that backs up entire volumes by name.
   * Each volume is synced to a corresponding directory under `/media/startos/backup/volumes/`.
   * @param volumeNames - One or more volume IDs from the manifest
   */
  static ofVolumes<M extends T.SDKManifest = never>(
    ...volumeNames: Array<M['volumes'][number]>
  ): Backups<M> {
    return Backups.ofSyncs(
      ...volumeNames.map((srcVolume) => ({
        dataPath: `/media/startos/volumes/${srcVolume}/` as const,
        backupPath: `/media/startos/backup/volumes/${srcVolume}/` as const,
      })),
    )
  }

  /**
   * Create a Backups configuration from explicit source/destination sync pairs.
   * @param syncs - Array of `{ dataPath, backupPath }` objects with optional per-sync options
   */
  static ofSyncs<M extends T.SDKManifest = never>(
    ...syncs: BackupSync<M['volumes'][number]>[]
  ) {
    return syncs.reduce((acc, x) => acc.addSync(x), new Backups<M>())
  }

  /**
   * Create an empty Backups configuration with custom default rsync options.
   * Chain `.addVolume()` or `.addSync()` to add sync targets.
   * @param options - Partial rsync options to override defaults (e.g. `{ exclude: ['cache'] }`)
   */
  static withOptions<M extends T.SDKManifest = never>(
    options?: Partial<T.SyncOptions>,
  ) {
    return new Backups<M>({ ...DEFAULT_OPTIONS, ...options })
  }

  /**
   * Configure PostgreSQL dump-based backup for a volume.
   *
   * Instead of rsyncing the raw PostgreSQL data directory (which is slow and error-prone),
   * this uses `pg_dump` to create a logical dump before backup and `pg_restore` to rebuild
   * the database after restore.
   *
   * The dump file is written directly to the backup target — no data duplication on disk.
   *
   * @returns A configured Backups instance with pre/post hooks. Chain `.addVolume()` or
   * `.addSync()` to include additional volumes/paths in the backup.
   */
  static withPgDump<M extends T.SDKManifest = never>(
    config: PgDumpConfig<M>,
  ): Backups<M> {
    const {
      imageId,
      dbVolume,
      pgdata,
      database,
      user,
      password,
      initdbArgs = [],
    } = config
    const dumpFile = `${BACKUP_CONTAINER_MOUNT}/${database}-db.dump`
    const pgMountpoint = pgdata.replace(/\/data$/, '') || pgdata

    function dbMounts() {
      return Mounts.of<M>().mountVolume({
        volumeId: dbVolume,
        mountpoint: pgMountpoint,
        readonly: false,
        subpath: null,
      })
    }

    async function startPg(
      sub: {
        exec(cmd: string[], opts?: any): Promise<{ exitCode: number | null }>
        execFail(
          cmd: string[],
          opts?: any,
          timeout?: number | null,
        ): Promise<any>
      },
      label: string,
    ) {
      await sub.exec(['rm', '-f', `${pgdata}/postmaster.pid`], {
        user: 'postgres',
      })
      await sub.exec(['mkdir', '-p', '/var/run/postgresql'], {
        user: 'root',
      })
      await sub.exec(['chown', 'postgres:postgres', '/var/run/postgresql'], {
        user: 'root',
      })
      console.log(`[${label}] starting postgres`)
      await sub.execFail(
        ['pg_ctl', 'start', '-D', pgdata, '-o', '-c listen_addresses='],
        { user: 'postgres' },
      )
      for (let i = 0; i < 60; i++) {
        const { exitCode } = await sub.exec(['pg_isready', '-U', user], {
          user: 'postgres',
        })
        if (exitCode === 0) {
          console.log(`[${label}] postgres is ready`)
          return
        }
        await new Promise((r) => setTimeout(r, 1000))
      }
      throw new Error('PostgreSQL failed to become ready within 60 seconds')
    }

    return new Backups<M>()
      .setPreBackup(async (effects) => {
        await SubContainerRc.withTemp<M, void, BackupEffects>(
          effects,
          { imageId },
          dbMounts() as any,
          'pg-dump',
          async (sub) => {
            console.log('[pg-dump] mounting backup target')
            await mountBackupTarget(sub.rootfs)
            await startPg(sub, 'pg-dump')
            console.log('[pg-dump] dumping database')
            await sub.execFail(
              ['pg_dump', '-U', user, '-Fc', '-f', dumpFile, database],
              { user: 'postgres' },
              null,
            )
            console.log('[pg-dump] stopping postgres')
            await sub.execFail(['pg_ctl', 'stop', '-D', pgdata, '-w'], {
              user: 'postgres',
            })
            console.log('[pg-dump] complete')
          },
        )
      })
      .setPostRestore(async (effects) => {
        const resolvedPassword = await resolvePassword(password)
        await SubContainerRc.withTemp<M, void, BackupEffects>(
          effects,
          { imageId },
          dbMounts() as any,
          'pg-restore',
          async (sub) => {
            await mountBackupTarget(sub.rootfs)
            await sub.execFail(
              ['initdb', '-D', pgdata, '-U', user, ...initdbArgs],
              { user: 'postgres' },
            )
            await startPg(sub, 'pg-restore')
            await sub.execFail(['createdb', '-U', user, database], {
              user: 'postgres',
            })
            await sub.execFail(
              [
                'pg_restore',
                '-U',
                user,
                '-d',
                database,
                '--no-owner',
                dumpFile,
              ],
              { user: 'postgres' },
              null,
            )
            await sub.execFail(
              [
                'psql',
                '-U',
                user,
                '-d',
                database,
                '-c',
                `ALTER USER ${user} WITH PASSWORD '${resolvedPassword}'`,
              ],
              { user: 'postgres' },
            )
            await sub.execFail(['pg_ctl', 'stop', '-D', pgdata, '-w'], {
              user: 'postgres',
            })
          },
        )
      })
  }

  /**
   * Configure MySQL/MariaDB dump-based backup for a volume.
   *
   * Instead of rsyncing the raw MySQL data directory (which is slow and error-prone),
   * this uses `mysqldump` to create a logical dump before backup and `mysql` to restore
   * the database after restore.
   *
   * The dump file is stored temporarily in `dumpVolume` during backup and cleaned up afterward.
   *
   * @returns A configured Backups instance with pre/post hooks. Chain `.addVolume()` or
   * `.addSync()` to include additional volumes/paths in the backup.
   */
  static withMysqlDump<M extends T.SDKManifest = never>(
    config: MysqlDumpConfig<M>,
  ): Backups<M> {
    const {
      imageId,
      dbVolume,
      datadir,
      database,
      user,
      password,
      engine,
      readyCommand,
    } = config
    const dumpFile = `${BACKUP_CONTAINER_MOUNT}/${database}-db.dump`

    function dbMounts() {
      return Mounts.of<M>().mountVolume({
        volumeId: dbVolume,
        mountpoint: datadir,
        readonly: false,
        subpath: null,
      })
    }

    async function waitForMysql(
      sub: { exec(cmd: string[]): Promise<{ exitCode: number | null }> },
      cmd: string[],
    ) {
      for (let i = 0; i < 30; i++) {
        const { exitCode } = await sub.exec(cmd)
        if (exitCode === 0) return
        await new Promise((r) => setTimeout(r, 1000))
      }
      throw new Error('MySQL/MariaDB failed to become ready within 30 seconds')
    }

    return new Backups<M>()
      .setPreBackup(async (effects) => {
        const pw = await resolvePassword(password)
        const readyCmd = readyCommand || [
          'mysqladmin',
          'ping',
          '-u',
          user,
          `-p${pw}`,
          '--silent',
        ]
        await SubContainerRc.withTemp<M, void, BackupEffects>(
          effects,
          { imageId },
          dbMounts() as any,
          'mysql-dump',
          async (sub) => {
            await mountBackupTarget(sub.rootfs)
            await sub.exec(['mkdir', '-p', '/var/run/mysqld'], {
              user: 'root',
            })
            await sub.exec(['chown', 'mysql:mysql', '/var/run/mysqld'], {
              user: 'root',
            })
            if (engine === 'mysql') {
              await sub.execFail(['chown', '-R', 'mysql:mysql', datadir], {
                user: 'root',
              })
            }
            await sub.execFail(
              [
                'mysqld',
                '--user=mysql',
                `--datadir=${datadir}`,
                '--skip-networking',
                '--daemonize',
              ],
              { user: 'root' },
              null,
            )
            await waitForMysql(sub, readyCmd)
            await sub.execFail(
              [
                'mysqldump',
                '-u',
                user,
                `-p${pw}`,
                '--single-transaction',
                `--result-file=${dumpFile}`,
                database,
              ],
              { user: 'root' },
              null,
            )
            await sub.execFail(
              ['mysqladmin', '-u', user, `-p${pw}`, 'shutdown'],
              { user: 'root' },
            )
          },
        )
      })
      .setPostRestore(async (effects) => {
        const pw = await resolvePassword(password)
        await SubContainerRc.withTemp<M, void, BackupEffects>(
          effects,
          { imageId },
          dbMounts() as any,
          'mysql-restore',
          async (sub) => {
            await mountBackupTarget(sub.rootfs)
            await sub.exec(['mkdir', '-p', '/var/run/mysqld'], {
              user: 'root',
            })
            await sub.exec(['chown', 'mysql:mysql', '/var/run/mysqld'], {
              user: 'root',
            })
            // Initialize fresh data directory
            if (engine === 'mariadb') {
              await sub.execFail(
                ['mysql_install_db', '--user=mysql', `--datadir=${datadir}`],
                { user: 'root' },
              )
            } else {
              await sub.execFail(
                [
                  'mysqld',
                  '--initialize-insecure',
                  '--user=mysql',
                  `--datadir=${datadir}`,
                ],
                { user: 'root' },
              )
            }
            await sub.execFail(
              [
                'mysqld',
                '--user=mysql',
                `--datadir=${datadir}`,
                '--skip-networking',
                '--daemonize',
              ],
              { user: 'root' },
              null,
            )
            // After fresh init, root has no password
            await waitForMysql(sub, [
              'mysqladmin',
              'ping',
              '-u',
              'root',
              '--silent',
            ])
            // Create database, user, and set password
            await sub.execFail(
              [
                'mysql',
                '-u',
                'root',
                '-e',
                `CREATE DATABASE IF NOT EXISTS \`${database}\`; CREATE USER IF NOT EXISTS '${user}'@'localhost' IDENTIFIED BY '${pw}'; GRANT ALL ON \`${database}\`.* TO '${user}'@'localhost'; ALTER USER 'root'@'localhost' IDENTIFIED BY '${pw}'; FLUSH PRIVILEGES;`,
              ],
              { user: 'root' },
            )
            // Restore from dump
            await sub.execFail(
              [
                'sh',
                '-c',
                `mysql -u root -p'${pw}' \`${database}\` < ${dumpFile}`,
              ],
              { user: 'root' },
              null,
            )
            await sub.execFail(
              ['mysqladmin', '-u', 'root', `-p${password}`, 'shutdown'],
              { user: 'root' },
            )
          },
        )
      })
  }

  /**
   * Override the default rsync options for both backup and restore.
   * @param options - Partial rsync options to merge with current defaults
   */
  setOptions(options?: Partial<T.SyncOptions>) {
    this.options = {
      ...this.options,
      ...options,
    }
    return this
  }

  /**
   * Override rsync options used only during backup (not restore).
   * @param options - Partial rsync options for the backup phase
   */
  setBackupOptions(options?: Partial<T.SyncOptions>) {
    this.backupOptions = {
      ...this.backupOptions,
      ...options,
    }
    return this
  }

  /**
   * Override rsync options used only during restore (not backup).
   * @param options - Partial rsync options for the restore phase
   */
  setRestoreOptions(options?: Partial<T.SyncOptions>) {
    this.restoreOptions = {
      ...this.restoreOptions,
      ...options,
    }
    return this
  }

  /**
   * Register a hook to run before backup rsync begins (e.g. dump a database).
   * @param fn - Async function receiving backup-scoped effects
   */
  setPreBackup(fn: (effects: BackupEffects) => Promise<void>) {
    this.preBackup = fn
    return this
  }

  /**
   * Register a hook to run after backup rsync completes.
   * @param fn - Async function receiving backup-scoped effects
   */
  setPostBackup(fn: (effects: BackupEffects) => Promise<void>) {
    this.postBackup = fn
    return this
  }

  /**
   * Register a hook to run before restore rsync begins.
   * @param fn - Async function receiving backup-scoped effects
   */
  setPreRestore(fn: (effects: BackupEffects) => Promise<void>) {
    this.preRestore = fn
    return this
  }

  /**
   * Register a hook to run after restore rsync completes.
   * @param fn - Async function receiving backup-scoped effects
   */
  setPostRestore(fn: (effects: BackupEffects) => Promise<void>) {
    this.postRestore = fn
    return this
  }

  /**
   * Add a volume to the backup set by its ID.
   * @param volume - The volume ID from the manifest
   * @param options - Optional per-volume rsync overrides
   */
  addVolume(
    volume: M['volumes'][number],
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
   * Add a custom sync pair to the backup set.
   * @param sync - A `{ dataPath, backupPath }` object with optional per-sync rsync options
   */
  addSync(sync: BackupSync<M['volumes'][0]>) {
    this.backupSet.push(sync)
    return this
  }

  /**
   * Execute the backup: runs pre-hook, rsyncs all configured paths, saves the data version, then runs post-hook.
   * @param effects - The effects context
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
      await fs.writeFile('/media/startos/backup/dataVersion.txt', dataVersion, {
        encoding: 'utf-8',
      })
    await this.postBackup(effects as BackupEffects)
    return
  }

  async init(effects: T.Effects, kind: InitKind): Promise<void> {
    if (kind === 'restore') {
      await this.restoreBackup(effects)
    }
  }

  /**
   * Execute the restore: runs pre-hook, rsyncs all configured paths from backup to data, restores the data version, then runs post-hook.
   * @param effects - The effects context
   */
  async restoreBackup(effects: T.Effects) {
    await this.preRestore(effects as BackupEffects)

    for (const item of this.backupSet) {
      const rsyncResults = await runRsync({
        srcPath: item.backupPath,
        dstPath: item.dataPath,
        options: {
          ...this.options,
          ...this.restoreOptions,
          ...item.options,
          ...item.restoreOptions,
        },
      })
      await rsyncResults.wait()
    }
    const dataVersion = await fs
      .readFile('/media/startos/backup/dataVersion.txt', {
        encoding: 'utf-8',
      })
      .catch((_) => null)
    if (dataVersion) await effects.setDataVersion({ version: dataVersion })
    await this.postRestore(effects as BackupEffects)
    return
  }
}

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

  const command = 'rsync'
  const args: string[] = []
  if (options.delete) {
    args.push('--delete')
  }
  for (const exclude of options.exclude) {
    args.push(`--exclude=${exclude}`)
  }
  args.push('-rlptgoAXH')
  args.push('--partial')
  args.push('--inplace')
  args.push('--timeout=300')
  args.push('--info=progress2')
  args.push('--no-inc-recursive')
  args.push(srcPath)
  args.push(dstPath)
  const spawned = child_process.spawn(command, args, { detached: true })
  let percentage = 0.0
  spawned.stdout.on('data', (data: unknown) => {
    const lines = String(data).replace(/\r/g, '\n').split('\n')
    for (const line of lines) {
      const parsed = /([0-9.]+)%/.exec(line)?.[1]
      if (!parsed) {
        if (line) console.log(line)
        continue
      }
      percentage = Number.parseFloat(parsed)
    }
  })

  let stderr = ''

  spawned.stderr.on('data', (data: string | Buffer) => {
    const errString = data.toString('utf-8')
    stderr += errString
    console.error(`Backups.runAsync`, asError(errString))
  })

  const id = async () => {
    const pid = spawned.pid
    if (pid === undefined) {
      throw new Error('rsync process has no pid')
    }
    return String(pid)
  }
  const waitPromise = new Promise<null>((resolve, reject) => {
    spawned.on('exit', (code: any) => {
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
