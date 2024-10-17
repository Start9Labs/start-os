import * as T from "../../../base/lib/types"
import * as child_process from "child_process"
import { asError } from "../util"

export const DEFAULT_OPTIONS: T.SyncOptions = {
  delete: true,
  exclude: [],
}
export type BackupSync<Volumes extends string> = {
  dataPath: `/media/startos/volumes/${Volumes}/${string}`
  backupPath: `/media/startos/backup/${string}`
  options?: Partial<T.SyncOptions>
  backupOptions?: Partial<T.SyncOptions>
  restoreOptions?: Partial<T.SyncOptions>
}
/**
 * This utility simplifies the volume backup process.
 * ```ts
 * export const { createBackup, restoreBackup } = Backups.volumes("main").build();
 * ```
 *
 * Changing the options of the rsync, (ie excludes) use either
 * ```ts
 *  Backups.volumes("main").set_options({exclude: ['bigdata/']}).volumes('excludedVolume').build()
 * // or
 *  Backups.with_options({exclude: ['bigdata/']}).volumes('excludedVolume').build()
 * ```
 *
 * Using the more fine control, using the addSets for more control
 * ```ts
 * Backups.addSets({
 * srcVolume: 'main', srcPath:'smallData/', dstPath: 'main/smallData/', dstVolume: : Backups.BACKUP
 * }, {
 * srcVolume: 'main', srcPath:'bigData/', dstPath: 'main/bigData/', dstVolume: : Backups.BACKUP, options: {exclude:['bigData/excludeThis']}}
 * ).build()q
 * ```
 */
export class Backups<M extends T.SDKManifest> {
  private constructor(
    private options = DEFAULT_OPTIONS,
    private restoreOptions: Partial<T.SyncOptions> = {},
    private backupOptions: Partial<T.SyncOptions> = {},
    private backupSet = [] as BackupSync<M["volumes"][number]>[],
  ) {}

  static withVolumes<M extends T.SDKManifest = never>(
    ...volumeNames: Array<M["volumes"][number]>
  ): Backups<M> {
    return Backups.withSyncs(
      ...volumeNames.map((srcVolume) => ({
        dataPath: `/media/startos/volumes/${srcVolume}/` as const,
        backupPath: `/media/startos/backup/${srcVolume}/` as const,
      })),
    )
  }

  static withSyncs<M extends T.SDKManifest = never>(
    ...syncs: BackupSync<M["volumes"][number]>[]
  ) {
    return syncs.reduce((acc, x) => acc.addSync(x), new Backups<M>())
  }

  static withOptions<M extends T.SDKManifest = never>(
    options?: Partial<T.SyncOptions>,
  ) {
    return new Backups<M>({ ...DEFAULT_OPTIONS, ...options })
  }

  setOptions(options?: Partial<T.SyncOptions>) {
    this.options = {
      ...this.options,
      ...options,
    }
    return this
  }

  setBackupOptions(options?: Partial<T.SyncOptions>) {
    this.backupOptions = {
      ...this.backupOptions,
      ...options,
    }
    return this
  }

  setRestoreOptions(options?: Partial<T.SyncOptions>) {
    this.restoreOptions = {
      ...this.restoreOptions,
      ...options,
    }
    return this
  }

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
      backupPath: `/media/startos/backup/${volume}/` as const,
      ...options,
    })
  }
  addSync(sync: BackupSync<M["volumes"][0]>) {
    this.backupSet.push({
      ...sync,
      options: { ...this.options, ...sync.options },
    })
    return this
  }

  async createBackup() {
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
    return
  }

  async restoreBackup() {
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

  const command = "rsync"
  const args: string[] = []
  if (options.delete) {
    args.push("--delete")
  }
  for (const exclude of options.exclude) {
    args.push(`--exclude=${exclude}`)
  }
  args.push("-actAXH")
  args.push("--info=progress2")
  args.push("--no-inc-recursive")
  args.push(srcPath)
  args.push(dstPath)
  const spawned = child_process.spawn(command, args, { detached: true })
  let percentage = 0.0
  spawned.stdout.on("data", (data: unknown) => {
    const lines = String(data).replace("\r", "\n").split("\n")
    for (const line of lines) {
      const parsed = /$([0-9.]+)%/.exec(line)?.[1]
      if (!parsed) continue
      percentage = Number.parseFloat(parsed)
    }
  })

  spawned.stderr.on("data", (data: unknown) => {
    console.error(`Backups.runAsync`, asError(data))
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
        reject(new Error(`rsync exited with code ${code}`))
      }
    })
  })
  const wait = () => waitPromise
  const progress = () => Promise.resolve(percentage)
  return { id, wait, progress }
}
