import * as T from "../types"
import * as child_process from "child_process"
import { asError } from "../util"

export type BACKUP = "BACKUP"
export const DEFAULT_OPTIONS: T.BackupOptions = {
  delete: true,
  exclude: [],
}
export type BackupSet<Volumes extends string> = {
  srcPath: string
  srcVolume: Volumes | BACKUP
  dstPath: string
  dstVolume: Volumes | BACKUP
  options?: Partial<T.BackupOptions>
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
export class Backups<M extends T.Manifest> {
  static BACKUP: BACKUP = "BACKUP"

  private constructor(
    private options = DEFAULT_OPTIONS,
    private backupSet = [] as BackupSet<M["volumes"][number]>[],
  ) {}
  /** Accepts as parameters every volume to be backed up */
  static volumes<M extends T.Manifest = never>(
    ...volumeNames: Array<M["volumes"][0]>
  ): Backups<M> {
    return new Backups<M>().addSets(
      ...volumeNames.map((srcVolume) => ({
        srcVolume,
        srcPath: "./",
        dstPath: `./${srcVolume}/`,
        dstVolume: Backups.BACKUP,
      })),
    )
  }
  static addSets<M extends T.Manifest = never>(
    ...options: BackupSet<M["volumes"][0]>[]
  ) {
    return new Backups().addSets(...options)
  }
  /**
   * @description Advanced options for more control over backups
   * @property {boolean} delete - Whether or not to delete files old backup before creating the new one. Defaults to true.
   * @property {boolean} exclude - A list of directories to exclude from being backed up. This should be used for any large datasets that can recovered another way, such as the Bitcoin blockchain.
   */
  static with_options<M extends T.Manifest = never>(
    options?: Partial<T.BackupOptions>,
  ) {
    return new Backups({ ...DEFAULT_OPTIONS, ...options })
  }

  static withOptions = Backups.with_options
  setOptions(options?: Partial<T.BackupOptions>) {
    this.options = {
      ...this.options,
      ...options,
    }
    return this
  }
  volumes(...volumeNames: Array<M["volumes"][0]>) {
    return this.addSets(
      ...volumeNames.map((srcVolume) => ({
        srcVolume,
        srcPath: "./",
        dstPath: `./${srcVolume}/`,
        dstVolume: Backups.BACKUP,
      })),
    )
  }
  addSets(...options: BackupSet<M["volumes"][0]>[]) {
    options.forEach((x) =>
      this.backupSet.push({ ...x, options: { ...this.options, ...x.options } }),
    )
    return this
  }
  build(pathMaker: T.PathMaker) {
    const createBackup: T.ExpectedExports.createBackup = async ({
      effects,
    }) => {
      for (const item of this.backupSet) {
        const rsyncResults = await runRsync(
          {
            dstPath: item.dstPath,
            dstVolume: item.dstVolume,
            options: { ...this.options, ...item.options },
            srcPath: item.srcPath,
            srcVolume: item.srcVolume,
          },
          pathMaker,
        )
        await rsyncResults.wait()
      }
      return
    }
    const restoreBackup: T.ExpectedExports.restoreBackup = async ({
      effects,
    }) => {
      for (const item of this.backupSet) {
        const rsyncResults = await runRsync(
          {
            dstPath: item.dstPath,
            dstVolume: item.dstVolume,
            options: { ...this.options, ...item.options },
            srcPath: item.srcPath,
            srcVolume: item.srcVolume,
          },
          pathMaker,
        )
        await rsyncResults.wait()
      }
      return
    }
    return { createBackup, restoreBackup }
  }
}
function notEmptyPath(file: string) {
  return ["", ".", "./"].indexOf(file) === -1
}
async function runRsync(
  rsyncOptions: {
    srcVolume: string
    dstVolume: string
    srcPath: string
    dstPath: string
    options: T.BackupOptions
  },
  pathMaker: T.PathMaker,
): Promise<{
  id: () => Promise<string>
  wait: () => Promise<null>
  progress: () => Promise<number>
}> {
  const { srcVolume, dstVolume, srcPath, dstPath, options } = rsyncOptions

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
  args.push(pathMaker({ volume: srcVolume, path: srcPath }))
  args.push(pathMaker({ volume: dstVolume, path: dstPath }))
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
