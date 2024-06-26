import { SDKManifest } from "../manifest/ManifestTypes"
import * as T from "../types"

import * as child_process from "child_process"

export type BACKUP = "BACKUP"
export const DEFAULT_OPTIONS: T.BackupOptions = {
  delete: true,
  force: true,
  ignoreExisting: false,
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
 * Changing the options of the rsync, (ie exludes) use either
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
export class Backups<M extends SDKManifest> {
  static BACKUP: BACKUP = "BACKUP"

  private constructor(
    private options = DEFAULT_OPTIONS,
    private backupSet = [] as BackupSet<M["volumes"][number]>[],
  ) {}
  static volumes<M extends SDKManifest = never>(
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
  static addSets<M extends SDKManifest = never>(
    ...options: BackupSet<M["volumes"][0]>[]
  ) {
    return new Backups().addSets(...options)
  }
  static with_options<M extends SDKManifest = never>(
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
        await runRsync(
          {
            dstPath: item.dstPath,
            dstVolume: item.dstVolume,
            options: { ...this.options, ...item.options },
            srcPath: item.srcPath,
            srcVolume: item.srcVolume,
          },
          pathMaker,
        )
      }
      return
    }
    const restoreBackup: T.ExpectedExports.restoreBackup = async ({
      effects,
    }) => {
      for (const item of this.backupSet) {
        await runRsync(
          {
            srcPath: item.dstPath,
            srcVolume: item.dstVolume,
            options: { ...this.options, ...item.options },
            dstPath: item.srcPath,
            dstVolume: item.srcVolume,
          },
          pathMaker,
        )
      }
      return
    }
    return { createBackup, restoreBackup }
  }
}
function notEmptyPath(file: string) {
  return ["", ".", "./"].indexOf(file) === -1
}
function runRsync(
  rsyncOptions: {
    srcVolume: string
    dstVolume: string
    srcPath: string
    dstPath: string
    options: T.BackupOptions
  },
  pathMaker: T.PathMaker,
): {
  id: () => Promise<string>
  wait: () => Promise<null>
  progress: () => Promise<number>
} {
  const { srcVolume, dstVolume, srcPath, dstPath, options } = rsyncOptions
  const command = "rsync"
  const args: string[] = []
  if (options.delete) {
    args.push("--delete")
  }
  if (options.force) {
    args.push("--force")
  }
  if (options.ignoreExisting) {
    args.push("--ignore-existing")
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
    console.error(String(data))
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
