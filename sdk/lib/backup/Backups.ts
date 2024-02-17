import { SDKManifest } from "../manifest/ManifestTypes"
import * as T from "../types"

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
    private backupSet = [] as BackupSet<M["volumes"][0]>[],
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
  build() {
    const createBackup: T.ExpectedExports.createBackup = async ({
      effects,
    }) => {
      // const previousItems = (
      //   await effects
      //     .readDir({
      //       volumeId: Backups.BACKUP,
      //       path: ".",
      //     })
      //     .catch(() => [])
      // ).map((x) => `${x}`)
      // const backupPaths = this.backupSet
      //   .filter((x) => x.dstVolume === Backups.BACKUP)
      //   .map((x) => x.dstPath)
      //   .map((x) => x.replace(/\.\/([^]*)\//, "$1"))
      // const filteredItems = previousItems.filter(
      //   (x) => backupPaths.indexOf(x) === -1,
      // )
      // for (const itemToRemove of filteredItems) {
      //   effects.console.error(`Trying to remove ${itemToRemove}`)
      //   await effects
      //     .removeDir({
      //       volumeId: Backups.BACKUP,
      //       path: itemToRemove,
      //     })
      //     .catch(() =>
      //       effects.removeFile({
      //         volumeId: Backups.BACKUP,
      //         path: itemToRemove,
      //       }),
      //     )
      //     .catch(() => {
      //       console.warn(`Failed to remove ${itemToRemove} from backup volume`)
      //     })
      // }
      for (const item of this.backupSet) {
        // if (notEmptyPath(item.dstPath)) {
        //   await effects.createDir({
        //     volumeId: item.dstVolume,
        //     path: item.dstPath,
        //   })
        // }
        // await effects
        //   .runRsync({
        //     ...item,
        //     options: {
        //       ...this.options,
        //       ...item.options,
        //     },
        //   })
        //   .wait()
      }
      return
    }
    const restoreBackup: T.ExpectedExports.restoreBackup = async ({
      effects,
    }) => {
      for (const item of this.backupSet) {
        // if (notEmptyPath(item.srcPath)) {
        //   await new Promise((resolve, reject) => fs.mkdir(items.src)).createDir(
        //     {
        //       volumeId: item.srcVolume,
        //       path: item.srcPath,
        //     },
        //   )
        // }
        // await effects
        //   .runRsync({
        //     options: {
        //       ...this.options,
        //       ...item.options,
        //     },
        //     srcVolume: item.dstVolume,
        //     dstVolume: item.srcVolume,
        //     srcPath: item.dstPath,
        //     dstPath: item.srcPath,
        //   })
        //   .wait()
      }
      return
    }
    return { createBackup, restoreBackup }
  }
}
function notEmptyPath(file: string) {
  return ["", ".", "./"].indexOf(file) === -1
}
