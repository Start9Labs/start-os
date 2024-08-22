import { Backups } from "./Backups"
import * as T from "../types"
import { _ } from "../util"

export type SetupBackupsParams<M extends T.Manifest> = Array<
  M["volumes"][number] | Backups<M>
>

/**
 * @description Use this function to determine which volumes are backed up when a user creates a backup, including advanced options.
 * @example
 * In this example, we back up the entire "main" volume and nothing else.
 *
 * ```
 * export const { createBackup, restoreBackup } = sdk.setupBackups(sdk.Backups.addVolume('main'))
 * ```
 * @example
 * In this example, we back up the "main" and the "other" volume, but exclude hypothetical directory "excludedDir" from the "other".
 *
 * ```
 * export const { createBackup, restoreBackup } = sdk.setupBackups(sdk.Backups
 *   .addVolume('main')
 *   .addVolume('other', { exclude: ['path/to/excludedDir'] })
 * )
 * ```
 */
export function setupBackups<M extends T.Manifest>(
  manifest: M,
  ...args: _<SetupBackupsParams<M>>
) {
  const backups = Array<Backups<M>>()
  const volumes = new Set<M["volumes"][0]>()
  for (const arg of args) {
    if (arg instanceof Backups) {
      backups.push(arg)
    } else {
      volumes.add(arg)
    }
  }
  backups.push(Backups.volumes(...volumes))
  const answer: {
    createBackup: T.ExpectedExports.createBackup
    restoreBackup: T.ExpectedExports.restoreBackup
  } = {
    get createBackup() {
      return (async (options) => {
        for (const backup of backups) {
          await backup.build(options.pathMaker).createBackup(options)
        }
      }) as T.ExpectedExports.createBackup
    },
    get restoreBackup() {
      return (async (options) => {
        for (const backup of backups) {
          await backup.build(options.pathMaker).restoreBackup(options)
        }
        await options.effects.setDataVersion({ version: manifest.version })
      }) as T.ExpectedExports.restoreBackup
    },
  }
  return answer
}
