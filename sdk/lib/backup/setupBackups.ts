import { Backups } from "./Backups"
import { SDKManifest } from "../manifest/ManifestTypes"
import { ExpectedExports } from "../types"
import { _ } from "../util"

export type SetupBackupsParams<M extends SDKManifest> = Array<
  M["volumes"][0] | Backups<M>
>

export function setupBackups<M extends SDKManifest>(
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
    createBackup: ExpectedExports.createBackup
    restoreBackup: ExpectedExports.restoreBackup
  } = {
    get createBackup() {
      return (async (options) => {
        for (const backup of backups) {
          await backup.build().createBackup(options)
        }
      }) as ExpectedExports.createBackup
    },
    get restoreBackup() {
      return (async (options) => {
        for (const backup of backups) {
          await backup.build().restoreBackup(options)
        }
      }) as ExpectedExports.restoreBackup
    },
  }
  return answer
}
