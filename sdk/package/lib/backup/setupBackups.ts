import { Backups } from "./Backups"
import * as T from "../../../base/lib/types"
import { _ } from "../util"

export type SetupBackupsParams<M extends T.SDKManifest> =
  | M["volumes"][number][]
  | ((_: { effects: T.Effects }) => Promise<Backups<M>>)

type SetupBackupsRes = {
  createBackup: T.ExpectedExports.createBackup
  restoreBackup: T.ExpectedExports.restoreBackup
}

export function setupBackups<M extends T.SDKManifest>(
  options: SetupBackupsParams<M>,
) {
  let backupsFactory: (_: { effects: T.Effects }) => Promise<Backups<M>>
  if (options instanceof Function) {
    backupsFactory = options
  } else {
    backupsFactory = async () => Backups.withVolumes(...options)
  }
  const answer: {
    createBackup: T.ExpectedExports.createBackup
    restoreBackup: T.ExpectedExports.restoreBackup
  } = {
    get createBackup() {
      return (async (options) => {
        return (await backupsFactory(options)).createBackup()
      }) as T.ExpectedExports.createBackup
    },
    get restoreBackup() {
      return (async (options) => {
        return (await backupsFactory(options)).restoreBackup()
      }) as T.ExpectedExports.restoreBackup
    },
  }
  return answer
}
