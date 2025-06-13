import { Backups } from "./Backups"
import * as T from "../../../base/lib/types"
import { _ } from "../util"
import { InitScript } from "../../../base/lib/inits"

export type SetupBackupsParams<M extends T.SDKManifest> =
  | M["volumes"][number][]
  | ((_: { effects: T.Effects }) => Promise<Backups<M>>)

type SetupBackupsRes = {
  createBackup: T.ExpectedExports.createBackup
  restoreInit: InitScript
}

export function setupBackups<M extends T.SDKManifest>(
  options: SetupBackupsParams<M>,
) {
  let backupsFactory: (_: { effects: T.Effects }) => Promise<Backups<M>>
  if (options instanceof Function) {
    backupsFactory = options
  } else {
    backupsFactory = async () => Backups.ofVolumes(...options)
  }
  const answer: SetupBackupsRes = {
    get createBackup() {
      return (async (options) => {
        return (await backupsFactory(options)).createBackup(options.effects)
      }) as T.ExpectedExports.createBackup
    },
    get restoreInit(): InitScript {
      return {
        init: async (effects, kind) => {
          return (await backupsFactory({ effects })).init(effects, kind)
        },
      }
    },
  }
  return answer
}
