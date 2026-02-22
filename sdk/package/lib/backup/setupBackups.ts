import { Backups } from './Backups'
import * as T from '../../../base/lib/types'
import { _ } from '../util'
import { InitScript } from '../../../base/lib/inits'

/**
 * Parameters for `setupBackups`. Either:
 * - An array of volume IDs to back up entirely, or
 * - An async factory function that returns a fully configured {@link Backups} instance
 */
export type SetupBackupsParams<M extends T.SDKManifest> =
  | M['volumes'][number][]
  | ((_: { effects: T.Effects }) => Promise<Backups<M>>)

type SetupBackupsRes = {
  createBackup: T.ExpectedExports.createBackup
  restoreInit: InitScript
}

/**
 * Set up backup and restore exports for the service.
 *
 * Returns `{ createBackup, restoreInit }` which should be exported and wired into
 * the service's init and backup entry points.
 *
 * @param options - Either an array of volume IDs or an async factory returning a Backups instance
 * @returns An object with `createBackup` (the backup export) and `restoreInit` (an InitScript for restore)
 */
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
