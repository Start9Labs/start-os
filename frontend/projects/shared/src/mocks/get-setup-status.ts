import { SetupStatus } from '../types/setup-status'
import { pauseFor } from '../util/misc.util'

let tries: number | undefined

export async function getSetupStatusMock(): Promise<SetupStatus | null> {
  const restoreOrMigrate = true
  const total = 4

  await pauseFor(1000)

  if (tries === undefined) {
    tries = 0
    return null
  }

  tries++
  const progress = tries - 1

  return {
    'bytes-transferred': restoreOrMigrate ? progress : 0,
    'total-bytes': restoreOrMigrate ? total : null,
    complete: progress === total,
  }
}
