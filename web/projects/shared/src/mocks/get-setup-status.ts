import { SetupStatus } from '../types/api'
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
    bytesTransferred: restoreOrMigrate ? progress : 0,
    totalBytes: restoreOrMigrate ? total : null,
    complete: progress === total,
  }
}
