import { T } from '@start9labs/start-sdk'

export function formatProgress({ phases, overall }: T.FullProgress): {
  total: number
  message: string
} {
  return {
    total: getDecimal(overall),
    message: phases
      .filter(
        (
          p,
        ): p is {
          name: string
          progress: {
            done: number
            total: number | null
          }
        } => p.progress !== true && p.progress !== null,
      )
      .map(p => `<b>${p.name}</b>${getPhaseBytes(p.progress)}`)
      .join(', '),
  }
}

function getDecimal(progress: T.Progress): number {
  if (progress === true) {
    return 1
  } else if (!progress || !progress.total) {
    return 0
  } else {
    return progress.total && progress.done / progress.total
  }
}

function getPhaseBytes(progress: T.Progress): string {
  return progress === true || !progress
    ? ''
    : `: (${progress.done}/${progress.total})`
}
