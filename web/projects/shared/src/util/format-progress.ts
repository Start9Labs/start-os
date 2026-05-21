import { T } from '@start9labs/start-sdk'

export function formatProgress({ phases, overall }: T.FullProgress): {
  total: number
  message: string
} {
  return {
    total: getDecimal(overall),
    message: phases
      .map(p => ({ name: p.name, progress: phaseLeaf(p.progress) }))
      .filter(
        (
          p,
        ): p is {
          name: string
          progress: false | ProgressDetails
        } => p.progress !== true && p.progress !== null,
      )
      .map(p => `<b>${p.name}</b>${getDetails(p.progress)}`)
      .join(', '),
  }
}

/** Reduce a (possibly nested) phase progress to its overall leaf Progress. */
export function phaseLeaf(p: T.PhaseProgress): T.Progress {
  if (p !== null && typeof p === 'object' && 'overall' in p) {
    return p.overall
  }
  return p as T.Progress
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

function getDetails(progress: false | ProgressDetails) {
  return progress
    ? `: ${progress.done}/${progress.total} ${progress.units || ''}`
    : ''
}

type ProgressDetails = {
  done: number
  total: number | null
  units: T.ProgressUnits | null
}
