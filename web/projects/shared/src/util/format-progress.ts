// @TODO Matt this is T.FullProgress but shared does not depend on sdk
type Progress = null | boolean | { done: number; total: number | null }
type NamedProgress = { name: string; progress: Progress }
type FullProgress = { overall: Progress; phases: Array<NamedProgress> }

export function formatProgress({ phases, overall }: FullProgress): {
  total: number
  message: string
} {
  return {
    total: getDecimal(overall),
    message: phases
      .filter(p => p.progress !== true && p.progress !== null)
      .map(p => `${p.name}${getPhaseBytes(p.progress)}`)
      .join(', '),
  }
}

function getDecimal(progress: Progress): number {
  if (progress === true) {
    return 1
  } else if (!progress || !progress.total) {
    return 0
  } else {
    return progress.total && progress.done / progress.total
  }
}

function getPhaseBytes(progress: Progress): string {
  return progress === true || !progress
    ? ''
    : `: (${progress.done}/${progress.total})`
}
