import { Pipe, PipeTransform } from '@angular/core'
import { i18nKey, phaseLeaf } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'

@Pipe({
  name: 'installingProgress',
})
export class InstallingProgressPipe implements PipeTransform {
  transform(progress: T.PhaseProgress = false): number {
    const leaf = phaseLeaf(progress)
    if (leaf === true) return 100
    if (leaf === false || leaf === null || !leaf.total) return 0
    return Math.floor((100 * leaf.done) / leaf.total)
  }
}

export function getProgressText(progress: T.Progress): i18nKey {
  if (progress === true) return 'finalizing'
  if (!progress || !progress.total) return 'unknown %'

  const percentage = Math.round((100 * progress.done) / progress.total)

  return percentage < 99 ? (`${percentage}%` as i18nKey) : 'finalizing'
}
