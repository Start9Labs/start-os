import { Pipe, PipeTransform } from '@angular/core'
import { i18nKey, leafProgress } from '@start9labs/shared'
import { T } from '@start9labs/start-core'

@Pipe({
  name: 'installingProgress',
})
export class InstallingProgressPipe implements PipeTransform {
  transform(progress: T.Progress | undefined = false): number {
    const leaf = leafProgress(progress)
    if (leaf === true) return 100
    if (leaf === false || leaf === null || !leaf.total) return 0
    return Math.floor((100 * leaf.done) / leaf.total)
  }
}

export function getProgressText(progress: T.Progress): i18nKey {
  const leaf = leafProgress(progress)
  if (leaf === true) return 'finalizing'
  if (!leaf || !leaf.total) return 'unknown %'

  const percentage = Math.round((100 * leaf.done) / leaf.total)

  return percentage < 99 ? (`${percentage}%` as i18nKey) : 'finalizing'
}
