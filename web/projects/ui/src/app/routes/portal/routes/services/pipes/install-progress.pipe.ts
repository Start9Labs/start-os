import { Pipe, PipeTransform } from '@angular/core'
import { T } from '@start9labs/start-sdk'

@Pipe({
  standalone: true,
  name: 'installingProgress',
})
export class InstallingProgressPipe implements PipeTransform {
  transform(progress: T.Progress = false): number {
    if (progress === true) return 100
    if (progress === false || progress === null || !progress.total) return 0
    return Math.floor((100 * progress.done) / progress.total)
  }
}

export function getProgressText(progress: T.Progress): string {
  if (progress === true) return 'finalizing'
  if (!progress || !progress.total) return 'unknown %'

  const percentage = Math.round((100 * progress.done) / progress.total)

  return percentage < 99 ? `${percentage}%` : 'finalizing'
}
