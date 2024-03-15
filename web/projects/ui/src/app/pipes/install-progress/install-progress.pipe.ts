import { Pipe, PipeTransform } from '@angular/core'
import { Progress } from 'src/app/services/patch-db/data-model'

@Pipe({
  name: 'installingProgressString',
})
export class InstallingProgressDisplayPipe implements PipeTransform {
  transform(progress: Progress): string {
    if (progress === true) return 'finalizing'
    if (progress === false || !progress.total) return 'unknown %'
    const percentage = Math.round((100 * progress.done) / progress.total)

    return percentage < 99 ? String(percentage) + '%' : 'finalizing'
  }
}

@Pipe({
  name: 'installingProgress',
})
export class InstallingProgressPipe implements PipeTransform {
  transform(progress: Progress): number | null {
    if (progress === true) return 1
    if (progress === false || !progress.total) return null
    return Number((progress.done / progress.total).toFixed(2))
  }
}

function getProgress(progress: Progress): number | null {
  if (progress === true) return 1
  if (progress === false || !progress.total) return null
  return Number((progress.done / progress.total).toFixed(2))
}
