import { Pipe, PipeTransform } from '@angular/core'
import { Progress } from 'src/app/services/patch-db/data-model'

@Pipe({
  name: 'installProgress',
  standalone: true,
})
export class InstallingProgressDisplayPipe implements PipeTransform {
  transform(progress: Progress): string {
    if (progress === true) return 'finalizing'
    if (progress === false || !progress.total) return 'unknown %'
    const percentage = Math.round((100 * progress.done) / progress.total)

    return percentage < 99 ? String(percentage) + '%' : 'finalizing'
  }
}
