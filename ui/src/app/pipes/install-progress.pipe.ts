import { Pipe, PipeTransform } from '@angular/core'
import { InstallProgress } from '../services/patch-db/data-model'
import { packageLoadingProgress } from '../util/package-loading-progress'

@Pipe({
  name: 'installProgress',
})
export class InstallProgressPipe implements PipeTransform {
  transform(loadData: InstallProgress): string {
    const { totalProgress } = packageLoadingProgress(loadData)

    return totalProgress < 99 ? totalProgress + '%' : 'finalizing'
  }
}
