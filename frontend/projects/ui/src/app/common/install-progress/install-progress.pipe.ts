import { Pipe, PipeTransform } from '@angular/core'
import { InstallProgress } from '../../services/patch-db/data-model'
import { packageLoadingProgress } from '../../util/package-loading-progress'

@Pipe({
  name: 'installProgressDisplay',
})
export class InstallProgressDisplayPipe implements PipeTransform {
  transform(installProgress?: InstallProgress): string {
    const totalProgress =
      packageLoadingProgress(installProgress)?.totalProgress || 0

    return totalProgress < 99 ? totalProgress + '%' : 'finalizing'
  }
}
