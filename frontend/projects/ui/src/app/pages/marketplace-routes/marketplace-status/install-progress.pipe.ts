import { Pipe, PipeTransform } from '@angular/core'
import { InstallProgress } from 'src/app/types/install-progress'
import { packageLoadingProgress } from 'src/app/util/package-loading-progress'

@Pipe({
  name: 'installProgress',
})
export class InstallProgressPipe implements PipeTransform {
  transform(loadData?: InstallProgress): string {
    const totalProgress = packageLoadingProgress(loadData)?.totalProgress || 0

    return totalProgress < 99 ? totalProgress + '%' : 'finalizing'
  }
}
