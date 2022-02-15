import { Pipe, PipeTransform } from '@angular/core'
import { InstallProgress, packageLoadingProgress } from '@start9labs/shared'

@Pipe({
  name: 'installProgress',
})
export class InstallProgressPipe implements PipeTransform {
  transform(loadData: InstallProgress): string {
    const { totalProgress } = packageLoadingProgress(loadData)

    return totalProgress < 99 ? totalProgress + '%' : 'finalizing'
  }
}
