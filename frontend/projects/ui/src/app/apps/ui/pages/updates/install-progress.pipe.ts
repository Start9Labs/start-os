import { Pipe, PipeTransform } from '@angular/core'
import { InstallProgress } from 'src/app/services/patch-db/data-model'
import { packageLoadingProgress } from 'src/app/util/package-loading-progress'

@Pipe({
  name: 'installProgress',
})
export class InstallProgressPipe implements PipeTransform {
  transform(installProgress?: InstallProgress): number {
    return packageLoadingProgress(installProgress)?.totalProgress || 0
  }
}
