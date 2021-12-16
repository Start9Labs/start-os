import { Pipe, PipeTransform } from '@angular/core'
import { InstallProgress } from '../services/patch-db/data-model'
import {
  packageLoadingProgress,
  ProgressData,
} from '../util/package-loading-progress'

@Pipe({
  name: 'installState',
})
export class InstallState implements PipeTransform {
  transform(loadData: InstallProgress): ProgressData | null {
    return packageLoadingProgress(loadData)
  }
}
