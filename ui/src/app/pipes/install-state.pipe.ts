import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from '../services/patch-db/data-model'
import {
  packageLoadingProgress,
  ProgressData,
} from '../util/package-loading-progress'

@Pipe({
  name: 'installState',
})
export class InstallState implements PipeTransform {
  transform(pkg: PackageDataEntry): ProgressData | null {
    return packageLoadingProgress(pkg['install-progress'])
  }
}
