import { Pipe, PipeTransform } from '@angular/core'
import {
  ProgressData,
  PackageDataEntry,
  packageLoadingProgress,
} from '@start9labs/shared'

@Pipe({
  name: 'installState',
})
export class InstallStatePipe implements PipeTransform {
  transform(pkg: PackageDataEntry): ProgressData | null {
    return packageLoadingProgress(pkg['install-progress'])
  }
}
