import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ProgressData } from 'src/app/types/progress-data'
import { packageLoadingProgress } from 'src/app/util/package-loading-progress'

@Pipe({
  name: 'progressData',
})
export class ProgressDataPipe implements PipeTransform {
  transform(pkg: PackageDataEntry): ProgressData | null {
    return packageLoadingProgress(pkg['install-progress'])
  }
}
