import { Pipe, PipeTransform } from '@angular/core'
import { PackageLoadingService, ProgressData } from '../services/package-loading.service'
import { InstallProgress } from '../services/patch-db/data-model'

@Pipe({
  name: 'installState',
})
export class InstallState implements PipeTransform {

  constructor (
    private readonly pkgLoading: PackageLoadingService,
  ) { }

  transform (loadData: InstallProgress): ProgressData {
    return this.pkgLoading.transform(loadData)
  }
}
