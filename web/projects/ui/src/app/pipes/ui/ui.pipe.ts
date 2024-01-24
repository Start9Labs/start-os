import { Pipe, PipeTransform } from '@angular/core'
import { InstalledPackageDataEntry } from '../../services/patch-db/data-model'
import { hasUi } from '../../services/config.service'

@Pipe({
  name: 'hasUi',
})
export class UiPipe implements PipeTransform {
  transform(
    interfaces: InstalledPackageDataEntry['network-interfaces'],
  ): boolean {
    return interfaces ? hasUi(interfaces) : false
  }
}
