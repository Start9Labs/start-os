import { Pipe, PipeTransform } from '@angular/core'
import { InstalledPackageInfo } from 'src/app/services/patch-db/data-model'
import { hasUi } from '../../services/config.service'

@Pipe({
  name: 'hasUi',
})
export class UiPipe implements PipeTransform {
  transform(addressInfo: InstalledPackageInfo['address-info']): boolean {
    return hasUi(addressInfo)
  }
}
