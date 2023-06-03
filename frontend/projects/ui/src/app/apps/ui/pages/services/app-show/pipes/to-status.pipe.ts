import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import {
  PackageStatus,
  renderPkgStatus,
} from 'src/app/services/pkg-status-rendering.service'

@Pipe({
  name: 'toStatus',
})
export class ToStatusPipe implements PipeTransform {
  transform(pkg: PackageDataEntry): PackageStatus {
    return renderPkgStatus(pkg)
  }
}
