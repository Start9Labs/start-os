import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from '../services/patch-db/data-model'
import { FEStatus, renderPkgStatus } from '../services/pkg-status-rendering.service'

@Pipe({
  name: 'status',
})
export class StatusPipe implements PipeTransform {
  transform (pkg: PackageDataEntry): FEStatus {
    return renderPkgStatus(pkg).feStatus
  }
}