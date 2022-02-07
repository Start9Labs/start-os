import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from '@start9labs/shared'
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
