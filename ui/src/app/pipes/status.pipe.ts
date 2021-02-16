import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from '../models/patch-db/data-model'
import { ConnectionState } from '../services/connection.service'
import { FEStatus, renderPkgStatus } from '../services/pkg-status-rendering.service'

@Pipe({
  name: 'status',
})
export class StatusPipe implements PipeTransform {
  transform (pkg: PackageDataEntry, connection: ConnectionState): FEStatus {
    return renderPkgStatus(pkg, connection).feStatus
  }
}