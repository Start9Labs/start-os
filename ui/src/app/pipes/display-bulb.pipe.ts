import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from '../models/patch-db/data-model'
import { ConnectionState } from '../services/connection.service'
import { renderPkgStatus } from '../services/pkg-status-rendering.service'

@Pipe({
  name: 'displayBulb',
})
export class DisplayBulbPipe implements PipeTransform {

  transform (pkg: PackageDataEntry, bulb: DisplayBulb, connection: ConnectionState): boolean {
    const { color } = renderPkgStatus(pkg, connection)
    switch (color) {
      case 'danger': return bulb === 'red'
      case 'success': return bulb === 'green'
      case 'warning': return bulb === 'yellow'
      default: return bulb === 'off'
    }
  }
}

type DisplayBulb = 'off' | 'red' | 'green' | 'yellow'
