import { Pipe, PipeTransform } from '@angular/core'
import { Observable } from 'rxjs'
import { map } from 'rxjs/operators'
import { PatchDbModel } from '../services/patch-db/patch-db.service'
import { renderPkgStatus } from '../services/pkg-status-rendering.service'

@Pipe({
  name: 'displayBulb',
})
export class DisplayBulbPipe implements PipeTransform {

  constructor (
    private readonly patch: PatchDbModel,
  ) { }

  transform (pkgId: string, bulb: DisplayBulb, connected: boolean): Observable<boolean> {
    return this.patch.sequence$.pipe(
      map(_ => {
        if (!connected) return bulb === 'off'
        const pkg = this.patch.data['package-data'][pkgId]
        const { color } = renderPkgStatus(pkg.state, pkg.installed.status)
        switch (color) {
          case 'danger': return bulb === 'red'
          case 'success': return bulb === 'green'
          case 'warning': return bulb === 'yellow'
          default: return bulb === 'off'
        }
      }),
    )
  }
}

type DisplayBulb = 'off' | 'red' | 'green' | 'yellow'
