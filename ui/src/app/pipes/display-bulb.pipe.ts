import { Pipe, PipeTransform } from '@angular/core'
import { combineLatest, Observable } from 'rxjs'
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
    return combineLatest([
      this.patch.watch$('package-data', pkgId, 'state'),
      this.patch.watch$('package-data', pkgId, 'installed', 'status'),
    ])
    .pipe(
      map(([state, status]) => {
        if (!connected) return bulb === 'off'
        const { color } = renderPkgStatus(state, status)
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
