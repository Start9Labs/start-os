import { Pipe, PipeTransform } from '@angular/core'
import { combineLatest, Observable } from 'rxjs'
import { map } from 'rxjs/operators'
import { PatchDbService } from '../services/patch-db/patch-db.service'
import { FEStatus, renderPkgStatus } from '../services/pkg-status-rendering.service'

@Pipe({
  name: 'status',
})
export class StatusPipe implements PipeTransform {

  constructor (
    private readonly patch: PatchDbService,
  ) { }

  transform (pkgId: string): Observable<FEStatus> {
    return combineLatest([
      this.patch.watch$('package-data', pkgId, 'state'),
      this.patch.watch$('package-data', pkgId, 'installed', 'status'),
    ])
    .pipe(
      map(([state, status]) => {
        return renderPkgStatus(state, status).feStatus
      }),
    )
  }
}