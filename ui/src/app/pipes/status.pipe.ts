import { Pipe, PipeTransform } from '@angular/core'
import { Observable } from 'rxjs'
import { map } from 'rxjs/operators'
import { PatchDbModel } from '../services/patch-db/patch-db.service'
import { FEStatus, renderPkgStatus } from '../services/pkg-status-rendering.service'

@Pipe({
  name: 'status',
})
export class StatusPipe implements PipeTransform {

  constructor (
    private readonly patch: PatchDbModel,
  ) { }

  transform (pkgId: string): Observable<FEStatus> {
    return this.patch.sequence$.pipe(
      map(_ => {
        const pkg = this.patch.data['package-data'][pkgId]
        return renderPkgStatus(pkg.state, pkg.installed.status).feStatus
      }),
    )
  }
}