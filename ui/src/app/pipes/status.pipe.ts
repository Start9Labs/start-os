import { Pipe, PipeTransform } from '@angular/core'
import { PatchDbModel } from '../services/patch-db/patch-db.service'
import { FEStatus, renderPkgStatus } from '../services/pkg-status-rendering.service'

@Pipe({
  name: 'status',
})
export class StatusPipe implements PipeTransform {

  constructor (
    private readonly patch: PatchDbModel,
  ) { }

  transform (pkgId: string): FEStatus {
    console.log(pkgId)
    const pkg = this.patch.data['package-data'][pkgId]
    return renderPkgStatus(pkg.state, pkg.installed.status).feStatus
  }
}