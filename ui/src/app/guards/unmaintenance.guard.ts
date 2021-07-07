import { Injectable } from '@angular/core'
import { CanActivate, Router } from '@angular/router'
import { tap } from 'rxjs/operators'
import { ServerStatus } from '../services/patch-db/data-model'
import { PatchDbModel } from '../services/patch-db/patch-db.service'

@Injectable({
  providedIn: 'root',
})
export class UnmaintenanceGuard implements CanActivate {
  serverStatus: ServerStatus

  constructor (
    private readonly router: Router,
    private readonly patch: PatchDbModel,
  ) {
    this.patch.sequence$.subscribe(_ => {
      this.serverStatus = this.patch.data['server-info'].status
    })
  }

  canActivate (): boolean {
    if (![ServerStatus.Updating, ServerStatus.BackingUp].includes(this.serverStatus)) {
      this.router.navigate([''], { replaceUrl: true })
      return false
    } else {
      return true
    }
  }
}