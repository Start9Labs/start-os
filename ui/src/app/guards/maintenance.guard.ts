import { Injectable } from '@angular/core'
import { CanActivate, Router, CanActivateChild } from '@angular/router'
import { tap } from 'rxjs/operators'
import { ServerStatus } from '../services/patch-db/data-model'
import { PatchDbModel } from '../services/patch-db/patch-db.service'

@Injectable({
  providedIn: 'root',
})
export class MaintenanceGuard implements CanActivate, CanActivateChild {
  constructor (
    private readonly router: Router,
    private readonly patch: PatchDbModel,
  ) { }

  canActivate (): boolean {
    return this.runServerStatusCheck()
  }

  canActivateChild (): boolean {
    return this.runServerStatusCheck()
  }

  private runServerStatusCheck (): boolean {
    if ([ServerStatus.Updating, ServerStatus.BackingUp].includes(this.patch.data['server-info']?.status)) {
      this.router.navigate(['/maintenance'], { replaceUrl: true })
      return false
    } else {
      return true
    }
  }
}