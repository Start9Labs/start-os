import { Injectable } from '@angular/core'
import { CanActivate, Router } from '@angular/router'
import { ServerStatus } from '../services/patch-db/data-model'
import { PatchDbService } from '../services/patch-db/patch-db.service'

@Injectable({
  providedIn: 'root',
})
export class UnmaintenanceGuard implements CanActivate {
  serverStatus: ServerStatus

  constructor (
    private readonly router: Router,
    private readonly patch: PatchDbService,
  ) {
    this.patch.watch$('server-info', 'status').subscribe(status => {
      this.serverStatus = status
    })
  }

  canActivate (): boolean {
    if (ServerStatus.BackingUp === this.serverStatus) {
      return true
    } else {
      this.router.navigate([''], { replaceUrl: true })
      return false
    }
  }
}