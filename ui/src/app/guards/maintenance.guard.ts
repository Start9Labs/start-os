import { Injectable } from '@angular/core'
import { CanActivate, Router, CanActivateChild } from '@angular/router'
import { ServerStatus } from '../services/patch-db/data-model'
import { PatchDbService } from '../services/patch-db/patch-db.service'

@Injectable({
  providedIn: 'root',
})
export class MaintenanceGuard implements CanActivate, CanActivateChild {
  serverStatus: ServerStatus
  isFullyDownloaded: boolean = false

  constructor (
    private readonly router: Router,
    private readonly patch: PatchDbService,
  ) {
    this.patch.watch$('server-info', 'status').subscribe(status => {
      this.serverStatus = status
    })
    this.patch.watch$('server-info', 'update-progress').subscribe(progress => {
      this.isFullyDownloaded = !!progress && (progress.size === progress.downloaded)
    })
  }

  canActivate (): boolean {
    return this.runServerStatusCheck()
  }

  canActivateChild (): boolean {
    return this.runServerStatusCheck()
  }

  private runServerStatusCheck (): boolean {
    if (ServerStatus.BackingUp === this.serverStatus && !this.isFullyDownloaded) {
      this.router.navigate(['/maintenance'], { replaceUrl: true })
      return false
    } else {
      return true
    }
  }
}