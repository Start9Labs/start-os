import { Injectable } from '@angular/core'
import { Observable } from 'rxjs'
import { tap } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { AuthService } from 'src/app/services/auth.service'
import { DataModel } from './patch-db/data-model'
import { LocalStorageBootstrap } from './patch-db/local-storage-bootstrap'

// Start and stop PatchDb upon verification
@Injectable({
  providedIn: 'root',
})
export class PatchMonitorService extends Observable<any> {
  // @TODO not happy with Observable<void>
  private readonly stream$ = this.authService.isVerified$.pipe(
    tap(verified => {
      if (verified) {
        this.patch.start(this.bootstrapper)
      } else {
        this.patch.stop()
      }
    }),
  )

  constructor(
    private readonly authService: AuthService,
    private readonly patch: PatchDB<DataModel>,
    private readonly bootstrapper: LocalStorageBootstrap,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
