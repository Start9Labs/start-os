import { Injectable } from '@angular/core'
import { tap, Observable } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { AuthService } from 'src/app/services/auth.service'
import { DataModel } from './patch-db/data-model'

// Start and stop PatchDb upon verification
@Injectable({
  providedIn: 'root',
})
export class PatchMonitorService extends Observable<unknown> {
  private readonly stream$ = this.authService.isVerified$.pipe(
    tap(verified => (verified ? this.patch.start() : this.patch.stop())),
  )

  constructor(
    private readonly authService: AuthService,
    private readonly patch: PatchDB<DataModel>,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
