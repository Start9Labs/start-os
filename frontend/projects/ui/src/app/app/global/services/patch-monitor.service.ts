import { Injectable } from '@angular/core'
import { Storage } from '@ionic/storage-angular'
import { from, Observable, of } from 'rxjs'
import { mapTo, share, switchMap } from 'rxjs/operators'

import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { AuthService } from 'src/app/services/auth.service'

// Start and stop PatchDb upon verification
@Injectable({
  providedIn: 'root',
})
export class PatchMonitorService extends Observable<boolean> {
  private readonly stream$ = this.authService.isVerified$.pipe(
    switchMap(verified => {
      if (verified) {
        return from(this.patch.start()).pipe(mapTo(true))
      }

      this.patch.stop()
      this.storage.clear()

      return of(false)
    }),
    share(),
  )

  constructor(
    private readonly authService: AuthService,
    private readonly patch: PatchDbService,
    private readonly storage: Storage,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
