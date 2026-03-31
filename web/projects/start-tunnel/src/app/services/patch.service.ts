import { inject, Injectable } from '@angular/core'
import { tap, Observable } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { TunnelData } from './patch-db/data-model'
import { AuthService } from './auth.service'
import { toObservable } from '@angular/core/rxjs-interop'

@Injectable({
  providedIn: 'root',
})
export class PatchService extends Observable<unknown> {
  private readonly patch = inject<PatchDB<TunnelData>>(PatchDB)
  private readonly auth = inject(AuthService)

  private readonly stream$ = toObservable(this.auth.authenticated).pipe(
    tap(authed => (authed ? this.patch.start() : this.patch.stop())),
  )

  constructor() {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
