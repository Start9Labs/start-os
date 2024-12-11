import { inject, Injectable } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { endWith, ReplaySubject, shareReplay, Subject, switchMap } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Injectable({
  providedIn: 'root',
})
export class SideloadService {
  private readonly api = inject(ApiService)
  private readonly guid$ = new Subject<string>()

  readonly progress$ = this.guid$.pipe(
    switchMap(guid =>
      this.api.openWebsocket$<T.FullProgress>(guid).pipe(endWith(null)),
    ),
    shareReplay(1),
  )

  followProgress(guid: string) {
    this.guid$.next(guid)
  }
}
