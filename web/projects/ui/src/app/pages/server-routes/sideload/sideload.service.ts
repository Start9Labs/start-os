import { Injectable } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { endWith, shareReplay, Subject, switchMap } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Injectable({
  providedIn: 'root',
})
export class SideloadService {
  private readonly guid$ = new Subject<string>()

  readonly progress$ = this.guid$.pipe(
    switchMap(guid =>
      this.api.openWebsocket$<T.FullProgress>(guid).pipe(endWith(null)),
    ),
    shareReplay(1),
  )

  constructor(private readonly api: ApiService) {}

  followProgress(guid: string) {
    this.guid$.next(guid)
  }
}
