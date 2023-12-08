import { StaticClassProvider } from '@angular/core'
import { defer, Observable, switchMap } from 'rxjs'
import { WebSocketSubjectConfig } from 'rxjs/webSocket'
import { FollowLogsReq, FollowLogsRes, Log } from '../types/api'
import { Constructor } from '../types/constructor'

interface Api {
  followServerLogs: (params: FollowLogsReq) => Promise<FollowLogsRes>
  openLogsWebsocket$: (config: WebSocketSubjectConfig<Log>) => Observable<Log>
}

export function provideSetupLogsService(
  api: Constructor<Api>,
): StaticClassProvider {
  return {
    provide: SetupLogsService,
    deps: [api],
    useClass: SetupLogsService,
  }
}

export class SetupLogsService extends Observable<Log> {
  private readonly log$ = defer(() => this.api.followServerLogs({})).pipe(
    switchMap(({ guid }) =>
      this.api.openLogsWebsocket$({ url: `/rpc/${guid}` }),
    ),
  )

  constructor(private readonly api: Api) {
    super(subscriber => this.log$.subscribe(subscriber))
  }
}
