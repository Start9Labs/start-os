import { Inject, InjectionToken, NgModule, OnDestroy } from '@angular/core'
import { merge, Observable } from 'rxjs'
import { OfflineService } from './services/offline.service'
import { LogoutService } from './services/logout.service'
import { PatchMonitorService } from './services/patch-monitor.service'
import { PatchDataService } from './services/patch-data.service'
import { ConnectionMonitorService } from './services/connection-monitor.service'
import { UnreadToastService } from './services/unread-toast.service'
import { RefreshToastService } from './services/refresh-toast.service'
import { UpdateToastService } from './services/update-toast.service'

const GLOBAL_SERVICE = new InjectionToken<readonly Observable<unknown>[]>(
  'A multi token of global Observable services',
)

@NgModule({
  providers: [
    {
      provide: GLOBAL_SERVICE,
      multi: true,
      useClass: ConnectionMonitorService,
    },
    {
      provide: GLOBAL_SERVICE,
      multi: true,
      useClass: LogoutService,
    },
    {
      provide: GLOBAL_SERVICE,
      multi: true,
      useClass: OfflineService,
    },
    {
      provide: GLOBAL_SERVICE,
      multi: true,
      useExisting: PatchDataService,
    },
    {
      provide: GLOBAL_SERVICE,
      multi: true,
      useExisting: PatchMonitorService,
    },
    {
      provide: GLOBAL_SERVICE,
      multi: true,
      useClass: RefreshToastService,
    },
    {
      provide: GLOBAL_SERVICE,
      multi: true,
      useClass: UnreadToastService,
    },
    {
      provide: GLOBAL_SERVICE,
      multi: true,
      useClass: UpdateToastService,
    },
  ],
})
export class GlobalModule implements OnDestroy {
  readonly subscription = merge(...this.services).subscribe()

  constructor(
    @Inject(GLOBAL_SERVICE)
    private readonly services: readonly Observable<unknown>[],
  ) {}

  ngOnDestroy() {
    this.subscription.unsubscribe()
  }
}
