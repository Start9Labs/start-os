import {
  ClassProvider,
  ExistingProvider,
  Inject,
  InjectionToken,
  NgModule,
  OnDestroy,
  Type,
} from '@angular/core'
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
    [
      ConnectionMonitorService,
      LogoutService,
      OfflineService,
      RefreshToastService,
      UnreadToastService,
      UpdateToastService,
    ].map(useClass),
    [PatchDataService, PatchMonitorService].map(useExisting),
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

function useClass(useClass: Type<unknown>): ClassProvider {
  return {
    provide: GLOBAL_SERVICE,
    multi: true,
    useClass,
  }
}

function useExisting(useExisting: Type<unknown>): ExistingProvider {
  return {
    provide: GLOBAL_SERVICE,
    multi: true,
    useExisting,
  }
}
