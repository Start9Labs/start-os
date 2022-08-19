import {
  ClassProvider,
  ExistingProvider,
  InjectionToken,
  NgModule,
  Type,
} from '@angular/core'
import { Observable } from 'rxjs'
import { OfflineService } from './services/offline.service'
import { LogoutService } from './services/logout.service'
import { PatchMonitorService } from './services/patch-monitor.service'
import { PatchDataService } from './services/patch-data.service'
import { UnreadToastService } from './services/unread-toast.service'
import { RefreshToastService } from './services/refresh-toast.service'
import { UpdateToastService } from './services/update-toast.service'

export const GLOBAL_SERVICE = new InjectionToken<
  readonly Observable<unknown>[]
>('A multi token of global Observable services')

// This module is purely for providers organization purposes
@NgModule({
  providers: [
    [
      LogoutService,
      OfflineService,
      RefreshToastService,
      UnreadToastService,
      UpdateToastService,
    ].map(useClass),
    [PatchDataService, PatchMonitorService].map(useExisting),
  ],
})
export class GlobalModule {}

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
