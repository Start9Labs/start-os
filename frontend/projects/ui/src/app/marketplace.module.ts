import { NgModule } from '@angular/core'
import {
  LIST_HEADER_CONTENT,
  LOAD_TRIGGER,
  LOCAL_PACKAGES,
  LocalPackages,
  AbstractMarketplaceService,
} from '@start9labs/marketplace'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { BadgeMenuComponent } from 'src/app/components/badge-menu-button/badge-menu.component'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { Observable } from 'rxjs'
import { filter, first, tap } from 'rxjs/operators'
import { exists, isEmptyObject } from '@start9labs/shared'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'

@NgModule({
  imports: [BadgeMenuComponentModule],
  providers: [
    {
      provide: AbstractMarketplaceService,
      useClass: MarketplaceService,
    },
    {
      provide: LIST_HEADER_CONTENT,
      useValue: BadgeMenuComponent,
    },
    {
      provide: LOAD_TRIGGER,
      deps: [PatchDbService],
      useFactory: loadTriggerFactory,
    },
    {
      provide: LOCAL_PACKAGES,
      deps: [PatchDbService],
      useFactory: localPackagesFactory,
    },
  ],
})
export class MarketplaceModule {}

export function loadTriggerFactory(patch: PatchDbService): Observable<unknown> {
  return patch.watch$('server-info').pipe(
    filter(data => exists(data) && !isEmptyObject(data)),
    first(),
  )
}

export function localPackagesFactory(
  patch: PatchDbService,
): Observable<LocalPackages> {
  return patch.watch$('package-data').pipe(
    filter(data => exists(data) && !isEmptyObject(data)),
    tap(pkgs =>
      Object.values(pkgs).forEach(pkg => {
        pkg['install-progress'] = { ...pkg['install-progress'] }
      }),
    ),
  )
}
