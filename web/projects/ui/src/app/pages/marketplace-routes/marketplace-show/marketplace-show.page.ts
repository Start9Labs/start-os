import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { Exver, getPkgId } from '@start9labs/shared'
import {
  AbstractMarketplaceService,
  AbstractPkgFlavorService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import { PatchDB } from 'patch-db-client'
import { BehaviorSubject, combineLatest, Observable } from 'rxjs'
import { filter, map, shareReplay, switchMap } from 'rxjs/operators'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/util/get-package-data'
import { ExtendedVersion } from '@start9labs/start-sdk'

@Component({
  selector: 'marketplace-show',
  templateUrl: './marketplace-show.page.html',
  styleUrls: ['./marketplace-show.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowPage {
  readonly pkgId = getPkgId(this.route)
  readonly url = this.route.snapshot.queryParamMap.get('url') || undefined

  readonly loadVersion$ = new BehaviorSubject<string>('*')

  readonly localPkg$ = combineLatest([
    this.patch.watch$('packageData', this.pkgId).pipe(filter(Boolean)),
    this.loadVersion$,
  ]).pipe(
    map(([pkg, version]) => {
      if (ExtendedVersion.parse(getManifest(pkg).version).flavor) {
        this.pkgFlavorService.toggleFlavorStatus(true)
      }
      if (
        version === '*' ||
        this.exver.greaterThanOrEqual(version, getManifest(pkg).version || '')
      ) {
        return pkg
      }
      return null
    }),
    shareReplay({ bufferSize: 1, refCount: true }),
  )

  readonly pkg$: Observable<MarketplacePkg> = this.loadVersion$.pipe(
    switchMap(version =>
      this.marketplaceService.getPackage$(
        {
          id: this.pkgId,
          version,
          otherVersions: 'short',
          sourceVersion: null,
        },
        this.url,
      ),
    ),
  )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
    private readonly marketplaceService: AbstractMarketplaceService,
    private readonly exver: Exver,
    private readonly pkgFlavorService: AbstractPkgFlavorService,
  ) {}
}
