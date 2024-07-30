import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute, Router } from '@angular/router'
import { Exver, getPkgId } from '@start9labs/shared'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import { PatchDB } from 'patch-db-client'
import { combineLatest, Observable } from 'rxjs'
import { filter, map, shareReplay, startWith, switchMap } from 'rxjs/operators'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/util/get-package-data'

@Component({
  selector: 'marketplace-show',
  templateUrl: './marketplace-show.page.html',
  styleUrls: ['./marketplace-show.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowPage {
  readonly pkgId = getPkgId(this.route)
  readonly url = this.route.snapshot.queryParamMap.get('url') || undefined

  readonly localPkg$ = combineLatest([
    this.patch.watch$('packageData', this.pkgId).pipe(filter(Boolean)),
    this.route.queryParamMap,
  ]).pipe(
    map(([localPkg, paramMap]) =>
      this.exver.getFlavor(getManifest(localPkg).version) ===
      paramMap.get('flavor')
        ? localPkg
        : null,
    ),
    shareReplay({ bufferSize: 1, refCount: true }),
  )

  readonly localFlavor$ = this.localPkg$.pipe(
    map(pkg => !pkg),
    startWith(false),
  )

  readonly pkg$: Observable<MarketplacePkg> = this.route.queryParamMap.pipe(
    switchMap(paramMap =>
      this.marketplaceService.getPackage$(
        this.pkgId,
        paramMap.get('version'),
        paramMap.get('flavor'),
        this.url,
      ),
    ),
  )

  readonly conflict$: Observable<string> = combineLatest([
    this.pkg$,
    this.patch.watch$('packageData', this.pkgId),
    this.patch.watch$('serverInfo'),
  ]).pipe(
    map(([pkg, localPkg, server]) => {
      let conflicts: string[] = []

      // version
      if (localPkg) {
        const localVersion = getManifest(localPkg).version
        if (
          pkg.sourceVersion &&
          !this.exver.satisfies(localVersion, pkg.sourceVersion)
        ) {
          conflicts.push(
            `Currently installed version ${localVersion} cannot be upgraded to version ${pkg.version}. Try installing an older version first.`,
          )
        }
      }

      const { arch, ram, device } = pkg.hardwareRequirements

      // arch
      if (arch && !arch.includes(server.arch)) {
        conflicts.push(
          `Arch ${server.arch} not supported. Supported: ${arch.join(', ')}.`,
        )
      }

      // ram
      if (ram && ram > server.ram) {
        return `Minimum ${ram}GB of RAM required, detected ${server.ram}GB.`
      }

      // devices
      conflicts.concat(
        device
          .filter(d =>
            server.devices.some(
              sd =>
                d.class === sd.class && !new RegExp(d.pattern).test(sd.product),
            ),
          )
          .map(d => d.patternDescription),
      )

      return conflicts.join(' ')
    }),
    shareReplay({ bufferSize: 1, refCount: true }),
  )

  readonly flavors$ = this.route.queryParamMap.pipe(
    switchMap(paramMap =>
      this.marketplaceService
        .getSelectedStore$()
        .pipe(
          map(s =>
            s.packages.filter(
              p => p.id === this.pkgId && p.flavor !== paramMap.get('flavor'),
            ),
          ),
        ),
    ),
  )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly router: Router,
    private readonly patch: PatchDB<DataModel>,
    private readonly marketplaceService: AbstractMarketplaceService,
    private readonly exver: Exver,
  ) {}

  updateVersion(version: string) {
    this.router.navigate([], {
      relativeTo: this.route,
      queryParams: { version },
      queryParamsHandling: 'merge',
    })
  }
}
