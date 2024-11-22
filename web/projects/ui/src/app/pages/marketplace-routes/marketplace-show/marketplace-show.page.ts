import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute, Router } from '@angular/router'
import { convertBytes, Exver, getPkgId } from '@start9labs/shared'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import { PatchDB } from 'patch-db-client'
import { combineLatest, Observable } from 'rxjs'
import {
  filter,
  first,
  map,
  pairwise,
  shareReplay,
  startWith,
  switchMap,
} from 'rxjs/operators'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/util/get-package-data'
import { Version, VersionRange } from '@start9labs/start-sdk'

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
    this.patch.watch$('packageData', this.pkgId).pipe(
      map(pkg => getManifest(pkg).version),
      pairwise(),
      filter(([prev, curr]) => prev !== curr),
      map(([_, curr]) => curr),
    ),
    this.patch.watch$('serverInfo').pipe(first()),
  ]).pipe(
    map(([pkg, localVersion, server]) => {
      let conflicts: string[] = []

      // OS version
      if (
        !Version.parse(pkg.osVersion).satisfies(
          VersionRange.parse(server.packageVersionCompat),
        )
      ) {
        const compare = Version.parse(pkg.osVersion).compare(
          Version.parse(server.version),
        )
        conflicts.push(
          compare === 'greater'
            ? `Minimum StartOS version ${pkg.osVersion}. Detected ${server.version}`
            : `Version ${pkg.version} is outdated and cannot run newer versions of StartOS`,
        )
      }

      // package version
      if (
        localVersion &&
        pkg.sourceVersion &&
        !this.exver.satisfies(localVersion, pkg.sourceVersion)
      ) {
        conflicts.push(
          `Currently installed version ${localVersion} cannot be upgraded to version ${pkg.version}. Try installing an older version first.`,
        )
      }

      const { arch, ram, device } = pkg.hardwareRequirements

      // arch
      if (arch && !arch.includes(server.arch)) {
        conflicts.push(
          `Arch ${server.arch} is not supported. Supported: ${arch.join(
            ', ',
          )}.`,
        )
      }

      // ram
      if (ram && ram > server.ram) {
        conflicts.push(
          `Minimum ${convertBytes(
            ram,
          )} of RAM required, detected ${convertBytes(server.ram)}.`,
        )
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
      this.marketplaceService.getSelectedStore$().pipe(
        map(s =>
          s.packages.filter(
            p => p.id === this.pkgId && p.flavor !== paramMap.get('flavor'),
          ),
        ),
        filter(p => p.length > 0),
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
