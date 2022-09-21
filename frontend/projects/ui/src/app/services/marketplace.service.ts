import { Injectable } from '@angular/core'
import { Emver, ErrorToastService } from '@start9labs/shared'
import {
  MarketplacePkg,
  AbstractMarketplaceService,
  Marketplace,
  FilterPackagesPipe,
  MarketplaceData,
} from '@start9labs/marketplace'
import { from, Observable, of, Subject } from 'rxjs'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import {
  DataModel,
  ServerInfo,
  UIMarketplaceData,
} from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import {
  catchError,
  distinctUntilChanged,
  filter,
  map,
  shareReplay,
  startWith,
  switchMap,
  take,
  tap,
} from 'rxjs/operators'

@Injectable()
export class MarketplaceService extends AbstractMarketplaceService {
  private readonly notes = new Map<string, Record<string, string>>()
  private readonly hasPackages$ = new Subject<boolean>()

  private readonly uiMarketplaceData$ = this.patch
    .watch$('ui', 'marketplace')
    .pipe(
      map(
        m =>
          m || {
            'selected-id': null,
            'known-hosts': {},
          },
      ),
      distinctUntilChanged(
        (prev, curr) => prev['selected-id'] === curr['selected-id'],
      ),
      shareReplay(1),
    )

  private readonly marketplace$ = this.uiMarketplaceData$.pipe(
    map(data => this.toMarketplace(data)),
  )

  private readonly serverInfo$: Observable<ServerInfo> = this.patch
    .watch$('server-info')
    .pipe(take(1), shareReplay())

  private readonly registryData$: Observable<MarketplaceData> =
    this.uiMarketplaceData$.pipe(
      switchMap(data =>
        this.serverInfo$.pipe(
          switchMap(({ id }) =>
            from(
              this.getMarketplaceData(
                { 'server-id': id },
                this.toMarketplace(data).url,
              ),
            ).pipe(tap(({ name }) => this.updateName(data, name))),
          ),
        ),
      ),
      shareReplay(1),
    )

  private readonly categories$: Observable<Set<string>> =
    this.registryData$.pipe(
      map(
        ({ categories }) =>
          new Set(['featured', 'updates', ...categories, 'all']),
      ),
    )

  private readonly pkgs$: Observable<MarketplacePkg[]> = this.marketplace$.pipe(
    switchMap(({ url }) =>
      this.serverInfo$.pipe(
        switchMap(info =>
          from(
            this.getMarketplacePkgs(
              { page: 1, 'per-page': 100 },
              url,
              info['eos-version-compat'],
            ),
          ).pipe(tap(() => this.hasPackages$.next(true))),
        ),
      ),
    ),
    catchError(e => {
      this.errToast.present(e)

      return of([])
    }),
    shareReplay(1),
  )

  private readonly updates$: Observable<MarketplacePkg[]> =
    this.hasPackages$.pipe(
      switchMap(() =>
        this.patch.watch$('package-data').pipe(
          switchMap(localPkgs =>
            this.pkgs$.pipe(
              map(pkgs => {
                return this.filterPkgsPipe.transform(
                  pkgs,
                  '',
                  'updates',
                  localPkgs,
                )
              }),
            ),
          ),
        ),
      ),
    )

  constructor(
    private readonly api: ApiService,
    private readonly patch: PatchDB<DataModel>,
    private readonly config: ConfigService,
    private readonly errToast: ErrorToastService,
    private readonly emver: Emver,
    private readonly filterPkgsPipe: FilterPackagesPipe,
  ) {
    super()
  }

  getMarketplace(): Observable<Marketplace> {
    return this.marketplace$
  }

  getAltMarketplaceData() {
    return this.uiMarketplaceData$
  }

  getCategories(): Observable<Set<string>> {
    return this.categories$
  }

  getPackages(): Observable<MarketplacePkg[]> {
    return this.pkgs$
  }

  getPackage(id: string, version: string): Observable<MarketplacePkg | null> {
    const params = { ids: [{ id, version }] }
    const fallback$ = this.marketplace$.pipe(
      switchMap(({ url }) =>
        this.serverInfo$.pipe(
          switchMap(info =>
            from(
              this.getMarketplacePkgs(params, url, info['eos-version-compat']),
            ),
          ),
        ),
      ),
      map(pkgs => this.findPackage(pkgs, id, version)),
      startWith(null),
    )

    return this.getPackages().pipe(
      map(pkgs => this.findPackage(pkgs, id, version)),
      switchMap(pkg => (pkg ? of(pkg) : fallback$)),
      filter((pkg): pkg is MarketplacePkg | null => {
        if (pkg === undefined) {
          throw new Error(`No results for ${id}${version ? ' ' + version : ''}`)
        }

        return true
      }),
    )
  }

  getUpdates(): Observable<MarketplacePkg[]> {
    return this.updates$
  }

  getReleaseNotes(id: string): Observable<Record<string, string>> {
    if (this.notes.has(id)) {
      return of(this.notes.get(id) || {})
    }

    return this.marketplace$.pipe(
      switchMap(({ url }) => this.loadReleaseNotes(id, url)),
      tap(response => this.notes.set(id, response)),
      catchError(e => {
        this.errToast.present(e)

        return of({})
      }),
    )
  }

  installPackage(
    req: Omit<RR.InstallPackageReq, 'marketplace-url'>,
  ): Observable<unknown> {
    return this.getMarketplace().pipe(
      take(1),
      switchMap(({ url }) =>
        from(
          this.api.installPackage({
            ...req,
            'marketplace-url': url,
          }),
        ),
      ),
    )
  }

  getPackageMarkdown(type: string, pkgId: string): Observable<string> {
    return this.getMarketplace().pipe(
      switchMap(({ url }) =>
        from(
          this.api.marketplaceProxy<string>(
            `/package/v0/${type}/${pkgId}`,
            {},
            url,
          ),
        ),
      ),
    )
  }

  async getMarketplaceData(
    params: RR.GetMarketplaceDataReq,
    url: string,
  ): Promise<RR.GetMarketplaceDataRes> {
    return this.api.marketplaceProxy('/package/v0/info', params, url)
  }

  async getMarketplacePkgs(
    params: Omit<RR.GetMarketplacePackagesReq, 'eos-version-compat'>,
    url: string,
    eosVersionCompat: string,
  ): Promise<RR.GetMarketplacePackagesRes> {
    let clonedParams = { ...params }
    if (params.query) delete params.category
    if (clonedParams.ids) clonedParams.ids = JSON.stringify(clonedParams.ids)

    const qp: RR.GetMarketplacePackagesReq = {
      ...clonedParams,
      'eos-version-compat': eosVersionCompat,
    }

    return this.api.marketplaceProxy('/package/v0/index', qp, url)
  }

  private loadReleaseNotes(
    id: string,
    url: string,
  ): Observable<Record<string, string>> {
    return from(
      this.api.marketplaceProxy<Record<string, string>>(
        `/package/v0/release-notes/${id}`,
        {},
        url,
      ),
    )
  }

  private updateName(
    uiMarketplaceData: UIMarketplaceData | undefined,
    name: string,
  ) {
    if (!uiMarketplaceData?.['selected-id']) {
      return
    }

    const selectedId = uiMarketplaceData['selected-id']
    const knownHosts = uiMarketplaceData['known-hosts']

    if (knownHosts[selectedId].name !== name) {
      this.api.setDbValue({
        pointer: `/marketplace/known-hosts/${selectedId}/name`,
        value: name,
      })
    }
  }

  private toMarketplace(marketplace: UIMarketplaceData): Marketplace {
    return marketplace['selected-id']
      ? marketplace['known-hosts'][marketplace['selected-id']]
      : this.config.marketplace
  }

  private findPackage(
    pkgs: readonly MarketplacePkg[],
    id: string,
    version: string,
  ): MarketplacePkg | undefined {
    return pkgs.find(pkg => {
      const versionIsSame =
        version === '*' ||
        this.emver.compare(pkg.manifest.version, version) === 0

      return pkg.manifest.id === id && versionIsSame
    })
  }
}
