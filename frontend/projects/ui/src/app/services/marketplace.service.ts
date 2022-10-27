import { Injectable } from '@angular/core'
import {
  MarketplacePkg,
  AbstractMarketplaceService,
  StoreData,
  Marketplace,
  StoreInfo,
} from '@start9labs/marketplace'
import {
  BehaviorSubject,
  combineLatest,
  distinctUntilKeyChanged,
  from,
  mergeMap,
  Observable,
  of,
  scan,
} from 'rxjs'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import {
  catchError,
  filter,
  map,
  pairwise,
  shareReplay,
  startWith,
  switchMap,
  tap,
} from 'rxjs/operators'
import { getNewEntries } from '@start9labs/shared'

@Injectable()
export class MarketplaceService implements AbstractMarketplaceService {
  private readonly knownHosts$ = this.patch.watch$(
    'ui',
    'marketplace',
    'known-hosts',
  )

  private readonly selectedHost$ = this.patch.watch$('ui', 'marketplace').pipe(
    distinctUntilKeyChanged('selected-url'),
    map(data => ({
      url: data['selected-url'],
      name: data['known-hosts'][data['selected-url']],
    })),
    shareReplay(1),
  )

  private readonly marketplace$ = this.knownHosts$.pipe(
    startWith<Record<string, string>>({}),
    pairwise(),
    mergeMap(([prev, curr]) => from(Object.entries(getNewEntries(prev, curr)))),
    mergeMap(([url, name]) =>
      this.fetchStore$(url).pipe(
        map<StoreData, [string, StoreData | null]>(data => {
          if (data.info) this.updateName(url, name, data.info.name)

          return [url, data]
        }),
        startWith<[string, StoreData | null]>([url, null]),
      ),
    ),
    scan<[string, StoreData | null], Record<string, StoreData | null>>(
      (requests, [url, store]) => {
        requests[url] = store

        return requests
      },
      {},
    ),
    shareReplay(1),
  )

  private readonly selectedStore$ = this.selectedHost$.pipe(
    switchMap(({ url }) => this.marketplace$.pipe(map(m => m[url]))),
  )

  private readonly requestErrors$ = new BehaviorSubject<string[]>([])

  constructor(
    private readonly api: ApiService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  getKnownHosts$(): Observable<Record<string, string>> {
    return this.knownHosts$
  }

  getSelectedHost$(): Observable<{ url: string; name: string }> {
    return this.selectedHost$
  }

  getMarketplace$(): Observable<Marketplace> {
    return this.marketplace$
  }

  getSelectedStore$(): Observable<StoreData | null> {
    return this.selectedStore$
  }

  getPackage$(
    id: string,
    version: string,
    optionalUrl?: string,
  ): Observable<MarketplacePkg | undefined> {
    return this.patch.watch$('ui', 'marketplace').pipe(
      switchMap(marketplace => {
        const url = optionalUrl || marketplace['selected-url']

        if (version !== '*' || !marketplace['known-hosts'][url]) {
          return this.fetchPackage$(id, version, url)
        }

        return this.selectedStore$.pipe(
          filter(Boolean),
          map(s => s.packages.find(p => p.manifest.id === id)),
        )
      }),
    )
  }

  // UI only

  getRequestErrors$(): Observable<string[]> {
    return this.requestErrors$
  }

  async installPackage(
    id: string,
    version: string,
    url: string,
  ): Promise<void> {
    const params: RR.InstallPackageReq = {
      id,
      'version-spec': `=${version}`,
      'marketplace-url': url,
    }

    await this.api.installPackage(params)
  }

  fetchInfo$(url: string): Observable<StoreInfo> {
    return this.patch
      .watch$('server-info', 'id')
      .pipe(
        switchMap(id =>
          this.api.marketplaceProxy<RR.GetMarketplaceInfoRes>(
            '/package/v0/info',
            { 'server-d': id },
            url,
          ),
        ),
      )
  }

  private fetchStore$(url: string): Observable<StoreData> {
    return combineLatest([this.fetchInfo$(url), this.fetchPackages$(url)]).pipe(
      map(([info, packages]) => ({ info, packages })),
      catchError(e => {
        this.requestErrors$.next(this.requestErrors$.value.concat(url))
        return of(e)
      }),
    )
  }

  private fetchPackages$(
    url: string,
    params: Omit<
      RR.GetMarketplacePackagesReq,
      'eos-version-compat' | 'page' | 'per-page'
    > = {},
  ): Observable<MarketplacePkg[]> {
    return this.patch.watch$('server-info', 'eos-version-compat').pipe(
      switchMap(versionCompat => {
        const qp: RR.GetMarketplacePackagesReq = {
          ...params,
          'eos-version-compat': versionCompat,
          page: 1,
          'per-page': 100,
        }
        if (qp.ids) qp.ids = JSON.stringify(qp.ids)

        return this.api.marketplaceProxy<RR.GetMarketplacePackagesRes>(
          '/package/v0/index',
          qp,
          url,
        )
      }),
    )
  }

  fetchPackage$(
    id: string,
    version: string,
    url: string,
  ): Observable<MarketplacePkg | undefined> {
    return this.fetchPackages$(url, { ids: [{ id, version }] }).pipe(
      map(pkgs => pkgs[0]),
    )
  }

  fetchReleaseNotes$(
    id: string,
    url?: string,
  ): Observable<Record<string, string>> {
    return this.selectedHost$.pipe(
      switchMap(m => {
        return from(
          this.api.marketplaceProxy<Record<string, string>>(
            `/package/v0/release-notes/${id}`,
            {},
            url || m.url,
          ),
        )
      }),
    )
  }

  fetchStatic$(id: string, type: string, url?: string): Observable<string> {
    return this.selectedHost$.pipe(
      switchMap(m => {
        return from(
          this.api.marketplaceProxy<string>(
            `/package/v0/${type}/${id}`,
            {},
            url || m.url,
          ),
        )
      }),
    )
  }

  private async updateName(
    url: string,
    name: string,
    newName: string,
  ): Promise<void> {
    if (name !== newName) {
      this.api.setDbValue(['marketplace', 'known-hosts', url], newName)
    }
  }
}
