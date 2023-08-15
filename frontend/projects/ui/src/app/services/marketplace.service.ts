import { Injectable } from '@angular/core'
import { sameUrl } from '@start9labs/shared'
import {
  AbstractMarketplaceService,
  Marketplace,
  MarketplacePkg,
  StoreData,
  StoreIdentity,
  StoreIdentityWithData,
  StoreInfo,
} from '@start9labs/marketplace'
import { PatchDB } from 'patch-db-client'
import {
  BehaviorSubject,
  catchError,
  combineLatest,
  distinctUntilKeyChanged,
  filter,
  from,
  map,
  mergeMap,
  Observable,
  of,
  pairwise,
  scan,
  shareReplay,
  startWith,
  switchMap,
  take,
  tap,
} from 'rxjs'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel, UIStore } from 'src/app/services/patch-db/data-model'
import { ConfigService } from './config.service'
import { ClientStorageService } from './client-storage.service'

@Injectable()
export class MarketplaceService implements AbstractMarketplaceService {
  private readonly knownHosts$: Observable<StoreIdentity[]> = this.patch
    .watch$('ui', 'marketplace', 'known-hosts')
    .pipe(
      map(hosts => {
        const { start9, community } = this.config.marketplace
        let arr = [
          toStoreIdentity(start9, hosts[start9]),
          toStoreIdentity(community, hosts[community]),
        ]

        return arr.concat(
          Object.entries(hosts)
            .filter(([url, _]) => ![start9, community].includes(url as any))
            .map(([url, store]) => toStoreIdentity(url, store)),
        )
      }),
    )

  private readonly filteredKnownHosts$: Observable<StoreIdentity[]> =
    combineLatest([
      this.clientStorageService.showDevTools$,
      this.knownHosts$,
    ]).pipe(
      map(([devMode, knownHosts]) =>
        devMode
          ? knownHosts
          : knownHosts.filter(
              ({ url }) => !url.includes('alpha') && !url.includes('beta'),
            ),
      ),
    )

  private readonly selectedHost$: Observable<StoreIdentity> = this.patch
    .watch$('ui', 'marketplace')
    .pipe(
      distinctUntilKeyChanged('selected-url'),
      map(({ 'selected-url': url, 'known-hosts': hosts }) =>
        toStoreIdentity(url, hosts[url]),
      ),
      shareReplay(1),
    )

  private readonly marketplace$ = this.knownHosts$.pipe(
    startWith<StoreIdentity[]>([]),
    pairwise(),
    mergeMap(([prev, curr]) =>
      curr.filter(c => !prev.find(p => sameUrl(c.url, p.url))),
    ),
    mergeMap(({ url, name }) =>
      this.fetchStore$(url).pipe(
        tap(data => {
          if (data?.info) this.updateStoreName(url, name, data.info.name)
        }),
        map<StoreData | null, [string, StoreData | null]>(data => {
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

  private readonly filteredMarketplace$ = combineLatest([
    this.clientStorageService.showDevTools$,
    this.marketplace$,
  ]).pipe(
    map(([devMode, marketplace]) =>
      Object.entries(marketplace).reduce(
        (filtered, [url, store]) =>
          !devMode && (url.includes('alpha') || url.includes('beta'))
            ? filtered
            : {
                [url]: store,
                ...filtered,
              },
        {} as Marketplace,
      ),
    ),
  )

  private readonly selectedStore$: Observable<StoreData> =
    this.selectedHost$.pipe(
      switchMap(({ url }) =>
        this.marketplace$.pipe(
          map(m => m[url]),
          filter(Boolean),
          take(1),
        ),
      ),
    )

  private readonly requestErrors$ = new BehaviorSubject<string[]>([])

  constructor(
    private readonly api: ApiService,
    private readonly patch: PatchDB<DataModel>,
    private readonly config: ConfigService,
    private readonly clientStorageService: ClientStorageService,
  ) {}

  getKnownHosts$(filtered = false): Observable<StoreIdentity[]> {
    // option to filter out hosts containing 'alpha' or 'beta' substrings in registryURL
    return filtered ? this.filteredKnownHosts$ : this.knownHosts$
  }

  getSelectedHost$(): Observable<StoreIdentity> {
    return this.selectedHost$
  }

  getMarketplace$(filtered = false): Observable<Marketplace> {
    // option to filter out hosts containing 'alpha' or 'beta' substrings in registryURL
    return filtered ? this.filteredMarketplace$ : this.marketplace$
  }

  getSelectedStore$(): Observable<StoreData> {
    return this.selectedStore$
  }

  getSelectedStoreWithAllCategories$() {
    return this.selectedHost$.pipe(
      switchMap(({ url }) =>
        this.marketplace$.pipe(
          map(m => m[url]),
          filter(Boolean),
          map(store => {
            const categories = new Set<string>()
            categories.add('all')
            store?.info.categories.forEach(c => categories.add(c))

            return {
              url,
              info: {
                ...store?.info,
                categories: Array.from(categories),
              },
              packages: store?.packages,
            }
          }),
        ),
      ),
    )
  }

  getPackage$(
    id: string,
    version: string,
    optionalUrl?: string,
  ): Observable<MarketplacePkg> {
    return this.patch.watch$('ui', 'marketplace').pipe(
      switchMap(uiMarketplace => {
        const url = optionalUrl || uiMarketplace['selected-url']

        if (version !== '*' || !uiMarketplace['known-hosts'][url]) {
          return this.fetchPackage$(id, version, url)
        }

        return this.marketplace$.pipe(
          map(m => m[url]),
          filter(Boolean),
          take(1),
          map(
            store =>
              store.packages.find(p => p.manifest.id === id) ||
              ({} as MarketplacePkg),
          ),
        )
      }),
    )
  }

  // UI only
  readonly updateErrors: Record<string, string> = {}
  readonly updateQueue: Record<string, boolean> = {}

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
    return this.patch.watch$('server-info').pipe(
      take(1),
      switchMap(serverInfo => {
        const qp: RR.GetMarketplaceInfoReq = { 'server-id': serverInfo.id }
        return this.api.marketplaceProxy<RR.GetMarketplaceInfoRes>(
          '/package/v0/info',
          qp,
          url,
        )
      }),
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

  private fetchStore$(url: string): Observable<StoreData | null> {
    return combineLatest([this.fetchInfo$(url), this.fetchPackages$(url)]).pipe(
      map(([info, packages]) => ({ info, packages })),
      catchError(e => {
        console.error(e)
        this.requestErrors$.next(this.requestErrors$.value.concat(url))
        return of(null)
      }),
    )
  }

  private fetchPackages$(
    url: string,
    params: Omit<RR.GetMarketplacePackagesReq, 'page' | 'per-page'> = {},
  ): Observable<MarketplacePkg[]> {
    const qp: RR.GetMarketplacePackagesReq = {
      ...params,
      page: 1,
      'per-page': 100,
    }
    if (qp.ids) qp.ids = JSON.stringify(qp.ids)

    return from(
      this.api.marketplaceProxy<RR.GetMarketplacePackagesRes>(
        '/package/v0/index',
        qp,
        url,
      ),
    )
  }

  private fetchPackage$(
    id: string,
    version: string,
    url: string,
  ): Observable<MarketplacePkg> {
    return this.fetchPackages$(url, { ids: [{ id, version }] }).pipe(
      map(pkgs => pkgs[0] || {}),
    )
  }

  private async updateStoreName(
    url: string,
    oldName: string | undefined,
    newName: string,
  ): Promise<void> {
    if (oldName !== newName) {
      this.api.setDbValue<string>(
        ['marketplace', 'known-hosts', url, 'name'],
        newName,
      )
    }
  }
}

function toStoreIdentity(url: string, uiStore: UIStore): StoreIdentity {
  return {
    url,
    ...uiStore,
  }
}
