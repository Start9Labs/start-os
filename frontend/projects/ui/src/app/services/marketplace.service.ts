import { Injectable } from '@angular/core'
import {
  MarketplacePkg,
  AbstractMarketplaceService,
  StoreData,
  Marketplace,
  StoreInfo,
  StoreIdentity,
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
import { DataModel, UIStore } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import {
  catchError,
  filter,
  map,
  pairwise,
  shareReplay,
  startWith,
  switchMap,
  take,
  tap,
} from 'rxjs/operators'
import { ConfigService } from './config.service'

@Injectable()
export class MarketplaceService implements AbstractMarketplaceService {
  private readonly knownHosts$: Observable<StoreIdentity[]> = this.patch
    .watch$('ui', 'marketplace', 'known-hosts')
    .pipe(
      map(hosts => {
        const { start9, community } = this.config.marketplace
        let arr = [
          toStoreIdentity(start9, hosts[start9]),
          // toStoreIdentity(community, hosts[community]),
        ]

        return arr.concat(
          Object.entries(hosts)
            .filter(([url, _]) => ![start9, community].includes(url as any))
            .map(([url, store]) => toStoreIdentity(url, store)),
        )
      }),
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
      curr.filter(c => !prev.find(p => c.url === p.url)),
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
  ) {}

  getKnownHosts$(): Observable<StoreIdentity[]> {
    return this.knownHosts$
  }

  getSelectedHost$(): Observable<StoreIdentity> {
    return this.selectedHost$
  }

  getMarketplace$(): Observable<Marketplace> {
    return this.marketplace$
  }

  getSelectedStore$(): Observable<StoreData> {
    return this.selectedStore$
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
      switchMap(serverInfo => {
        const qp: RR.GetMarketplaceInfoReq = {
          'server-id': serverInfo.id,
          'eos-version': serverInfo.version,
        }
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
