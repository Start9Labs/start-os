import { Inject, Injectable } from '@angular/core'
import {
  AbstractMarketplaceService,
  StoreData,
  Marketplace,
  StoreIdentity,
  MarketplacePkg,
  GetPackageRes,
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
import { Exver, sameUrl } from '@start9labs/shared'
import { ClientStorageService } from './client-storage.service'
import { T } from '@start9labs/start-sdk'

@Injectable()
export class MarketplaceService implements AbstractMarketplaceService {
  private readonly knownHosts$: Observable<StoreIdentity[]> = this.patch
    .watch$('ui', 'marketplace', 'knownHosts')
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
      distinctUntilKeyChanged('selectedUrl'),
      map(({ selectedUrl: url, knownHosts: hosts }) =>
        toStoreIdentity(url, hosts[url]),
      ),
      shareReplay({ bufferSize: 1, refCount: true }),
    )

  private readonly marketplace$: Observable<Marketplace> =
    this.knownHosts$.pipe(
      startWith<StoreIdentity[]>([]),
      pairwise(),
      mergeMap(([prev, curr]) =>
        curr.filter(c => !prev.find(p => sameUrl(c.url, p.url))),
      ),
      mergeMap(({ url, name }) =>
        this.fetchStore$(url).pipe(
          tap(data => {
            if (data?.info.name) this.updateStoreName(url, name, data.info.name)
          }),
          map<StoreData | null, [string, StoreData | null]>(data => [
            url,
            data,
          ]),
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
      shareReplay({ bufferSize: 1, refCount: true }),
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
    private readonly exver: Exver,
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

  getPackage$(
    id: string,
    version: string | null,
    flavor: string | null,
    registryUrl?: string,
  ): Observable<MarketplacePkg> {
    return this.selectedHost$.pipe(
      switchMap(selected =>
        this.marketplace$.pipe(
          switchMap(m => {
            const url = registryUrl || selected.url

            const pkg = m[url]?.packages.find(
              p =>
                p.id === id &&
                p.flavor === flavor &&
                (!version || this.exver.compareExver(p.version, version) === 0),
            )

            return !!pkg
              ? of(pkg)
              : this.fetchPackage$(url, id, version, flavor)
          }),
        ),
      ),
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
      version,
      registry: url,
    }

    await this.api.installPackage(params)
  }

  fetchInfo$(url: string): Observable<T.RegistryInfo> {
    return from(this.api.getRegistryInfo(url))
  }

  fetchStatic$(
    pkg: MarketplacePkg,
    type: 'LICENSE.md' | 'instructions.md',
  ): Observable<string> {
    return from(this.api.getStaticProxy(pkg, type))
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

  private fetchPackages$(url: string): Observable<MarketplacePkg[]> {
    return from(this.api.getRegistryPackages(url)).pipe(
      map(packages => {
        return Object.entries(packages).flatMap(([id, pkgInfo]) =>
          Object.keys(pkgInfo.best).map(version =>
            this.convertToMarketplacePkg(
              id,
              version,
              this.exver.getFlavor(version),
              pkgInfo,
            ),
          ),
        )
      }),
    )
  }

  convertToMarketplacePkg(
    id: string,
    version: string | null,
    flavor: string | null,
    pkgInfo: GetPackageRes,
  ): MarketplacePkg {
    version =
      version ||
      Object.keys(pkgInfo.best).find(v => this.exver.getFlavor(v) === flavor) ||
      null

    return !version || !pkgInfo.best[version]
      ? ({} as MarketplacePkg)
      : {
          id,
          version,
          flavor,
          ...pkgInfo,
          ...pkgInfo.best[version],
        }
  }

  private fetchPackage$(
    url: string,
    id: string,
    version: string | null,
    flavor: string | null,
  ): Observable<MarketplacePkg> {
    return from(
      this.api.getRegistryPackage(url, id, version ? `=${version}` : null),
    ).pipe(
      map(pkgInfo =>
        this.convertToMarketplacePkg(
          id,
          version === '*' ? null : version,
          flavor,
          pkgInfo,
        ),
      ),
    )
  }

  private async updateStoreName(
    url: string,
    oldName: string | undefined,
    newName: string,
  ): Promise<void> {
    if (oldName !== newName) {
      this.api.setDbValue<string>(
        ['marketplace', 'knownHosts', url, 'name'],
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
