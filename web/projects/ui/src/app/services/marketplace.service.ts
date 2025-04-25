import { Injectable } from '@angular/core'
import {
  GetPackageRes,
  Marketplace,
  MarketplacePkg,
  StoreData,
  StoreDataWithUrl,
  StoreIdentity,
} from '@start9labs/marketplace'
import { Exver, sameUrl } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { PatchDB } from 'patch-db-client'
import {
  BehaviorSubject,
  catchError,
  combineLatest,
  distinctUntilChanged,
  filter,
  from,
  map,
  mergeMap,
  Observable,
  of,
  pairwise,
  ReplaySubject,
  scan,
  shareReplay,
  startWith,
  switchMap,
  tap,
} from 'rxjs'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel, UIStore } from 'src/app/services/patch-db/data-model'
import { ClientStorageService } from './client-storage.service'
import { ConfigService } from './config.service'

@Injectable({
  providedIn: 'root',
})
export class MarketplaceService {
  private readonly registryUrlSubject$ = new ReplaySubject<string>(1)
  private readonly registryUrl$ = this.registryUrlSubject$.pipe(
    distinctUntilChanged(),
  )

  private readonly registry$: Observable<StoreDataWithUrl> =
    this.registryUrl$.pipe(
      switchMap(url => this.fetchRegistry$(url)),
      filter(Boolean),
      map(registry => {
        // @TODO Aiden let's drop description. We do not use it. categories should just be Record<string, string>
        registry.info.categories = {
          all: {
            name: 'All',
            description: {
              short: 'All registry packages',
              long: 'An unfiltered list of all packages available on this registry.',
            },
          },
          ...registry.info.categories,
        }

        return registry
      }),
      shareReplay(1),
    )

  private readonly knownHosts$: Observable<StoreIdentity[]> = this.patch
    .watch$('ui', 'marketplace', 'knownHosts')
    .pipe(
      map(hosts => {
        const { start9, community } = this.config.marketplace
        let arr = [
          toStoreIdentity(start9, hosts[start9]!),
          toStoreIdentity(community, {
            ...hosts[community],
            name: 'Community Registry',
          }),
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

  private readonly requestErrors$ = new BehaviorSubject<string[]>([])

  readonly marketplace$: Observable<Marketplace> = this.knownHosts$.pipe(
    startWith<StoreIdentity[]>([]),
    pairwise(),
    mergeMap(([prev, curr]) =>
      curr.filter(c => !prev.find(p => sameUrl(c.url, p.url))),
    ),
    mergeMap(({ url, name }) =>
      this.fetchRegistry$(url).pipe(
        tap(data => {
          if (data?.info.name) this.updateStoreName(url, name, data.info.name)
        }),
        map<StoreData | null, [string, StoreData | null]>(data => [url, data]),
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

  getRegistryUrl$() {
    return this.registryUrl$
  }

  setRegistryUrl(url: string | null) {
    const registryUrl = url || this.config.marketplace.start9
    this.registryUrlSubject$.next(registryUrl)
  }

  getRegistry$(): Observable<StoreDataWithUrl> {
    return this.registry$
  }

  getPackage$(
    id: string,
    version: string | null,
    flavor: string | null,
    registryUrl?: string,
  ): Observable<MarketplacePkg> {
    return this.registry$.pipe(
      switchMap(registry => {
        const url = registryUrl || registry.url
        const pkg = registry.packages.find(
          p =>
            p.id === id &&
            p.flavor === flavor &&
            (!version || this.exver.compareExver(p.version, version) === 0),
        )
        return pkg ? of(pkg) : this.fetchPackage$(url, id, version, flavor)
      }),
    )
  }

  fetchInfo$(url: string): Observable<T.RegistryInfo> {
    return from(this.api.getRegistryInfo(url)).pipe(
      map(info => ({
        ...info,
        // @TODO Aiden let's drop description. We do not use it. categories should just be Record<string, string>
        categories: {
          all: {
            name: 'All',
            description: {
              short: '',
              long: '',
            },
          },
          ...info.categories,
        },
      })),
    )
  }

  fetchStatic$(
    pkg: MarketplacePkg,
    type: 'LICENSE.md' | 'instructions.md',
  ): Observable<string> {
    return from(this.api.getStaticProxy(pkg, type))
  }

  private fetchRegistry$(url: string): Observable<StoreDataWithUrl | null> {
    console.log('FETCHING REGISTRY: ', url)
    return combineLatest([this.fetchInfo$(url), this.fetchPackages$(url)]).pipe(
      map(([info, packages]) => ({ info, packages, url })),
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
            this.convertRegistryPkgToMarketplacePkg(
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
        this.convertRegistryPkgToMarketplacePkg(id, version, flavor, pkgInfo),
      ),
    )
  }

  private convertRegistryPkgToMarketplacePkg(
    id: string,
    version: string | null | undefined,
    flavor: string | null,
    pkgInfo: GetPackageRes,
  ): MarketplacePkg {
    const ver =
      version ||
      Object.keys(pkgInfo.best).find(v => this.exver.getFlavor(v) === flavor) ||
      null
    const best = ver && pkgInfo.best[ver]

    if (!best) {
      return {} as MarketplacePkg
    }

    return {
      id,
      flavor,
      version: ver || '',
      ...pkgInfo,
      ...best,
    }
  }

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
