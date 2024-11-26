import { Injectable } from '@angular/core'
import {
  StoreIdentity,
  MarketplacePkg,
  GetPackageRes,
  StoreDataWithUrl,
} from '@start9labs/marketplace'
import { PatchDB } from 'patch-db-client'
import {
  BehaviorSubject,
  catchError,
  combineLatest,
  filter,
  from,
  map,
  Observable,
  of,
  shareReplay,
  switchMap,
  distinctUntilChanged,
  ReplaySubject,
  tap,
} from 'rxjs'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel, UIStore } from 'src/app/services/patch-db/data-model'
import { ConfigService } from './config.service'
import { Exver } from '@start9labs/shared'
import { ClientStorageService } from './client-storage.service'
import { T } from '@start9labs/start-sdk'

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
      // @TODO is updateStoreName needed?
      map(registry => {
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
        categories: {
          all: {
            name: 'All',
            description: {
              short: 'All services',
              long: 'An unfiltered list of all services available on this registry.',
            },
          },
          ...info.categories,
        },
      })),
    )
  }

  getStatic$(
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
        this.convertToMarketplacePkg(id, version, flavor, pkgInfo),
      ),
    )
  }

  private convertToMarketplacePkg(
    id: string,
    version: string | null | undefined,
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
}

function toStoreIdentity(url: string, uiStore: UIStore): StoreIdentity {
  return {
    url,
    ...uiStore,
  }
}
