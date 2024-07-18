import { Inject, Injectable } from '@angular/core'
import {
  AbstractMarketplaceService,
  StoreData,
  Marketplace,
  StoreIdentity,
  MarketplacePkg,
  MarketplaceSinglePkg,
  AbstractPkgFlavorService,
  DefaultGetPackageParams,
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
import {
  DataModel,
  UIMarketplaceData,
  UIStore,
} from 'src/app/services/patch-db/data-model'
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
import { sameUrl } from '@start9labs/shared'
import { ClientStorageService } from './client-storage.service'
import { ExtendedVersion, T } from '@start9labs/start-sdk'
import { PkgFlavorService } from './pkg-flavor.service'

@Injectable()
export class MarketplaceService implements AbstractMarketplaceService {
  private readonly knownHosts$: Observable<StoreIdentity[]> = this.patch
    .watch$('ui', 'marketplace', 'knownHosts')
    .pipe(
      map((hosts: UIMarketplaceData['knownHosts']) => {
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

  private readonly marketplace$ = this.knownHosts$.pipe(
    startWith<StoreIdentity[]>([]),
    pairwise(),
    mergeMap(([prev, curr]) =>
      curr.filter(c => !prev.find(p => sameUrl(c.url, p.url))),
    ),
    mergeMap(({ url, name }) =>
      this.fetchStore$(url, {
        id: null,
        version: null,
        sourceVersion: null,
        otherVersions: 'full',
      }).pipe(
        tap(data => {
          if (data?.info.name) this.updateStoreName(url, name, data.info.name)
        }),
        map<
          StoreData<T.GetPackageParams> | null,
          [string, StoreData<T.GetPackageParams> | null]
        >(data => {
          return [url, data]
        }),
        startWith<[string, StoreData<T.GetPackageParams> | null]>([url, null]),
      ),
    ),
    scan<
      [string, StoreData<T.GetPackageParams> | null],
      Record<string, StoreData<T.GetPackageParams> | null>
    >((requests, [url, store]) => {
      requests[url] = store

      return requests
    }, {}),
    shareReplay({ bufferSize: 1, refCount: true }),
  )
  //   private readonly marketplace$: Observable<Record<string, StoreData<DefaultGetPackageParams>>> = this.knownHosts$.pipe(
  //   startWith<StoreIdentity[]>([]),
  //   pairwise(),
  //   mergeMap(([prev, curr]) =>
  //     curr.filter(c => !prev.find(p => sameUrl(c.url, p.url))),
  //   ),
  //   mergeMap(({ url, name }) =>
  //     this.fetchStore$(url, {
  //       id: null,
  //       version: null,
  //       sourceVersion: null,
  //       otherVersions: 'short',
  //     }).pipe(
  //       tap((data) => {
  //         if (data?.info.name) this.updateStoreName(url, name, data.info.name)
  //       }),
  //       map(data => {
  //         return [url, data] as const
  //       }),
  //       startWith([url, null] as const),
  //     ),
  //   ),
  //   scan((requests, [url, store]) => {
  //     return {
  //       ...requests,
  //       [url]: store
  //     }
  //   }, {}),
  //   shareReplay({ bufferSize: 1, refCount: true }),
  // )

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
        {} as Marketplace<T.GetPackageParams>,
      ),
    ),
  )

  private readonly selectedStore$: Observable<StoreData<T.GetPackageParams>> =
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
    @Inject(AbstractPkgFlavorService)
    private readonly pkgFlavorService: PkgFlavorService,
  ) {}

  getKnownHosts$(filtered = false): Observable<StoreIdentity[]> {
    // option to filter out hosts containing 'alpha' or 'beta' substrings in registryURL
    return filtered ? this.filteredKnownHosts$ : this.knownHosts$
  }

  getSelectedHost$(): Observable<StoreIdentity> {
    return this.selectedHost$
  }

  getMarketplace$<T extends RR.GetRegistryPackagesReq>(
    filtered = false,
  ): Observable<Marketplace<T>> {
    // option to filter out hosts containing 'alpha' or 'beta' substrings in registryURL
    return filtered ? this.filteredMarketplace$ : this.marketplace$
  }

  getSelectedStore$<T extends RR.GetRegistryPackagesReq>(): Observable<
    StoreData<T>
  > {
    return this.selectedStore$
  }

  getPackage$<T extends RR.GetRegistryPackagesReq>(
    params: T,
    optionalUrl?: string,
  ): Observable<MarketplacePkg<T>> {
    return this.patch.watch$('ui', 'marketplace').pipe(
      switchMap(uiMarketplace => {
        const url = optionalUrl || uiMarketplace.selectedUrl
        if (
          !params.version ||
          (params.version && params.version === '*') ||
          !uiMarketplace.knownHosts[url]
        ) {
          return this.fetchPackage$(url, params)
        } else {
          return this.marketplace$.pipe(
            map(m => m[url]),
            filter(Boolean),
            take(1),
            map((store: StoreData<T>) =>
              store.packages.find(
                p =>
                  p.id === params.id &&
                  (p.version === params.version ||
                    p.flavorVersion === params.version),
              ),
            ),
            switchMap(p => (p ? of(p) : this.fetchPackage$(url, params))),
          )
        }

        // if (params.version !== '*' || !uiMarketplace.knownHosts[url]) {
        //   return this.fetchPackage$(url, params)
        // }
        //             return this.marketplace$.pipe(
        //       map(m => m[url]),
        //       filter(Boolean),
        //       take(1),
        //       map(
        //         (store: StoreData<T>) =>
        //           store.packages.find(p => p.id === params.id) ||
        //           ({} as MarketplacePkg<T>),
        //       ),
        //     )
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
      versionSpec: `=${version}`,
      registry: url,
    }

    await this.api.installPackage(params)
  }

  fetchInfo$(url: string): Observable<T.RegistryInfo> {
    return this.patch.watch$('serverInfo').pipe(
      take(1),
      switchMap(serverInfo => {
        // TODO hit MAU
        // const qp: RR.GetMarketplaceInfoReq = { serverId: serverInfo.id }
        return from(this.api.getRegistryInfo(url))
      }),
    )
  }

  fetchReleaseNotes$(
    id: string,
    url?: string,
  ): Observable<Record<string, T.PackageInfoShort>> {
    return this.selectedHost$.pipe(
      switchMap(m => {
        return from(
          this.api.getRegistryPackage(url || m.url, {
            id,
            version: null,
            sourceVersion: null,
            otherVersions: 'short',
          }),
        ).pipe(
          map(res => {
            return res.otherVersions
          }),
        )
      }),
    )
  }

  fetchStatic$(
    id: string,
    type: string,
    version: string,
    url: string | null,
  ): Observable<string> {
    return this.selectedHost$.pipe(
      switchMap(m => {
        return from(this.api.getStatic(url || m.url, type, id, version))
      }),
    )
  }

  private fetchStore$<T extends RR.GetRegistryPackagesReq>(
    url: string,
    params: T,
  ): Observable<StoreData<T> | null> {
    return combineLatest([
      this.fetchInfo$(url),
      this.fetchPackages$(url, params),
    ]).pipe(
      map(([info, packages]) => ({ info, packages })),
      catchError(e => {
        console.error(e)
        this.requestErrors$.next(this.requestErrors$.value.concat(url))
        return of(null)
      }),
    )
  }

  private fetchPackages$<T extends RR.GetRegistryPackagesReq>(
    url: string,
    params: T,
  ): Observable<MarketplacePkg<T>[]> {
    return combineLatest([
      this.pkgFlavorService.getFlavorStatus$(),
      from(this.api.getRegistryPackages(url, params)),
    ]).pipe(
      map(([active, packages]) => {
        return Object.keys(packages).map(p =>
          this.expandData(p, packages[p], active),
        )
      }),
    )
  }

  // expand data for search filter accessability
  expandData<T extends RR.GetRegistryPackagesReq>(
    id: string,
    pkg: RR.GetRegistryPackageOptions,
    active: boolean,
  ) {
    const { version, flavor } = this.findVersions(pkg.best)
    const bestVersion = flavor
      ? active
        ? flavor.toString()
        : version
      : version
    return {
      id,
      version: bestVersion,
      defaultVersion: version,
      flavorVersion: flavor ? flavor.toString() : undefined,
      ...pkg.best[bestVersion],
      ...pkg,
    } as MarketplacePkg<T>
  }

  private fetchPackage$<T extends RR.GetRegistryPackagesReq>(
    url: string,
    params: T,
  ): Observable<MarketplacePkg<T>> {
    return combineLatest([
      this.pkgFlavorService.getFlavorStatus$(),
      from(this.api.getRegistryPackage(url, params)),
    ]).pipe(map(([active, pkg]) => this.expandData(params.id!, pkg, active)))
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

  private findVersions(pkgBestVersions: {
    [key: string]: T.PackageVersionInfo
  }) {
    // only ever contains 2 items, a default version and a flavor version
    const parsed = Object.keys(pkgBestVersions).map(v =>
      ExtendedVersion.parse(v),
    )
    const flavor = parsed.find(v => v.flavor)
    const version = parsed.find(v => !v.flavor)!
    return { version: version.toString(), flavor }
  }
}

function toStoreIdentity(url: string, uiStore: UIStore): StoreIdentity {
  return {
    url,
    ...uiStore,
  }
}
