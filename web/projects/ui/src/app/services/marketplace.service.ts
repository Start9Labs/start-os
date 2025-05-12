import { inject, Injectable } from '@angular/core'
import {
  GetPackageRes,
  Marketplace,
  MarketplacePkg,
  StoreDataWithUrl,
  StoreIdentity,
} from '@start9labs/marketplace'
import { defaultRegistries, Exver, sameUrl } from '@start9labs/shared'
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
import { DataModel } from 'src/app/services/patch-db/data-model'

const { start9, community } = defaultRegistries

@Injectable({
  providedIn: 'root',
})
export class MarketplaceService {
  private readonly api = inject(ApiService)
  private readonly patch: PatchDB<DataModel> = inject(PatchDB)
  private readonly exver = inject(Exver)

  readonly registries$: Observable<StoreIdentity[]> = this.patch
    .watch$('ui', 'registries')
    .pipe(
      map(registries => [
        toStoreIdentity(start9, registries[start9]),
        toStoreIdentity(community, registries[community]),
        ...Object.entries(registries)
          .filter(([url, _]) => ![start9, community].includes(url as any))
          .map(([url, name]) => toStoreIdentity(url, name)),
      ]),
    )

  readonly currentRegistryUrl$ = new ReplaySubject<string>(1)

  readonly requestErrors$ = new BehaviorSubject<string[]>([])

  readonly marketplace$: Observable<Marketplace> = this.registries$.pipe(
    startWith<StoreIdentity[]>([]),
    pairwise(),
    mergeMap(([prev, curr]) =>
      curr.filter(c => !prev.find(p => sameUrl(c.url, p.url))),
    ),
    mergeMap(({ url, name }) =>
      this.fetchRegistry$(url).pipe(
        tap(data => {
          if (data?.info.name)
            this.updateRegistryName(url, name, data.info.name)
        }),
        map(data => [url, data] satisfies [string, StoreDataWithUrl | null]),
        startWith<[string, StoreDataWithUrl | null]>([url, null]),
      ),
    ),
    scan<[string, StoreDataWithUrl | null], Marketplace>(
      (requests, [url, store]) => {
        requests[url] = store

        return requests
      },
      {},
    ),
    shareReplay(1),
  )

  readonly currentRegistry$: Observable<StoreDataWithUrl> = combineLatest([
    this.marketplace$,
    this.currentRegistryUrl$,
    this.currentRegistryUrl$.pipe(
      distinctUntilChanged(),
      switchMap(url => this.fetchRegistry$(url).pipe(startWith(null))),
    ),
  ]).pipe(
    map(([all, url, current]) => current || all[url]),
    filter(Boolean),
    shareReplay(1),
  )

  getPackage$(
    id: string,
    version: string | null,
    flavor: string | null,
    registryUrl?: string,
  ): Observable<MarketplacePkg> {
    return this.currentRegistry$.pipe(
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

  fetchInfo$(registry: string): Observable<T.RegistryInfo> {
    return from(this.api.getRegistryInfo({ registry })).pipe(
      map(info => ({
        ...info,
        categories: {
          all: { name: 'All' },
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
    console.warn('FETCHING REGISTRY: ', url)
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
    return from(
      this.api.getRegistryPackages({
        registry: url,
        id: null,
        targetVersion: null,
        otherVersions: 'short',
      }),
    ).pipe(
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
      this.api.getRegistryPackage({
        registry: url,
        id,
        targetVersion: version ? `=${version}` : null,
        otherVersions: 'short',
      }),
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

  private async updateRegistryName(
    url: string,
    oldName: string | null,
    newName: string,
  ): Promise<void> {
    console.warn(oldName, newName)
    if (oldName !== newName) {
      this.api.setDbValue<string>(['registries', url], newName)
    }
  }
}

function toStoreIdentity(url: string, name?: string | null): StoreIdentity {
  return {
    url,
    name: name || url,
  }
}
