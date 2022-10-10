import { Injectable } from '@angular/core'
import { Emver } from '@start9labs/shared'
import {
  MarketplacePkg,
  AbstractMarketplaceService,
  MarketplaceInfo,
} from '@start9labs/marketplace'
import { BehaviorSubject, firstValueFrom, from, Observable, of } from 'rxjs'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  DataModel,
  Manifest,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import {
  distinctUntilChanged,
  map,
  shareReplay,
  switchMap,
  take,
  tap,
} from 'rxjs/operators'
import { getServerInfo } from '../util/get-server-info'

type MarketplaceURL = string

interface MarketplaceData {
  info: MarketplaceInfo | null
  packages: MarketplacePkg[]
}
type MasterCache = Record<MarketplaceURL, MarketplaceData>

type UpdateRequests = Record<MarketplaceURL, RequestStatus>

export enum RequestStatus {
  Queued = 'queued',
  Active = 'active',
  Complete = 'complete',
  Failed = 'failed',
}

@Injectable()
export class MarketplaceService implements AbstractMarketplaceService {
  private readonly cache: MasterCache = {}
  private readonly cache$ = new BehaviorSubject(this.cache)
  private readonly updateRequests$ = new BehaviorSubject<UpdateRequests>({})

  private readonly uiMarketplace$: Observable<{ url: string; name: string }> =
    this.patch.watch$('ui', 'marketplace').pipe(
      distinctUntilChanged(
        (prev, curr) => prev['selected-url'] === curr['selected-url'],
      ),
      map(data => {
        const url = data['selected-url']
        return {
          url,
          name: data['known-hosts'][url],
        }
      }),
      shareReplay(1),
    )

  private readonly marketplaceData$: Observable<MarketplaceData> =
    this.uiMarketplace$.pipe(
      switchMap(({ url, name }) =>
        from(this.loadMarketplace(url)).pipe(
          tap(data => {
            this.updateName(url, name, data.info!.name)
          }),
        ),
      ),
      shareReplay(1),
    )

  private readonly marketplaceInfo$: Observable<MarketplaceInfo> =
    this.marketplaceData$.pipe(map(data => data.info!))

  private readonly marketplacePkgs$: Observable<MarketplacePkg[]> =
    this.marketplaceData$.pipe(map(data => data.packages))

  private readonly urlToLocalMap$: Observable<
    Record<MarketplaceURL, Manifest[]>
  > = this.patch.watch$('package-data').pipe(
    take(1),
    map(pkgs =>
      Object.values(pkgs)
        .filter(localPkg => localPkg.state === PackageState.Installed)
        .reduce((localPkgMap, pkg) => {
          const url = pkg.installed!['marketplace-url'] || '' // side-loaded services will not have marketplace-url
          if (url) {
            if (!localPkgMap[url]) {
              localPkgMap[url] = [pkg.manifest]
            } else {
              localPkgMap[url].push(pkg.manifest)
            }
          }
          return localPkgMap
        }, {} as Record<MarketplaceURL, Manifest[]>),
    ),
    shareReplay(),
  )

  constructor(
    private readonly api: ApiService,
    private readonly patch: PatchDB<DataModel>,
    private readonly emver: Emver,
  ) {}

  getUpdateRequests$() {
    return this.updateRequests$
  }

  getCache$() {
    return this.cache$
  }

  getUiMarketplace$(): Observable<{ url: string; name: string }> {
    return this.uiMarketplace$
  }

  getMarketplaceInfo$(): Observable<MarketplaceInfo> {
    return this.marketplaceInfo$
  }

  getPackages$(): Observable<MarketplacePkg[]> {
    return this.marketplacePkgs$
  }

  getPackage$(
    id: string,
    version: string,
    url?: string,
  ): Observable<MarketplacePkg | undefined> {
    return this.uiMarketplace$.pipe(
      switchMap(m => {
        url = url || m.url
        if (this.cache[url]) {
          const pkg = this.getPkgFromCache(id, version, url)
          if (pkg) return of(pkg)
        }

        if (version === '*') {
          return from(this.loadPackage(id, url))
        } else {
          return from(this.fetchPackage(id, version, url))
        }
      }),
    )
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

  async validateMarketplace(url: string): Promise<string> {
    await this.loadInfo(url)
    return this.cache[url]!.info!.name
  }

  fetchReleaseNotes$(
    id: string,
    url?: string,
  ): Observable<Record<string, string>> {
    return this.uiMarketplace$.pipe(
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

  fetchPackageMarkdown$(
    id: string,
    type: string,
    url?: string,
  ): Observable<string> {
    return this.uiMarketplace$.pipe(
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

  async loadUpdates(): Promise<void> {
    const localMap = await firstValueFrom(this.urlToLocalMap$)

    const urls = Object.keys(localMap)

    const statusMap = urls.reduce((obj, url) => {
      return {
        ...obj,
        [url]: RequestStatus.Active,
      }
    }, {} as UpdateRequests)

    this.updateRequests$.next(statusMap)

    urls.forEach(url => {
      const ids = localMap[url].map(({ id }) => {
        return { id, version: '*' }
      })

      this.loadPackages({ ids }, url)
        .catch(() => {
          this.updateRequests$.next({
            ...this.updateRequests$.value,
            [url]: RequestStatus.Failed,
          })
        })
        .then(() => {
          this.updateRequests$.next({
            ...this.updateRequests$.value,
            [url]: RequestStatus.Complete,
          })
        })
    })
  }

  private async loadMarketplace(url: string): Promise<MarketplaceData> {
    const cachedInfo = this.cache[url]?.info
    const [info, packages] = await Promise.all([
      cachedInfo || this.loadInfo(url),
      this.loadPackages({}, url),
    ])
    return { info, packages }
  }

  private async loadInfo(url: string): Promise<MarketplaceInfo> {
    const info = await this.fetchInfo(url)
    this.updateCache(url, info)
    return info
  }

  private async loadPackage(
    id: string,
    url: string,
  ): Promise<MarketplacePkg | undefined> {
    const pkgs = await this.loadPackages({ ids: [{ id, version: '*' }] }, url)
    return pkgs[0]
  }

  private async loadPackages(
    params: Omit<
      RR.GetMarketplacePackagesReq,
      'eos-version-compat' | 'page' | 'per-page'
    >,
    url: string,
  ): Promise<MarketplacePkg[]> {
    const pkgs = await this.fetchPackages(params, url)
    this.updateCache(url, undefined, pkgs)
    return pkgs
  }

  private async fetchInfo(url: string): Promise<RR.GetMarketplaceDataRes> {
    const { id } = await getServerInfo(this.patch)

    const params: RR.GetMarketplaceDataReq = {
      'server-id': id,
    }

    return this.api.marketplaceProxy<RR.GetMarketplaceDataRes>(
      '/package/v0/info',
      params,
      url,
    )
  }

  private async fetchPackage(
    id: string,
    version: string,
    url: string,
  ): Promise<MarketplacePkg | undefined> {
    const pkgs = await this.fetchPackages({ ids: [{ id, version }] }, url)
    return pkgs[0]
  }

  private async fetchPackages(
    params: Omit<
      RR.GetMarketplacePackagesReq,
      'eos-version-compat' | 'page' | 'per-page'
    >,
    url: string,
  ): Promise<RR.GetMarketplacePackagesRes> {
    const qp: RR.GetMarketplacePackagesReq = {
      ...params,
      'eos-version-compat': (await getServerInfo(this.patch))[
        'eos-version-compat'
      ],
      page: 1,
      'per-page': 100,
    }
    if (qp.ids) qp.ids = JSON.stringify(qp.ids)

    return this.api.marketplaceProxy<RR.GetMarketplacePackagesRes>(
      '/package/v0/index',
      qp,
      url,
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

  private getPkgFromCache(
    id: string,
    version: string,
    url: string,
  ): MarketplacePkg | undefined {
    return this.cache[url]?.packages.find(p => {
      const versionIsSame =
        version === '*' || this.emver.compare(p.manifest.version, version) === 0

      return p.manifest.id === id && versionIsSame
    })
  }

  private updateCache(
    url: string,
    info?: MarketplaceInfo,
    pkgs?: MarketplacePkg[],
  ): void {
    const cache = this.cache[url]

    let packages = cache?.packages || []
    if (pkgs) {
      const filtered = packages.filter(
        cachedPkg =>
          !pkgs.find(pkg => pkg.manifest.id === cachedPkg.manifest.id),
      )
      packages = filtered.concat(pkgs)
    }

    this.cache[url] = {
      info: info || cache?.info || null,
      packages,
    }

    this.cache$.next(this.cache)
  }
}
