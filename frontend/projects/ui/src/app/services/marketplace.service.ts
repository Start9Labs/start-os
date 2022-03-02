import { Injectable } from '@angular/core'
import { LoadingController } from '@ionic/angular'
import { Emver, ErrorToastService } from '@start9labs/shared'
import {
  MarketplacePkg,
  AbstractMarketplaceService,
  Marketplace,
  MarketplaceData,
} from '@start9labs/marketplace'
import { defer, from, Observable, of, Subscription } from 'rxjs'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import {
  PackageDataEntry,
  ServerInfo,
} from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { catchError, map, shareReplay, switchMap, tap } from 'rxjs/operators'

@Injectable()
export class MarketplaceService extends AbstractMarketplaceService {
  notes = new Map<string, Record<string, string>>()

  private init$: Observable<Marketplace> = defer(() =>
    this.patch.watch$('ui', 'marketplace'),
  ).pipe(
    map(marketplace =>
      marketplace && marketplace['selected-id']
        ? marketplace['known-hosts'][marketplace['selected-id']]
        : this.config.marketplace,
    ),
    shareReplay(),
  )

  private data$: Observable<MarketplaceData> = this.init$.pipe(
    switchMap(({ url }) =>
      from(this.getMarketplaceData({ 'server-id': this.serverInfo.id }, url)),
    ),
    shareReplay(),
  )

  private pkg$: Observable<MarketplacePkg[]> = this.init$.pipe(
    switchMap(({ url, name }) =>
      from(this.getMarketplacePkgs({ page: 1, 'per-page': 100 }, url)).pipe(
        tap(() => {
          const { marketplace } = this.patch.getData().ui

          if (!marketplace?.['selected-id']) {
            return
          }

          const selectedId = marketplace['selected-id']
          const knownHosts = marketplace['known-hosts']

          if (knownHosts[selectedId].name !== name) {
            this.api.setDbValue({
              pointer: `/marketplace/known-hosts/${selectedId}/name`,
              value: name,
            })
          }
        }),
      ),
    ),
    catchError(e => this.errToast.present(e) && of([])),
  )

  constructor(
    private readonly api: ApiService,
    private readonly emver: Emver,
    private readonly patch: PatchDbService,
    private readonly config: ConfigService,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
  ) {
    super()
  }

  getCategories(): Observable<string[]> {
    return this.data$.pipe(map(({ categories }) => categories))
  }

  getPackages(): Observable<MarketplacePkg[]> {
    return this.pkg$
  }

  getReleaseNotes(id: string): Observable<Record<string, string>> {
    if (this.notes.has(id)) {
      return of(this.notes.get(id))
    }

    return this.init$.pipe(
      switchMap(({ url }) => this.loadReleaseNotes(id, url)),
      tap(response => this.notes.set(id, response)),
      catchError(e => this.errToast.present(e) && of({})),
    )
  }

  // TODO: Get rid of
  init(): Subscription {
    return this.patch.watch$('ui', 'marketplace').subscribe(marketplace => {
      if (!marketplace || !marketplace['selected-id']) {
        console.log('**MARKETPLACE', this.config)
        this.marketplace = this.config.marketplace
      } else {
        this.marketplace =
          marketplace['known-hosts'][marketplace['selected-id']]
      }
    })
  }

  // TODO: Get rid of
  async load(): Promise<void> {
    try {
      const [data, pkgs] = await Promise.all([
        this.getMarketplaceData(
          {
            'server-id': this.serverInfo.id,
          },
          this.marketplace.url,
        ),
        this.getMarketplacePkgs(
          { page: 1, 'per-page': 100 },
          this.marketplace.url,
        ),
      ])

      this.data = data
      this.pkgs = pkgs

      if (this.patch.getData().ui.marketplace?.['selected-id']) {
        const { 'selected-id': selectedId, 'known-hosts': knownHosts } =
          this.patch.getData().ui.marketplace
        if (knownHosts[selectedId].name !== this.data.name) {
          this.api.setDbValue({
            pointer: `/marketplace/known-hosts/${selectedId}/name`,
            value: this.data.name,
          })
        }
      }
    } catch (e) {
      this.data = undefined
      this.pkgs = []
      throw e
    }
  }

  async install(id: string, version?: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Beginning Installation',
      cssClass: 'loader',
    })
    loader.present()

    try {
      await this.installPackage({
        id,
        'version-spec': version ? `=${version}` : undefined,
      })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  async installPackage(req: Omit<RR.InstallPackageReq, 'marketplace-url'>) {
    req['marketplace-url'] = this.marketplace.url
    return this.api.installPackage(req as RR.InstallPackageReq)
  }

  async getUpdates(
    localPkgs: Record<string, PackageDataEntry>,
  ): Promise<MarketplacePkg[]> {
    const id = this.patch.getData().ui.marketplace?.['selected-id']
    const url = id
      ? this.patch.getData().ui.marketplace['known-hosts'][id].url
      : this.config.marketplace.url

    const idAndCurrentVersions = Object.keys(localPkgs)
      .map(key => ({
        id: key,
        version: localPkgs[key].manifest.version,
        marketplaceUrl: localPkgs[key].installed['marketplace-url'],
      }))
      .filter(pkg => {
        return pkg.marketplaceUrl === url
      })
    const latestPkgs = await this.getMarketplacePkgs(
      {
        ids: idAndCurrentVersions,
      },
      this.marketplace.url,
    )

    return latestPkgs.filter(latestPkg => {
      const latestVersion = latestPkg.manifest.version
      const curVersion = localPkgs[latestPkg.manifest.id]?.manifest.version
      return !!curVersion && this.emver.compare(latestVersion, curVersion) === 1
    })
  }

  async getPkg(id: string, version = '*'): Promise<MarketplacePkg> {
    const pkgs = await this.getMarketplacePkgs(
      {
        ids: [{ id, version }],
      },
      this.marketplace.url,
    )
    const pkg = pkgs.find(pkg => pkg.manifest.id == id)

    if (!pkg) {
      throw new Error(`No results for ${id}${version ? ' ' + version : ''}`)
    } else {
      return pkg
    }
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
  ): Promise<RR.GetMarketplacePackagesRes> {
    if (params.query) delete params.category
    if (params.ids) params.ids = JSON.stringify(params.ids) as any

    const qp: RR.GetMarketplacePackagesReq = {
      ...params,
      'eos-version-compat': this.serverInfo['eos-version-compat'],
    }

    return this.api.marketplaceProxy('/package/v0/index', qp, url)
  }

  private get serverInfo(): ServerInfo {
    return this.patch.getData()['server-info']
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
}
