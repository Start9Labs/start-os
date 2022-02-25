import { Injectable } from '@angular/core'
import { Subscription } from 'rxjs'
import {
  MarketplaceData,
  MarketplacePkg,
  RR,
} from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import { Emver } from '@start9labs/shared'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Injectable({
  providedIn: 'root',
})
export class MarketplaceService {
  data: MarketplaceData
  pkgs: MarketplacePkg[] = []
  releaseNotes: {
    [id: string]: {
      [version: string]: string
    }
  } = {}
  marketplaceUrl: string

  constructor(
    private readonly api: ApiService,
    private readonly emver: Emver,
    private readonly patch: PatchDbService,
    private readonly config: ConfigService,
  ) {}

  init(): Subscription {
    return this.patch.watch$('ui', 'marketplace').subscribe(marketplace => {
      if (!marketplace || !marketplace['selected-id']) {
        this.marketplaceUrl = this.config.marketplace.url
      } else {
        this.marketplaceUrl =
          marketplace['known-hosts'][marketplace['selected-id']].url
      }
    })
  }

  async load(): Promise<void> {
    try {
      const [data, pkgs] = await Promise.all([
        this.getMarketplaceData({}),
        this.getMarketplacePkgs({ page: 1, 'per-page': 100 }),
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

  async installPackage(req: Omit<RR.InstallPackageReq, 'marketplace-url'>) {
    req['marketplace-url'] = this.marketplaceUrl
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
    const latestPkgs = await this.getMarketplacePkgs({
      ids: idAndCurrentVersions,
    })

    return latestPkgs.filter(latestPkg => {
      const latestVersion = latestPkg.manifest.version
      const curVersion = localPkgs[latestPkg.manifest.id]?.manifest.version
      return !!curVersion && this.emver.compare(latestVersion, curVersion) === 1
    })
  }

  async getPkg(id: string, version = '*'): Promise<MarketplacePkg> {
    const pkgs = await this.getMarketplacePkgs({
      ids: [{ id, version }],
    })
    const pkg = pkgs.find(pkg => pkg.manifest.id == id)

    if (!pkg) {
      throw new Error(`No results for ${id}${version ? ' ' + version : ''}`)
    } else {
      return pkg
    }
  }

  async cacheReleaseNotes(id: string): Promise<void> {
    this.releaseNotes[id] = await this.getReleaseNotes({ id })
  }

  async getMarketplaceData(
    params: RR.GetMarketplaceDataReq,
    url?: string,
  ): Promise<RR.GetMarketplaceDataRes> {
    url = url || this.marketplaceUrl
    return this.api.marketplaceProxy('/package/v0/info', params, url)
  }

  async getMarketplacePkgs(
    params: Omit<RR.GetMarketplacePackagesReq, 'eos-version-compat'>,
  ): Promise<RR.GetMarketplacePackagesRes> {
    if (params.query) delete params.category
    if (params.ids) params.ids = JSON.stringify(params.ids) as any

    const qp: RR.GetMarketplacePackagesReq = {
      ...params,
      'eos-version-compat':
        this.patch.getData()['server-info']['eos-version-compat'],
    }

    return this.api.marketplaceProxy(
      '/package/v0/index',
      qp,
      this.marketplaceUrl,
    )
  }

  async getReleaseNotes(
    params: RR.GetReleaseNotesReq,
  ): Promise<RR.GetReleaseNotesRes> {
    return this.api.marketplaceProxy(
      `/package/v0/release-notes/${params.id}`,
      {},
      this.marketplaceUrl,
    )
  }
}
