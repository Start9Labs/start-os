import { Injectable } from '@angular/core'
import { MarketplaceData, MarketplaceEOS, MarketplacePkg } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Emver } from 'src/app/services/emver.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Injectable({
  providedIn: 'root',
})
export class MarketplaceService {
  data: MarketplaceData
  eos: MarketplaceEOS
  pkgs: MarketplacePkg[] = []
  releaseNotes: { [id: string]: {
    [version: string]: string
  } } = { }

  constructor (
    private readonly api: ApiService,
    private readonly emver: Emver,
  ) { }

  async load (): Promise<void> {
    const [data, eos, pkgs] = await Promise.all([
      this.api.getMarketplaceData({ }),
      this.api.getEos({ }),
      this.getPkgs(1, 100),
    ])
    this.data = data
    this.eos = eos
    this.pkgs = pkgs
  }

  async getUpdates (localPkgs: { [id: string]: PackageDataEntry }) : Promise<MarketplacePkg[]>   {
    const idAndCurrentVersions =  Object.keys(localPkgs).map(key => ({ id: key, version: localPkgs[key].manifest.version }))
    const latestPkgs = await this.api.getMarketplacePkgs({
      ids: idAndCurrentVersions,
    })

    return latestPkgs.filter(latestPkg => {
      const latestVersion = latestPkg.manifest.version
      const curVersion = localPkgs[latestPkg.manifest.id]?.manifest.version
      return !!curVersion && this.emver.compare(latestVersion, curVersion) === 1
    })
  }

  async getPkg (id: string, version?: string): Promise<MarketplacePkg> {
    const pkgs = await this.api.getMarketplacePkgs({
      ids: [{ id, version: version || '*' }],
    })
    const pkg = pkgs.find(pkg => pkg.manifest.id == id)

    if (!pkg) {
      throw new Error(`No results for ${id}${version ? ' ' + version : ''}`)
    } else {
      return pkg
    }
  }

  async getReleaseNotes (id: string): Promise<void> {
    this.releaseNotes[id] = await this.api.getReleaseNotes({ id })
  }

  private async getPkgs (page: number, perPage: number) : Promise<MarketplacePkg[]> {
    const pkgs = await this.api.getMarketplacePkgs({
      page: String(page),
      'per-page': String(perPage),
    })

    return pkgs
  }
}


