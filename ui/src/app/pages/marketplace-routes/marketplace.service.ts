import { Injectable } from '@angular/core'
import { MarketplacePkg } from 'src/app/services/api/api.types'
import { MarketplaceApiService } from 'src/app/services/api/marketplace/marketplace-api.service'
import { Emver } from 'src/app/services/emver.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Injectable({
  providedIn: 'root',
})
export class MarketplaceService {
  pkgs: { [id: string]: MarketplacePkg } = { }
  updates: MarketplacePkg[] = null
  releaseNotes: { [id: string]: {
    [version: string]: string
  } }

  constructor (
    private readonly marketplaceApiService: MarketplaceApiService,
    private readonly emver: Emver,
  ) { }

  async getUpdates (pkgData: { [id: string]: PackageDataEntry}) : Promise<MarketplacePkg[]>   {
    const idAndCurrentVersions =  Object.keys(pkgData).map(key => ({ id: key, version: pkgData[key].manifest.version }))
    console.log(JSON.stringify(idAndCurrentVersions))
    const latestPkgs = (await this.marketplaceApiService.getMarketplacePkgs({
      ids: idAndCurrentVersions,
    }))

    const updates = latestPkgs.filter(latestPkg => {
      const latestVersion = latestPkg.manifest.version
      const curVersion = pkgData[latestPkg.manifest.id]?.manifest.version
      return !!curVersion && this.emver.compare(latestVersion, curVersion) === 1
    })

    this.updates = updates
    return updates
  }

  async getPkgs (category: string, query: string, page: number, perPage: number) : Promise<MarketplacePkg[]> {
    const pkgs = await this.marketplaceApiService.getMarketplacePkgs({
      category: category !== 'all' ? category : undefined,
      query,
      page: String(page),
      'per-page': String(perPage),
    })
    this.pkgs = pkgs.reduce((cur, val) => {
      cur[val.manifest.id] = val
      return cur
    }, { })
    return pkgs
  }

  async getPkg (id: string, version?: string): Promise<void> {
    const pkg = (await this.marketplaceApiService.getMarketplacePkgs({ ids: [{ id, version }]}))[0]
    if (pkg) {
      this.pkgs[id] = pkg
    } else {
      throw new Error(`No results for ${id}${version ? ' ' + version : ''}.`)
    }
  }

  async getReleaseNotes (id: string): Promise<void> {
    this.releaseNotes[id] = await this.marketplaceApiService.getReleaseNotes({ id })
  }
}


