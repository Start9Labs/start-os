import { Injectable } from '@angular/core'
import { MarketplacePkg } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Emver } from 'src/app/services/emver.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Injectable({
  providedIn: 'root',
})
export class MarketplaceService {
  allPkgs: MarketplacePkg[] = []
  pkgs: { [id: string]: MarketplacePkg } = { }
  updates: MarketplacePkg[] = []
  releaseNotes: { [id: string]: {
    [version: string]: string
  } } = { }

  constructor (
    private readonly api: ApiService,
    private readonly emver: Emver,
  ) { }

  async getUpdates (localPkgs: { [id: string]: PackageDataEntry }) : Promise<void>   {
    const idAndCurrentVersions =  Object.keys(localPkgs).map(key => ({ id: key, version: localPkgs[key].manifest.version }))
    const latestPkgs = (await this.api.getMarketplacePkgs({
      ids: idAndCurrentVersions,
    }))

    const updates = latestPkgs.filter(latestPkg => {
      const latestVersion = latestPkg.manifest.version
      const curVersion = localPkgs[latestPkg.manifest.id]?.manifest.version
      return !!curVersion && this.emver.compare(latestVersion, curVersion) === 1
    })

    this.updates = updates
  }

  async getAllPkgs (): Promise<void> {
    this.allPkgs = await this.getPkgs(
      undefined,
      null,
      1,
      100000,
    )
  }

  async getPkgs (category: string, query: string, page: number, perPage: number) : Promise<MarketplacePkg[]> {
    const pkgs = await this.api.getMarketplacePkgs({
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
    const pkgs = await this.api.getMarketplacePkgs({
      ids: [{ id, version: version || '*' }],
    })
    const pkg = pkgs.find(pkg => pkg.manifest.id == id)
    if (pkg) {
      this.pkgs[id] = pkg
    } else {
      throw new Error(`No results for ${id}${version ? ' ' + version : ''}.`)
    }
  }

  async getReleaseNotes (id: string): Promise<void> {
    this.releaseNotes[id] = await this.api.getReleaseNotes({ id })
  }
}


