import { Injectable } from '@angular/core'
import { MarketplacePkg } from 'src/app/services/api/api.types'
import { MarketplaceApiService } from 'src/app/services/api/marketplace/marketplace-api.service'

@Injectable({
  providedIn: 'root',
})
export class MarketplaceService {
  pkgs: { [id: string]: MarketplacePkg } = { }
  releaseNotes: { [id: string]: {
    [version: string]: string
  } }

  constructor (
    private readonly marketplaceApiService: MarketplaceApiService,
  ) { }

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
    const pkg = (await this.marketplaceApiService.getMarketplacePkgs({ id, version }))[0]
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


