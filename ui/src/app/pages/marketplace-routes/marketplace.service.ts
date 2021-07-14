import { Injectable } from '@angular/core'
import { MarketplacePkg } from 'src/app/services/api/api-types'
import { ApiService } from 'src/app/services/api/api.service'

@Injectable({
  providedIn: 'root',
})
export class MarketplaceService {
  pkgs: { [id: string]: MarketplacePkg } = { }
  releaseNotes: { [id: string]: {
    [version: string]: string
  } }

  constructor (
    private readonly apiService: ApiService,
  ) { }

  async getMarketplaceData () {
    return this.apiService.getMarketplaceData({ })
  }

  async getEos () {
    return this.apiService.getEos({ })
  }

  async getPkgs (category: string, query: string, page: number, perPage: number) : Promise<MarketplacePkg[]> {
    const pkgs = await this.apiService.getMarketplacePkgs({
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
    const pkg = (await this.apiService.getMarketplacePkgs({ id, version }))[0]
    if (pkg) {
      this.pkgs[id] = pkg
    } else {
      throw new Error(`No results for ${id}${version ? ' ' + version : ''}.`)
    }
  }

  async getReleaseNotes (id: string): Promise<void> {
    this.releaseNotes[id] = await this.apiService.getReleaseNotes({ id })
  }
}


