import { Injectable } from '@angular/core'
import { Method } from '../../http.service'
import { RR } from '../api.types'
import { MarketplaceApiService } from './marketplace-api.service'
import { PatchDbService } from '../../patch-db/patch-db.service'
import { ConfigService } from '../../config.service'
import { ApiService } from '../embassy/embassy-api.service'

@Injectable()
export class MarketplaceLiveApiService extends MarketplaceApiService {

  constructor (
    private readonly embassyApiService: ApiService,
    config: ConfigService,
    patch: PatchDbService,
  ) {
    super( config, patch)
  }

  async getEos (params: RR.GetMarketplaceEOSReq): Promise<RR.GetMarketplaceEOSRes> {
    return this.embassyApiService.marketplaceProxy('/marketplace/eos', params)
  }

  async getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise < RR.GetMarketplaceDataRes > {
    return this.embassyApiService.marketplaceProxy('/marketplace/package/data', params)
  }

  async getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise < RR.GetMarketplacePackagesRes > {
    return this.embassyApiService.marketplaceProxy('/marketplace/package/packages', { ...params, ids: JSON.stringify(params.ids) })
  }

  async getReleaseNotes (params: RR.GetReleaseNotesReq): Promise < RR.GetReleaseNotesRes > {
    return this.embassyApiService.marketplaceProxy('/marketplace/package/release-notes', params)
  }

  async getLatestVersion (params: RR.GetLatestVersionReq): Promise < RR.GetLatestVersionRes > {
    return this.embassyApiService.marketplaceProxy('/marketplace/package/latest-version', params)
  }
}
