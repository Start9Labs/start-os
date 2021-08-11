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
    return this.embassyApiService.marketplaceProxy({
      isEos: true,
      relativePath: '/eos',
      params,
      withCredentials: false,
      method: Method.GET,
    })
  }

  async getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes> {
    return this.embassyApiService.marketplaceProxy({
      isEos: false,
      relativePath: '/data',
      params,
      withCredentials: false,
      method: Method.GET,
    })
  }

  async getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise<RR.GetMarketplacePackagesRes> {
    return this.embassyApiService.marketplaceProxy({
      isEos: false,
      relativePath: '/packages',
      params: {
        ...params,
        ids: JSON.stringify(params.ids),
      },
      withCredentials: false,
      method: Method.GET,
    })
  }

  async getReleaseNotes (params: RR.GetReleaseNotesReq): Promise<RR.GetReleaseNotesRes> {
    return this.embassyApiService.marketplaceProxy({
      isEos: false,
      relativePath: '/release-notes',
      params,
      withCredentials: false,
      method: Method.GET,
    })
  }

  async getLatestVersion (params: RR.GetLatestVersionReq): Promise<RR.GetLatestVersionRes> {
    return this.embassyApiService.marketplaceProxy({
      isEos: false,
      relativePath: '/latest-version',
      params,
      withCredentials: false,
      method: Method.GET,
    })
  }
}
