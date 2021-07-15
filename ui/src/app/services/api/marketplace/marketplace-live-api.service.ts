import { Injectable } from '@angular/core'
import { HttpService } from '../../http.service'
import { RR } from '../api.types'
import { MarketplaceApiService } from './marketplace-api.service'
import { PatchDbService } from '../../patch-db/patch-db.service'
import { ConfigService } from '../../config.service'

@Injectable()
export class MarketplaceLiveApiService extends MarketplaceApiService {

  constructor (
    private readonly http: HttpService,
    config: ConfigService,
    patch: PatchDbService,
  ) {
    super(config, patch)
  }

  async getEos (params: RR.GetMarketplaceEOSReq): Promise<RR.GetMarketplaceEOSRes> {
    return this.http.simpleGet<RR.GetMarketplaceEOSRes>(this.getMarketplaceURL('eos'), params)
  }

  async getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes> {
    return this.http.simpleGet<RR.GetMarketplaceDataRes>(this.getMarketplaceURL('package'), params)
  }

  async getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise<RR.GetMarketplacePackagesRes> {
    return this.http.simpleGet<RR.GetMarketplacePackagesRes>(this.getMarketplaceURL('package'), params)
  }

  async getReleaseNotes (params: RR.GetReleaseNotesReq): Promise<RR.GetReleaseNotesRes> {
    return this.http.simpleGet<RR.GetReleaseNotesRes>(this.getMarketplaceURL('package'), params)
  }
}
