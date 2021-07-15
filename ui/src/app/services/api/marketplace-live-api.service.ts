import { Injectable } from '@angular/core'
import { HttpService } from '../http.service'
import { getMarketURL  } from './marketplace-api.service'
import { RR } from './api-types'
import { MarketplaceApiService } from './marketplace-api.service'
import { PatchDbModel } from '../patch-db/patch-db.service'

@Injectable()
export class MarketplaceLiveApiService extends MarketplaceApiService {

  constructor (
    private readonly http: HttpService,
    private readonly patch: PatchDbModel,
  ) { super() }

  async getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes> {
    return this.http.simpleGet<RR.GetMarketplaceDataRes>(getMarketURL('package', this.patch.data), params)
  }

  async getEos (params: RR.GetMarketplaceEOSReq): Promise<RR.GetMarketplaceEOSRes> {
    return this.http.simpleGet<RR.GetMarketplaceEOSRes>(getMarketURL('eos', this.patch.data), params)
  }

  async getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise<RR.GetMarketplacePackagesRes> {
    return this.http.simpleGet<RR.GetMarketplacePackagesRes>(getMarketURL('package', this.patch.data), params)
  }

  async getReleaseNotes (params: RR.GetReleaseNotesReq): Promise<RR.GetReleaseNotesRes> {
    return this.http.simpleGet<RR.GetReleaseNotesRes>(getMarketURL('package', this.patch.data), params)
  }
}
