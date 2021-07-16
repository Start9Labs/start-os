import { Injectable } from '@angular/core'
import { HttpService, Method } from '../../http.service'
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
    const url = this.getMarketplaceURL('eos')
    return this.http.httpRequest<RR.GetMarketplaceEOSRes>({
      method: Method.GET,
      url: url + '/eos',
      params,
      withCredentials: false,
    })
  }

  async getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes> {
    const url = this.getMarketplaceURL('package')
    return this.http.httpRequest<RR.GetMarketplaceDataRes>({
      method: Method.GET,
      url: url + '/data',
      params,
      withCredentials: false,
    })
  }

  async getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise<RR.GetMarketplacePackagesRes> {
    const url = this.getMarketplaceURL('package', params.ids?.length > 1)
    return this.http.httpRequest<RR.GetMarketplacePackagesRes>({
      method: Method.GET,
      url: url + '/packages',
      params: {
        ...params,
        ids: JSON.stringify(params.ids),
      },
      withCredentials: false,
    })
  }

  async getReleaseNotes (params: RR.GetReleaseNotesReq): Promise<RR.GetReleaseNotesRes> {
    const url = this.getMarketplaceURL('package')
    return this.http.httpRequest<RR.GetReleaseNotesRes>({
      method: Method.GET,
      url: url + + '/release-notes',
      params,
      withCredentials: false,
    })
  }

  async getLatestVersion (params: RR.GetLatestVersionReq): Promise<RR.GetLatestVersionRes> {
    const url = this.getMarketplaceURL('package', params.ids?.length > 1)
    return this.http.httpRequest<RR.GetLatestVersionRes>({
      method: Method.GET,
      url: url + '/latest-version',
      params,
      withCredentials: false,
    })
  }
}
