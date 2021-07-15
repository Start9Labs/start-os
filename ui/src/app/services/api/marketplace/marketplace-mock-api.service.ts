import { Injectable } from '@angular/core'
import { pauseFor } from '../../../util/misc.util'
import { RR } from '../api.types'
import { Mock } from '../api.fixures'
import { HttpService } from '../../http.service'
import { MarketplaceApiService } from './marketplace-api.service'
import { PatchDbService } from '../../patch-db/patch-db.service'
import { ConfigService } from '../../config.service'

@Injectable()
export class MarketplaceMockApiService extends MarketplaceApiService {

  constructor (
    private readonly http: HttpService,
    config: ConfigService,
    patch: PatchDbService,
  ) {
    super(config, patch)
  }

  // marketplace

  async getEos (params: RR.GetMarketplaceEOSReq): Promise<RR.GetMarketplaceEOSRes> {
    let url = this.getMarketplaceURL('eos')
    if (this.useLocal(url)) {
      await pauseFor(2000)
      return Mock.MarketplaceEos
    }
    url = `${url}/sys/version/eos`
    return this.http.simpleGet<RR.GetMarketplaceEOSRes>(url)
  }

  async getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes> {
    let url = this.getMarketplaceURL('package')
    if (this.useLocal(url)) {
      await pauseFor(2000)
      return {
        categories: ['featured', 'bitcoin', 'lightning', 'data', 'messaging', 'social', 'alt coin'],
      }
    }
    url = `${url}/marketplace/data`
    return this.http.simpleGet<RR.GetMarketplaceDataRes>(url)
  }

  async getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise<RR.GetMarketplacePackagesRes> {
    let url = this.getMarketplaceURL('package')
    if (this.useLocal(url)) {
      await pauseFor(2000)
      return Mock.AvailableList
    }
    url = `${url}/marketplace/packages`
    return this.http.simpleGet<RR.GetMarketplacePackagesRes>(url, params)
  }

  async getReleaseNotes (params: RR.GetReleaseNotesReq): Promise<RR.GetReleaseNotesRes> {
    let url = this.getMarketplaceURL('package')
    if (this.useLocal(url)) {
      await pauseFor(2000)
      return Mock.ReleaseNotes
    }
    url = `${url}/marketplace/release-notes`
    return this.http.simpleGet<RR.GetReleaseNotesRes>(url)
  }

  private useLocal (url: string): boolean {
    return !url || this.config.mocks.marketplace
  }
}
