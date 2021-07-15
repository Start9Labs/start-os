import { Injectable } from '@angular/core'
import { pauseFor } from '../../util/misc.util'
import { RR } from './api-types'
import { Mock } from './mock-app-fixures'
import { HttpService } from '../http.service'
import { MarketplaceApiService } from './marketplace-api.service'
import { getMarketURL  } from './marketplace-api.service'
import { PatchDbModel } from '../patch-db/patch-db.service'

@Injectable()
export class MarketplaceMockApiService extends MarketplaceApiService {
  welcomeAck = false

  constructor (
    private readonly http: HttpService,
    private readonly patch: PatchDbModel,
  ) { super() }

  // marketplace

  async getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes> {
    const registryURL = getMarketURL('package', this.patch.data)
    if (!registryURL) {
      await pauseFor(2000)
      return {
        categories: ['featured', 'bitcoin', 'lightning', 'data', 'messaging', 'social', 'alt coin'],
      }
    }
    const url = `${registryURL}/marketplace/data`
    return this.http.simpleGet<RR.GetMarketplaceDataRes>(url)
  }

  async getEos (params: RR.GetMarketplaceEOSReq): Promise<RR.GetMarketplaceEOSRes> {
    const registryURL = getMarketURL('eos', this.patch.data)
    if (!registryURL) {
      await pauseFor(2000)
      return Mock.MarketplaceEos
    }
    const url = `${registryURL}/sys/version/eos`
    return this.http.simpleGet<RR.GetMarketplaceEOSRes>(url)
  }

  async getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise<RR.GetMarketplacePackagesRes> {
    const registryURL = getMarketURL('package', this.patch.data)
    if (!registryURL) {
      await pauseFor(2000)
      return Mock.AvailableList
    }
    const url = `${registryURL}/marketplace/packages`
    return this.http.simpleGet<RR.GetMarketplacePackagesRes>(url, params)
  }

  async getReleaseNotes (params: RR.GetReleaseNotesReq): Promise<RR.GetReleaseNotesRes> {
    const registryURL = getMarketURL('package', this.patch.data)
    if (!registryURL) {
      await pauseFor(2000)
      return Mock.ReleaseNotes
    }
    const url = `${registryURL}/marketplace/release-notes`
    return this.http.simpleGet<RR.GetReleaseNotesRes>(url)
  }
}
