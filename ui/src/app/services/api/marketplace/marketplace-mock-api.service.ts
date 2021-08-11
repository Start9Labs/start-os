import { Injectable } from '@angular/core'
import { pauseFor } from '../../../util/misc.util'
import { RR } from '../api.types'
import { Mock } from '../api.fixures'
import { HttpService, Method } from '../../http.service'
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
    const url = getMarketplaceURL('eos')
    if (this.useLocal(url)) {
      await pauseFor(2000)
      return Mock.MarketplaceEos
    }
    return this.http.httpRequest<RR.GetMarketplaceEOSRes>({
      method: Method.GET,
      url: `${url}/eos`,
      params,
      withCredentials: false,
    })
  }

  async getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes> {
    const url = getMarketplaceURL('package')
    if (this.useLocal(url)) {
      await pauseFor(2000)
      return {
        categories: ['featured', 'bitcoin', 'lightning', 'data', 'messaging', 'social', 'alt coin'],
      }
    }
    return this.http.httpRequest<RR.GetMarketplaceDataRes>({
      method: Method.GET,
      url: `${url}/data`,
      params,
      withCredentials: false,
    })
  }

  async getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise<RR.GetMarketplacePackagesRes> {
    const url = getMarketplaceURL('package')
    if (this.useLocal(url)) {
      await pauseFor(2000)
      return Mock.MarketplacePkgsList
    }
    return this.http.httpRequest<RR.GetMarketplacePackagesRes>({
      method: Method.GET,
      url: `${url}/packages`,
      params: {
        ...params,
        ids: JSON.stringify(params.ids),
      },
      withCredentials: false,
    })
  }

  async getReleaseNotes (params: RR.GetReleaseNotesReq): Promise<RR.GetReleaseNotesRes> {
    const url = getMarketplaceURL('package')
    if (this.useLocal(url)) {
      await pauseFor(2000)
      return Mock.ReleaseNotes
    }
    return this.http.httpRequest<RR.GetReleaseNotesRes>({
      method: Method.GET,
      url: `${url}/release-notes`,
      params,
      withCredentials: false,
    })
  }

  async getLatestVersion (params: RR.GetLatestVersionReq): Promise<RR.GetLatestVersionRes> {
    const url = getMarketplaceURL('package')
    if (this.useLocal(url)) {
      await pauseFor(2000)
      return params.ids.reduce((obj, id) => {
        obj[id] = this.patch.getData()['package-data']?.[id]?.manifest.version.replace('0', '1')
        return obj
      }, { })
    }

    return this.http.httpRequest<RR.GetLatestVersionRes>({
      method: Method.GET,
      url: `${url}/latest-version`,
      params,
      withCredentials: false,
    })
  }

  private useLocal (url: string): boolean {
    return !url || this.config.mocks.marketplace
  }
}


function getMarketplaceURL (type: 'eos' | 'package'): string {
  const packageMarketplace = this.server['package-marketplace']
  const eosMarketplace = this.server['eos-marketplace'] || this.config.start9Marketplace.clearnet
  if (type === 'eos') {
    return eosMarketplace
  } else {
    return packageMarketplace || eosMarketplace
  }
}