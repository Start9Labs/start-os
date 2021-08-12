import { Injectable } from '@angular/core'
import { pauseFor } from '../../../util/misc.util'
import { RR } from '../api.types'
import { Mock } from '../api.fixures'
import { MarketplaceApiService } from './marketplace-api.service'
import { PatchDbService } from '../../patch-db/patch-db.service'
import { ConfigService } from '../../config.service'

@Injectable()
export class MarketplaceMockApiService extends MarketplaceApiService {
  constructor (
    config: ConfigService,
    patch: PatchDbService,
  ) {
    super(config, patch)
  }

  // marketplace

  async getEos (params: RR.GetMarketplaceEOSReq): Promise<RR.GetMarketplaceEOSRes> {
    await pauseFor(2000)
    return Mock.MarketplaceEos
  }

  async getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes> {
    await pauseFor(2000)
    return {
      categories: ['featured', 'bitcoin', 'lightning', 'data', 'messaging', 'social', 'alt coin'],
    }
  }

  async getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise<RR.GetMarketplacePackagesRes> {
    await pauseFor(2000)
    return Mock.MarketplacePkgsList
  }

  async getReleaseNotes (params: RR.GetReleaseNotesReq): Promise<RR.GetReleaseNotesRes> {
    await pauseFor(2000)
    return Mock.ReleaseNotes
  }

  async getLatestVersion (params: RR.GetLatestVersionReq): Promise<RR.GetLatestVersionRes> {
    await pauseFor(2000)
    return params.ids.reduce((obj, id) => {
      obj[id] = this.patch.getData()['package-data']?.[id]?.manifest.version.replace('0', '1')
      return obj
    }, { })
  }
}
