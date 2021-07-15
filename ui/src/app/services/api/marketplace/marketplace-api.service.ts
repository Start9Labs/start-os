import { RR } from '../api.types'
import { ConfigService } from '../../config.service'
import { PatchDbService } from '../../patch-db/patch-db.service'

export abstract class MarketplaceApiService {

  constructor (
    readonly config: ConfigService,
    readonly patch: PatchDbService,
  ) { }

  abstract getEos (params: RR.GetMarketplaceEOSReq): Promise<RR.GetMarketplaceEOSRes>

  abstract getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes>

  abstract getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise<RR.GetMarketplacePackagesRes>

  abstract getReleaseNotes (params: RR.GetReleaseNotesReq): Promise<RR.GetReleaseNotesRes>

  getMarketplaceURL (type: 'eos' | 'package'): string {
    const eosMarketplace = this.patch.data['server-info']['eos-marketplace'] || this.config.start9Marketplace.clearnet
    if (type === 'eos') {
      return eosMarketplace
    } else {
      const packageMarketplace = this.patch.data['server-info']['package-marketplace']
      return packageMarketplace || eosMarketplace
    }
  }
}