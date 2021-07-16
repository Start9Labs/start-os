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

  abstract getLatestVersion (params: RR.GetLatestVersionReq): Promise<RR.GetLatestVersionRes>

  getMarketplaceURL (type: 'eos' | 'package', defaultToTor = false): string {
    const packageMarketplace = this.patch.data['server-info']['package-marketplace']
    if (defaultToTor && !packageMarketplace) {
      return this.config.start9Marketplace.tor
    }
    const eosMarketplace = this.patch.data['server-info']['eos-marketplace'] || this.config.start9Marketplace.clearnet
    if (type === 'eos') {
      return eosMarketplace
    } else {
      return packageMarketplace || eosMarketplace
    }
  }
}