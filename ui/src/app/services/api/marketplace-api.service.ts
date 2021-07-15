import { RR } from './api-types'
// import { DataModel } from 'src/app/services/patch-db/data-model'
import { DataModel } from '../../../../src/app/services/patch-db/data-model'

export abstract class MarketplaceApiService {
  abstract getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes>

  abstract getEos (params: RR.GetMarketplaceEOSReq): Promise<RR.GetMarketplaceEOSRes>

  abstract getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise<RR.GetMarketplacePackagesRes>

  abstract getReleaseNotes (params: RR.GetReleaseNotesReq): Promise<RR.GetReleaseNotesRes>

}

export function getMarketURL (eosOrPackage: 'eos' | 'package', data: DataModel): string {
  const eosMarketplace = data['server-info']['eos-marketplace']
  if (eosOrPackage === 'eos') {
    return eosMarketplace
  } else {
    const packageMarketplace = data['server-info']['package-marketplace']
    return packageMarketplace || eosMarketplace
  }
}