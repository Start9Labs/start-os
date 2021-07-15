import { HttpService } from '../http.service'
import { MockApiService } from './embassy/embassy-mock-api.service'
import { LiveApiService } from './embassy/embassy-live-api.service'
import { ConfigService } from '../config.service'
import { PatchDbService } from '../patch-db/patch-db.service'
import { MarketplaceLiveApiService } from './marketplace/marketplace-live-api.service'
import { MarketplaceMockApiService } from './marketplace/marketplace-mock-api.service'

export function ApiServiceFactory (config: ConfigService, http: HttpService) {
  if (config.mocks.enabled) {
    return new MockApiService(http, config)
  } else {
    return new LiveApiService(http, config)
  }
}

export function MarketplaceApiServiceFactory (config: ConfigService, http: HttpService, patch: PatchDbService) {
  if (config.mocks.enabled) {
    return new MarketplaceMockApiService(http, config, patch)
  } else {
    return new MarketplaceLiveApiService(http, config, patch)
  }
}