import { HttpService } from '../http.service'
import { MockApiService } from './embassy/embassy-mock-api.service'
import { LiveApiService } from './embassy/embassy-live-api.service'
import { ConfigService } from '../config.service'
import { PatchDbService } from '../patch-db/patch-db.service'
import { MarketplaceLiveApiService } from './marketplace/marketplace-live-api.service'
import { MarketplaceMockApiService } from './marketplace/marketplace-mock-api.service'
import { ApiService } from './embassy/embassy-api.service'

export function ApiServiceFactory (config: ConfigService, http: HttpService) {
  if (config.mocks.enabled) {
    return new MockApiService(http, config)
  } else {
    return new LiveApiService(http, config)
  }
}

export function MarketplaceApiServiceFactory (config: ConfigService, patch: PatchDbService, apiService: ApiService) {
  if (config.mocks.enabled) {
    return new MarketplaceMockApiService(config, patch)
  } else {
    return new MarketplaceLiveApiService(apiService, config, patch)
  }
}