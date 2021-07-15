import { HttpService } from '../http.service'
import { MockApiService } from './mock-api.service'
import { LiveApiService } from './live-api.service'
import { ConfigService } from '../config.service'
import { PatchDbModel } from '../patch-db/patch-db.service'
import { MarketplaceLiveApiService } from './marketplace-live-api.service'
import { MarketplaceMockApiService } from './marketplace-mock-api.service'

export function ApiServiceFactory (config: ConfigService, http: HttpService) {
  if (config.mocks.enabled) {
    return new MockApiService(config, http)
  } else {
    return new LiveApiService(config, http)
  }
}

export function MarketplaceApiServiceFactory (config: ConfigService, http: HttpService, patch: PatchDbModel) {
  if (config.mocks.enabled) {
    return new MarketplaceMockApiService(http, patch)
  } else {
    return new MarketplaceLiveApiService(http, patch)
  }
}