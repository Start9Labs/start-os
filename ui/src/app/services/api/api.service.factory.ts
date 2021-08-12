import { HttpService } from '../http.service'
import { MockApiService } from './embassy-mock-api.service'
import { LiveApiService } from './embassy-live-api.service'
import { ConfigService } from '../config.service'

export function ApiServiceFactory (config: ConfigService, http: HttpService) {
  if (config.mocks.enabled) {
    return new MockApiService(http)
  } else {
    return new LiveApiService(http)
  }
}
