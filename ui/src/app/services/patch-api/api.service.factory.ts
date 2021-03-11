import { HttpService } from '../http.service'
import { MockApiService } from './mock-api.service'
import { LiveApiService } from './live-api.service'
import { ConfigService } from '../config.service'

export function ApiServiceFactory (config: ConfigService, http: HttpService) {
  if (config.api.mocks) {
    return new MockApiService(config)
  } else {
    return new LiveApiService(http, config)
  }
}
