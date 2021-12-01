import { HttpService } from '../http.service'
import { MockApiService } from './embassy-mock-api.service'
import { LiveApiService } from './embassy-live-api.service'
import { ConfigService } from '../config.service'
import { LocalStorageBootstrap } from '../patch-db/local-storage-bootstrap'

export function ApiServiceFactory (config: ConfigService, http: HttpService, bootstrapper: LocalStorageBootstrap) {
  if (config.mocks.enabled) {
    return new MockApiService(bootstrapper)
  } else {
    return new LiveApiService(http)
  }
}
