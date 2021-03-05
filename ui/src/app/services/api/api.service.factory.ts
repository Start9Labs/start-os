import { HttpService } from '../http.service'
import { AppModel } from '../../models/app-model'
import { MockApiService } from './mock-api.service'
import { LiveApiService } from './live-api.service'
import { ServerModel } from 'src/app/models/server-model'
import { ConfigService } from '../config.service'

export function ApiServiceFactory (config: ConfigService, http: HttpService, appModel: AppModel, serverModel: ServerModel) {
  if (config.api.useMocks) {
    return new MockApiService(appModel, serverModel, config)
  } else {
    return new LiveApiService(http, appModel, serverModel, config)
  }
}
