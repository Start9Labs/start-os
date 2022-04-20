import { DOCUMENT } from '@angular/common'
import { APP_INITIALIZER, ErrorHandler, Provider } from '@angular/core'
import { FormBuilder } from '@angular/forms'
import { Router, RouteReuseStrategy } from '@angular/router'
import { IonicRouteStrategy, IonNav } from '@ionic/angular'
import { Storage } from '@ionic/storage-angular'
import { WorkspaceConfig } from '@start9labs/shared'

import { ApiService } from './services/api/embassy-api.service'
import { MockApiService } from './services/api/embassy-mock-api.service'
import { LiveApiService } from './services/api/embassy-live-api.service'
import {
  PATCH_SOURCE,
  mockSourceFactory,
  realSourceFactory,
} from './services/patch-db/patch-db.factory'
import { ConfigService } from './services/config.service'
import { GlobalErrorHandler } from './services/global-error-handler.service'
import { AuthService } from './services/auth.service'
import { LocalStorageService } from './services/local-storage.service'

const { useMocks } = require('../../../../config.json') as WorkspaceConfig

export const APP_PROVIDERS: Provider[] = [
  FormBuilder,
  IonNav,
  {
    provide: RouteReuseStrategy,
    useClass: IonicRouteStrategy,
  },
  {
    provide: ApiService,
    useClass: useMocks ? MockApiService : LiveApiService,
  },
  {
    provide: PATCH_SOURCE,
    deps: [ApiService, ConfigService, DOCUMENT],
    useFactory: useMocks ? mockSourceFactory : realSourceFactory,
  },
  {
    provide: ErrorHandler,
    useClass: GlobalErrorHandler,
  },
  {
    provide: APP_INITIALIZER,
    deps: [Storage, AuthService, LocalStorageService, Router],
    useFactory: appInitializer,
    multi: true,
  },
]

export function appInitializer(
  storage: Storage,
  auth: AuthService,
  localStorage: LocalStorageService,
  router: Router,
): () => Promise<void> {
  return async () => {
    await storage.create()
    await auth.init()
    await localStorage.init()

    router.initialNavigation()
  }
}
