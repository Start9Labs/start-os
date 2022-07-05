import { Bootstrapper, DBCache } from 'patch-db-client'
import { APP_INITIALIZER, ErrorHandler, Provider } from '@angular/core'
import { FormBuilder } from '@angular/forms'
import { Router, RouteReuseStrategy } from '@angular/router'
import { IonicRouteStrategy, IonNav } from '@ionic/angular'
import { Storage } from '@ionic/storage-angular'
import { WorkspaceConfig } from '@start9labs/shared'

import { ApiService } from './services/api/embassy-api.service'
import { MockApiService } from './services/api/embassy-mock-api.service'
import { LiveApiService } from './services/api/embassy-live-api.service'
import { BOOTSTRAPPER, PATCH_CACHE } from './services/patch-db/patch-db.factory'
import { GlobalErrorHandler } from './services/global-error-handler.service'
import { AuthService } from './services/auth.service'
import { LocalStorageService } from './services/local-storage.service'
import { DataModel } from './services/patch-db/data-model'
import { FilterPackagesPipe } from '../../../marketplace/src/pipes/filter-packages.pipe'

const { useMocks } = require('../../../../config.json') as WorkspaceConfig

export const APP_PROVIDERS: Provider[] = [
  FilterPackagesPipe,
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
    provide: ErrorHandler,
    useClass: GlobalErrorHandler,
  },
  {
    provide: APP_INITIALIZER,
    deps: [
      Storage,
      AuthService,
      LocalStorageService,
      Router,
      BOOTSTRAPPER,
      PATCH_CACHE,
    ],
    useFactory: appInitializer,
    multi: true,
  },
]

export function appInitializer(
  storage: Storage,
  auth: AuthService,
  localStorage: LocalStorageService,
  router: Router,
  bootstrapper: Bootstrapper<DataModel>,
  cache: DBCache<DataModel>,
): () => Promise<void> {
  return async () => {
    await storage.create()
    await auth.init()
    await localStorage.init()

    const localCache = await bootstrapper.init()

    cache.sequence = localCache.sequence
    cache.data = localCache.data

    router.initialNavigation()
  }
}
