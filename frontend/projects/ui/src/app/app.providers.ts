import { APP_INITIALIZER, Provider } from '@angular/core'
import { UntypedFormBuilder } from '@angular/forms'
import { Router, RouteReuseStrategy } from '@angular/router'
import { IonicRouteStrategy, IonNav } from '@ionic/angular'
import { Storage } from '@ionic/storage-angular'
import { WorkspaceConfig } from '@start9labs/shared'
import { ApiService } from './services/api/embassy-api.service'
import { MockApiService } from './services/api/embassy-mock-api.service'
import { LiveApiService } from './services/api/embassy-live-api.service'
import { AuthService } from './services/auth.service'
import { LocalStorageService } from './services/local-storage.service'
import { FilterPackagesPipe } from '../../../marketplace/src/pipes/filter-packages.pipe'

const { useMocks } = require('../../../../config.json') as WorkspaceConfig

export const APP_PROVIDERS: Provider[] = [
  FilterPackagesPipe,
  UntypedFormBuilder,
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
