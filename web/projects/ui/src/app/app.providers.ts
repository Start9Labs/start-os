import { APP_INITIALIZER, Provider } from '@angular/core'
import { UntypedFormBuilder } from '@angular/forms'
import { Router, RouteReuseStrategy } from '@angular/router'
import { IonicRouteStrategy, IonNav } from '@ionic/angular'
import { RELATIVE_URL, THEME, WorkspaceConfig } from '@start9labs/shared'
import { TUI_ICONS_PATH } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import {
  PATCH_CACHE,
  PatchDbSource,
} from 'src/app/services/patch-db/patch-db-source'
import { ApiService } from './services/api/embassy-api.service'
import { MockApiService } from './services/api/embassy-mock-api.service'
import { LiveApiService } from './services/api/embassy-live-api.service'
import { AuthService } from './services/auth.service'
import { ClientStorageService } from './services/client-storage.service'
import { FilterPackagesPipe } from '../../../marketplace/src/pipes/filter-packages.pipe'
import { ThemeSwitcherService } from './services/theme-switcher.service'
import { StorageService } from './services/storage.service'

const {
  useMocks,
  ui: { api },
} = require('../../../../config.json') as WorkspaceConfig

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
    provide: PatchDB,
    deps: [PatchDbSource, PATCH_CACHE],
    useClass: PatchDB,
  },
  {
    provide: APP_INITIALIZER,
    deps: [StorageService, AuthService, ClientStorageService, Router],
    useFactory: appInitializer,
    multi: true,
  },
  {
    provide: RELATIVE_URL,
    useValue: `/${api.url}/${api.version}`,
  },
  {
    provide: THEME,
    useExisting: ThemeSwitcherService,
  },
  {
    provide: TUI_ICONS_PATH,
    useValue: (name: string) => `/assets/taiga-ui/icons/${name}.svg#${name}`,
  },
]

export function appInitializer(
  storage: StorageService,
  auth: AuthService,
  localStorage: ClientStorageService,
  router: Router,
): () => void {
  return () => {
    storage.migrate036()
    auth.init()
    localStorage.init()
    router.initialNavigation()
  }
}
