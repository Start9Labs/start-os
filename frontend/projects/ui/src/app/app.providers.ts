import { APP_INITIALIZER, Provider } from '@angular/core'
import { UntypedFormBuilder } from '@angular/forms'
import { Router, RouteReuseStrategy } from '@angular/router'
import { IonicRouteStrategy, IonNav } from '@ionic/angular'
import { TUI_DATE_FORMAT, TUI_DATE_SEPARATOR } from '@taiga-ui/cdk'
import {
  tuiButtonOptionsProvider,
  tuiNumberFormatProvider,
} from '@taiga-ui/core'
import {
  TUI_DATE_TIME_VALUE_TRANSFORMER,
  TUI_DATE_VALUE_TRANSFORMER,
} from '@taiga-ui/kit'
import { RELATIVE_URL, THEME, WorkspaceConfig } from '@start9labs/shared'
import { ApiService } from './services/api/embassy-api.service'
import { MockApiService } from './services/api/embassy-mock-api.service'
import { LiveApiService } from './services/api/embassy-live-api.service'
import { AuthService } from './services/auth.service'
import { ClientStorageService } from './services/client-storage.service'
import { FilterPackagesPipe } from '../../../marketplace/src/pipes/filter-packages.pipe'
import { ThemeSwitcherService } from './services/theme-switcher.service'
import { DateTransformerService } from './services/date-transformer.service'
import { DatetimeTransformerService } from './services/datetime-transformer.service'

const {
  useMocks,
  ui: { api },
} = require('../../../../config.json') as WorkspaceConfig

export const APP_PROVIDERS: Provider[] = [
  FilterPackagesPipe,
  UntypedFormBuilder,
  IonNav,
  tuiNumberFormatProvider({ decimalSeparator: '.', thousandSeparator: '' }),
  tuiButtonOptionsProvider({ size: 'm' }),
  {
    provide: TUI_DATE_FORMAT,
    useValue: 'MDY',
  },
  {
    provide: TUI_DATE_SEPARATOR,
    useValue: '/',
  },
  {
    provide: TUI_DATE_VALUE_TRANSFORMER,
    useClass: DateTransformerService,
  },
  {
    provide: TUI_DATE_TIME_VALUE_TRANSFORMER,
    useClass: DatetimeTransformerService,
  },
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
    deps: [AuthService, ClientStorageService, Router],
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
]

export function appInitializer(
  auth: AuthService,
  localStorage: ClientStorageService,
  router: Router,
): () => void {
  return () => {
    auth.init()
    localStorage.init()
    router.initialNavigation()
  }
}
