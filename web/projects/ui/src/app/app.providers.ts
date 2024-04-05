import { APP_INITIALIZER, Provider } from '@angular/core'
import { UntypedFormBuilder } from '@angular/forms'
import { Router } from '@angular/router'
import {
  AbstractCategoryService,
  AbstractMarketplaceService,
  FilterPackagesPipe,
} from '@start9labs/marketplace'
import { RELATIVE_URL, THEME, WorkspaceConfig } from '@start9labs/shared'
import { TUI_DATE_FORMAT, TUI_DATE_SEPARATOR } from '@taiga-ui/cdk'
import {
  tuiDropdownOptionsProvider,
  tuiNumberFormatProvider,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import { tuiButtonOptionsProvider } from '@taiga-ui/experimental'
import {
  TUI_DATE_TIME_VALUE_TRANSFORMER,
  TUI_DATE_VALUE_TRANSFORMER,
} from '@taiga-ui/kit'
import { PATCH_DB_PROVIDERS } from 'src/app/services/patch-db/patch-db.providers'
import { ApiService } from './services/api/embassy-api.service'
import { LiveApiService } from './services/api/embassy-live-api.service'
import { MockApiService } from './services/api/embassy-mock-api.service'
import { AuthService } from './services/auth.service'
import { CategoryService } from './services/category.service'
import { ClientStorageService } from './services/client-storage.service'
import { DateTransformerService } from './services/date-transformer.service'
import { DatetimeTransformerService } from './services/datetime-transformer.service'
import { MarketplaceService } from './services/marketplace.service'
import { ThemeSwitcherService } from './services/theme-switcher.service'

const {
  useMocks,
  ui: { api },
} = require('../../../../config.json') as WorkspaceConfig

export const APP_PROVIDERS: Provider[] = [
  PATCH_DB_PROVIDERS,
  FilterPackagesPipe,
  UntypedFormBuilder,
  tuiNumberFormatProvider({ decimalSeparator: '.', thousandSeparator: '' }),
  tuiButtonOptionsProvider({ size: 'm' }),
  tuiTextfieldOptionsProvider({ hintOnDisabled: true }),
  tuiDropdownOptionsProvider({ appearance: 'start-os' }),
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
  {
    provide: AbstractMarketplaceService,
    useClass: MarketplaceService,
  },
  {
    provide: AbstractCategoryService,
    useClass: CategoryService,
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
