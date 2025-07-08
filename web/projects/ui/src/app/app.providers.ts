import { inject, provideAppInitializer } from '@angular/core'
import { UntypedFormBuilder } from '@angular/forms'
import { Router } from '@angular/router'
import { WA_LOCATION } from '@ng-web-apis/common'
import initArgon from '@start9labs/argon2'
import {
  AbstractCategoryService,
  FilterPackagesPipe,
} from '@start9labs/marketplace'
import {
  I18N_PROVIDERS,
  I18N_STORAGE,
  i18nService,
  RELATIVE_URL,
  VERSION,
  WorkspaceConfig,
} from '@start9labs/shared'
import {
  TUI_DATE_FORMAT,
  TUI_DIALOGS_CLOSE,
  tuiAlertOptionsProvider,
  tuiButtonOptionsProvider,
  tuiDropdownOptionsProvider,
  tuiNumberFormatProvider,
} from '@taiga-ui/core'
import { provideEventPlugins } from '@taiga-ui/event-plugins'
import {
  TUI_DATE_TIME_VALUE_TRANSFORMER,
  TUI_DATE_VALUE_TRANSFORMER,
} from '@taiga-ui/kit'
import { tuiTextfieldOptionsProvider } from '@taiga-ui/legacy'
import { PatchDB } from 'patch-db-client'
import { filter, of, pairwise } from 'rxjs'
import { ConfigService } from 'src/app/services/config.service'
import {
  PATCH_CACHE,
  PatchDbSource,
} from 'src/app/services/patch-db/patch-db-source'
import { StateService } from 'src/app/services/state.service'
import { ApiService } from './services/api/embassy-api.service'
import { LiveApiService } from './services/api/embassy-live-api.service'
import { MockApiService } from './services/api/embassy-mock-api.service'
import { AuthService } from './services/auth.service'
import { CategoryService } from './services/category.service'
import { ClientStorageService } from './services/client-storage.service'
import { DateTransformerService } from './services/date-transformer.service'
import { DatetimeTransformerService } from './services/datetime-transformer.service'
import { StorageService } from './services/storage.service'
import { FilterUpdatesPipe } from './routes/portal/routes/updates/filter-updates.pipe'

const {
  useMocks,
  ui: { api },
} = require('../../../../config.json') as WorkspaceConfig

export const APP_PROVIDERS = [
  provideEventPlugins(),
  I18N_PROVIDERS,
  FilterPackagesPipe,
  FilterUpdatesPipe,
  UntypedFormBuilder,
  tuiNumberFormatProvider({ decimalSeparator: '.', thousandSeparator: '' }),
  tuiButtonOptionsProvider({ size: 'm' }),
  tuiTextfieldOptionsProvider({ hintOnDisabled: true }),
  tuiDropdownOptionsProvider({ appearance: 'start-os' }),
  tuiAlertOptionsProvider({
    autoClose: appearance => (appearance === 'negative' ? 0 : 3000),
  }),
  {
    provide: TUI_DATE_FORMAT,
    useValue: of({
      mode: 'MDY',
      separator: '/',
    }),
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
    provide: PatchDB,
    deps: [PatchDbSource, PATCH_CACHE],
    useClass: PatchDB,
  },
  provideAppInitializer(() => {
    const i18n = inject(i18nService)
    const origin = inject(WA_LOCATION).origin
    const module_or_path = new URL('/assets/argon2_bg.wasm', origin)

    initArgon({ module_or_path })
    inject(StorageService).migrate036()
    inject(AuthService).init()
    inject(ClientStorageService).init()
    inject(Router).initialNavigation()
    i18n.setLanguage(i18n.language || 'english')
  }),
  {
    provide: RELATIVE_URL,
    useValue: `/${api.url}/${api.version}`,
  },
  {
    provide: AbstractCategoryService,
    useClass: CategoryService,
  },
  {
    provide: TUI_DIALOGS_CLOSE,
    useFactory: () =>
      inject(StateService).pipe(
        pairwise(),
        filter(
          ([prev, curr]) =>
            prev === 'running' && (curr === 'error' || curr === 'initializing'),
        ),
      ),
  },
  {
    provide: I18N_STORAGE,
    useFactory: () => {
      const api = inject(ApiService)

      return (language: string) => api.setDbValue(['language'], language)
    },
  },
  {
    provide: VERSION,
    useFactory: () => inject(ConfigService).version,
  },
]
