import { APP_INITIALIZER, inject, Provider } from '@angular/core'
import { UntypedFormBuilder } from '@angular/forms'
import { Router } from '@angular/router'
import {
  AbstractCategoryService,
  FilterPackagesPipe,
} from '@start9labs/marketplace'
import { RELATIVE_URL, WorkspaceConfig } from '@start9labs/shared'
import {
  TUI_DATE_FORMAT,
  TUI_DIALOGS_CLOSE,
  tuiButtonOptionsProvider,
  tuiDropdownOptionsProvider,
  tuiNumberFormatProvider,
} from '@taiga-ui/core'
import { NG_EVENT_PLUGINS } from '@taiga-ui/event-plugins'
import {
  TUI_DATE_TIME_VALUE_TRANSFORMER,
  TUI_DATE_VALUE_TRANSFORMER,
} from '@taiga-ui/kit'
import { tuiTextfieldOptionsProvider } from '@taiga-ui/legacy'
import { PatchDB } from 'patch-db-client'
import { filter, of, pairwise } from 'rxjs'
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

const {
  useMocks,
  ui: { api },
} = require('../../../../config.json') as WorkspaceConfig

export const APP_PROVIDERS: Provider[] = [
  NG_EVENT_PLUGINS,
  FilterPackagesPipe,
  UntypedFormBuilder,
  tuiNumberFormatProvider({ decimalSeparator: '.', thousandSeparator: '' }),
  tuiButtonOptionsProvider({ size: 'm' }),
  tuiTextfieldOptionsProvider({ hintOnDisabled: true }),
  tuiDropdownOptionsProvider({ appearance: 'start-os' }),
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
