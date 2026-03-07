import {
  provideHttpClient,
  withFetch,
  withInterceptorsFromDi,
} from '@angular/common/http'
import {
  ApplicationConfig,
  inject,
  provideAppInitializer,
  provideZoneChangeDetection,
} from '@angular/core'
import { UntypedFormBuilder } from '@angular/forms'
import { provideAnimations } from '@angular/platform-browser/animations'
import {
  ActivationStart,
  PreloadAllModules,
  provideRouter,
  Router,
  withComponentInputBinding,
  withDisabledInitialNavigation,
  withInMemoryScrolling,
  withPreloading,
  withRouterConfig,
} from '@angular/router'
import { provideServiceWorker } from '@angular/service-worker'
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
  Languages,
  RELATIVE_URL,
  VERSION,
  WorkspaceConfig,
} from '@start9labs/shared'
import { tuiObfuscateOptionsProvider } from '@taiga-ui/cdk'
import {
  TUI_DATE_FORMAT,
  TUI_DIALOGS_CLOSE,
  TUI_MEDIA,
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
import { PatchDB } from 'patch-db-client'
import { filter, identity, merge, of, pairwise } from 'rxjs'
import { FilterUpdatesPipe } from 'src/app/routes/portal/routes/updates/filter-updates.pipe'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { LiveApiService } from 'src/app/services/api/embassy-live-api.service'
import { MockApiService } from 'src/app/services/api/embassy-mock-api.service'
import { AuthService } from 'src/app/services/auth.service'
import { CategoryService } from 'src/app/services/category.service'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { ConfigService } from 'src/app/services/config.service'
import { DateTransformerService } from 'src/app/services/date-transformer.service'
import { DatetimeTransformerService } from 'src/app/services/datetime-transformer.service'
import {
  PATCH_CACHE,
  PatchDbSource,
} from 'src/app/services/patch-db/patch-db-source'
import { StateService } from 'src/app/services/state.service'
import { StorageService } from 'src/app/services/storage.service'
import { environment } from 'src/environments/environment'

import { ROUTES } from './app.routes'

const {
  useMocks,
  ui: { api },
} = require('../../../../config.json') as WorkspaceConfig

export const APP_CONFIG: ApplicationConfig = {
  providers: [
    provideZoneChangeDetection(),
    provideAnimations(),
    provideEventPlugins(),
    provideHttpClient(withInterceptorsFromDi(), withFetch()),
    provideRouter(
      ROUTES,
      withDisabledInitialNavigation(),
      withComponentInputBinding(),
      withPreloading(PreloadAllModules),
      withInMemoryScrolling({ scrollPositionRestoration: 'enabled' }),
      withRouterConfig({ paramsInheritanceStrategy: 'always' }),
    ),
    provideServiceWorker('ngsw-worker.js', {
      enabled: environment.useServiceWorker,
      // Register the ServiceWorker as soon as the application is stable
      // or after 30 seconds (whichever comes first).
      registrationStrategy: 'registerWhenStable:30000',
    }),
    I18N_PROVIDERS,
    FilterPackagesPipe,
    FilterUpdatesPipe,
    UntypedFormBuilder,
    tuiNumberFormatProvider({ decimalSeparator: '.', thousandSeparator: '' }),
    tuiButtonOptionsProvider({ size: 'm' }),
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
        merge(
          inject(Router).events.pipe(filter(e => e instanceof ActivationStart)),
          inject(StateService).pipe(
            pairwise(),
            filter(
              ([prev, curr]) =>
                prev === 'running' &&
                (curr === 'error' || curr === 'initializing'),
            ),
          ),
        ),
    },
    {
      provide: I18N_STORAGE,
      useFactory: () => {
        const api = inject(ApiService)

        return (language: Languages) => api.setLanguage({ language })
      },
    },
    {
      provide: VERSION,
      useFactory: () => inject(ConfigService).version,
    },
    tuiObfuscateOptionsProvider({
      recipes: {
        mask: ({ length }) => '•'.repeat(length),
        none: identity,
      },
    }),
    {
      provide: TUI_MEDIA,
      useValue: {
        mobile: 1000,
        desktopSmall: 1280,
        desktopLarge: Infinity,
      },
    },
  ],
}
