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
  signal,
} from '@angular/core'
import { provideAnimations } from '@angular/platform-browser/animations'
import {
  PreloadAllModules,
  provideRouter,
  withDisabledInitialNavigation,
  withPreloading,
} from '@angular/router'
import { WA_LOCATION } from '@ng-web-apis/common'
import initArgon from '@start9labs/argon2'
import {
  I18N_PROVIDERS,
  provideSetupLogsService,
  RELATIVE_URL,
  VERSION,
  WorkspaceConfig,
} from '@start9labs/shared'
import {
  tuiButtonOptionsProvider,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import { provideEventPlugins } from '@taiga-ui/event-plugins'

import { ROUTES } from './app.routes'
import { ApiService } from './services/api.service'
import { LiveApiService } from './services/live-api.service'
import { MockApiService } from './services/mock-api.service'

const {
  useMocks,
  ui: { api },
} = require('../../../../config.json') as WorkspaceConfig

const version = require('../../../../package.json').version

export const APP_CONFIG: ApplicationConfig = {
  providers: [
    provideZoneChangeDetection(),
    provideAnimations(),
    provideEventPlugins(),
    provideRouter(
      ROUTES,
      withDisabledInitialNavigation(),
      withPreloading(PreloadAllModules),
    ),
    I18N_PROVIDERS,
    provideSetupLogsService(ApiService),
    tuiButtonOptionsProvider({ size: 'm' }),
    {
      provide: ApiService,
      useClass: useMocks ? MockApiService : LiveApiService,
    },
    {
      provide: RELATIVE_URL,
      useValue: `/${api.url}/${api.version}`,
    },
    {
      provide: VERSION,
      useValue: version,
    },
    provideHttpClient(withInterceptorsFromDi(), withFetch()),
    provideAppInitializer(() => {
      const origin = inject(WA_LOCATION).origin
      const module_or_path = new URL('/assets/argon2_bg.wasm', origin)

      initArgon({ module_or_path })
    }),
    tuiTextfieldOptionsProvider({ cleaner: signal(false) }),
  ],
}
