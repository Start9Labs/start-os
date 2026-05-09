import {
  provideHttpClient,
  withFetch,
  withInterceptorsFromDi,
} from '@angular/common/http'
import {
  ApplicationConfig,
  inject,
  provideAppInitializer,
  provideBrowserGlobalErrorListeners,
  provideZonelessChangeDetection,
  signal,
} from '@angular/core'
import { provideRouter, withRouterConfig } from '@angular/router'
import { tuiSheetDialogOptionsProvider } from '@taiga-ui/addon-mobile'
import {
  provideTaiga,
  TUI_APPEARANCE_OPTIONS,
  tuiButtonOptionsProvider,
  tuiCheckboxOptionsProvider,
  tuiDialogOptionsProvider,
  tuiDropdownOptionsProvider,
  tuiIconsProvider,
  tuiRadioOptionsProvider,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import { tuiBadgeOptionsProvider, tuiTabsOptionsProvider } from '@taiga-ui/kit'
import {
  tuiCardOptionsProvider,
  tuiFormOptionsProvider,
} from '@taiga-ui/layout'
import { ICONS } from 'src/app/app.icons'

import { routes } from './app.routes'
import { ApiService } from './services/api/api.service'
import { LiveApiService } from './services/api/live-api.service'
import { MockApiService } from './services/api/mock-api.service'
import { AuthService } from './services/auth.service'
import { RELATIVE_URL } from './services/http.service'
import { GIT_HASH, IS_MOCK, WorkspaceConfig } from './utils/workspace-config'

const { useMocks, api, gitHash } =
  require('../../config.json') as WorkspaceConfig

export const appConfig: ApplicationConfig = {
  providers: [
    provideBrowserGlobalErrorListeners(),
    provideZonelessChangeDetection(),
    provideRouter(routes, withRouterConfig({ onSameUrlNavigation: 'reload' })),
    provideTaiga({ scrollbars: 'native' }),
    tuiIconsProvider(ICONS),
    tuiButtonOptionsProvider({ size: 'm' }),
    tuiBadgeOptionsProvider({ size: 'm' }),
    tuiTabsOptionsProvider({ size: 'm' }),
    tuiRadioOptionsProvider({ size: 's' }),
    tuiCheckboxOptionsProvider({ size: 's' }),
    tuiTextfieldOptionsProvider({ size: signal('m'), cleaner: signal(false) }),
    tuiFormOptionsProvider({ size: 'm' }),
    tuiCardOptionsProvider({ space: 'compact' }),
    tuiDialogOptionsProvider({ size: 's' }),
    tuiSheetDialogOptionsProvider({ bar: false, offset: 72 }),
    {
      provide: TUI_APPEARANCE_OPTIONS,
      useValue: { appearance: 'floating' },
    },
    {
      provide: ApiService,
      useClass: useMocks ? MockApiService : LiveApiService,
    },
    {
      provide: IS_MOCK,
      useValue: useMocks,
    },
    {
      provide: GIT_HASH,
      useValue: gitHash,
    },
    {
      provide: RELATIVE_URL,
      useValue: `/${api.url}/${api.version}`,
    },
    provideHttpClient(withInterceptorsFromDi(), withFetch()),
    provideAppInitializer(() => inject(AuthService).whenReady()),
  ],
}
