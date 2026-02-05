import {
  ApplicationConfig,
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
  tuiDropdownOptionsProvider,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import { tuiDialogOptionsProvider } from '@taiga-ui/core'
import {
  tuiBadgeOptionsProvider,
  tuiCheckboxOptionsProvider,
  tuiRadioOptionsProvider,
  tuiTabsOptionsProvider,
} from '@taiga-ui/kit'
import {
  tuiCardOptionsProvider,
  tuiFormOptionsProvider,
} from '@taiga-ui/layout'

import { routes } from './app.routes'
import {
  provideHttpClient,
  withFetch,
  withInterceptorsFromDi,
} from '@angular/common/http'
import { ApiService } from './services/api/api.service'
import { MockApiService } from './services/api/mock-api.service'
import { LiveApiService } from './services/api/live-api.service'
import { RELATIVE_URL } from './services/http.service'
import { WorkspaceConfig } from './utils/workspace-config'

const { useMocks, api } = require('../../config.json') as WorkspaceConfig

export const appConfig: ApplicationConfig = {
  providers: [
    provideBrowserGlobalErrorListeners(),
    provideZonelessChangeDetection(),
    provideRouter(routes, withRouterConfig({ onSameUrlNavigation: 'reload' })),
    provideTaiga({ scrollbars: 'native' }),
    tuiButtonOptionsProvider({ size: 'm' }),
    tuiBadgeOptionsProvider({ size: 'm' }),
    tuiTabsOptionsProvider({ size: 'm' }),
    tuiRadioOptionsProvider({ size: 's' }),
    tuiCheckboxOptionsProvider({ size: 's' }),
    tuiTextfieldOptionsProvider({ size: signal('m') }),
    tuiFormOptionsProvider({ size: 'm' }),
    tuiCardOptionsProvider({ space: 'compact' }),
    tuiDropdownOptionsProvider({ appearance: 'start-9' }),
    tuiDialogOptionsProvider({ appearance: 'start-9 taiga', size: 's' }),
    tuiSheetDialogOptionsProvider({
      appearance: 'start-9 taiga',
      bar: false,
      offset: 72,
    }),
    {
      provide: TUI_APPEARANCE_OPTIONS,
      useValue: { appearance: 'neutral' },
    },
    {
      provide: ApiService,
      useClass: useMocks ? MockApiService : LiveApiService,
    },
    {
      provide: RELATIVE_URL,
      useValue: `/${api.url}/${api.version}`,
    },
    provideHttpClient(withInterceptorsFromDi(), withFetch()),
  ],
}
