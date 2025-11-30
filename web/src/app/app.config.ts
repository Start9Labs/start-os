import {
  ApplicationConfig,
  DOCUMENT,
  effect,
  inject,
  provideAppInitializer,
  provideBrowserGlobalErrorListeners,
  provideZonelessChangeDetection,
  signal,
} from '@angular/core'
import { provideAnimations } from '@angular/platform-browser/animations'
import { provideRouter, withRouterConfig } from '@angular/router'
import {
  TUI_APPEARANCE_OPTIONS,
  TUI_DARK_MODE,
  tuiButtonOptionsProvider,
  tuiDropdownOptionsProvider,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import { provideEventPlugins } from '@taiga-ui/event-plugins'
import { tuiDialogOptionsProvider } from '@taiga-ui/experimental'
import {
  tuiBadgeOptionsProvider,
  tuiCheckboxOptionsProvider,
  tuiRadioOptionsProvider,
  tuiTabsOptionsProvider,
} from '@taiga-ui/kit'
import { tuiFormOptionsProvider } from '@taiga-ui/layout'

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
    provideAnimations(),
    provideBrowserGlobalErrorListeners(),
    provideZonelessChangeDetection(),
    provideRouter(routes, withRouterConfig({ onSameUrlNavigation: 'reload' })),
    provideEventPlugins(),
    tuiButtonOptionsProvider({ size: 'm' }),
    tuiBadgeOptionsProvider({ size: 'm' }),
    tuiTabsOptionsProvider({ size: 'm' }),
    tuiRadioOptionsProvider({ size: 's' }),
    tuiCheckboxOptionsProvider({ size: 's' }),
    tuiTextfieldOptionsProvider({ size: signal('m') }),
    tuiFormOptionsProvider({ size: 'm' }),
    tuiDropdownOptionsProvider({ appearance: 'start-9' }),
    tuiDialogOptionsProvider({ appearance: 'start-9 taiga' }),
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
    // TODO: Remove in Taiga UI 5
    provideAppInitializer(() => {
      const doc = inject(DOCUMENT)
      const mode = inject(TUI_DARK_MODE)

      effect(() => {
        if (mode()) {
          doc.body.setAttribute('tuiTheme', 'dark')
        } else {
          doc.body.removeAttribute('tuiTheme')
        }
      })
    }),
  ],
}
