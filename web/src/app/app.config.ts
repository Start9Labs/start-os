import {
  ApplicationConfig,
  provideBrowserGlobalErrorListeners,
  provideZonelessChangeDetection,
  signal,
} from '@angular/core'
import { provideAnimations } from '@angular/platform-browser/animations'
import { provideRouter, withRouterConfig } from '@angular/router'
import {
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
  ],
}
