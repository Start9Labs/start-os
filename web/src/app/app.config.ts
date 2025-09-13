import { provideEventPlugins } from '@taiga-ui/event-plugins'
import { provideAnimations } from '@angular/platform-browser/animations'
import {
  ApplicationConfig,
  provideBrowserGlobalErrorListeners,
  provideZonelessChangeDetection,
} from '@angular/core'
import { provideRouter, withRouterConfig } from '@angular/router'

import { routes } from './app.routes'

export const appConfig: ApplicationConfig = {
  providers: [
    provideAnimations(),
    provideBrowserGlobalErrorListeners(),
    provideZonelessChangeDetection(),
    provideRouter(routes, withRouterConfig({ onSameUrlNavigation: 'reload' })),
    provideEventPlugins(),
  ],
}
