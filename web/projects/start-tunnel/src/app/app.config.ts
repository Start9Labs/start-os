import { tuiDropdownOptionsProvider } from '@taiga-ui/core'
import { provideEventPlugins } from '@taiga-ui/event-plugins'
import { provideAnimations } from '@angular/platform-browser/animations'
import {
  ApplicationConfig,
  provideBrowserGlobalErrorListeners,
  provideZonelessChangeDetection,
} from '@angular/core'
import { provideRouter, withRouterConfig } from '@angular/router'
import { tuiDialogOptionsProvider } from '@taiga-ui/experimental'
import { PatchDB } from 'patch-db-client'
import {
  PATCH_CACHE,
  PatchDbSource,
} from 'src/app/services/patch-db/patch-db-source'
import { routes } from './app.routes'
import { ApiService } from './services/api/api.service'
import { LiveApiService } from './services/api/live-api.service'
import { MockApiService } from './services/api/mock-api.service'
import { RELATIVE_URL, WorkspaceConfig } from '@start9labs/shared'
import {
  provideHttpClient,
  withFetch,
  withInterceptorsFromDi,
} from '@angular/common/http'

const {
  useMocks,
  ui: { api },
} = require('../../../../config.json') as WorkspaceConfig

export const appConfig: ApplicationConfig = {
  providers: [
    provideAnimations(),
    provideBrowserGlobalErrorListeners(),
    provideZonelessChangeDetection(),
    provideRouter(routes, withRouterConfig({ onSameUrlNavigation: 'reload' })),
    provideEventPlugins(),
    tuiDropdownOptionsProvider({ appearance: 'start-9' }),
    tuiDialogOptionsProvider({ appearance: 'start-9 taiga' }),
    {
      provide: PatchDB,
      deps: [PatchDbSource, PATCH_CACHE],
      useClass: PatchDB,
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
