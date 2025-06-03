import { provideHttpClient, withInterceptorsFromDi } from '@angular/common/http'
import { inject, NgModule, provideAppInitializer } from '@angular/core'
import { BrowserAnimationsModule } from '@angular/platform-browser/animations'
import { PreloadAllModules, RouterModule } from '@angular/router'
import { WA_LOCATION } from '@ng-web-apis/common'
import initArgon from '@start9labs/argon2'
import {
  provideSetupLogsService,
  RELATIVE_URL,
  VERSION,
  WorkspaceConfig,
} from '@start9labs/shared'
import { tuiButtonOptionsProvider, TuiRoot } from '@taiga-ui/core'
import { NG_EVENT_PLUGINS } from '@taiga-ui/event-plugins'
import { ApiService } from 'src/app/services/api.service'
import { LiveApiService } from 'src/app/services/live-api.service'
import { MockApiService } from 'src/app/services/mock-api.service'
import { AppComponent } from './app.component'
import { ROUTES } from './app.routes'

const {
  useMocks,
  ui: { api },
} = require('../../../../config.json') as WorkspaceConfig

const version = require('../../../../package.json').version

@NgModule({
  declarations: [AppComponent],
  imports: [
    BrowserAnimationsModule,
    RouterModule.forRoot(ROUTES, {
      preloadingStrategy: PreloadAllModules,
      initialNavigation: 'disabled',
    }),
    TuiRoot,
  ],
  providers: [
    NG_EVENT_PLUGINS,
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
    provideHttpClient(withInterceptorsFromDi()),
    provideAppInitializer(() => {
      const origin = inject(WA_LOCATION).origin
      const module_or_path = new URL('/assets/argon2_bg.wasm', origin)

      initArgon({ module_or_path })
    }),
  ],
  bootstrap: [AppComponent],
})
export class AppModule {}
