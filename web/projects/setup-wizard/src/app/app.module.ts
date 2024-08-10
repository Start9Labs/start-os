import { HttpClientModule } from '@angular/common/http'
import { NgModule } from '@angular/core'
import { BrowserAnimationsModule } from '@angular/platform-browser/animations'
import { PreloadAllModules, RouterModule } from '@angular/router'
import {
  provideSetupLogsService,
  RELATIVE_URL,
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

@NgModule({
  declarations: [AppComponent],
  imports: [
    BrowserAnimationsModule,
    HttpClientModule,
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
  ],
  bootstrap: [AppComponent],
})
export class AppModule {}
