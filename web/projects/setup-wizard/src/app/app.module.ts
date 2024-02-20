import { HttpClientModule } from '@angular/common/http'
import { NgModule } from '@angular/core'
import { BrowserAnimationsModule } from '@angular/platform-browser/animations'
import { PreloadAllModules, RouterModule } from '@angular/router'
import {
  LoadingModule,
  provideSetupLogsService,
  provideSetupService,
  RELATIVE_URL,
  WorkspaceConfig,
} from '@start9labs/shared'
import {
  TuiAlertModule,
  TuiDialogModule,
  TuiModeModule,
  TuiRootModule,
  TuiThemeNightModule,
} from '@taiga-ui/core'
import { tuiButtonOptionsProvider } from '@taiga-ui/experimental'
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
    LoadingModule,
    TuiRootModule,
    TuiDialogModule,
    TuiAlertModule,
    TuiModeModule,
    TuiThemeNightModule,
  ],
  providers: [
    provideSetupService(ApiService),
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
