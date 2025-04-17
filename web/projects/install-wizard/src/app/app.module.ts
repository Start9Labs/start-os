import { HttpClientModule } from '@angular/common/http'
import { NgModule } from '@angular/core'
import { BrowserAnimationsModule } from '@angular/platform-browser/animations'
import {
  DriveComponent,
  i18nPipe,
  RELATIVE_URL,
  WorkspaceConfig,
} from '@start9labs/shared'
import {
  TuiButton,
  TuiIcon,
  TuiRoot,
  TuiSurface,
  TuiTitle,
} from '@taiga-ui/core'
import { NG_EVENT_PLUGINS } from '@taiga-ui/event-plugins'
import { TuiCardLarge, TuiCell } from '@taiga-ui/layout'
import { ApiService } from 'src/app/services/api.service'
import { LiveApiService } from 'src/app/services/live-api.service'
import { MockApiService } from 'src/app/services/mock-api.service'
import { AppComponent } from './app.component'

const {
  useMocks,
  ui: { api },
} = require('../../../../config.json') as WorkspaceConfig

@NgModule({
  declarations: [AppComponent],
  imports: [
    HttpClientModule,
    BrowserAnimationsModule,
    TuiRoot,
    DriveComponent,
    TuiButton,
    TuiCardLarge,
    TuiCell,
    TuiIcon,
    TuiSurface,
    TuiTitle,
    i18nPipe,
  ],
  providers: [
    NG_EVENT_PLUGINS,
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
