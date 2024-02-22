import { HttpClientModule } from '@angular/common/http'
import { NgModule } from '@angular/core'
import { BrowserAnimationsModule } from '@angular/platform-browser/animations'
import {
  DriveComponent,
  LoadingModule,
  RELATIVE_URL,
  UnitConversionPipesModule,
  WorkspaceConfig,
} from '@start9labs/shared'
import { TuiDialogModule, TuiRootModule } from '@taiga-ui/core'
import {
  TuiButtonModule,
  TuiCardModule,
  TuiCellModule,
  TuiIconModule,
  TuiSurfaceModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'
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
    TuiRootModule,
    TuiDialogModule,
    LoadingModule,
    DriveComponent,
    TuiButtonModule,
    TuiCardModule,
    TuiCellModule,
    TuiIconModule,
    TuiSurfaceModule,
    TuiTitleModule,
    UnitConversionPipesModule,
  ],
  providers: [
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
