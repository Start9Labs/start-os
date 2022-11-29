import { NgModule } from '@angular/core'
import { BrowserAnimationsModule } from '@angular/platform-browser/animations'
import { RouteReuseStrategy } from '@angular/router'
import { IonicModule, IonicRouteStrategy } from '@ionic/angular'
import { TuiRootModule } from '@taiga-ui/core'
import { AppComponent } from './app.component'
import { AppRoutingModule } from './app-routing.module'
import { HttpClientModule } from '@angular/common/http'
import { ApiService } from './services/api/api.service'
import { MockApiService } from './services/api/mock-api.service'
import { LiveApiService } from './services/api/live-api.service'
import { RELATIVE_URL, WorkspaceConfig } from '@start9labs/shared'

const {
  useMocks,
  ui: { api },
} = require('../../../../config.json') as WorkspaceConfig

@NgModule({
  declarations: [AppComponent],
  imports: [
    HttpClientModule,
    BrowserAnimationsModule,
    IonicModule.forRoot({
      mode: 'md',
    }),
    AppRoutingModule,
    TuiRootModule,
  ],
  providers: [
    { provide: RouteReuseStrategy, useClass: IonicRouteStrategy },
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
