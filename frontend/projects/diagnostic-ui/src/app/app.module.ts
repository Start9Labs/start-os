import { ErrorHandler, NgModule } from '@angular/core'
import { BrowserModule } from '@angular/platform-browser'
import { RouteReuseStrategy } from '@angular/router'
import { IonicModule, IonicRouteStrategy } from '@ionic/angular'
import { AppComponent } from './app.component'
import { AppRoutingModule } from './app-routing.module'
import { HttpClientModule } from '@angular/common/http'
import { ApiService } from './services/api/api.service'
import { MockApiService } from './services/api/mock-api.service'
import { LiveApiService } from './services/api/live-api.service'
import { GlobalErrorHandler } from './services/global-error-handler.service'
import { WorkspaceConfig } from '@start9labs/shared'

const { useMocks } = require('../../../../config.json') as WorkspaceConfig

@NgModule({
  declarations: [AppComponent],
  imports: [
    HttpClientModule,
    BrowserModule,
    IonicModule.forRoot({
      mode: 'md',
    }),
    AppRoutingModule,
  ],
  providers: [
    { provide: RouteReuseStrategy, useClass: IonicRouteStrategy },
    {
      provide: ApiService,
      useClass: useMocks ? MockApiService : LiveApiService,
    },
    { provide: ErrorHandler, useClass: GlobalErrorHandler },
  ],
  bootstrap: [AppComponent],
})
export class AppModule {}
