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
import { HttpService } from './services/http.service'
import { GlobalErrorHandler } from './services/global-error-handler.service'

const useMocks = require('../../config.json').useMocks

@NgModule({
  declarations: [AppComponent],
  entryComponents: [],
  imports: [
    HttpClientModule,
    BrowserModule,
    IonicModule.forRoot(),
    AppRoutingModule,
  ],
  providers: [
    { provide: RouteReuseStrategy, useClass: IonicRouteStrategy },
    { 
      provide: ApiService,
      useFactory: (http: HttpService) => {
        if (useMocks) {
          return new MockApiService()
        } else {
          return new LiveApiService(http)
        }
      },
      deps: [HttpService]
    },
    { provide: ErrorHandler, useClass: GlobalErrorHandler},
  ],
  bootstrap: [AppComponent],
})
export class AppModule {}
