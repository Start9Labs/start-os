import { NgModule } from '@angular/core'
import { BrowserModule } from '@angular/platform-browser'
import { RouteReuseStrategy } from '@angular/router'
import { HttpClientModule } from '@angular/common/http'
import { ApiService } from './services/api/api.service'
import { MockApiService } from './services/api/mock-api.service'
import { LiveApiService } from './services/api/live-api.service'
import { HttpService } from './services/api/http.service'
import { IonicModule, IonicRouteStrategy, iosTransitionAnimation } from '@ionic/angular'
import { AppComponent } from './app.component'
import { AppRoutingModule } from './app-routing.module'
const useMocks = require('../../config.json').useMocks as boolean

@NgModule({
  declarations: [AppComponent],
  entryComponents: [],
  imports: [
    BrowserModule,
    IonicModule.forRoot(),
    AppRoutingModule,
    HttpClientModule,
    IonicModule.forRoot({  
      navAnimation: iosTransitionAnimation,
    }), 
  ],
  providers: [
    { provide: RouteReuseStrategy, useClass: IonicRouteStrategy },
    { 
      provide: ApiService ,
      useFactory: (http: HttpService) => {
        if (useMocks) {
          return new MockApiService()
        } else {
          return new LiveApiService(http)
        }
      },
      deps: [HttpService]
    },
  ],
  bootstrap: [AppComponent],
})
export class AppModule {}
