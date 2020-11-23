import { NgModule, CUSTOM_ELEMENTS_SCHEMA, Type, Injectable } from '@angular/core'
import { BrowserModule, HammerGestureConfig, HammerModule, HAMMER_GESTURE_CONFIG } from '@angular/platform-browser'
import { RouteReuseStrategy } from '@angular/router'
import { IonicModule, IonicRouteStrategy } from '@ionic/angular'
import { IonicStorageModule } from '@ionic/storage'
import { HttpClientModule } from '@angular/common/http'
import { AppComponent } from './app.component'
import { AppRoutingModule } from './app-routing.module'
import { ApiService } from './services/api/api.service'
import { ApiServiceFactory } from './services/api/api.service.factory'
import { AppModel } from './models/app-model'
import { HttpService } from './services/http.service'
import { ServerModel } from './models/server-model'
import { ConfigService } from './services/config.service'
import { QRCodeModule } from 'angularx-qrcode'
import { APP_CONFIG_COMPONENT_MAPPING } from './modals/app-config-injectable/modal-injectable-token'
import { appConfigComponents } from './modals/app-config-injectable/modal-injectable-value'

@Injectable()
export class HammerConfig extends HammerGestureConfig {
  // overrides = {
    // // pan: { enable: false },
    // pinch: { enable: false },
    // rotate: { enable: false },
    // swipe: {direction: Hammer.DIRECTION_ALL},
    // // swipe: { enable: false },
    // tap: { enable: false },
  // }
  overrides = {
    swipe: { direction: Hammer.DIRECTION_DOWN },
    pan: { direction: Hammer.DIRECTION_DOWN },
    // pinch: { enable: false },
    // rotate: { enable: false },
    // swipe: { enable: false },
    // tap: { enable: false },
  }
}
@NgModule({
  declarations: [AppComponent],
  entryComponents: [],
  imports: [
    HttpClientModule,
    BrowserModule,
    IonicModule.forRoot(),
    AppRoutingModule,
    IonicStorageModule.forRoot(),
    QRCodeModule,
    HammerModule,
  ],
  providers: [
    { provide: RouteReuseStrategy, useClass: IonicRouteStrategy },
    { provide: ApiService, useFactory: ApiServiceFactory, deps: [ConfigService, HttpService, AppModel, ServerModel] },
    { provide: APP_CONFIG_COMPONENT_MAPPING, useValue: appConfigComponents },
    { provide: HAMMER_GESTURE_CONFIG, useClass: HammerConfig },
  ],
  bootstrap: [AppComponent],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
})
export class AppModule { }



