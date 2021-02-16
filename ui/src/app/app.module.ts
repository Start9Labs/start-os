import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core'
import { BrowserModule } from '@angular/platform-browser'
import { RouteReuseStrategy } from '@angular/router'
import { IonicModule, IonicRouteStrategy } from '@ionic/angular'
import { Drivers } from '@ionic/storage'
import { IonicStorageModule } from '@ionic/storage-angular'
import { HttpClientModule } from '@angular/common/http'
import { AppComponent } from './app.component'
import { AppRoutingModule } from './app-routing.module'
import { ApiService } from './services/api/api.service'
import { ApiServiceFactory } from './services/api/api.service.factory'
import { PatchDbModelFactory } from './models/patch-db/patch-db-model.factory'
import { HttpService } from './services/http.service'
import { ConfigService } from './services/config.service'
import { QRCodeModule } from 'angularx-qrcode'
import { APP_CONFIG_COMPONENT_MAPPING } from './modals/app-config-injectable/modal-injectable-token'
import { appConfigComponents } from './modals/app-config-injectable/modal-injectable-value'
import { OSWelcomePageModule } from './modals/os-welcome/os-welcome.module'
import { PatchDbModel } from './models/patch-db/patch-db-model'
import { LocalStorageBootstrap } from './models/patch-db/local-storage-bootstrap'
import { SharingModule } from './modules/sharing.module'

@NgModule({
  declarations: [AppComponent],
  entryComponents: [],
  imports: [
    HttpClientModule,
    BrowserModule,
    IonicModule.forRoot(),
    AppRoutingModule,
    IonicStorageModule.forRoot({
      storeName: '_embassykv',
      dbKey: '_embassykey',
      name: '_embassystorage',
      driverOrder: [Drivers.LocalStorage, Drivers.IndexedDB],
    }),
    QRCodeModule,
    OSWelcomePageModule,
    SharingModule,
  ],
  providers: [
    Storage,
    { provide: RouteReuseStrategy, useClass: IonicRouteStrategy },
    { provide: ApiService , useFactory: ApiServiceFactory, deps: [ConfigService, HttpService] },
    { provide: PatchDbModel, useFactory: PatchDbModelFactory, deps: [ConfigService, LocalStorageBootstrap, ApiService] },
    { provide: APP_CONFIG_COMPONENT_MAPPING, useValue: appConfigComponents },
  ],
  bootstrap: [AppComponent],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
})
export class AppModule { }



