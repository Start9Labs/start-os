import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core'
import { BrowserModule } from '@angular/platform-browser'
import { RouteReuseStrategy } from '@angular/router'
import { IonicModule, IonicRouteStrategy } from '@ionic/angular'
import { IonicStorageModule } from '@ionic/storage'
import { HttpClientModule } from '@angular/common/http'
import { AppComponent } from './app.component'
import { AppRoutingModule } from './app-routing.module'
import { ApiService } from './services/api/api.service'
import { ApiServiceFactory } from './services/api/api.service.factory'
import { PatchDbModelFactory } from './models/patch-db/patch-db-model.factory'
import { AppModel } from './models/app-model'
import { HttpService } from './services/http.service'
import { ServerModel } from './models/server-model'
import { ConfigService } from './services/config.service'
import { QRCodeModule } from 'angularx-qrcode'
import { APP_CONFIG_COMPONENT_MAPPING } from './modals/app-config-injectable/modal-injectable-token'
import { appConfigComponents } from './modals/app-config-injectable/modal-injectable-value';
import { OSWelcomePageModule } from './modals/os-welcome/os-welcome.module'
import { PatchDbModel } from './models/patch-db/patch-db-model'
import { LocalStorageBootstrap } from './models/patch-db/local-storage-bootstrap'

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
    OSWelcomePageModule,
  ],
  providers: [
    Storage,
    { provide: RouteReuseStrategy, useClass: IonicRouteStrategy },
    { provide: ApiService , useFactory: ApiServiceFactory, deps: [ConfigService, HttpService, AppModel, ServerModel] },
    { provide: PatchDbModel, useFactory: PatchDbModelFactory, deps: [ConfigService, LocalStorageBootstrap] },
    { provide: APP_CONFIG_COMPONENT_MAPPING, useValue: appConfigComponents },
  ],
  bootstrap: [AppComponent],
  schemas: [ CUSTOM_ELEMENTS_SCHEMA ],
})
export class AppModule { }



