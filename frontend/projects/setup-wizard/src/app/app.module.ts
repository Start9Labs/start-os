import { NgModule } from '@angular/core'
import { BrowserModule } from '@angular/platform-browser'
import { RouteReuseStrategy } from '@angular/router'
import { HttpClientModule } from '@angular/common/http'
import { ApiService } from './services/api/api.service'
import { MockApiService } from './services/api/mock-api.service'
import { LiveApiService } from './services/api/live-api.service'
import {
  IonicModule,
  IonicRouteStrategy,
  iosTransitionAnimation,
} from '@ionic/angular'
import { AppComponent } from './app.component'
import { AppRoutingModule } from './app-routing.module'
import { SuccessPageModule } from './pages/success/success.module'
import { HomePageModule } from './pages/home/home.module'
import { LoadingPageModule } from './pages/loading/loading.module'
import { RecoverPageModule } from './pages/recover/recover.module'
import { TransferPageModule } from './pages/transfer/transfer.module'
import { RELATIVE_URL, WorkspaceConfig } from '@start9labs/shared'

const {
  useMocks,
  ui: { api },
} = require('../../../../config.json') as WorkspaceConfig

@NgModule({
  declarations: [AppComponent],
  imports: [
    BrowserModule,
    IonicModule.forRoot({
      mode: 'md',
      navAnimation: iosTransitionAnimation,
    }),
    AppRoutingModule,
    HttpClientModule,
    SuccessPageModule,
    HomePageModule,
    LoadingPageModule,
    RecoverPageModule,
    TransferPageModule,
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
