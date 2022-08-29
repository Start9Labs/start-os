import { HttpClientModule } from '@angular/common/http'
import { NgModule } from '@angular/core'
import { BrowserAnimationsModule } from '@angular/platform-browser/animations'
import { IonicModule } from '@ionic/angular'
import { Drivers } from '@ionic/storage'
import { IonicStorageModule } from '@ionic/storage-angular'
import { MonacoEditorModule } from '@materia-ui/ngx-monaco-editor'
import { MarkdownModule, SharedPipesModule } from '@start9labs/shared'

import { AppComponent } from './app.component'
import { AppRoutingModule } from './app-routing.module'
import { OSWelcomePageModule } from './modals/os-welcome/os-welcome.module'
import { GenericInputComponentModule } from './modals/generic-input/generic-input.component.module'
import { MarketplaceModule } from './marketplace.module'
import { PreloaderModule } from './app/preloader/preloader.module'
import { FooterModule } from './app/footer/footer.module'
import { MenuModule } from './app/menu/menu.module'
import { EnterModule } from './app/enter/enter.module'
import { APP_PROVIDERS } from './app.providers'
import { PatchDbModule } from './services/patch-db/patch-db.module'
import { ToastContainerModule } from './components/toast-container/toast-container.module'
import { ConnectionBarComponentModule } from './components/connection-bar/connection-bar.component.module'

@NgModule({
  declarations: [AppComponent],
  imports: [
    HttpClientModule,
    BrowserAnimationsModule,
    IonicModule.forRoot({
      mode: 'md',
    }),
    AppRoutingModule,
    IonicStorageModule.forRoot({
      storeName: '_embassykv',
      dbKey: '_embassykey',
      name: '_embassystorage',
      driverOrder: [Drivers.LocalStorage, Drivers.IndexedDB],
    }),
    MenuModule,
    PreloaderModule,
    FooterModule,
    EnterModule,
    OSWelcomePageModule,
    MarkdownModule,
    GenericInputComponentModule,
    MonacoEditorModule,
    SharedPipesModule,
    MarketplaceModule,
    PatchDbModule,
    ToastContainerModule,
    ConnectionBarComponentModule,
  ],
  providers: APP_PROVIDERS,
  bootstrap: [AppComponent],
})
export class AppModule {}
