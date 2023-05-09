import {
  TuiAlertModule,
  TuiDialogModule,
  TuiModeModule,
  TuiRootModule,
  TuiThemeNightModule,
} from '@taiga-ui/core'
import { HttpClientModule } from '@angular/common/http'
import { NgModule } from '@angular/core'
import { BrowserAnimationsModule } from '@angular/platform-browser/animations'
import { IonicModule } from '@ionic/angular'
import { MonacoEditorModule } from '@materia-ui/ngx-monaco-editor'
import {
  MarkdownModule,
  DarkThemeModule,
  ResponsiveColModule,
  SharedPipesModule,
  LightThemeModule,
} from '@start9labs/shared'

import { AppComponent } from './app.component'
import { AppRoutingModule } from './app-routing.module'
import { OSWelcomePageModule } from './modals/os-welcome/os-welcome.module'
import { GenericInputComponentModule } from './modals/generic-input/generic-input.component.module'
import { GenericFormPageModule } from './modals/generic-form/generic-form.module'
import { MarketplaceModule } from './marketplace.module'
import { PreloaderModule } from './app/preloader/preloader.module'
import { FooterModule } from './app/footer/footer.module'
import { MenuModule } from './app/menu/menu.module'
import { EnterModule } from './app/enter/enter.module'
import { APP_PROVIDERS } from './app.providers'
import { PatchDbModule } from './services/patch-db/patch-db.module'
import { ToastContainerModule } from './components/toast-container/toast-container.module'
import { ConnectionBarComponentModule } from './components/connection-bar/connection-bar.component.module'
import { WidgetsPageModule } from './pages/widgets/widgets.module'
import { ServiceWorkerModule } from '@angular/service-worker'
import { environment } from '../environments/environment'
import { LoadingModule } from './modals/loading/loading.module'
import { FormPageModule } from './modals/form/form.module'

@NgModule({
  declarations: [AppComponent],
  imports: [
    HttpClientModule,
    BrowserAnimationsModule,
    IonicModule.forRoot({
      mode: 'md',
    }),
    AppRoutingModule,
    MenuModule,
    PreloaderModule,
    FooterModule,
    EnterModule,
    OSWelcomePageModule,
    MarkdownModule,
    GenericInputComponentModule,
    GenericFormPageModule,
    MonacoEditorModule,
    SharedPipesModule,
    MarketplaceModule,
    PatchDbModule,
    ToastContainerModule,
    ConnectionBarComponentModule,
    TuiRootModule,
    TuiDialogModule,
    TuiAlertModule,
    TuiModeModule,
    TuiThemeNightModule,
    WidgetsPageModule,
    ResponsiveColModule,
    DarkThemeModule,
    LightThemeModule,
    ServiceWorkerModule.register('ngsw-worker.js', {
      enabled: environment.useServiceWorker,
      // Register the ServiceWorker as soon as the application is stable
      // or after 30 seconds (whichever comes first).
      registrationStrategy: 'registerWhenStable:30000',
    }),
    LoadingModule,
    FormPageModule,
  ],
  providers: APP_PROVIDERS,
  bootstrap: [AppComponent],
})
export class AppModule {}
