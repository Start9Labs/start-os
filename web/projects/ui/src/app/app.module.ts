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
  DarkThemeModule,
  SharedPipesModule,
  LightThemeModule,
  LoadingModule,
  ResponsiveColViewportDirective,
  EnterModule,
  MarkdownModule,
} from '@start9labs/shared'

import { AppComponent } from './app.component'
import { RoutingModule } from './routing.module'
import { OSWelcomePageModule } from './common/os-welcome/os-welcome.module'
import { QRComponentModule } from './common/qr/qr.module'
import { PreloaderModule } from './app/preloader/preloader.module'
import { FooterModule } from './app/footer/footer.module'
import { MenuModule } from './app/menu/menu.module'
import { APP_PROVIDERS } from './app.providers'
import { PatchDbModule } from './services/patch-db/patch-db.module'
import { ToastContainerModule } from './common/toast-container/toast-container.module'
import { ConnectionBarComponentModule } from './app/connection-bar/connection-bar.component.module'
import { WidgetsPageModule } from 'src/app/apps/ui/pages/widgets/widgets.module'
import { ServiceWorkerModule } from '@angular/service-worker'
import { environment } from '../environments/environment'

@NgModule({
  declarations: [AppComponent],
  imports: [
    HttpClientModule,
    BrowserAnimationsModule,
    IonicModule.forRoot({
      mode: 'md',
    }),
    RoutingModule,
    MenuModule,
    PreloaderModule,
    FooterModule,
    EnterModule,
    OSWelcomePageModule,
    MarkdownModule,
    MonacoEditorModule,
    SharedPipesModule,
    PatchDbModule,
    ToastContainerModule,
    ConnectionBarComponentModule,
    TuiRootModule,
    TuiDialogModule,
    TuiAlertModule,
    TuiModeModule,
    TuiThemeNightModule,
    WidgetsPageModule,
    ResponsiveColViewportDirective,
    DarkThemeModule,
    LightThemeModule,
    ServiceWorkerModule.register('ngsw-worker.js', {
      enabled: environment.useServiceWorker,
      // Register the ServiceWorker as soon as the application is stable
      // or after 30 seconds (whichever comes first).
      registrationStrategy: 'registerWhenStable:30000',
    }),
    LoadingModule,
    QRComponentModule,
  ],
  providers: APP_PROVIDERS,
  bootstrap: [AppComponent],
})
export class AppModule {}
