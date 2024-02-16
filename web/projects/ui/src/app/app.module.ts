import { HttpClientModule } from '@angular/common/http'
import { NgModule } from '@angular/core'
import { BrowserAnimationsModule } from '@angular/platform-browser/animations'
import { ServiceWorkerModule } from '@angular/service-worker'
import { IonicModule } from '@ionic/angular'
import { MonacoEditorModule } from '@materia-ui/ngx-monaco-editor'
import {
  DarkThemeModule,
  EnterModule,
  LightThemeModule,
  LoadingModule,
  MarkdownModule,
  ResponsiveColViewportDirective,
  SharedPipesModule,
} from '@start9labs/shared'
import {
  TuiAlertModule,
  TuiDialogModule,
  TuiModeModule,
  TuiRootModule,
  TuiThemeNightModule,
} from '@taiga-ui/core'
import { environment } from '../environments/environment'
import { AppComponent } from './app.component'
import { APP_PROVIDERS } from './app.providers'
import { ConnectionBarComponentModule } from './app/connection-bar/connection-bar.component.module'
import { FooterModule } from './app/footer/footer.module'
import { MenuModule } from './app/menu/menu.module'
import { PreloaderModule } from './app/preloader/preloader.module'
import { SidebarHostComponent } from './app/sidebar-host.component'
import { OSWelcomePageModule } from './common/os-welcome/os-welcome.module'
import { QRComponentModule } from './common/qr/qr.module'
import { ToastContainerModule } from './common/toast-container/toast-container.module'
import { RoutingModule } from './routing.module'

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
    ToastContainerModule,
    ConnectionBarComponentModule,
    TuiRootModule,
    TuiDialogModule,
    TuiAlertModule,
    TuiModeModule,
    TuiThemeNightModule,
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
    SidebarHostComponent,
  ],
  providers: APP_PROVIDERS,
  bootstrap: [AppComponent],
})
export class AppModule {}
