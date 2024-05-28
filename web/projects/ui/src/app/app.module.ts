import { HttpClientModule } from '@angular/common/http'
import { NgModule } from '@angular/core'
import { BrowserAnimationsModule } from '@angular/platform-browser/animations'
import { ServiceWorkerModule } from '@angular/service-worker'
import { LoadingModule } from '@start9labs/shared'
import { TuiSheetDialogModule } from '@taiga-ui/addon-mobile'
import {
  TuiAlertModule,
  TuiDialogModule,
  TuiModeModule,
  TuiRootModule,
  TuiThemeNightModule,
} from '@taiga-ui/core'
import { SidebarHostComponent } from 'src/app/components/sidebar-host.component'
import { SvgDefinitionsComponent } from 'src/app/components/svg-definitions.component'
import { ToastContainerComponent } from 'src/app/components/toast-container.component'
import { environment } from '../environments/environment'
import { AppComponent } from './app.component'
import { APP_PROVIDERS } from './app.providers'
import { RoutingModule } from './routing.module'

@NgModule({
  declarations: [AppComponent],
  imports: [
    HttpClientModule,
    BrowserAnimationsModule,
    RoutingModule,
    ToastContainerComponent,
    TuiRootModule,
    TuiDialogModule,
    TuiSheetDialogModule,
    TuiAlertModule,
    TuiModeModule,
    TuiThemeNightModule,
    ServiceWorkerModule.register('ngsw-worker.js', {
      enabled: environment.useServiceWorker,
      // Register the ServiceWorker as soon as the application is stable
      // or after 30 seconds (whichever comes first).
      registrationStrategy: 'registerWhenStable:30000',
    }),
    LoadingModule,
    SidebarHostComponent,
    SvgDefinitionsComponent,
  ],
  providers: APP_PROVIDERS,
  bootstrap: [AppComponent],
})
export class AppModule {}
