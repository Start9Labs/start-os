import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppShowPage } from './app-show.page'
import { EmverPipesModule } from '@start9labs/shared'
import { StatusComponentModule } from 'src/app/components/status/status.component.module'
import { AppConfigPageModule } from 'src/app/modals/app-config/app-config.module'
import { LaunchablePipeModule } from 'src/app/pipes/launchable/launchable.module'
import { UiPipeModule } from 'src/app/pipes/ui/ui.module'
import { AppShowHeaderComponent } from './components/app-show-header/app-show-header.component'
import { AppShowProgressComponent } from './components/app-show-progress/app-show-progress.component'
import { AppShowStatusComponent } from './components/app-show-status/app-show-status.component'
import { AppShowDependenciesComponent } from './components/app-show-dependencies/app-show-dependencies.component'
import { AppShowMenuComponent } from './components/app-show-menu/app-show-menu.component'
import { AppShowHealthChecksComponent } from './components/app-show-health-checks/app-show-health-checks.component'
import { HealthColorPipe } from './pipes/health-color.pipe'
import { ToHealthChecksPipe } from './pipes/to-health-checks.pipe'
import { ToButtonsPipe } from './pipes/to-buttons.pipe'
import { ToDependenciesPipe } from './pipes/to-dependencies.pipe'
import { ToStatusPipe } from './pipes/to-status.pipe'
import { ProgressDataPipe } from './pipes/progress-data.pipe'

const routes: Routes = [
  {
    path: '',
    component: AppShowPage,
  },
]

@NgModule({
  declarations: [
    AppShowPage,
    HealthColorPipe,
    ProgressDataPipe,
    ToHealthChecksPipe,
    ToButtonsPipe,
    ToDependenciesPipe,
    ToStatusPipe,
    AppShowHeaderComponent,
    AppShowProgressComponent,
    AppShowStatusComponent,
    AppShowDependenciesComponent,
    AppShowMenuComponent,
    AppShowHealthChecksComponent,
  ],
  imports: [
    CommonModule,
    StatusComponentModule,
    IonicModule,
    RouterModule.forChild(routes),
    AppConfigPageModule,
    EmverPipesModule,
    LaunchablePipeModule,
    UiPipeModule,
  ],
})
export class AppShowPageModule {}
