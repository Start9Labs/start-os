import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppShowPage } from './app-show.page'
import {
  EmverPipesModule,
  ResponsiveColModule,
  SharedPipesModule,
} from '@start9labs/shared'
import { StatusComponentModule } from '../status/status.component.module'
import { AppConfigPageModule } from './modals/app-config/app-config.module'
import { AppShowHeaderComponent } from './components/app-show-header/app-show-header.component'
import { AppShowProgressComponent } from './components/app-show-progress/app-show-progress.component'
import { AppShowStatusComponent } from './components/app-show-status/app-show-status.component'
import { AppShowDependenciesComponent } from './components/app-show-dependencies/app-show-dependencies.component'
import { AppShowMenuComponent } from './components/app-show-menu/app-show-menu.component'
import {
  AppShowInterfacesComponent,
  InterfaceInfoPipe,
} from './components/app-show-interfaces/app-show-interfaces.component'
import { AppShowHealthChecksComponent } from './components/app-show-health-checks/app-show-health-checks.component'
import { AppShowAdditionalComponent } from './components/app-show-additional/app-show-additional.component'
import { HealthColorPipe } from './pipes/health-color.pipe'
import { ToDependenciesPipe } from './pipes/to-dependencies.pipe'
import { ToStatusPipe } from './pipes/to-status.pipe'
import { ProgressDataPipe } from './pipes/progress-data.pipe'
import { InsecureWarningComponentModule } from 'src/app/common/insecure-warning/insecure-warning.module'
import { LaunchMenuComponentModule } from '../app-list/app-list-pkg/launch-menu/launch-menu.module'

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
    ToDependenciesPipe,
    ToStatusPipe,
    AppShowHeaderComponent,
    AppShowProgressComponent,
    AppShowStatusComponent,
    AppShowDependenciesComponent,
    AppShowMenuComponent,
    AppShowInterfacesComponent,
    AppShowHealthChecksComponent,
    AppShowAdditionalComponent,
    InterfaceInfoPipe,
  ],
  imports: [
    CommonModule,
    StatusComponentModule,
    IonicModule,
    RouterModule.forChild(routes),
    AppConfigPageModule,
    EmverPipesModule,
    ResponsiveColModule,
    SharedPipesModule,
    InsecureWarningComponentModule,
    LaunchMenuComponentModule,
  ],
})
export class AppShowPageModule {}
