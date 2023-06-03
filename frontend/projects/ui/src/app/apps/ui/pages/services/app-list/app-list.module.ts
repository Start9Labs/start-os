import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppListPage } from './app-list.page'
import {
  EmverPipesModule,
  ResponsiveColModule,
  TextSpinnerComponentModule,
  TickerModule,
} from '@start9labs/shared'
import { BadgeMenuComponentModule } from 'src/app/common/badge-menu-button/badge-menu.component.module'
import { WidgetListComponentModule } from 'src/app/common/widget-list/widget-list.component.module'
import { StatusComponentModule } from '../status/status.component.module'
import { UiPipesModule } from '../ui-pipes/ui.module'
import { AppListIconComponent } from './app-list-icon/app-list-icon.component'
import { AppListPkgComponent } from './app-list-pkg/app-list-pkg.component'
import { PackageInfoPipe } from './package-info.pipe'
import { LaunchMenuComponentModule } from '../launch-menu/launch-menu.module'

const routes: Routes = [
  {
    path: '',
    component: AppListPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    StatusComponentModule,
    EmverPipesModule,
    TextSpinnerComponentModule,
    UiPipesModule,
    IonicModule,
    RouterModule.forChild(routes),
    BadgeMenuComponentModule,
    WidgetListComponentModule,
    ResponsiveColModule,
    TickerModule,
    LaunchMenuComponentModule,
  ],
  declarations: [
    AppListPage,
    AppListIconComponent,
    AppListPkgComponent,
    PackageInfoPipe,
  ],
})
export class AppListPageModule {}
