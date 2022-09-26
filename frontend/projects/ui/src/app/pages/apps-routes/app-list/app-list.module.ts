import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppListPage } from './app-list.page'
import {
  EmverPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { StatusComponentModule } from 'src/app/components/status/status.component.module'
import { LaunchablePipeModule } from 'src/app/pipes/launchable/launchable.module'
import { UiPipeModule } from 'src/app/pipes/ui/ui.module'
import { AppListIconComponent } from './app-list-icon/app-list-icon.component'
import { AppListEmptyComponent } from './app-list-empty/app-list-empty.component'
import { AppListPkgComponent } from './app-list-pkg/app-list-pkg.component'
import { PackageInfoPipe } from './package-info.pipe'

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
    LaunchablePipeModule,
    UiPipeModule,
    IonicModule,
    RouterModule.forChild(routes),
    BadgeMenuComponentModule,
  ],
  declarations: [
    AppListPage,
    AppListIconComponent,
    AppListEmptyComponent,
    AppListPkgComponent,
    PackageInfoPipe,
  ],
})
export class AppListPageModule {}
