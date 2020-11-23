import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'

import { IonicModule } from '@ionic/angular'

import { DependencyListComponentModule } from 'src/app/components/dependency-list/dependency-list.component.module'
import { AppInstalledListPage } from './app-installed-list.page'
import { StatusComponentModule } from 'src/app/components/status/status.component.module'
import { SharingModule } from 'src/app/modules/sharing.module'
import { AppBackupPageModule } from 'src/app/modals/app-backup/app-backup.module'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'

const routes: Routes = [
  {
    path: '',
    component: AppInstalledListPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    StatusComponentModule,
    DependencyListComponentModule,
    AppBackupPageModule,
    SharingModule,
    IonicModule,
    RouterModule.forChild(routes),
    PwaBackComponentModule,
    BadgeMenuComponentModule,
  ],
  declarations: [AppInstalledListPage],
})
export class AppInstalledListPageModule { }
