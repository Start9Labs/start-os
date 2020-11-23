import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'

import { IonicModule } from '@ionic/angular'

import { DependencyListComponentModule } from 'src/app/components/dependency-list/dependency-list.component.module'
import { AppInstalledShowPage } from './app-installed-show.page'
import { StatusComponentModule } from 'src/app/components/status/status.component.module'
import { SharingModule } from 'src/app/modules/sharing.module'
import { AppBackupPageModule } from 'src/app/modals/app-backup/app-backup.module'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { InstallWizardComponentModule } from 'src/app/components/install-wizard/install-wizard.component.module'
import { ErrorMessageComponentModule } from 'src/app/components/error-message/error-message.component.module'
import { InformationPopoverComponentModule } from 'src/app/components/information-popover/information-popover.component.module'

const routes: Routes = [
  {
    path: '',
    component: AppInstalledShowPage,
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
    InstallWizardComponentModule,
    ErrorMessageComponentModule,
    InformationPopoverComponentModule,
  ],
  declarations: [AppInstalledShowPage],
})
export class AppInstalledShowPageModule { }
