import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppInstalledShowPage } from './app-installed-show.page'
import { StatusComponentModule } from 'src/app/components/status/status.component.module'
import { SharingModule } from 'src/app/modules/sharing.module'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { InstallWizardComponentModule } from 'src/app/components/install-wizard/install-wizard.component.module'
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
    SharingModule,
    IonicModule,
    RouterModule.forChild(routes),
    PwaBackComponentModule,
    BadgeMenuComponentModule,
    InstallWizardComponentModule,
    InformationPopoverComponentModule,
  ],
  declarations: [AppInstalledShowPage],
})
export class AppInstalledShowPageModule { }

