import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppShowPage } from './app-show.page'
import { StatusComponentModule } from 'src/app/components/status/status.component.module'
import { SharingModule } from 'src/app/modules/sharing.module'
import { InstallWizardComponentModule } from 'src/app/components/install-wizard/install-wizard.component.module'
import { AppConfigPageModule } from 'src/app/modals/app-config/app-config.module'

const routes: Routes = [
  {
    path: '',
    component: AppShowPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    StatusComponentModule,
    IonicModule,
    RouterModule.forChild(routes),
    InstallWizardComponentModule,
    AppConfigPageModule,
    SharingModule,
  ],
  declarations: [AppShowPage],
})
export class AppShowPageModule { }

