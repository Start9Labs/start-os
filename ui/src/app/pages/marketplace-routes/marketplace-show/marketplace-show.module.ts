import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { MarketplaceShowPage } from './marketplace-show.page'
import { SharingModule } from 'src/app/modules/sharing.module'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { StatusComponentModule } from 'src/app/components/status/status.component.module'
import { InstallWizardComponentModule } from 'src/app/components/install-wizard/install-wizard.component.module'
import { TextSpinnerComponentModule } from 'src/app/components/text-spinner/text-spinner.component.module'

const routes: Routes = [
  {
    path: '',
    component: MarketplaceShowPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    StatusComponentModule,
    TextSpinnerComponentModule,
    RouterModule.forChild(routes),
    SharingModule,
    PwaBackComponentModule,
    BadgeMenuComponentModule,
    InstallWizardComponentModule,
  ],
  declarations: [MarketplaceShowPage],
})
export class MarketplaceShowPageModule { }
