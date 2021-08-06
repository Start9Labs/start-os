import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { MarketplaceShowPage } from './marketplace-show.page'
import { SharingModule } from 'src/app/modules/sharing.module'
import { StatusComponentModule } from 'src/app/components/status/status.component.module'
import { InstallWizardComponentModule } from 'src/app/components/install-wizard/install-wizard.component.module'

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
    RouterModule.forChild(routes),
    SharingModule,
    InstallWizardComponentModule,
  ],
  declarations: [MarketplaceShowPage],
})
export class MarketplaceShowPageModule { }
