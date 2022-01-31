import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { MarketplaceListPage } from './marketplace-list.page'
import { SharingModule } from '../../../modules/sharing.module'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { StatusComponentModule } from 'src/app/components/status/status.component.module'
import { FormsModule } from '@angular/forms'

const routes: Routes = [
  {
    path: '',
    component: MarketplaceListPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    RouterModule.forChild(routes),
    StatusComponentModule,
    SharingModule,
    BadgeMenuComponentModule,
  ],
  declarations: [MarketplaceListPage],
})
export class MarketplaceListPageModule { }
