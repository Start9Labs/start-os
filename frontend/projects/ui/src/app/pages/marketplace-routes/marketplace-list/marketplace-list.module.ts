import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import {
  SharedPipesModule,
  TextSpinnerComponentModule,
  StatusComponentModule,
} from '@start9labs/shared'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import { FormsModule } from '@angular/forms'
import { MarketplacePipesModule } from '../pipes/marketplace-pipes.module'
import { MarketplaceListPage } from './marketplace-list.page'

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
    TextSpinnerComponentModule,
    SharedPipesModule,
    MarketplacePipesModule,
    BadgeMenuComponentModule,
  ],
  declarations: [MarketplaceListPage],
})
export class MarketplaceListPageModule {}
