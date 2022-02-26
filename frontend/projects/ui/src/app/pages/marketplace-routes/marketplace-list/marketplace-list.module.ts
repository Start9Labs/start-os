import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule } from '@angular/forms'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import {
  SharedPipesModule,
  EmverPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'
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
    TextSpinnerComponentModule,
    SharedPipesModule,
    EmverPipesModule,
    MarketplacePipesModule,
  ],
  declarations: [MarketplaceListPage],
})
export class MarketplaceListPageModule {}
