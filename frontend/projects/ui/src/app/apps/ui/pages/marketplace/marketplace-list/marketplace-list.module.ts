import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule } from '@angular/forms'
import { RouterModule, Routes } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import {
  EmverPipesModule,
  ResponsiveColModule,
  SharedPipesModule,
} from '@start9labs/shared'
import {
  CategoriesModule,
  FilterPackagesPipeModule,
  ItemModule,
  MarketplaceHeaderModule,
  SearchModule,
  SkeletonModule,
  StoreIconComponentModule,
} from '@start9labs/marketplace'
import { BadgeMenuComponentModule } from 'src/app/common/badge-menu-button/badge-menu.component.module'
import { MarketplaceStatusModule } from '../marketplace-status/marketplace-status.module'
import { MarketplaceListPage } from './marketplace-list.page'
import { MarketplaceSettingsPageModule } from './marketplace-settings/marketplace-settings.module'
import { TuiButtonModule, TuiNotificationModule } from '@taiga-ui/core'

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
    SharedPipesModule,
    EmverPipesModule,
    FilterPackagesPipeModule,
    MarketplaceStatusModule,
    BadgeMenuComponentModule,
    ItemModule,
    CategoriesModule,
    SearchModule,
    SkeletonModule,
    MarketplaceSettingsPageModule,
    StoreIconComponentModule,
    ResponsiveColModule,
    MarketplaceHeaderModule,
    TuiNotificationModule,
    MarketplaceSettingsPageModule,
    TuiButtonModule,
  ],
  declarations: [MarketplaceListPage],
  exports: [MarketplaceListPage],
})
export class MarketplaceListPageModule {}
