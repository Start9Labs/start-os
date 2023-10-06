import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule } from '@angular/forms'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import {
  SharedPipesModule,
  EmverPipesModule,
  ResponsiveColDirective,
} from '@start9labs/shared'
import {
  FilterPackagesPipeModule,
  CategoriesModule,
  ItemModule,
  SearchModule,
  SkeletonModule,
  StoreIconComponentModule,
} from '@start9labs/marketplace'
import { BadgeMenuComponentModule } from 'src/app/common/badge-menu-button/badge-menu.component.module'
import { MarketplaceStatusModule } from '../marketplace-status/marketplace-status.module'
import { MarketplaceListPage } from './marketplace-list.page'
import { MarketplaceSettingsPageModule } from './marketplace-settings/marketplace-settings.module'

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
    ResponsiveColDirective,
  ],
  declarations: [MarketplaceListPage],
  exports: [MarketplaceListPage],
})
export class MarketplaceListPageModule {}