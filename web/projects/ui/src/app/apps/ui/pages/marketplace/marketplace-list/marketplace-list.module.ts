import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { RouterModule, Routes } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { ResponsiveColDirective, SharedPipesModule } from '@start9labs/shared'
import { FilterPackagesPipeModule } from '@start9labs/marketplace'
import { MarketplaceMenuModule } from '../components/marketplace-menu/marketplace-menu.module'
import { MarketplaceListPage } from './marketplace-list.page'
import { MarketplaceSettingsPageModule } from './marketplace-settings/marketplace-settings.module'
import { TuiNotificationModule } from '@taiga-ui/core'
import { TuiLetModule } from '@taiga-ui/cdk'
import { MarketplaceItemToggleComponent } from '../components/marketplace-item-toggle.component'
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
    RouterModule.forChild(routes),
    SharedPipesModule,
    FilterPackagesPipeModule,
    MarketplaceMenuModule,
    MarketplaceSettingsPageModule,
    TuiNotificationModule,
    TuiLetModule,
    ResponsiveColDirective,
    MarketplaceItemToggleComponent,
  ],
  declarations: [MarketplaceListPage],
  exports: [MarketplaceListPage],
})
export class MarketplaceListPageModule {}
