import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { MarketplaceSettingsPage } from './marketplace-settings.page'
import { SharedPipesModule } from '@start9labs/shared'
import { StoreIconComponentModule } from 'src/app/components/store-icon/store-icon.component.module'
import {
  TuiDataListModule,
  TuiHostedDropdownModule,
  TuiSvgModule,
} from '@taiga-ui/core'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    SharedPipesModule,
    StoreIconComponentModule,
    TuiHostedDropdownModule,
    TuiDataListModule,
    TuiSvgModule,
  ],
  declarations: [MarketplaceSettingsPage],
})
export class MarketplaceSettingsPageModule {}
