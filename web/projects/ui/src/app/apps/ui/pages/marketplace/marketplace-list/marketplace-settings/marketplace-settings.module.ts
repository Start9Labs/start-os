import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { SharedPipesModule } from '@start9labs/shared'
import {
  TuiDataListModule,
  TuiHostedDropdownModule,
  TuiSvgModule,
} from '@taiga-ui/core'
import { FormPageModule } from 'src/app/apps/ui/modals/form/form.module'
import { MarketplaceSettingsPage } from './marketplace-settings.page'
import { StoreIconComponentModule } from '@start9labs/marketplace'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    SharedPipesModule,
    StoreIconComponentModule,
    TuiHostedDropdownModule,
    TuiDataListModule,
    TuiSvgModule,
    FormPageModule,
  ],
  declarations: [MarketplaceSettingsPage],
})
export class MarketplaceSettingsPageModule {}
