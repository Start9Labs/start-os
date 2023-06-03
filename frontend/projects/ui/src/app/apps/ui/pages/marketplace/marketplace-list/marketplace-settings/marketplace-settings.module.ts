import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { SharedPipesModule } from '@start9labs/shared'
import {
  TuiDataListModule,
  TuiHostedDropdownModule,
  TuiSvgModule,
} from '@taiga-ui/core'
import { StoreIconComponentModule } from 'src/app/common/store-icon/store-icon.component.module'
import { FormPageModule } from 'src/app/apps/ui/modals/form/form.module'
import { MarketplaceSettingsPage } from './marketplace-settings.page'

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
