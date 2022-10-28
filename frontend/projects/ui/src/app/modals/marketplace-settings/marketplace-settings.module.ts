import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { MarketplaceSettingsPage } from './marketplace-settings.page'
import { SharedPipesModule } from '@start9labs/shared'

@NgModule({
  imports: [CommonModule, IonicModule, SharedPipesModule],
  declarations: [MarketplaceSettingsPage],
})
export class MarketplaceSettingsPageModule {}
