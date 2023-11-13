import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import { EmverPipesModule } from '@start9labs/shared'

import { InstallProgressPipeModule } from '../../../pipes/install-progress/install-progress.module'
import { MarketplaceStatusComponent } from './marketplace-status.component'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    EmverPipesModule,
    InstallProgressPipeModule,
  ],
  declarations: [MarketplaceStatusComponent],
  exports: [MarketplaceStatusComponent],
})
export class MarketplaceStatusModule {}
