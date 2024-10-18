import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import { ExverPipesModule } from '@start9labs/shared'
import { InstallingProgressPipeModule } from '../../../pipes/install-progress/install-progress.module'
import { MarketplaceStatusComponent } from './marketplace-status.component'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    ExverPipesModule,
    InstallingProgressPipeModule,
  ],
  declarations: [MarketplaceStatusComponent],
  exports: [MarketplaceStatusComponent],
})
export class MarketplaceStatusModule {}
