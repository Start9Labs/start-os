import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import { EmverPipesModule } from '@start9labs/shared'

import { InstallProgressPipe } from './install-progress.pipe'
import { MarketplaceStatusComponent } from './marketplace-status.component'

@NgModule({
  imports: [CommonModule, IonicModule, EmverPipesModule],
  declarations: [MarketplaceStatusComponent, InstallProgressPipe],
  exports: [MarketplaceStatusComponent],
})
export class MarketplaceStatusModule {}
