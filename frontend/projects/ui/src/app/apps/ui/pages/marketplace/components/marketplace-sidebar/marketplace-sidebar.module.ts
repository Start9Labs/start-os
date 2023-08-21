import { NgModule } from '@angular/core'
import { MarketplaceSidebarComponent } from './marketplace-sidebar.component'
import { SidebarModule } from '@start9labs/marketplace'
import { TuiButtonModule } from '@taiga-ui/core'

@NgModule({
  imports: [SidebarModule, TuiButtonModule],
  exports: [MarketplaceSidebarComponent],
  declarations: [MarketplaceSidebarComponent],
})
export class MarketplaceSidebarModule {}
