import { NgModule } from '@angular/core'
import { MarketplaceSidebarComponent } from './marketplace-sidebar.component'
import { SidebarModule } from '@start9labs/marketplace'
import { TuiButtonModule } from '@taiga-ui/core'
import { RouterModule } from '@angular/router'

@NgModule({
  imports: [SidebarModule, TuiButtonModule, RouterModule],
  exports: [MarketplaceSidebarComponent],
  declarations: [MarketplaceSidebarComponent],
})
export class MarketplaceSidebarModule {}
