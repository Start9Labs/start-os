import { NgModule } from '@angular/core'
import { MarketplaceSidebarComponent } from './marketplace-sidebar.component'
import { SidebarModule } from '@start9labs/marketplace'
import { TuiButtonModule } from '@taiga-ui/core'
import { RouterModule } from '@angular/router'
import { CommonModule } from '@angular/common'

@NgModule({
  imports: [SidebarModule, TuiButtonModule, RouterModule, CommonModule],
  exports: [MarketplaceSidebarComponent],
  declarations: [MarketplaceSidebarComponent],
})
export class MarketplaceSidebarModule {}
