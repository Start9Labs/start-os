import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { SharedPipesModule, TickerModule } from '@start9labs/shared'
import { ItemComponent } from './item.component'

@NgModule({
  declarations: [ItemComponent],
  exports: [ItemComponent],
  imports: [CommonModule, RouterModule, SharedPipesModule, TickerModule],
})
export class ItemModule {}
