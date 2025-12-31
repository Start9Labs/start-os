import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { SharedPipesModule, TickerComponent } from '@start9labs/shared'
import { ItemComponent } from './item.component'

@NgModule({
  declarations: [ItemComponent],
  exports: [ItemComponent],
  imports: [CommonModule, RouterModule, SharedPipesModule, TickerComponent],
})
export class ItemModule {}
