import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { SharedPipesModule, TickerComponent } from '@start9labs/shared'
import { TuiLet } from '@taiga-ui/cdk'
import { ItemComponent } from './item.component'

@NgModule({
  declarations: [ItemComponent],
  exports: [ItemComponent],
  imports: [
    CommonModule,
    RouterModule,
    SharedPipesModule,
    TickerComponent,
    TuiLet,
  ],
})
export class ItemModule {}
