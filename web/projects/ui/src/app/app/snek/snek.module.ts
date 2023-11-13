import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'

import { SnekDirective } from './snek.directive'
import { SnakePage } from './snake.page'

@NgModule({
  imports: [CommonModule, IonicModule],
  declarations: [SnekDirective, SnakePage],
  exports: [SnekDirective, SnakePage],
})
export class SnekModule {}
