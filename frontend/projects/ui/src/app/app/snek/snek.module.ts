import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'

import { SnekDirective } from './snek.directive'
import { SnakePage } from './snake.page'
import { TuiButtonModule } from '@taiga-ui/core'

@NgModule({
  imports: [CommonModule, IonicModule, TuiButtonModule],
  declarations: [SnekDirective, SnakePage],
  exports: [SnekDirective, SnakePage],
})
export class SnekModule {}
