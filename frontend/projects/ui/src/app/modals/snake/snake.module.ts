import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'

import { SnakePage } from './snake.page'

@NgModule({
  imports: [CommonModule, IonicModule],
  declarations: [SnakePage],
  exports: [SnakePage],
})
export class SnakePageModule {}
