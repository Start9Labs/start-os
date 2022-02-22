import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { SnakePage } from './snake.page'

@NgModule({
  declarations: [SnakePage],
  imports: [CommonModule, IonicModule],
  exports: [SnakePage],
})
export class SnakePageModule {}
