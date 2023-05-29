import { NgModule } from '@angular/core'

import { SnakePageModule } from 'src/app/modals/snake/snake.module'
import { SnekDirective } from './snek.directive'

@NgModule({
  imports: [SnakePageModule],
  declarations: [SnekDirective],
  exports: [SnekDirective],
})
export class SnekModule {}
