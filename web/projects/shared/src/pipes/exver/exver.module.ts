import { NgModule } from '@angular/core'
import {
  ExverComparesPipe,
  ExverDisplayPipe,
  ExverSatisfiesPipe,
} from './exver.pipe'

@NgModule({
  declarations: [ExverComparesPipe, ExverDisplayPipe, ExverSatisfiesPipe],
  exports: [ExverComparesPipe, ExverDisplayPipe, ExverSatisfiesPipe],
})
export class ExverPipesModule {}
