import { NgModule } from '@angular/core'
import {
  EmverComparesPipe,
  EmverDisplayPipe,
  EmverSatisfiesPipe,
} from './emver.pipe'

@NgModule({
  declarations: [EmverComparesPipe, EmverDisplayPipe, EmverSatisfiesPipe],
  exports: [EmverComparesPipe, EmverDisplayPipe, EmverSatisfiesPipe],
})
export class EmverPipesModule {}
