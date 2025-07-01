import { NgModule } from '@angular/core'
import { ExverComparesPipe, ExverSatisfiesPipe } from './exver.pipe'

@NgModule({
  declarations: [ExverComparesPipe, ExverSatisfiesPipe],
  exports: [ExverComparesPipe, ExverSatisfiesPipe],
})
export class ExverPipesModule {}
