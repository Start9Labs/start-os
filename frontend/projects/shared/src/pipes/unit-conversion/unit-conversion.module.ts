import { NgModule } from '@angular/core'
import { ConvertBytesPipe, DurationToSecondsPipe } from './unit-conversion.pipe'

@NgModule({
  declarations: [ConvertBytesPipe, DurationToSecondsPipe],
  exports: [ConvertBytesPipe, DurationToSecondsPipe],
})
export class UnitConversionPipesModule {}
