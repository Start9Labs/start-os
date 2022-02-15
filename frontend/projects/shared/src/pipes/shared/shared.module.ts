import { NgModule } from '@angular/core'
import { IncludesPipe } from './includes.pipe'
import { EmptyPipe } from './empty.pipe'

@NgModule({
  declarations: [IncludesPipe, EmptyPipe],
  exports: [IncludesPipe, EmptyPipe],
})
export class SharedPipesModule {}
