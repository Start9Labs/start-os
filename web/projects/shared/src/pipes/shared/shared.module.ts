import { NgModule } from '@angular/core'
import { IncludesPipe } from './includes.pipe'
import { EmptyPipe } from './empty.pipe'
import { TrustUrlPipe } from './trust.pipe'
import { SortPipe } from './sort.pipe'

@NgModule({
  declarations: [IncludesPipe, EmptyPipe, TrustUrlPipe, SortPipe],
  exports: [IncludesPipe, EmptyPipe, TrustUrlPipe, SortPipe],
})
export class SharedPipesModule {}
