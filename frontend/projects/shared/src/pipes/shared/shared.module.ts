import { NgModule } from '@angular/core'
import { IncludesPipe } from './includes.pipe'
import { EmptyPipe } from './empty.pipe'
import { TrustUrlPipe } from './trust.pipe'

@NgModule({
  declarations: [IncludesPipe, EmptyPipe, TrustUrlPipe],
  exports: [IncludesPipe, EmptyPipe, TrustUrlPipe],
})
export class SharedPipesModule {}
