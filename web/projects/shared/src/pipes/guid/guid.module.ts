import { NgModule } from '@angular/core'
import { GuidPipe } from './guid.pipe'

@NgModule({
  declarations: [GuidPipe],
  exports: [GuidPipe],
})
export class GuidPipePipesModule {}
