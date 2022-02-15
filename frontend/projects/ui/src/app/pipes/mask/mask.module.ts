import { NgModule } from '@angular/core'
import { MaskPipe } from './mask.pipe'

@NgModule({
  declarations: [MaskPipe],
  exports: [MaskPipe],
})
export class MaskPipeModule {}
