import { NgModule } from '@angular/core'

import { ResizeableDirective } from './resizeable.directive'
import { ResizerDirective } from './resizer.directive'

// TODO: Move to Taiga UI 3.14.0
@NgModule({
  declarations: [ResizeableDirective, ResizerDirective],
  exports: [ResizeableDirective, ResizerDirective],
})
export class ResizerModule {}
