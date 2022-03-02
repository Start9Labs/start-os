import { NgModule } from '@angular/core'

import { ElementDirective } from './element.directive'

@NgModule({
  declarations: [ElementDirective],
  exports: [ElementDirective],
})
export class ElementModule {}
