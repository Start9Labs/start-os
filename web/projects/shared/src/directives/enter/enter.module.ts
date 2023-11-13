import { NgModule } from '@angular/core'

import { EnterDirective } from './enter.directive'

@NgModule({
  declarations: [EnterDirective],
  exports: [EnterDirective],
})
export class EnterModule {}
