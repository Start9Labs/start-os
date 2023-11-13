import { NgModule } from '@angular/core'

import { ResponsiveColViewportDirective } from './responsive-col-viewport.directive'
import { ResponsiveColDirective } from './responsive-col.directive'

@NgModule({
  declarations: [ResponsiveColDirective, ResponsiveColViewportDirective],
  exports: [ResponsiveColDirective, ResponsiveColViewportDirective],
})
export class ResponsiveColModule {}
