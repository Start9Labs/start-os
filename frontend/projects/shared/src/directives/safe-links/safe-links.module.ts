import { NgModule } from '@angular/core'
import { SafeLinksDirective } from './safe-links.directive'

@NgModule({
  declarations: [SafeLinksDirective],
  exports: [SafeLinksDirective],
})
export class SafeLinksModule {}
