import { NgModule } from '@angular/core'
import { TuiAlertDirective } from './alert.directive'

@NgModule({
  declarations: [TuiAlertDirective],
  exports: [TuiAlertDirective],
})
export class TuiAlertModule {}
