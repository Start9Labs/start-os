import { NgModule } from '@angular/core'
import { AlertComponent } from './alert.component'
import { AlertButtonDirective } from './alert-button.directive'
import { AlertInputDirective } from './alert-input.directive'

@NgModule({
  declarations: [AlertComponent, AlertButtonDirective, AlertInputDirective],
  exports: [AlertComponent, AlertButtonDirective, AlertInputDirective],
})
export class AlertModule {}
