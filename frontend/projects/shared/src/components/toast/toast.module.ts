import { NgModule } from '@angular/core'
import { ToastComponent } from './toast.component'
import { ToastButtonDirective } from './toast-button.component'

@NgModule({
  declarations: [ToastComponent, ToastButtonDirective],
  exports: [ToastComponent, ToastButtonDirective],
})
export class ToastModule {}
