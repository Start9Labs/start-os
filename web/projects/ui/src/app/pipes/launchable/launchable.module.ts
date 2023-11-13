import { NgModule } from '@angular/core'
import { LaunchablePipe } from './launchable.pipe'

@NgModule({
  declarations: [LaunchablePipe],
  exports: [LaunchablePipe],
})
export class LaunchablePipeModule {}
