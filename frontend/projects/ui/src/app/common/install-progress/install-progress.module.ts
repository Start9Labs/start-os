import { NgModule } from '@angular/core'
import { InstallProgressDisplayPipe } from './install-progress.pipe'

@NgModule({
  declarations: [InstallProgressDisplayPipe],
  exports: [InstallProgressDisplayPipe],
})
export class InstallProgressPipeModule {}
