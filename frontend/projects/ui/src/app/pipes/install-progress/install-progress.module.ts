import { NgModule } from '@angular/core'
import { InstallProgressPipe } from './install-progress.pipe'

@NgModule({
  declarations: [InstallProgressPipe],
  exports: [InstallProgressPipe],
})
export class InstallProgressPipeModule {}
