import { NgModule } from '@angular/core'
import {
  InstallProgressDisplayPipe,
  InstallProgressPipe,
} from './install-progress.pipe'

@NgModule({
  declarations: [InstallProgressPipe, InstallProgressDisplayPipe],
  exports: [InstallProgressPipe, InstallProgressDisplayPipe],
})
export class InstallProgressPipeModule {}
