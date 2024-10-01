import { NgModule } from '@angular/core'
import {
  InstallingProgressDisplayPipe,
  InstallingProgressPipe,
} from './install-progress.pipe'

@NgModule({
  declarations: [InstallingProgressPipe, InstallingProgressDisplayPipe],
  exports: [InstallingProgressPipe, InstallingProgressDisplayPipe],
})
export class InstallingProgressPipeModule {}
