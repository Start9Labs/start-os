import { NgModule } from '@angular/core'
import { ToManifestPipe, UiPipe } from './ui.pipe'

@NgModule({
  declarations: [UiPipe, ToManifestPipe],
  exports: [UiPipe, ToManifestPipe],
})
export class UiPipeModule {}
