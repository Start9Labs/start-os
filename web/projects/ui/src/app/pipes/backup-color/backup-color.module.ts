import { NgModule } from '@angular/core'
import { BackupColorPipe } from './backup-color.pipe'

@NgModule({
  declarations: [BackupColorPipe],
  exports: [BackupColorPipe],
})
export class BackupColorPipeModule {}
