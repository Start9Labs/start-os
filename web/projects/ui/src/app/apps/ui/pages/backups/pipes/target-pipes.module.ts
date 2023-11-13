import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { HasValidBackupPipe } from './has-valid-backup.pipe'
import { GetDisplayInfoPipe } from './get-display-info.pipe'

@NgModule({
  declarations: [HasValidBackupPipe, GetDisplayInfoPipe],
  imports: [CommonModule],
  exports: [HasValidBackupPipe, GetDisplayInfoPipe],
})
export class TargetPipesModule {}
