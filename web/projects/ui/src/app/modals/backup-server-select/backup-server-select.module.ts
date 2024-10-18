import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { BackupServerSelectModal } from './backup-server-select.page'
import { AppRecoverSelectPageModule } from 'src/app/modals/app-recover-select/app-recover-select.module'

@NgModule({
  declarations: [BackupServerSelectModal],
  imports: [CommonModule, FormsModule, IonicModule, AppRecoverSelectPageModule],
  exports: [BackupServerSelectModal],
})
export class BackupServerSelectModule {}
