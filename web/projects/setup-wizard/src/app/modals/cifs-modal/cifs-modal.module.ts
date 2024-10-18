import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { CifsModal } from './cifs-modal.page'
import { ServerBackupSelectModule } from '../server-backup-select/server-backup-select.module'

@NgModule({
  declarations: [CifsModal],
  imports: [CommonModule, FormsModule, IonicModule, ServerBackupSelectModule],
  exports: [CifsModal],
})
export class CifsModalModule {}
