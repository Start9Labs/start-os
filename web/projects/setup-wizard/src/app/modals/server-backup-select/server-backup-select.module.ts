import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { ServerBackupSelectModal } from './server-backup-select.page'
import { PasswordPageModule } from '../password/password.module'

@NgModule({
  declarations: [ServerBackupSelectModal],
  imports: [CommonModule, FormsModule, IonicModule, PasswordPageModule],
  exports: [ServerBackupSelectModal],
})
export class ServerBackupSelectModule {}
