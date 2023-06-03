import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupSelectPage } from './backup-select.page'
import { FormsModule } from '@angular/forms'

@NgModule({
  declarations: [BackupSelectPage],
  imports: [CommonModule, IonicModule, FormsModule],
  exports: [BackupSelectPage],
})
export class BackupSelectPageModule {}
