import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupReportComponent } from './backup-report.component'

@NgModule({
  declarations: [BackupReportComponent],
  imports: [CommonModule, IonicModule],
  exports: [BackupReportComponent],
})
export class BackupReportPageModule {}
