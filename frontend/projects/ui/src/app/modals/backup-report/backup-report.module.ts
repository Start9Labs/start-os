import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupReportPage } from './backup-report.page'

@NgModule({
  declarations: [BackupReportPage],
  imports: [
    CommonModule,
    IonicModule,
  ],
  exports: [BackupReportPage],
})
export class BackupReportPageModule { }
