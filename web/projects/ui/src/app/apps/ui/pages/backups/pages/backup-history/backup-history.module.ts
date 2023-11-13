import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import {
  BackupHistoryPage,
  DurationPipe,
  HasErrorPipe,
} from './backup-history.page'
import { TargetPipesModule } from '../../pipes/target-pipes.module'
import { BackupReportPageModule } from 'src/app/apps/ui/modals/backup-report/backup-report.module'

const routes: Routes = [
  {
    path: '',
    component: BackupHistoryPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    TargetPipesModule,
    BackupReportPageModule,
    RouterModule.forChild(routes),
  ],
  declarations: [BackupHistoryPage, DurationPipe, HasErrorPipe],
})
export class BackupHistoryPageModule {}
