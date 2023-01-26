import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupJobsPage } from './backup-jobs.page'
import { BackupJobModal } from './backup-job-modal.page'
import { ToHumanCronPipe } from './pipes'
import { FormsModule } from '@angular/forms'

const routes: Routes = [
  {
    path: '',
    component: BackupJobsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    FormsModule,
  ],
  declarations: [BackupJobsPage, ToHumanCronPipe, BackupJobModal],
})
export class BackupJobsPageModule {}
