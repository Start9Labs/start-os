import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupJobsComponent } from './backup-jobs.page'
import { NewJobComponent } from './new-job/new-job.component'
import { ToHumanCronPipe } from './pipes'
import { FormsModule } from '@angular/forms'
import { TargetSelectPageModule } from '../../modals/target-select/target-select.module'

const routes: Routes = [
  {
    path: '',
    component: BackupJobsComponent,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    FormsModule,
    TargetSelectPageModule,
  ],
  declarations: [BackupJobsComponent, ToHumanCronPipe, NewJobComponent],
})
export class BackupJobsComponentModule {}
