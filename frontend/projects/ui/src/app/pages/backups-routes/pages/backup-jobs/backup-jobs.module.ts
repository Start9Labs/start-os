import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupJobsPage } from './backup-jobs.page'
import { NewJobPage } from './new-job/new-job.page'
import { EditJobPage } from './edit-job/edit-job.page'
import { JobOptionsComponent } from './job-options/job-options.component'
import { ToHumanCronPipe } from './pipes'
import { FormsModule } from '@angular/forms'
import { TargetSelectPageModule } from '../../modals/target-select/target-select.module'
import { TargetPipesModule } from '../../pipes/target-pipes.module'

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
    TargetSelectPageModule,
    TargetPipesModule,
  ],
  declarations: [
    BackupJobsPage,
    ToHumanCronPipe,
    NewJobPage,
    EditJobPage,
    JobOptionsComponent,
  ],
})
export class BackupJobsPageModule {}
