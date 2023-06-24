import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { RouterModule, Routes } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import {
  TuiButtonModule,
  TuiNotificationModule,
  TuiWrapperModule,
} from '@taiga-ui/core'
import { TuiInputModule, TuiToggleModule } from '@taiga-ui/kit'
import { BackupJobsPage } from './backup-jobs.page'
import { EditJobComponent } from './edit-job/edit-job.component'
import { ToHumanCronPipe } from './pipes'
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
    TuiNotificationModule,
    TuiButtonModule,
    TuiInputModule,
    TuiToggleModule,
    TuiWrapperModule,
  ],
  declarations: [BackupJobsPage, ToHumanCronPipe, EditJobComponent],
})
export class BackupJobsPageModule {}
