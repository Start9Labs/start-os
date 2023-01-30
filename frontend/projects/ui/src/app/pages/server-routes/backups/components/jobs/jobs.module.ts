import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupJobsComponent } from './jobs.component'
import { NewJobComponent } from '../new-job/new-job.component'
import { ToHumanCronPipe } from './pipes'
import { FormsModule } from '@angular/forms'

@NgModule({
  imports: [CommonModule, IonicModule, FormsModule],
  declarations: [BackupJobsComponent, ToHumanCronPipe, NewJobComponent],
  exports: [BackupJobsComponent],
})
export class BackupJobsComponentModule {}
