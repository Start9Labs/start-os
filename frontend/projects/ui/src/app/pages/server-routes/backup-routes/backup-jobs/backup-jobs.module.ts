import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupJobsPage } from './backup-jobs.page'

const routes: Routes = [
  {
    path: '',
    component: BackupJobsPage,
  },
]

@NgModule({
  imports: [CommonModule, IonicModule, RouterModule.forChild(routes)],
  declarations: [BackupJobsPage],
})
export class BackupJobsPageModule {}
