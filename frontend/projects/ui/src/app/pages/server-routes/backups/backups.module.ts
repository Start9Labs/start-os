import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupsPage } from './backups.page'
import { BackupCreateDirective } from './directives/backup-create.directive'
import { BackupRestoreDirective } from './directives/backup-restore.directive'
import {
  BackingUpComponent,
  PkgMainStatusPipe,
} from './components/backing-up/backing-up.component'
import { BackupDrivesComponentModule } from './components/backup-drives/backup-drives.component.module'
import { BackupSelectPageModule } from './components/backup-select/backup-select.module'
import { RecoverSelectPageModule } from './components/recover-select/recover-select.module'
import { BackupJobsComponentModule } from './components/jobs/jobs.module'

const routes: Routes = [
  {
    path: '',
    component: BackupsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    BackupSelectPageModule,
    RecoverSelectPageModule,
    BackupDrivesComponentModule,
    BackupJobsComponentModule,
  ],
  declarations: [
    BackupsPage,
    BackupCreateDirective,
    BackupRestoreDirective,
    BackingUpComponent,
    PkgMainStatusPipe,
  ],
})
export class BackupsPageModule {}
