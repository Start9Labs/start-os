import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupRestorePage } from './backup-restore.page'
import { SharedPipesModule } from '@start9labs/shared'
import { BackupDrivesComponentModule } from 'src/app/pages/server-routes/backup-routes/backup-drives/backup-drives.component.module'
import { AppRecoverSelectPageModule } from 'src/app/modals/app-recover-select/app-recover-select.module'

const routes: Routes = [
  {
    path: '',
    component: BackupRestorePage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    SharedPipesModule,
    BackupDrivesComponentModule,
    AppRecoverSelectPageModule,
  ],
  declarations: [BackupRestorePage],
})
export class BackupRestorePageModule {}
