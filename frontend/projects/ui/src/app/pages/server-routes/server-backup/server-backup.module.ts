import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { ServerBackupPage } from './server-backup.page'
import { RouterModule, Routes } from '@angular/router'
import { BackupDrivesComponentModule } from 'src/app/components/backup-drives/backup-drives.component.module'
import { SharedPipesModule } from '@start9labs/shared'

const routes: Routes = [
  {
    path: '',
    component: ServerBackupPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    SharedPipesModule,
    BackupDrivesComponentModule,
  ],
  declarations: [ServerBackupPage],
})
export class ServerBackupPageModule {}
