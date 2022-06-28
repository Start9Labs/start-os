import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { ServerBackupPage } from './server-backup.page'
import { BackingUpComponent } from './backing-up/backing-up.component'
import { RouterModule, Routes } from '@angular/router'
import { BackupDrivesComponentModule } from 'src/app/components/backup-drives/backup-drives.component.module'
import { SharedPipesModule } from '@start9labs/shared'
import { BackupSelectPageModule } from 'src/app/modals/backup-select/backup-select.module'
import { PkgMainStatusPipe } from './backing-up/backing-up.component'

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
    BackupSelectPageModule,
  ],
  declarations: [ServerBackupPage, BackingUpComponent, PkgMainStatusPipe],
})
export class ServerBackupPageModule {}
