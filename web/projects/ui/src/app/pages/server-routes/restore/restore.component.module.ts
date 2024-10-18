import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RestorePage } from './restore.component'
import { SharedPipesModule } from '@start9labs/shared'
import { BackupDrivesComponentModule } from 'src/app/components/backup-drives/backup-drives.component.module'
import { BackupServerSelectModule } from 'src/app/modals/backup-server-select/backup-server-select.module'

const routes: Routes = [
  {
    path: '',
    component: RestorePage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    SharedPipesModule,
    BackupDrivesComponentModule,
    BackupServerSelectModule,
  ],
  declarations: [RestorePage],
})
export class RestorePageModule {}
