import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupCreatePage } from './backup-create.page'
import { BackingUpComponent } from './backing-up/backing-up.component'
import { RouterModule, Routes } from '@angular/router'
import { BackupDrivesComponentModule } from 'src/app/pages/server-routes/backup-routes/backup-drives/backup-drives.component.module'
import { SharedPipesModule } from '@start9labs/shared'
import { BackupSelectPageModule } from 'src/app/modals/backup-select/backup-select.module'
import { PkgMainStatusPipe } from './backing-up/backing-up.component'

const routes: Routes = [
  {
    path: '',
    component: BackupCreatePage,
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
  declarations: [BackupCreatePage, BackingUpComponent, PkgMainStatusPipe],
})
export class BackupCreatePageModule {}
