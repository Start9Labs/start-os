import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { AppRestoreComponent } from './app-restore.component'
import { SharingModule } from '../../modules/sharing.module'
import { BackupDrivesComponentModule } from 'src/app/components/backup-drives/backup-drives.component.module'

@NgModule({
  declarations: [AppRestoreComponent],
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
    BackupDrivesComponentModule,
  ],
  exports: [AppRestoreComponent],

})
export class AppRestoreComponentModule { }