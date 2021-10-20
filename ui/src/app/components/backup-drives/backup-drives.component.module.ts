import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupDrivesComponent, BackupDrivesHeaderComponent } from './backup-drives.component'
import { SharingModule } from '../../modules/sharing.module'

@NgModule({
  declarations: [
    BackupDrivesComponent,
    BackupDrivesHeaderComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
  ],
  exports: [
    BackupDrivesComponent,
    BackupDrivesHeaderComponent,
  ],
})
export class BackupDrivesComponentModule { }
