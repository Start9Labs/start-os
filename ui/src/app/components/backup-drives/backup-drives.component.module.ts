import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { BackupDrivesComponent, BackupDrivesHeaderComponent, BackupDrivesStatusComponent } from './backup-drives.component'
import { SharingModule } from '../../modules/sharing.module'
import { GenericFormPageModule } from 'src/app/modals/generic-form/generic-form.module'

@NgModule({
  declarations: [
    BackupDrivesComponent,
    BackupDrivesHeaderComponent,
    BackupDrivesStatusComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
    GenericFormPageModule,
  ],
  exports: [
    BackupDrivesComponent,
    BackupDrivesHeaderComponent,
    BackupDrivesStatusComponent,
  ],
})
export class BackupDrivesComponentModule { }
