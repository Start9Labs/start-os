import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { AppRestoreComponent } from './app-restore.component'
import { BackupConfirmationComponentModule } from '../backup-confirmation/backup-confirmation.component.module'
import { SharingModule } from '../../modules/sharing.module'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    BackupConfirmationComponentModule,
    SharingModule,
  ],
  declarations: [
    AppRestoreComponent,
  ],
  exports: [AppRestoreComponent],

})
export class AppRestoreComponentModule { }