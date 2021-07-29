import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { AppRestoreComponent } from './app-restore.component'
import { PwaBackComponentModule } from '../../components/pwa-back-button/pwa-back.component.module'
import { BackupConfirmationComponentModule } from '../backup-confirmation/backup-confirmation.component.module'
import { SharingModule } from '../../modules/sharing.module'
import { TextSpinnerComponentModule } from '../../components/text-spinner/text-spinner.component.module'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
    BackupConfirmationComponentModule,
    PwaBackComponentModule,
    TextSpinnerComponentModule,
  ],
  declarations: [
    AppRestoreComponent,
  ],
  exports: [AppRestoreComponent],

})
export class AppRestoreComponentModule { }