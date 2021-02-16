import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { BackupConfirmationComponent } from './backup-confirmation.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'
import { FormsModule } from '@angular/forms'

@NgModule({
  declarations: [
    BackupConfirmationComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
    FormsModule,
  ],
  exports: [BackupConfirmationComponent],
})
export class BackupConfirmationComponentModule { }
