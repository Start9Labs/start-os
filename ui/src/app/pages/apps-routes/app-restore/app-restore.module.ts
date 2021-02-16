import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { AppRestorePage } from './app-restore.page'
import { RouterModule, Routes } from '@angular/router'
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module'
import { BackupConfirmationComponentModule } from 'src/app/modals/backup-confirmation/backup-confirmation.component.module'
import { SharingModule } from 'src/app/modules/sharing.module'

const routes: Routes = [
  {
    path: '',
    component: AppRestorePage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
    RouterModule.forChild(routes),
    BackupConfirmationComponentModule,
    PwaBackComponentModule,
  ],
  declarations: [
    AppRestorePage,
  ],
})
export class AppRestorePageModule { }