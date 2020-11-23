import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { AppBackupPage } from './app-backup.page'

@NgModule({
  declarations: [AppBackupPage],
  imports: [
    CommonModule,
    IonicModule,
  ],
  entryComponents: [AppBackupPage],
  exports: [AppBackupPage],
})
export class AppBackupPageModule { }