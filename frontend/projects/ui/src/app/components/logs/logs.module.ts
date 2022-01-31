import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { LogsPage } from './logs.page'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [LogsPage],
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
  ],
  exports: [LogsPage],
})
export class LogsPageModule { }
