import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { AppRecoverSelectPage } from './app-recover-select.page'
import { FormsModule } from '@angular/forms'

@NgModule({
  declarations: [AppRecoverSelectPage],
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
  ],
  exports: [AppRecoverSelectPage],
})
export class AppRecoverSelectPageModule { }