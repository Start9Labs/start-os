import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { SuccessPage } from './success.page'
import { PasswordPageModule } from '../../modals/password/password.module'

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    PasswordPageModule,
  ],
  declarations: [SuccessPage],
  exports: [SuccessPage],
})
export class SuccessPageModule { }
