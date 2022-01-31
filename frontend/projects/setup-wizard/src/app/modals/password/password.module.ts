import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { PasswordPage } from './password.page'

@NgModule({
  declarations: [
    PasswordPage,
  ],
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
  ],
  exports: [
    PasswordPage,
  ],
})
export class PasswordPageModule { }
