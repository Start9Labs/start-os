import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { SuccessPage } from './success.page'
import { PasswordPageModule } from '../password/password.module'

import { SuccessPageRoutingModule } from './success-routing.module'


@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    SuccessPageRoutingModule,
    PasswordPageModule,
  ],
  declarations: [SuccessPage],
})
export class SuccessPageModule {
  constructor () {
    console.log('SuccessModule loaded.')
  }
}
