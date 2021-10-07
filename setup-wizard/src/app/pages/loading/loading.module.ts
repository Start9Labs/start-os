import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { LoadingPage } from './loading.page'
import { PasswordPageModule } from '../password/password.module'
import { LoadingPageRoutingModule } from './loading-routing.module'

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    LoadingPageRoutingModule,
    PasswordPageModule,
  ],
  declarations: [LoadingPage],
})
export class LoadingPageModule { }
