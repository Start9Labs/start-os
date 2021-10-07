import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { ProductKeyPage } from './product-key.page'
import { PasswordPageModule } from '../password/password.module'
import { ProductKeyPageRoutingModule } from './product-key-routing.module'

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    ProductKeyPageRoutingModule,
    PasswordPageModule,
  ],
  declarations: [ProductKeyPage],
})
export class ProductKeyPageModule { }
