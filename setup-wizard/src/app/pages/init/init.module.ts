import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { InitPage } from './init.page'
import { InitPageRoutingModule } from './init-routing.module'
import { SuccessPageModule } from '../success/success.module'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    InitPageRoutingModule,
    SuccessPageModule,
  ],
  declarations: [InitPage],
  exports: [InitPage],
})
export class InitPageModule { }
