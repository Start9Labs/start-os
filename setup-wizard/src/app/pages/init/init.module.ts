import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { InitPage } from './init.page'
import { InitPageRoutingModule } from './init-routing.module'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    InitPageRoutingModule,
  ],
  declarations: [InitPage],
})
export class InitPageModule { }
