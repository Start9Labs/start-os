import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { ProdKeyModal } from './prod-key-modal.page'

import { ProdKeyModalRoutingModule } from './prod-key-modal-routing.module'

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    ProdKeyModalRoutingModule,
  ],
  declarations: [ProdKeyModal],
})
export class ProdKeyModalModule { }
