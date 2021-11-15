import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { ProdKeyModal } from './prod-key-modal.page'

@NgModule({
  declarations: [
    ProdKeyModal,
  ],
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
  ],
  exports: [
    ProdKeyModal,
  ],
})
export class ProdKeyModalModule { }
