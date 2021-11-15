import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { CifsModal } from './cifs-modal.page'

@NgModule({
  declarations: [
    CifsModal,
  ],
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
  ],
  exports: [
    CifsModal,
  ],
})
export class CifsModalModule { }
