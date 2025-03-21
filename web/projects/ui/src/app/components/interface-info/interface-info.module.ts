import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { InterfaceInfoComponent } from './interface-info.component'

@NgModule({
  declarations: [InterfaceInfoComponent],
  imports: [CommonModule, IonicModule],
  exports: [InterfaceInfoComponent],
})
export class InterfaceInfoModule {}
