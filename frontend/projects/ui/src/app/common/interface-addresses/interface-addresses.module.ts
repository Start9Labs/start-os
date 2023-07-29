import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import {
  InterfaceAddressesComponent,
  InterfaceClearnetPipe,
} from './interface-addresses.component'

@NgModule({
  imports: [CommonModule, IonicModule],
  declarations: [InterfaceAddressesComponent, InterfaceClearnetPipe],
  exports: [InterfaceAddressesComponent],
})
export class InterfaceAddressesComponentModule {}
