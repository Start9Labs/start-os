import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import {
  InterfaceAddressesComponent,
  InterfaceAddressItemComponent,
  InterfaceClearnetPipe,
} from './interface-addresses.component'

@NgModule({
  imports: [CommonModule, IonicModule],
  declarations: [
    InterfaceAddressesComponent,
    InterfaceAddressItemComponent,
    InterfaceClearnetPipe,
  ],
  exports: [InterfaceAddressesComponent],
})
export class InterfaceAddressesComponentModule {}
