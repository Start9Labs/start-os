import { Directive } from '@angular/core'
import { AddressesService } from '../interface.utils'

@Directive({
  standalone: true,
  selector: '[torAddresses]',
  providers: [
    { provide: AddressesService, useExisting: TorAddressesDirective },
  ],
})
export class TorAddressesDirective implements AddressesService {
  async add() {}
  async remove() {}
}
