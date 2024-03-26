import { Directive } from '@angular/core'
import { AddressesService } from '../interface.utils'

@Directive({
  standalone: true,
  selector: '[localAddresses]',
  providers: [
    { provide: AddressesService, useExisting: LocalAddressesDirective },
  ],
})
export class LocalAddressesDirective implements AddressesService {
  async add() {}
  async remove() {}
}
