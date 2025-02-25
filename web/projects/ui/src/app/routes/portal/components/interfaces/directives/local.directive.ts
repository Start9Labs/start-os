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
  static = true
  async add() {}
  async remove() {}
}
