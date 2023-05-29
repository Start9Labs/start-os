import { Pipe, PipeTransform } from '@angular/core'
import { InstalledPackageInfo } from 'src/app/services/patch-db/data-model'
import { hasUi } from 'src/app/services/config.service'

@Pipe({
  name: 'hasUi',
})
export class UiPipe implements PipeTransform {
  transform(addressInfo: InstalledPackageInfo['address-info']): boolean {
    return hasUi(addressInfo)
  }
}

@Pipe({
  name: 'uiAddresses',
})
export class UiAddressesPipe implements PipeTransform {
  transform(
    addressInfo: InstalledPackageInfo['address-info'],
  ): { name: string; addresses: string[] }[] {
    return Object.values(addressInfo)
      .filter(info => info.ui)
      .map(info => ({
        name: info.name,
        addresses: info.addresses,
      }))
  }
}

@Pipe({
  name: 'addressType',
})
export class AddressTypePipe implements PipeTransform {
  transform(address: string): string {
    if (isValidIpv4(address)) return 'IPv4'
    if (isValidIpv6(address)) return 'IPv6'

    const hostname = new URL(address).hostname
    if (hostname.endsWith('.onion')) return 'Tor'
    if (hostname.endsWith('.local')) return 'Local'

    return 'Custom'
  }
}

function isValidIpv4(address: string): boolean {
  const regexExp =
    /^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/
  return regexExp.test(address)
}

function isValidIpv6(address: string): boolean {
  const regexExp =
    /(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))/gi
  return regexExp.test(address)
}
