import { Pipe, PipeTransform } from '@angular/core'
import { IpInfo } from '../../services/patch-db/data-model'

@Pipe({
  name: 'primaryIp',
})
export class PrimaryIpPipe implements PipeTransform {
  transform(ipInfo: IpInfo): string {
    return getPrimaryIp(ipInfo)
  }
}

export function getPrimaryIp(ipInfo: IpInfo): string {
  return Object.values(ipInfo)
    .filter(iface => iface.ipv4)
    .sort((a, b) => (a.wireless ? -1 : 1))[0].ipv4!
}
