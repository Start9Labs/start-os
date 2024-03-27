import { Pipe, PipeTransform } from '@angular/core'
import { T } from '@start9labs/start-sdk'

@Pipe({
  standalone: true,
  name: 'primaryIp',
})
export class PrimaryIpPipe implements PipeTransform {
  transform(hostnames: T.HostnameInfo[]): string {
    return (
      hostnames.map(
        h => h.kind === 'ip' && h.hostname.kind === 'ipv4' && h.hostname.value,
      )[0] || ''
    )
  }
}
