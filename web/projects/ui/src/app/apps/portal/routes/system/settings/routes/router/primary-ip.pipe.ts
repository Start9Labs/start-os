import { Pipe, PipeTransform } from '@angular/core'
import { HostnameInfo } from '@start9labs/start-sdk/cjs/sdk/lib/types'

@Pipe({
  standalone: true,
  name: 'primaryIp',
})
export class PrimaryIpPipe implements PipeTransform {
  transform(hostnames: HostnameInfo[]): string {
    return (
      hostnames.map(
        h => h.kind === 'ip' && h.hostname.kind === 'ipv4' && h.hostname.value,
      )[0] || ''
    )
  }
}
