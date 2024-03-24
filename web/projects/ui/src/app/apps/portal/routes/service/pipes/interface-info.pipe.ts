import { Pipe, PipeTransform } from '@angular/core'
import { ServiceInterfaceWithHostInfo } from '@start9labs/start-sdk/cjs/sdk/lib/types'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

export interface ExtendedInterfaceInfo extends ServiceInterfaceWithHostInfo {
  id: string
  icon: string
  color: string
  typeDetail: string
  routerLink: string
}

@Pipe({
  name: 'interfaceInfo',
  standalone: true,
})
export class InterfaceInfoPipe implements PipeTransform {
  transform(pkg: PackageDataEntry): ExtendedInterfaceInfo[] {
    return Object.entries(pkg.serviceInterfaces).map(([id, val]) => {
      let color: string
      let icon: string
      let typeDetail: string

      switch (val.type) {
        case 'ui':
          color = 'var(--tui-primary)'
          icon = 'tuiIconMonitorLarge'
          typeDetail = 'User Interface (UI)'
          break
        case 'p2p':
          color = 'var(--tui-info-fill)'
          icon = 'tuiIconUsersLarge'
          typeDetail = 'Peer-To-Peer Interface (P2P)'
          break
        case 'api':
          color = 'var(--tui-support-09)'
          icon = 'tuiIconTerminalLarge'
          typeDetail = 'Application Program Interface (API)'
          break
      }

      return {
        ...val,
        id,
        color,
        icon,
        typeDetail,
        routerLink: `./interface/${id}`,
      }
    })
  }
}
