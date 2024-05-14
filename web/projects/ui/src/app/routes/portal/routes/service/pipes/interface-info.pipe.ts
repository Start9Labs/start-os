import { Pipe, PipeTransform } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

export interface ExtendedInterfaceInfo extends T.ServiceInterfaceWithHostInfo {
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
          icon = 'tuiIconMonitor'
          typeDetail = 'User Interface (UI)'
          break
        case 'p2p':
          color = 'var(--tui-info-fill)'
          icon = 'tuiIconUsers'
          typeDetail = 'Peer-To-Peer Interface (P2P)'
          break
        case 'api':
          color = 'var(--tui-support-09)'
          icon = 'tuiIconTerminal'
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
