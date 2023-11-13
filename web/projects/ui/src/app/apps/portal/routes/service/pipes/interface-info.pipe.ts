import { Pipe, PipeTransform } from '@angular/core'
import {
  InterfaceInfo,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'

export interface ExtendedInterfaceInfo extends InterfaceInfo {
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
  transform({ installed }: PackageDataEntry): ExtendedInterfaceInfo[] {
    return Object.entries(installed!.interfaceInfo).map(([id, val]) => {
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
        case 'other':
          color = 'var(--tui-text-02)'
          icon = 'tuiIconBoxLarge'
          typeDetail = 'Unknown Interface Type'
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
