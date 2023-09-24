import { inject, Pipe, PipeTransform } from '@angular/core'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import {
  InterfaceInfo,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { ServiceInterfaceModal } from '../modals/interface.component'

export interface ExtendedInterfaceInfo extends InterfaceInfo {
  id: string
  icon: string
  color: string
  typeDetail: string
  action: () => void
}

@Pipe({
  name: 'interfaceInfo',
  standalone: true,
})
export class InterfaceInfoPipe implements PipeTransform {
  private readonly dialogs = inject(TuiDialogService)

  transform({
    manifest,
    installed,
  }: PackageDataEntry): ExtendedInterfaceInfo[] {
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
        action: () =>
          this.dialogs
            .open(new PolymorpheusComponent(ServiceInterfaceModal), {
              label: val.name,
              size: 'l',
              data: {
                packageId: manifest.id,
                interfaceId: id,
              },
            })
            .subscribe(),
      }
    })
  }
}
