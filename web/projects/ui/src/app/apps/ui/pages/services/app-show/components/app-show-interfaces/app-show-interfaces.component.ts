import { DOCUMENT } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
} from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import {
  InstalledPackageInfo,
  InterfaceInfo,
} from 'src/app/services/patch-db/data-model'
import { Pipe, PipeTransform } from '@angular/core'

@Component({
  selector: 'app-show-interfaces',
  templateUrl: './app-show-interfaces.component.html',
  styleUrls: ['./app-show-interfaces.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowInterfacesComponent {
  @Input({ required: true })
  pkg!: InstalledPackageInfo

  constructor(
    private readonly config: ConfigService,
    @Inject(DOCUMENT) private readonly document: Document,
  ) {}

  launchUI(info: InterfaceInfo, e: Event) {
    e.stopPropagation()
    e.preventDefault()
    this.document.defaultView?.open(
      this.config.launchableAddress(info),
      '_blank',
      'noreferrer',
    )
  }
}

@Pipe({
  name: 'interfaceInfo',
})
export class InterfaceInfoPipe implements PipeTransform {
  transform(info: InstalledPackageInfo['interfaceInfo']) {
    return Object.entries(info).map(([id, val]) => {
      let color: string
      let icon: string
      let typeDetail: string

      switch (val.type) {
        case 'ui':
          color = 'primary'
          icon = 'desktop-outline'
          typeDetail = 'User Interface (UI)'
          break
        case 'p2p':
          color = 'secondary'
          icon = 'people-outline'
          typeDetail = 'Peer-To-Peer Interface (P2P)'
          break
        case 'api':
          color = 'tertiary'
          icon = 'terminal-outline'
          typeDetail = 'Application Program Interface (API)'
          break
        case 'other':
          color = 'dark'
          icon = 'cube-outline'
          typeDetail = 'Unknown Interface Type'
          break
      }

      return {
        ...val,
        id,
        color,
        icon,
        typeDetail,
      }
    })
  }
}
