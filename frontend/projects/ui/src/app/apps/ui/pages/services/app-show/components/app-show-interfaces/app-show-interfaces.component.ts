import { DOCUMENT } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
} from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import {
  InterfaceInfo,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'app-show-interfaces',
  templateUrl: './app-show-interfaces.component.html',
  styleUrls: ['./app-show-interfaces.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowInterfacesComponent {
  @Input({ required: true })
  pkg!: PackageDataEntry

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
