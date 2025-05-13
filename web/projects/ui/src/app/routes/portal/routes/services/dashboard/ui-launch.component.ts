import { DOCUMENT } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { tuiPure } from '@taiga-ui/cdk'
import { TuiDataList, TuiDropdown, TuiButton } from '@taiga-ui/core'
import { ConfigService } from 'src/app/services/config.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'app-ui-launch',
  template: `
    @if (interfaces.length > 1) {
      <button
        tuiIconButton
        iconStart="@tui.external-link"
        tuiDropdownOpen
        [disabled]="!isRunning"
        [tuiDropdown]="content"
      >
        {{ 'Open' | i18n }}
      </button>
      <ng-template #content>
        <tui-data-list>
          @for (interface of interfaces; track $index) {
            <a
              tuiOption
              target="_blank"
              rel="noreferrer"
              [attr.href]="getHref(interface)"
            >
              {{ interface.name }}
            </a>
          }
        </tui-data-list>
      </ng-template>
    } @else if (interfaces[0]) {
      <button
        tuiIconButton
        iconStart="@tui.external-link"
        [disabled]="!isRunning"
        (click)="openUI(interfaces[0])"
      >
        {{ interfaces[0].name }}
      </button>
    }
  `,
  styles: `
    :host-context(tui-root._mobile) *::before {
      font-size: 1.5rem !important;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiDropdown, TuiDataList, i18nPipe],
})
export class UILaunchComponent {
  private readonly config = inject(ConfigService)
  private readonly document = inject(DOCUMENT)

  @Input()
  pkg!: PackageDataEntry

  get interfaces(): readonly T.ServiceInterface[] {
    return this.getInterfaces(this.pkg)
  }

  get isRunning(): boolean {
    return this.pkg.status.main === 'running'
  }

  @tuiPure
  getInterfaces(pkg?: PackageDataEntry): T.ServiceInterface[] {
    return pkg
      ? Object.values(pkg.serviceInterfaces).filter(
          i =>
            i.type === 'ui' &&
            (i.addressInfo.scheme === 'http' ||
              i.addressInfo.sslScheme === 'https'),
        )
      : []
  }

  getHref(ui: T.ServiceInterface): string {
    return this.config.launchableAddress(ui, this.pkg.hosts)
  }

  openUI(ui: T.ServiceInterface) {
    this.document.defaultView?.open(this.getHref(ui), '_blank', 'noreferrer')
  }
}
