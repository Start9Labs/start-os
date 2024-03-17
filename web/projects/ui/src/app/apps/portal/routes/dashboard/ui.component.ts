import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { tuiPure } from '@taiga-ui/cdk'
import { TuiDataListModule, TuiHostedDropdownModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { ConfigService } from 'src/app/services/config.service'
import {
  InstalledPackageInfo,
  InterfaceInfo,
  PackageDataEntry,
  PackageMainStatus,
} from 'src/app/services/patch-db/data-model'

@Component({
  standalone: true,
  selector: 'app-ui',
  template: `
    @if (interfaces.length > 1) {
      <tui-hosted-dropdown [content]="content">
        <button
          tuiIconButton
          iconLeft="tuiIconExternalLink"
          [disabled]="!isRunning"
        >
          Interfaces
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
      </tui-hosted-dropdown>
    } @else {
      <a
        tuiIconButton
        iconLeft="tuiIconExternalLink"
        target="_blank"
        rel="noreferrer"
        [attr.href]="getHref(interfaces[0])"
      >
        {{ interfaces[0]?.name }}
      </a>
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButtonModule, TuiHostedDropdownModule, TuiDataListModule],
})
export class UIComponent {
  private readonly config = inject(ConfigService)

  @Input()
  pkg!: PackageDataEntry

  get interfaces(): readonly InterfaceInfo[] {
    return this.getInterfaces(this.pkg.installed)
  }

  get isRunning(): boolean {
    return this.pkg.installed?.status.main.status === PackageMainStatus.Running
  }

  @tuiPure
  getInterfaces(info?: InstalledPackageInfo): InterfaceInfo[] {
    return info
      ? Object.values(info.interfaceInfo).filter(({ type }) => type === 'ui')
      : []
  }

  getHref(info?: InterfaceInfo): string | null {
    return info && this.isRunning ? this.config.launchableAddress(info) : null
  }
}
