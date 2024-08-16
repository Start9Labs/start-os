import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { tuiPure } from '@taiga-ui/cdk'
import { TuiDataList, TuiDropdown, TuiButton } from '@taiga-ui/core'
import { ConfigService } from 'src/app/services/config.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  standalone: true,
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
        Launch UI
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
    } @else {
      <a
        tuiIconButton
        iconStart="@tui.external-link"
        target="_blank"
        rel="noreferrer"
        [attr.href]="getHref(first)"
      >
        {{ first?.name }}
      </a>
    }
  `,
  styles: `
    :host-context(tui-root._mobile) *::before {
      font-size: 1.5rem !important;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiDropdown, TuiDataList],
})
export class UILaunchComponent {
  private readonly config = inject(ConfigService)

  @Input()
  pkg!: PackageDataEntry

  get interfaces(): readonly T.ServiceInterface[] {
    return this.getInterfaces(this.pkg)
  }

  get isRunning(): boolean {
    return this.pkg.status.main.status === 'running'
  }

  get first(): T.ServiceInterface | undefined {
    return this.interfaces[0]
  }

  @tuiPure
  getInterfaces(pkg?: PackageDataEntry): T.ServiceInterface[] {
    return pkg
      ? Object.values(pkg.serviceInterfaces).filter(({ type }) => type === 'ui')
      : []
  }

  getHref(info?: T.ServiceInterface): string | null {
    return info && this.isRunning
      ? this.config.launchableAddress(info, this.pkg.hosts)
      : null
  }
}
