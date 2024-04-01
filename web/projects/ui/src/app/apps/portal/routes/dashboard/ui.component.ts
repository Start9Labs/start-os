import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { tuiPure } from '@taiga-ui/cdk'
import { TuiDataListModule, TuiHostedDropdownModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { ConfigService } from 'src/app/services/config.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  standalone: true,
  selector: 'app-ui-launch',
  template: `
    @if (interfaces.length > 1) {
      <tui-hosted-dropdown [content]="content">
        <button
          tuiIconButton
          iconLeft="tuiIconExternalLink"
          [disabled]="!isRunning"
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
  styles: `
    :host-context(tui-root._mobile) *:before {
      font-size: 1.5rem !important;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButtonModule, TuiHostedDropdownModule, TuiDataListModule],
})
export class UILaunchComponent {
  private readonly config = inject(ConfigService)

  @Input()
  pkg!: PackageDataEntry

  get interfaces(): readonly T.ServiceInterfaceWithHostInfo[] {
    return this.getInterfaces(this.pkg)
  }

  get isRunning(): boolean {
    return this.pkg.status.main.status === 'running'
  }

  @tuiPure
  getInterfaces(pkg?: PackageDataEntry): T.ServiceInterfaceWithHostInfo[] {
    return pkg
      ? Object.values(pkg.serviceInterfaces).filter(({ type }) => type === 'ui')
      : []
  }

  getHref(info?: T.ServiceInterfaceWithHostInfo): string | null {
    return info && this.isRunning ? this.config.launchableAddress(info) : null
  }
}
