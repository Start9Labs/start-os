import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  input,
  inject,
} from '@angular/core'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiSkeleton, TuiSwitch, TuiTooltip } from '@taiga-ui/kit'
import { FormsModule } from '@angular/forms'
import { i18nPipe, LoadingService, ErrorService } from '@start9labs/shared'
import { TuiCell } from '@taiga-ui/layout'
import { InterfaceGateway } from './interface.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { InterfaceComponent } from './interface.component'

@Component({
  selector: 'section[gateways]',
  template: `
    <header>{{ 'Gateways' | i18n }}</header>
    @for (gateway of gateways(); track $index) {
      <label tuiCell="s" [style.background]="">
        <span tuiTitle [style.opacity]="1">{{ gateway.ipInfo.name }}</span>
        @if (!interface.packageId() && !gateway.public) {
          <tui-icon
            [tuiTooltip]="
              'Cannot disable private gateways for StartOS UI' | i18n
            "
          />
        }
        <input
          type="checkbox"
          tuiSwitch
          size="s"
          [showIcons]="false"
          [ngModel]="gateway.enabled"
          (ngModelChange)="onToggle(gateway)"
          [disabled]="!interface.packageId() && !gateway.public"
        />
      </label>
    } @empty {
      @for (_ of [0, 1]; track $index) {
        <label tuiCell="s">
          <span tuiTitle [tuiSkeleton]="true">{{ 'Loading' | i18n }}</span>
        </label>
      }
    }
  `,
  styles: `
    :host {
      grid-column: span 2;

      &:has(+ section table) header {
        background: transparent;
      }
    }

    [tuiCell]:has([tuiTooltip]) {
      background: none !important;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    FormsModule,
    TuiSwitch,
    i18nPipe,
    TuiCell,
    TuiTitle,
    TuiSkeleton,
    TuiIcon,
    TuiTooltip,
  ],
})
export class InterfaceGatewaysComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  readonly interface = inject(InterfaceComponent)

  readonly gateways = input.required<InterfaceGateway[] | undefined>()

  async onToggle(gateway: InterfaceGateway) {
    const addressInfo = this.interface.value()!.addressInfo
    const pkgId = this.interface.packageId()

    const loader = this.loader.open().subscribe()

    try {
      if (pkgId) {
        await this.api.pkgBindingToggleGateway({
          gateway: gateway.id,
          enabled: !gateway.enabled,
          internalPort: addressInfo.internalPort,
          host: addressInfo.hostId,
          package: pkgId,
        })
      } else {
        await this.api.serverBindingToggleGateway({
          gateway: gateway.id,
          enabled: !gateway.enabled,
          internalPort: 80,
        })
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
