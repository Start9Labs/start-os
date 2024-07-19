import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  inject,
  Input,
} from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiButton, TuiDialogOptions, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiBadge } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import {
  FormComponent,
  FormContext,
} from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { Wifi, WiFiForm, wifiSpec } from './utils'
import { SettingsWifiComponent } from './wifi.component'

@Component({
  selector: '[wifi]',
  template: `
    @for (network of wifi; track $index) {
      @if (network.ssid) {
        <div tuiCell [style.padding]="0">
          <div tuiTitle>
            <strong>
              {{ network.ssid }}
              @if (network.connected) {
                <tui-badge appearance="success">Connected</tui-badge>
              }
            </strong>
          </div>
          @if (!network.connected) {
            <button
              tuiButton
              size="xs"
              appearance="opposite"
              (click)="prompt(network)"
            >
              Connect
            </button>
          }
          @if (network.connected !== undefined) {
            <button
              tuiIconButton
              size="s"
              appearance="icon"
              iconStart="@tui.trash-2"
              (click)="forget(network)"
            >
              Forget
            </button>
          } @else {
            <tui-icon
              [icon]="network.security.length ? '@tui.lock' : '@tui.lock-open'"
            />
          }
          @if (getSignal(network.strength); as signal) {
            <tui-icon
              background="@tui.wifi"
              [icon]="signal.icon"
              [style.background]="'var(--tui-background-neutral-2)'"
              [style.color]="signal.color"
            />
          } @else {
            <tui-icon icon="@tui.wifi-off" />
          }
        </div>
      }
    }
  `,
  host: { style: 'align-items: stretch' },
  styles: `
    tui-icon {
      width: 2rem;
      color: var(--tui-text-tertiary);
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiCell, TuiTitle, TuiBadge, TuiButton, TuiIcon],
})
export class WifiTableComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)
  private readonly component = inject(SettingsWifiComponent)
  private readonly cdr = inject(ChangeDetectorRef)

  @Input()
  wifi: readonly Wifi[] = []

  getSignal(signal: number) {
    if (signal < 5) {
      return null
    }

    if (signal >= 5 && signal < 50) {
      return {
        icon: '@tui.wifi-low',
        color: 'var(--tui-text-negative)',
      }
    }

    return signal >= 50 && signal < 90
      ? {
          icon: '@tui.wifi-high',
          color: 'var(--tui-status-warning)',
        }
      : {
          icon: '@tui.wifi',
          color: 'var(--tui-text-positive)',
        }
  }

  async forget({ ssid }: Wifi): Promise<void> {
    const loader = this.loader.open('Deleting...').subscribe()

    try {
      await this.api.deleteWifi({ ssid })
      this.wifi = this.wifi.filter(network => network.ssid !== ssid)
      this.cdr.markForCheck()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async prompt(network: Wifi): Promise<void> {
    if (!network.security.length) {
      await this.component.saveAndConnect(network.ssid)
    } else {
      const options: Partial<TuiDialogOptions<FormContext<WiFiForm>>> = {
        label: 'Password Needed',
        data: {
          spec: wifiSpec.spec,
          buttons: [
            {
              text: 'Connect',
              handler: async ({ ssid, password }) =>
                this.component.saveAndConnect(ssid, password),
            },
          ],
        },
      }

      this.formDialog.open(FormComponent, options)
    }
  }
}
