import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  inject,
  Input,
} from '@angular/core'
import { ErrorService, i18nPipe, LoadingService } from '@start9labs/shared'
import { TuiButton, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiBadge, TuiFade } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import {
  FormComponent,
  FormContext,
} from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { Wifi, WiFiForm } from './utils'
import SystemWifiComponent from './wifi.component'
import { wifiSpec } from './wifi.const'

@Component({
  selector: '[wifi]',
  template: `
    @for (network of wifi; track $index) {
      @if (network.ssid) {
        <button
          tuiCell
          [disabled]="network.connected"
          (click)="prompt(network)"
        >
          <div tuiTitle>
            <strong tuiFade>
              {{ network.ssid }}
              @if (network.connected) {
                <tui-badge appearance="positive">
                  {{ 'Connected' | i18n }}
                </tui-badge>
              }
            </strong>
          </div>
          @if (network.connected !== undefined) {
            <button
              tuiIconButton
              size="s"
              appearance="icon"
              iconStart="@tui.trash-2"
              (click.stop)="forget(network)"
            >
              {{ 'Forget' | i18n }}
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
        </button>
      }
    }
  `,
  styles: `
    :host {
      align-items: stretch;
      padding: 0.5rem !important;
    }

    [tuiCell] {
      padding-inline: 1rem !important;

      &:disabled > * {
        opacity: 1;
      }
    }

    tui-icon {
      width: 2rem;
      color: var(--tui-text-tertiary);
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiCell,
    TuiTitle,
    TuiBadge,
    TuiButton,
    TuiIcon,
    TuiFade,
    i18nPipe,
  ],
})
export class WifiTableComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)
  private readonly component = inject(SystemWifiComponent)
  private readonly cdr = inject(ChangeDetectorRef)
  private readonly i18n = inject(i18nPipe)

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
    const loader = this.loader.open('Deleting').subscribe()

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
      this.formDialog.open<FormContext<WiFiForm>>(FormComponent, {
        label: 'Password Needed',
        data: {
          spec: wifiSpec.spec,
          buttons: [
            {
              text: this.i18n.transform('Connect')!,
              handler: async ({ ssid, password }) =>
                this.component.saveAndConnect(ssid, password),
            },
          ],
        },
      })
    }
  }
}
