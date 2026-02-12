import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  inject,
  Input,
} from '@angular/core'
import { NgTemplateOutlet } from '@angular/common'
import {
  DialogService,
  ErrorService,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { filter } from 'rxjs'
import { IST } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiIcon,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
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
    <ng-template #row let-network>
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
      <tui-icon
        [icon]="network.security.length ? '@tui.lock' : '@tui.lock-open'"
      />
      <div tuiTitle>
        <strong tuiFade>
          {{ network.ssid }}
        </strong>
      </div>
      @if (network.connected) {
        <tui-badge appearance="positive">
          {{ 'Connected' | i18n }}
        </tui-badge>
      }
      @if (network.connected === false) {
        <button
          tuiIconButton
          tuiDropdown
          size="s"
          appearance="flat-grayscale"
          iconStart="@tui.ellipsis-vertical"
          [(tuiDropdownOpen)]="open"
        >
          {{ 'More' | i18n }}
          <tui-data-list *tuiTextfieldDropdown>
            <button
              tuiOption
              new
              iconStart="@tui.wifi"
              (click)="prompt(network)"
            >
              {{ 'Connect' | i18n }}
            </button>
            <button
              tuiOption
              new
              iconStart="@tui.trash"
              class="g-negative"
              (click)="forget(network)"
            >
              {{ 'Forget' | i18n }}
            </button>
          </tui-data-list>
        </button>
      }
    </ng-template>
    @for (network of wifi; track $index) {
      @if (network.ssid) {
        @if (network.connected === undefined) {
          <button tuiCell (click)="prompt(network)">
            <ng-container
              *ngTemplateOutlet="row; context: { $implicit: network }"
            />
          </button>
        } @else {
          <div tuiCell>
            <ng-container
              *ngTemplateOutlet="row; context: { $implicit: network }"
            />
          </div>
        }
      }
    }
  `,
  styles: `
    :host {
      align-items: stretch;
      padding: 0.5rem !important;
    }

    [tuiCell] {
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
  imports: [
    NgTemplateOutlet,
    TuiCell,
    TuiTitle,
    TuiBadge,
    TuiButton,
    TuiIcon,
    TuiFade,
    TuiDropdown,
    TuiDataList,
    TuiTextfield,
    i18nPipe,
  ],
})
export class WifiTableComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly dialogs = inject(DialogService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)
  private readonly component = inject(SystemWifiComponent)
  private readonly cdr = inject(ChangeDetectorRef)
  private readonly i18n = inject(i18nPipe)

  @Input()
  wifi: readonly Wifi[] = []

  open = false

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
      this.dialogs
        .openConfirm({
          label: `${this.i18n.transform('Connect to')} ${network.ssid}?`,
          size: 's',
        })
        .pipe(filter(Boolean))
        .subscribe(() => this.component.saveAndConnect(network.ssid))
    } else {
      const ssid = wifiSpec.spec['ssid'] as IST.ValueSpecText
      const spec: IST.InputSpec = {
        ...wifiSpec.spec,
        ssid: { ...ssid, disabled: 'ssid', default: network.ssid },
      }

      this.formDialog.open<FormContext<WiFiForm>>(FormComponent, {
        label: 'Password needed',
        data: {
          spec,
          value: { ssid: network.ssid, password: '' },
          buttons: [
            {
              text: this.i18n.transform('Connect')!,
              handler: async ({ password }) =>
                this.component.saveAndConnect(network.ssid, password),
            },
          ],
        },
      })
    }
  }
}
