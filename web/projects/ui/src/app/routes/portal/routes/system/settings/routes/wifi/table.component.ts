import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  inject,
  Input,
} from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions } from '@taiga-ui/core'
import {
  TuiBadgeModule,
  TuiButtonModule,
  TuiCellModule,
  TuiIconModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'
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
    <ng-container *ngFor="let network of wifi">
      <div *ngIf="network.ssid" tuiCell [style.padding]="0">
        <div tuiTitle>
          <strong>
            {{ network.ssid }}
            <tui-badge
              *ngIf="network.connected"
              appearance="success"
              [dot]="true"
            >
              Connected
            </tui-badge>
          </strong>
        </div>
        <button
          *ngIf="!network.connected"
          tuiButton
          size="xs"
          appearance="opposite"
          (click)="prompt(network)"
        >
          Connect
        </button>
        <button
          *ngIf="network.connected !== undefined; else strength"
          tuiIconButton
          size="s"
          appearance="icon"
          iconLeft="tuiIconTrash2"
          (click)="forget(network)"
        >
          Forget
        </button>
        <ng-template #strength>
          <tui-icon
            [style.width.rem]="2"
            [icon]="network.security.length ? 'tuiIconLock' : 'tuiIconUnlock'"
          />
        </ng-template>
        <img
          [src]="getSignal(network.strength)"
          [style.width.rem]="2"
          alt="Signal Strength: {{ network.strength }}"
        />
      </div>
    </ng-container>
  `,
  host: { style: 'align-items: stretch' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiCellModule,
    TuiTitleModule,
    TuiBadgeModule,
    TuiButtonModule,
    TuiIconModule,
  ],
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

  getSignal(signal: number): string {
    if (signal < 5) {
      return 'assets/img/icons/wifi-0.png'
    }

    if (signal >= 5 && signal < 50) {
      return 'assets/img/icons/wifi-1.png'
    }

    if (signal >= 50 && signal < 90) {
      return 'assets/img/icons/wifi-2.png'
    }

    return 'assets/img/icons/wifi-3.png'
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
