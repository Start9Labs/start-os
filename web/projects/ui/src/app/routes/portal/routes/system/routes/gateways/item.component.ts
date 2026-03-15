import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { DialogService, ErrorService, i18nPipe } from '@start9labs/shared'
import { ISB } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiIcon,
  TuiInput,
} from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { GatewayPlus } from 'src/app/services/gateway.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { PORT_FORWARDS_MODAL } from './port-forwards.component'

@Component({
  selector: 'tr[gateway]',
  template: `
    @if (gateway(); as gateway) {
      <td>
        @switch (gateway.ipInfo.deviceType) {
          @case ('ethernet') {
            <tui-icon icon="@tui.ethernet-port" />
          }
          @case ('wireless') {
            <tui-icon icon="@tui.wifi" />
          }
          @case ('wireguard') {
            <tui-icon icon="@tui.shield" />
          }
        }
        {{ gateway.name }}
      </td>
      <td>
        @if (gateway.type === 'outbound-only') {
          {{ 'Outbound Only' | i18n }}
        } @else {
          {{ 'Inbound/Outbound' | i18n }}
        }
      </td>
      <td class="lan">{{ gateway.lanIpv4.join(', ') || '-' }}</td>
      <td
        class="wan"
        [style.color]="
          gateway.ipInfo.wanIp ? undefined : 'var(--tui-text-warning)'
        "
      >
        {{ gateway.ipInfo.wanIp || ('Error' | i18n) }}
      </td>
      <td>
        <button
          tuiIconButton
          tuiDropdown
          size="s"
          appearance="flat-grayscale"
          iconStart="@tui.ellipsis-vertical"
          [tuiAppearanceState]="open ? 'hover' : null"
          [(tuiDropdownOpen)]="open"
        >
          {{ 'More' | i18n }}
          <tui-data-list *tuiDropdown>
            <button tuiOption (click)="rename()">
              {{ 'Rename' | i18n }}
            </button>
            @if (gateway.type !== 'outbound-only') {
              <button tuiOption (click)="viewPortForwards()">
                {{ 'View port forwards' | i18n }}
              </button>
            }
            @if (gateway.ipInfo.deviceType === 'wireguard') {
              <button tuiOption class="g-negative" (click)="remove()">
                {{ 'Delete' | i18n }}
              </button>
            }
          </tui-data-list>
        </button>
      </td>
    }
  `,
  styles: `
    tui-icon {
      font-size: 1.3rem;
      margin-right: 0.7rem;
    }

    td:first-child {
      width: 24rem;
    }

    td:last-child {
      text-align: right;
    }

    :host-context(tui-root._mobile) {
      td {
        width: auto !important;
        align-content: center;
      }

      td:first-child {
        font: var(--tui-typography-body-m);
        font-weight: bold;
        color: var(--tui-text-primary);
      }

      td:nth-child(2) {
        grid-area: 2 / 1 / 2 / 3;
      }

      td:nth-child(3),
      td:nth-child(4) {
        grid-area: auto / 1 / auto / 3;

        &::before {
          color: var(--tui-text-primary);
        }
      }

      td:nth-child(3)::before {
        content: 'LAN IP: ';
      }

      td:nth-child(4)::before {
        content: 'WAN IP: ';
      }

      td:last-child {
        grid-area: 1 / 3 / 6;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiDropdown, TuiDataList, TuiIcon, TuiInput, i18nPipe],
})
export class GatewaysItemComponent {
  private readonly dialog = inject(DialogService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)
  private readonly i18n = inject(i18nPipe)

  readonly gateway = input.required<GatewayPlus>()

  open = false

  viewPortForwards() {
    const { id, name } = this.gateway()
    this.dialog
      .openComponent(PORT_FORWARDS_MODAL, {
        label: 'Port Forwards',
        size: 'l',
        data: { gatewayId: id, gatewayName: name },
      })
      .subscribe()
  }

  remove() {
    this.dialog
      .openConfirm({ label: 'Are you sure?', size: 's' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open('Deleting').subscribe()

        try {
          await this.api.removeTunnel({ id: this.gateway().id })
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }

  async rename() {
    const { id, name } = this.gateway()
    const renameSpec = ISB.InputSpec.of({
      label: ISB.Value.text({
        name: this.i18n.transform('Name'),
        required: true,
        default: name,
      }),
    })

    this.formDialog.open(FormComponent, {
      label: 'Rename',
      data: {
        spec: await configBuilderToSpec(renameSpec),
        buttons: [
          {
            text: 'Save',
            handler: (value: typeof renameSpec._TYPE) =>
              this.update(id, value.label),
          },
        ],
      },
    })
  }

  private async update(id: string, name: string): Promise<boolean> {
    const loader = this.loader.open('Saving').subscribe()

    try {
      await this.api.updateTunnel({ id, name })
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }
}
