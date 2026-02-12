import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import {
  DialogService,
  ErrorService,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { ISB } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiIcon,
  TuiOptGroup,
  TuiTextfield,
} from '@taiga-ui/core'
import { filter } from 'rxjs'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { GatewayPlus } from 'src/app/services/gateway.service'
import { TuiBadge } from '@taiga-ui/kit'

@Component({
  selector: 'tr[gateway]',
  template: `
    @if (gateway(); as gateway) {
      <td>
        {{ gateway.name }}
        @if (gateway.isDefaultOutbound) {
          <span tuiBadge tuiStatus appearance="positive">Default outbound</span>
        }
      </td>
      <td>
        @switch (gateway.ipInfo.deviceType) {
          @case ('ethernet') {
            <tui-icon icon="@tui.cable" />
            {{ 'Ethernet' | i18n }}
          }
          @case ('wireless') {
            <tui-icon icon="@tui.wifi" />
            {{ 'WiFi' | i18n }}
          }
          @case ('wireguard') {
            <tui-icon icon="@tui.shield" />
            {{ 'WireGuard' | i18n }}
          }
          @default {
            {{ gateway.ipInfo.deviceType }}
          }
        }
      </td>
      <td>
        @if (gateway.type === 'outbound-only') {
          <tui-icon icon="@tui.arrow-up-right" />
          {{ 'Outbound Only' | i18n }}
        } @else {
          <tui-icon icon="@tui.arrow-left-right" />
          {{ 'Inbound/Outbound' | i18n }}
        }
      </td>
      <td
        class="wan"
        [style.color]="
          gateway.ipInfo.wanIp ? undefined : 'var(--tui-text-warning)'
        "
      >
        {{ gateway.ipInfo.wanIp || ('Error' | i18n) }}
      </td>
      <td class="lan">{{ gateway.lanIpv4.join(', ') || '-' }}</td>
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
          <tui-data-list *tuiTextfieldDropdown>
            <tui-opt-group>
              <button tuiOption new iconStart="@tui.pencil" (click)="rename()">
                {{ 'Rename' | i18n }}
              </button>
            </tui-opt-group>
            @if (!gateway.isDefaultOutbound) {
              <tui-opt-group>
                <button
                  tuiOption
                  new
                  iconStart="@tui.arrow-up-right"
                  (click)="setDefaultOutbound()"
                >
                  {{ 'Set as Default Outbound' | i18n }}
                </button>
              </tui-opt-group>
            }
            @if (gateway.ipInfo.deviceType === 'wireguard') {
              <tui-opt-group>
                <button
                  tuiOption
                  new
                  iconStart="@tui.trash"
                  class="g-negative"
                  (click)="remove()"
                >
                  {{ 'Delete' | i18n }}
                </button>
              </tui-opt-group>
            }
          </tui-data-list>
        </button>
      </td>
    }
  `,
  styles: `
    td:last-child {
      grid-area: 1 / 3 / 7;
      align-self: center;
      text-align: right;
    }

    :host-context(tui-root._mobile) {
      grid-template-columns: min-content 1fr min-content;

      .name {
        grid-column: span 2;
      }

      .connection {
        grid-column: span 2;
        order: -1;
      }

      .type {
        grid-column: span 2;
      }

      .lan,
      .wan {
        grid-column: span 2;

        &::before {
          content: 'LAN IP: ';
          color: var(--tui-text-primary);
        }
      }

      .wan::before {
        content: 'WAN IP: ';
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiButton,
    TuiDropdown,
    TuiDataList,
    TuiIcon,
    TuiOptGroup,
    TuiTextfield,
    i18nPipe,
    TuiBadge,
  ],
})
export class GatewaysItemComponent {
  private readonly dialog = inject(DialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)
  private readonly i18n = inject(i18nPipe)

  readonly gateway = input.required<GatewayPlus>()

  open = false

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

  async setDefaultOutbound() {
    const loader = this.loader.open().subscribe()

    try {
      await this.api.setDefaultOutbound({ gateway: this.gateway().id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
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
