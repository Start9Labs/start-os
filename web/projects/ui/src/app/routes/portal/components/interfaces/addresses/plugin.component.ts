import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
  signal,
} from '@angular/core'
import {
  CopyService,
  DialogService,
  i18nPipe,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import {
  TuiButton,
  tuiButtonOptionsProvider,
  TuiDataList,
  TuiDropdown,
  TuiTextfield,
} from '@taiga-ui/core'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { QRModal } from 'src/app/routes/portal/modals/qr.component'
import { ActionService } from 'src/app/services/action.service'
import {
  MappedServiceInterface,
  PluginAddress,
  PluginAddressGroup,
} from '../interface.service'

@Component({
  selector: 'section[pluginGroup]',
  template: `
    <header>
      {{ pluginGroup().pluginName }}
      @if (pluginGroup().tableAction; as action) {
        <button
          tuiButton
          iconStart="@tui.plus"
          [style.margin-inline-start]="'auto'"
          (click)="runTableAction()"
        >
          {{ action.metadata.name }}
        </button>
      }
    </header>
    <table [appTable]="['Protocol', 'URL', null]">
      @for (address of pluginGroup().addresses; track $index) {
        <tr>
          <td>{{ address.hostnameInfo.ssl ? 'HTTPS' : 'HTTP' }}</td>
          <td [style.grid-area]="'2 / 1 / 2 / 2'">
            <span class="url">{{ address.url }}</span>
          </td>
          <td [style.width.rem]="5">
            <div class="desktop">
              <button
                tuiIconButton
                appearance="flat-grayscale"
                iconStart="@tui.qr-code"
                (click)="showQR(address.url)"
              >
                {{ 'Show QR' | i18n }}
              </button>
              <button
                tuiIconButton
                appearance="flat-grayscale"
                iconStart="@tui.copy"
                (click)="copyService.copy(address.url)"
              >
                {{ 'Copy URL' | i18n }}
              </button>
              @if (address.hostnameInfo.metadata.kind === 'plugin') {
                @for (
                  actionId of address.hostnameInfo.metadata.rowActions;
                  track actionId
                ) {
                  @if (pluginGroup().pluginActions[actionId]; as meta) {
                    <button
                      tuiIconButton
                      appearance="flat-grayscale"
                      iconStart="@tui.play"
                      (click)="runRowAction(actionId, meta, address)"
                    >
                      {{ meta.name }}
                    </button>
                  }
                }
              }
            </div>
            <div class="mobile">
              <button
                tuiDropdown
                tuiIconButton
                appearance="flat-grayscale"
                iconStart="@tui.ellipsis-vertical"
                [tuiAppearanceState]="open() ? 'hover' : null"
                [(tuiDropdownOpen)]="open"
              >
                {{ 'Actions' | i18n }}
                <tui-data-list *tuiTextfieldDropdown (click)="open.set(false)">
                  <button
                    tuiOption
                    new
                    iconStart="@tui.qr-code"
                    (click)="showQR(address.url)"
                  >
                    {{ 'Show QR' | i18n }}
                  </button>
                  <button
                    tuiOption
                    new
                    iconStart="@tui.copy"
                    (click)="copyService.copy(address.url)"
                  >
                    {{ 'Copy URL' | i18n }}
                  </button>
                  @if (address.hostnameInfo.metadata.kind === 'plugin') {
                    @for (
                      actionId of address.hostnameInfo.metadata.rowActions;
                      track actionId
                    ) {
                      @if (pluginGroup().pluginActions[actionId]; as meta) {
                        <button
                          tuiOption
                          new
                          iconStart="@tui.play"
                          (click)="runRowAction(actionId, meta, address)"
                        >
                          {{ meta.name }}
                        </button>
                      }
                    }
                  }
                </tui-data-list>
              </button>
            </div>
          </td>
        </tr>
      } @empty {
        <tr>
          <td colspan="3">
            <app-placeholder icon="@tui.list-x">
              {{ 'No addresses' | i18n }}
            </app-placeholder>
          </td>
        </tr>
      }
    </table>
  `,
  styles: `
    :host ::ng-deep {
      th:first-child {
        width: 5rem;
      }
    }

    .desktop {
      display: flex;
      white-space: nowrap;
    }

    .url {
      white-space: normal;
      word-break: break-all;
      display: -webkit-box;
      -webkit-box-orient: vertical;
      -webkit-line-clamp: 1;
      overflow: hidden;
    }

    .mobile {
      display: none;
    }

    :host-context(tui-root._mobile) {
      .desktop {
        display: none;
      }

      .mobile {
        display: block;
      }

      tr {
        grid-template-columns: 1fr auto;
      }

      td {
        width: auto !important;
        align-content: center;
      }
    }
  `,
  host: { class: 'g-card' },
  imports: [
    TuiButton,
    TuiDropdown,
    TuiDataList,
    TuiTextfield,
    TableComponent,
    PlaceholderComponent,
    i18nPipe,
  ],
  providers: [tuiButtonOptionsProvider({ appearance: 'icon' })],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PluginAddressesComponent {
  private readonly isMobile = inject(TUI_IS_MOBILE)
  private readonly dialog = inject(DialogService)
  private readonly actionService = inject(ActionService)
  readonly copyService = inject(CopyService)
  readonly open = signal(false)

  readonly pluginGroup = input.required<PluginAddressGroup>()
  readonly packageId = input('')
  readonly value = input<MappedServiceInterface | undefined>()

  showQR(url: string) {
    this.dialog
      .openComponent(new PolymorpheusComponent(QRModal), {
        size: 'auto',
        closeable: this.isMobile,
        data: url,
      })
      .subscribe()
  }

  runTableAction() {
    const group = this.pluginGroup()
    if (!group.tableAction || !group.pluginPkgInfo) return

    const iface = this.value()
    const prefill: Record<string, unknown> = {}

    if (iface) {
      prefill['urlPluginMetadata'] = {
        packageId: this.packageId() || null,
        hostId: iface.addressInfo.hostId,
        interfaceId: iface.id,
        internalPort: iface.addressInfo.internalPort,
      }
    }

    this.actionService.present({
      pkgInfo: group.pluginPkgInfo,
      actionInfo: group.tableAction,
      prefill,
    })
  }

  runRowAction(
    actionId: string,
    metadata: T.ActionMetadata,
    address: PluginAddress,
  ) {
    const group = this.pluginGroup()
    if (!group.pluginPkgInfo) return

    const iface = this.value()
    const prefill: Record<string, unknown> = {}

    if (iface && address.hostnameInfo.metadata.kind === 'plugin') {
      prefill['urlPluginMetadata'] = {
        packageId: this.packageId() || null,
        hostId: iface.addressInfo.hostId,
        interfaceId: iface.id,
        internalPort: iface.addressInfo.internalPort,
        hostname: address.hostnameInfo.hostname,
        port: address.hostnameInfo.port,
        ssl: address.hostnameInfo.ssl,
        public: address.hostnameInfo.public,
        info: address.hostnameInfo.metadata.info,
      }
    }

    this.actionService.present({
      pkgInfo: group.pluginPkgInfo,
      actionInfo: { id: actionId, metadata },
      prefill,
    })
  }
}
