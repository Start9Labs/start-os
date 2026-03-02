import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable } from '@taiga-ui/addon-table'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiLink,
  TuiTitle,
} from '@taiga-ui/core'
import { TUI_CONFIRM, TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { Placeholder } from 'src/app/components/placeholder'
import { ADD_SERVER, ServerDialogResult } from 'src/app/routes/inbound/dialog'
import { InboundService, VpnServer } from 'src/app/routes/inbound/service'

@Component({
  template: `
    <header tuiHeader>
      <hgroup tuiTitle><h2>Inbound VPNs</h2></hgroup>
      <aside tuiAccessories>
        <button tuiButton iconStart="@tui.plus" (click)="edit()">Add</button>
      </aside>
    </header>
    <table tuiTable class="g-table" [tuiSkeleton]="!service.data()">
      <thead>
        <tr>
          <th tuiTh [sorter]="'enabled' | tuiSorter" [style.width.rem]="3"></th>
          <th tuiTh [sorter]="'label' | tuiSorter">Label</th>
          <th tuiTh [sorter]="'endpoint' | tuiSorter">Endpoint</th>
          <th tuiTh [sorter]="'listen_port' | tuiSorter">Port</th>
          <th tuiTh [sorter]="'profile' | tuiSorter">Security Profile</th>
          <th tuiTh></th>
        </tr>
      </thead>
      <tbody>
        @for (item of service.data() | tuiTableSort; track item.profile) {
          <tr>
            <td tuiTd>{{ item.enabled ? '🟢' : '⚪' }}</td>
            <td tuiTd>
              <a
                tuiLink
                routerLink="client"
                [queryParams]="{ port: item.listen_port.toString() }"
              >
                <b>{{ item.label }}</b>
              </a>
            </td>
            <td tuiTd>{{ item.endpoint }}</td>
            <td tuiTd>{{ item.listen_port }}</td>
            <td tuiTd>
              <a tuiLink routerLink="/profiles">{{ item.profile }}</a>
            </td>
            <td tuiTd>
              <button
                tuiIconButton
                size="xs"
                iconStart="@tui.ellipsis-vertical"
                appearance="icon"
                tuiDropdownAlign="end"
                tuiDropdownAuto
                tuiDropdown
              >
                Actions
                <tui-data-list
                  *tuiDropdown="let close"
                  size="s"
                  (click)="close()"
                >
                  <button
                    tuiOption
                    [iconStart]="
                      item.enabled ? '@tui.circle-pause' : '@tui.circle-play'
                    "
                    (click)="toggleEnabled(item)"
                  >
                    {{ item.enabled ? 'Disable' : 'Enable' }}
                  </button>
                  <a
                    tuiOption
                    iconStart="@tui.monitor-smartphone"
                    [routerLink]="item.listen_port.toString()"
                  >
                    Manage clients
                  </a>
                  <button
                    tuiOption
                    iconStart="@tui.pencil"
                    (click)="edit(item)"
                  >
                    Edit
                  </button>
                  <button
                    tuiOption
                    class="g-negative"
                    iconStart="@tui.trash"
                    (click)="delete(item.profile)"
                  >
                    Delete
                  </button>
                </tui-data-list>
              </button>
            </td>
          </tr>
        } @empty {
          <tr>
            <td tuiTd colspan="6">
              <app-placeholder icon="@tui.hard-drive-download">
                No Inbound VPN Servers configured
              </app-placeholder>
            </td>
          </tr>
        }
      </tbody>
    </table>
  `,
  styles: `
    :host {
      max-width: 50rem;
    }

    td:first-child {
      text-align: center;
    }

    td:last-child {
      text-align: end;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  imports: [
    TuiHeader,
    TuiTitle,
    TuiButton,
    TuiTable,
    TuiSkeleton,
    Placeholder,
    TuiLink,
    RouterLink,
    TuiDataList,
    TuiDropdown,
  ],
})
export default class InboundTable {
  protected readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly service = inject(InboundService)

  delete(profile: string) {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.service.deleteServer(profile)
      })
  }

  toggleEnabled(item: VpnServer) {
    this.service.setServer(item.profile, {
      label: item.label,
      enabled: !item.enabled,
      listen_port: item.listen_port,
      endpoint: item.endpoint,
    })
  }

  edit(data?: VpnServer) {
    this.dialogs
      .open<ServerDialogResult>(ADD_SERVER, {
        label: data ? 'Edit Inbound VPN' : 'Add Inbound VPN',
        data: {
          server: data,
          profiles: ['Admin', 'Guest'],
          usedPorts: (this.service.data() ?? [])
            .filter(s => s.listen_port !== data?.listen_port)
            .map(s => s.listen_port),
        },
      })
      .subscribe(response => {
        this.service.setServer(response.profile, {
          label: response.label,
          enabled: response.enabled,
          listen_port: response.listen_port,
          endpoint: response.endpoint,
        })
      })
  }
}
