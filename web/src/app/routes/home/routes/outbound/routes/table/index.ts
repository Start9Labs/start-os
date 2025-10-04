import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton, TuiLink, TuiTitle } from '@taiga-ui/core'
import { TuiDialogService } from '@taiga-ui/experimental'
import { TUI_CONFIRM } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { Help } from 'src/app/directives/help.directive'
import { ADD } from 'src/app/routes/home/routes/outbound/dialog'

import { OutboundAside } from './aside'

@Component({
  template: `
    <outbound-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Outbound VPNs (Clients)</h2></hgroup>
      <aside tuiAccessories>
        <button tuiButton iconStart="@tui.plus" (click)="add()">
          Add Client
        </button>
      </aside>
    </header>
    <table tuiTable size="m" class="g-table">
      <thead tuiThead>
        <tr>
          <th tuiTh>Status</th>
          <th tuiTh>Label</th>
          <th tuiTh>Type</th>
          <th tuiTh>Connects to</th>
          <th tuiTh>Used by</th>
          <th tuiTh></th>
        </tr>
      </thead>
      <tbody>
        @for (item of data(); track $index) {
          <tr>
            <td tuiTd><i [class.g-positive]="item.status"></i></td>
            <td tuiTd>
              <a tuiLink [routerLink]="item.label">
                <b>{{ item.label }}</b>
              </a>
            </td>
            <td tuiTd>{{ item.type }}</td>
            <td tuiTd>{{ item.connects }}</td>
            <td tuiTd>
              @if (item.used) {
                <button tuiLink iconStart="@tui.scroll-text">
                  {{ item.used }}
                </button>
              }
            </td>
            <td tuiTd>
              <button
                tuiIconButton
                iconStart="@tui.trash"
                size="xs"
                appearance="icon"
                (click)="remove($index)"
              >
                Remove
              </button>
            </td>
          </tr>
        }
      </tbody>
    </table>
  `,
  styles: `
    i {
      display: block;
      width: 0.5rem;
      height: 0.5rem;
      margin: 0 0.75rem;
      border-radius: 100%;
      background: currentColor;
      color: var(--tui-text-negative);
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    RouterLink,
    TuiHeader,
    TuiTitle,
    TuiTable,
    TuiButton,
    TuiLink,
    Help,
    OutboundAside,
  ],
})
export default class OutboundTable {
  private readonly dialogs = inject(TuiDialogService)

  protected readonly data = signal([
    {
      status: true,
      label: 'Proton',
      type: 'WireGuard',
      connects: 'Internet',
      used: 'Admin',
    },
    {
      status: true,
      label: 'Mullvad',
      type: 'OpenVPN',
      connects: 'Proton',
      used: 'Child',
    },
    {
      status: false,
      label: 'NordVPN',
      type: 'OpenVPN',
      connects: 'Internet',
      used: '',
    },
  ])

  protected add() {
    this.dialogs
      .open<any>(ADD, {
        label: 'Add Outbound VPN (Client)',
        closable: false,
        dismissible: false,
        size: 's',
      })
      .subscribe(({ label, type, chaining, vpn }) => {
        this.data.update(data => [
          ...data,
          {
            label,
            type,
            status: false,
            connects: chaining ? vpn : 'Internet',
            used: '',
          },
        ])
      })
  }

  protected remove(index: number) {
    this.dialogs
      .open(TUI_CONFIRM, {
        label: 'Are you sure?',
        size: 's',
      })
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.data.update(data => data.filter((_, i) => i !== index))
      })
  }
}
