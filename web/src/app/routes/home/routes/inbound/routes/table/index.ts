import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable } from '@taiga-ui/addon-table'
import {
  TuiButton,
  TuiDataListComponent,
  TuiDropdownContent,
  TuiDropdownDirective,
  TuiDropdownOpen,
  TuiLink,
  TuiOption,
  TuiTitle,
} from '@taiga-ui/core'
import { TUI_CONFIRM, TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { Help } from 'src/app/directives/help'
import { Placeholder } from 'src/app/routes/home/components/placeholder'
import { ADD_SERVER } from 'src/app/routes/home/routes/inbound/dialog'
import {
  Inbound,
  InboundService,
} from 'src/app/routes/home/routes/inbound/service'

import { InboundAside } from './aside'

@Component({
  template: `
    <inbound-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Inbound VPNs (Servers)</h2></hgroup>
      <aside tuiAccessories>
        <button tuiButton iconStart="@tui.plus" (click)="edit()">
          Add server
        </button>
      </aside>
    </header>
    <table tuiTable class="g-table" [tuiSkeleton]="!service.data()">
      <thead>
        <tr>
          <th tuiTh [sorter]="'enabled' | tuiSorter" [style.width.rem]="3"></th>
          <th tuiTh [sorter]="'label' | tuiSorter">Label</th>
          <th tuiTh [sorter]="'profile' | tuiSorter">Security Profile</th>
          <th tuiTh [sorter]="'port' | tuiSorter">Port</th>
          <th tuiTh></th>
        </tr>
      </thead>
      <tbody>
        @for (item of service.data() | tuiTableSort; track item.id) {
          <tr>
            <td tuiTd>{{ item.enabled ? '🟢' : '⚪' }}</td>
            <td tuiTd>
              <a tuiLink [routerLink]="item.id">
                <b>{{ item.label }}</b>
              </a>
            </td>
            <td tuiTd>
              <a tuiLink>{{ item.loh || 'Default' }}</a>
            </td>
            <td tuiTd>{{ item.port }}</td>
            <td tuiTd>
              <button
                tuiIconButton
                size="xs"
                iconStart="@tui.ellipsis-vertical"
                appearance="icon"
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
                    (click)="toggleEnabled(item.id)"
                  >
                    {{ item.enabled ? 'Disable' : 'Enable' }}
                  </button>
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
                    (click)="delete(item.id)"
                  >
                    Delete
                  </button>
                </tui-data-list>
              </button>
            </td>
          </tr>
        } @empty {
          <tr>
            <td tuiTd colspan="4">
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
    Help,
    InboundAside,
    TuiTable,
    TuiSkeleton,
    Placeholder,
    TuiLink,
    RouterLink,
    TuiDataListComponent,
    TuiDropdownContent,
    TuiDropdownDirective,
    TuiDropdownOpen,
    TuiOption,
  ],
})
export default class InboundTable {
  protected readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly service = inject(InboundService)

  delete(id: string) {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.service.save(
          this.service.data()?.filter(item => item.id !== id) || [],
        )
      })
  }

  toggleEnabled(id: string) {
    this.service.save(
      this.service
        .data()
        ?.map(item =>
          item.id === id ? { ...item, enabled: !item.enabled } : item,
        ) || [],
    )
  }

  edit(data?: Inbound) {
    const value = this.service.data() || []

    this.dialogs
      .open<Inbound>(ADD_SERVER, {
        label: data ? 'Edit Inbound VPN' : 'Add Inbound VPN',
        data,
      })
      .subscribe(response => {
        this.service.save(
          data
            ? value.map(item => (item === data ? response : item))
            : value.concat(response),
        )
      })
  }
}
