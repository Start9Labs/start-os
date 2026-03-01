import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton, TuiLink, TuiTitle } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Placeholder } from 'src/app/components/placeholder'
import { ADD_CLIENT } from 'src/app/routes/outbound/dialog'
import { OutboundService } from 'src/app/routes/outbound/service'

@Component({
  template: `
    <header tuiHeader>
      <hgroup tuiTitle><h2>Outbound VPNs</h2></hgroup>
      <aside tuiAccessories>
        @if (service.data()) {
          <button tuiButton iconStart="@tui.plus" (click)="add()">Add</button>
        }
      </aside>
    </header>
    <table tuiTable size="m" class="g-table" [tuiSkeleton]="!service.data()">
      <thead tuiThead>
        <tr>
          <th tuiTh [sorter]="'enabled' | tuiSorter" [style.width.rem]="3"></th>
          <th tuiTh [sorter]="'label' | tuiSorter">Label</th>
          <th tuiTh [sorter]="'target' | tuiSorter">Connects to</th>
          <th tuiTh>Used by</th>
        </tr>
      </thead>
      <tbody>
        @for (item of service.data() | tuiTableSort; track item.id) {
          <tr>
            <td tuiTd>{{ item.enabled ? '🟢' : '⚪' }}</td>
            <td tuiTd>
              <a tuiLink routerLink="vpn" [queryParams]="{ id: item.id }">
                <b>{{ item.label }}</b>
              </a>
            </td>
            <td tuiTd>{{ item.target }}</td>
            <td tuiTd>
              @if (item.usedBy) {
                <button tuiLink iconStart="@tui.scroll-text">
                  {{ item.usedBy }}
                </button>
              } @else {
                -
              }
            </td>
          </tr>
        } @empty {
          <tr>
            <td tuiTd colspan="4">
              <app-placeholder icon="@tui.hat-glasses">
                No Outbound VPN Clients configured
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
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  imports: [
    RouterLink,
    TuiHeader,
    TuiTitle,
    TuiTable,
    TuiButton,
    TuiLink,
    TuiSkeleton,
    Placeholder,
  ],
})
export default class OutboundTable {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly service = inject(OutboundService)

  protected add() {
    const existing = this.service.data() ?? []
    const data = ['Internet', ...existing.map(v => v.label)]

    this.dialogs
      .open<any>(ADD_CLIENT, { label: 'Add Outbound VPN', data })
      .subscribe(async ({ label, target, config }) => {
        await this.service.create({ label, target, config })
      })
  }
}
