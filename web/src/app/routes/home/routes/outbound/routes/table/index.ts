import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton, TuiLink, TuiTitle } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Help } from 'src/app/directives/help'
import { Placeholder } from 'src/app/routes/home/components/placeholder'
import { ADD } from 'src/app/routes/home/routes/outbound/dialog'
import { OutboundService } from 'src/app/routes/home/routes/outbound/service'

import { OutboundAside } from './aside'

@Component({
  template: `
    <outbound-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle><h2>Outbound VPNs (Clients)</h2></hgroup>
      <aside tuiAccessories>
        @if (!loading()) {
          <button tuiButton iconStart="@tui.plus" (click)="add()">Add</button>
        }
      </aside>
    </header>
    <table tuiTable size="m" class="g-table" [tuiSkeleton]="loading()">
      <thead tuiThead>
        <tr>
          <th tuiTh>Status</th>
          <th tuiTh>Label</th>
          <th tuiTh>Connects to</th>
          <th tuiTh>Used by</th>
        </tr>
      </thead>
      <tbody>
        @for (item of service.data(); track item.id) {
          <tr>
            <td tuiTd class="status">
              <i [class.g-positive]="item.enabled"></i>
            </td>
            <td tuiTd>
              <a tuiLink [routerLink]="item.id">
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
              <app-placeholder icon="@tui.globe-lock">
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
      padding-top: 0;
    }

    i {
      display: block;
      width: 0.5rem;
      height: 0.5rem;
      margin: 0 0.75rem;
      border-radius: 100%;
      background: currentColor;
      color: var(--tui-text-negative);
    }

    .status {
      width: 5rem;
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
    Help,
    OutboundAside,
    Placeholder,
  ],
})
export default class OutboundTable {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly service = inject(OutboundService)

  protected readonly loading = computed(() => !this.service.data())

  protected add() {
    const existingVpns = this.service.data() ?? []
    const targetOptions = ['Internet', ...existingVpns.map(v => v.label)]

    this.dialogs
      .open<any>(ADD, {
        label: 'Add Outbound VPN (Client)',
        size: 'm',
        data: { targetOptions },
      })
      .subscribe(async ({ label, target, config }) => {
        await this.service.create({ label, target, config })
      })
  }
}
