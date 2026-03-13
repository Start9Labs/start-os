import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { RouterLink } from '@angular/router'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton, TuiHint, TuiLink, TuiTitle } from '@taiga-ui/core'
import { TUI_CONFIRM, TuiSkeleton, TuiSwitch } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { Placeholder } from 'src/app/components/placeholder'
import { ADD_CLIENT } from 'src/app/routes/outbound/dialog'
import { OutboundService } from 'src/app/routes/outbound/service'
import { OutboundVpn } from 'src/app/services/api/api.service'

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
          <th tuiTh [sorter]="'label' | tuiSorter">Label</th>
          <th tuiTh [sorter]="'target' | tuiSorter">Connects to</th>
          <th tuiTh>Used by</th>
          <th tuiTh [sorter]="'enabled' | tuiSorter">Enable</th>
        </tr>
      </thead>
      <tbody>
        @for (item of service.data() | tuiTableSort; track item.id) {
          <tr>
            <td tuiTd>
              <a tuiLink routerLink="vpn" [queryParams]="{ id: item.id }">
                <b>{{ item.label }}</b>
              </a>
            </td>
            <td tuiTd>{{ item.target }}</td>
            <td tuiTd>
              @if (item.used_by.length) {
                <a tuiLink routerLink="/profiles">
                  {{ item.used_by.join(', ') }}
                </a>
              } @else {
                -
              }
            </td>
            <td tuiTd>
              <input
                type="checkbox"
                tuiSwitch
                size="s"
                [showIcons]="false"
                [ngModel]="item.enabled"
                [disabled]="hasDependentVpns(item)"
                [tuiHint]="getDependentVpnHint(item)"
                (click)="$event.preventDefault(); onToggle(item)"
              />
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

    td:last-child {
      text-align: center;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  imports: [
    RouterLink,
    FormsModule,
    TuiHeader,
    TuiTitle,
    TuiTable,
    TuiButton,
    TuiHint,
    TuiLink,
    TuiSkeleton,
    TuiSwitch,
    Placeholder,
  ],
})
export default class OutboundTable {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly service = inject(OutboundService)

  protected hasDependentVpns(item: OutboundVpn): boolean {
    const allVpns = this.service.data() ?? []
    return allVpns.some(v => v.target === item.label)
  }

  protected getDependentVpnHint(item: OutboundVpn): string | null {
    const allVpns = this.service.data() ?? []
    const dependents = allVpns
      .filter(v => v.target === item.label)
      .map(v => v.label)
    if (!dependents.length) return null
    return (
      'Cannot disable: ' +
      dependents.join(', ') +
      ' ' +
      (dependents.length === 1 ? 'uses' : 'use') +
      ' this VPN as a target. Change ' +
      (dependents.length === 1 ? 'its' : 'their') +
      ' target first.'
    )
  }

  protected onToggle(item: OutboundVpn) {
    if (!item.enabled || !item.used_by.length) {
      this.service.setEnabled(item.id, !item.enabled)
      return
    }

    const profiles = item.used_by.join(', ')
    this.dialogs
      .open(TUI_CONFIRM, {
        label: 'Disable VPN?',
        data: {
          content: `The following profiles currently route through this VPN and will be switched to WAN: ${profiles}.`,
          yes: 'Disable',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.service.setEnabled(item.id, false)
      })
  }

  protected add() {
    const existing = this.service.data() ?? []
    const data = {
      targets: ['Internet', ...existing.map(v => v.label)],
      existingLabels: existing.map(v => v.label),
    }

    this.dialogs
      .open<any>(ADD_CLIENT, { label: 'Add Outbound VPN', data })
      .subscribe(async ({ label, target, config }) => {
        await this.service.create({ label, target, config })
      })
  }
}
