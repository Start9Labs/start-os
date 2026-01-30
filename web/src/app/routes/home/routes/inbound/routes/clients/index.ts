import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { ReactiveFormsModule } from '@angular/forms'
import { ActivatedRoute, RouterLink } from '@angular/router'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable } from '@taiga-ui/addon-table'
import { TuiButton, TuiLink, TuiTitle } from '@taiga-ui/core'
import { TUI_CONFIRM, TuiSkeleton } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { Help } from 'src/app/directives/help'
import { Placeholder } from 'src/app/routes/home/components/placeholder'

import { ADD_CLIENT } from './dialog'
import { Client, InboundService } from '../../service'
import { InboundClientsAside } from './aside'

@Component({
  template: `
    <inbound-clients-aside *help />
    <header tuiHeader>
      <hgroup tuiTitle>
        <h2>
          <a
            tuiLink
            routerLink=".."
            appearance=""
            iconStart="@tui.chevron-left"
            [style.font]="'inherit'"
            [style.text-decoration]="'none'"
          >
            Client devices
            <span [tuiSkeleton]="!client()">({{ client()?.label }})</span>
          </a>
        </h2>
      </hgroup>
      <aside tuiAccessories>
        <button tuiButton iconStart="@tui.plus" (click)="edit()">
          Add client device
        </button>
      </aside>
    </header>
    <table tuiTable class="g-table" [tuiSkeleton]="!client()">
      <thead>
        <tr>
          <th tuiTh [sorter]="'name' | tuiSorter">Name</th>
          <th tuiTh [sorter]="'address' | tuiSorter">Lan IP Address</th>
          <th tuiTh></th>
        </tr>
      </thead>
      <tbody>
        @for (item of client()?.clients | tuiTableSort; track $index) {
          <tr>
            <td tuiTd>{{ item.name }}</td>
            <td tuiTd>{{ item.address }}</td>
            <td tuiTd>
              <button
                tuiIconButton
                size="xs"
                iconStart="@tui.pencil"
                appearance="icon"
                (click)="edit(item)"
              >
                Edit
              </button>
              <button
                tuiIconButton
                size="xs"
                iconStart="@tui.trash"
                appearance="icon"
                (click)="delete($index)"
              >
                Delete
              </button>
            </td>
          </tr>
        } @empty {
          <tr>
            <td tuiTd colspan="3">
              <app-placeholder icon="@tui.monitor-smartphone">
                No clients configured
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

    span {
      display: inline-block;
      min-width: 4rem;
      border-radius: var(--tui-radius-xs);
    }

    td:last-child {
      text-align: end;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  imports: [
    RouterLink,
    ReactiveFormsModule,
    TuiHeader,
    TuiTitle,
    TuiLink,
    TuiButton,
    TuiTable,
    TuiSkeleton,
    Help,
    Placeholder,
    InboundClientsAside,
  ],
})
export default class InboundClients {
  private readonly id = inject(ActivatedRoute).snapshot.params['id']
  private readonly service = inject(InboundService)
  private readonly dialogs = inject(TuiResponsiveDialogService)

  protected readonly client = computed(() =>
    this.service.data()?.find(item => item.id === this.id),
  )

  edit(data?: Client) {
    const value = this.service.data() || []

    this.dialogs
      .open<Client>(ADD_CLIENT, {
        label: data ? 'Edit client device' : 'Add client device',
        data,
      })
      .subscribe(response => {
        this.service.save(
          value.map(item =>
            item === this.client()
              ? {
                  ...item,
                  clients: data
                    ? item.clients.map(c => (c === data ? response : c))
                    : item.clients.concat(response),
                }
              : item,
          ),
        )
      })
  }

  delete(index: number) {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?' })
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.service.save(
          this.service.data()?.map(item =>
            item.id === this.client()?.id
              ? {
                  ...item,
                  clients:
                    this.client()?.clients.filter((_, i) => i !== index) || [],
                }
              : item,
          ) || [],
        )
      })
  }
}
