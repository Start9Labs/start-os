import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { RouterLink } from '@angular/router'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiTable, TuiTableDirective } from '@taiga-ui/addon-table'
import { TuiButton, TuiDataList, TuiDropdown, TuiLink } from '@taiga-ui/core'
import {
  TUI_CONFIRM,
  TuiBadge,
  TuiButtonSelect,
  TuiChevron,
} from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { TuiSorterPipe } from 'src/app/pipes/sorter.pipe'
import { injectFormService } from 'src/app/services/form.service'

import { EthernetPort } from './service'

@Component({
  selector: '[ethernetTable]',
  template: `
    <thead>
      <tr>
        <th tuiTh [sorter]="'name' | tuiSorter">Name</th>
        <th tuiTh [sorter]="'permissions' | tuiSorter">Permissions</th>
        <th tuiTh></th>
      </tr>
    </thead>
    <tbody>
      @for (item of ethernetTable() | tuiTableSort; track $index) {
        <tr>
          <td tuiTd>
            {{ item.name }}
            @if (item.wan) {
              <span tuiBadge size="s" appearance="primary">WAN</span>
            }
          </td>
          <td tuiTd>
            @if (item.wan) {
              -
            } @else {
              <button
                tuiLink
                tuiChevron
                tuiButtonSelect
                iconStart="@tui.user-lock"
                tuiDropdownAlign="start"
                [textContent]="item.permissions"
                [(ngModel)]="item.permissions"
                (ngModelChange)="onPermissions()"
              >
                <tui-data-list *tuiDropdown>
                  <tui-opt-group label="Profiles">
                    @for (item of permissions(); track $index) {
                      <button
                        iconStart="@tui.user-lock"
                        tuiOption
                        [value]="item"
                      >
                        {{ item }}
                      </button>
                    }
                  </tui-opt-group>
                  <tui-opt-group>
                    <a tuiOption iconStart="@tui.pencil" routerLink="/profiles">
                      Manage Profiles
                    </a>
                  </tui-opt-group>
                </tui-data-list>
              </button>
            }
          </td>
          <td tuiTd>
            @if (!item.wan) {
              <button tuiButton size="xs" (click)="makeWan($index)">
                Make WAN
              </button>
            }
          </td>
        </tr>
      }
    </tbody>
  `,
  styles: `
    :host {
      max-inline-size: 30rem;
      margin: 1rem 0;

      [tuiLink]::after {
        vertical-align: text-bottom;
      }

      td:last-child {
        text-align: end;
      }

      [tuiBadge] {
        vertical-align: baseline;
        margin: 0 0.125rem;
      }
    }

    // TODO: Remove after v5-RC.2
    a[tuiOption] ::ng-deep > *:first-child {
      display: none;
    }
  `,
  hostDirectives: [TuiTableDirective],
  host: { class: 'g-table' },
  imports: [
    TuiTable,
    TuiSorterPipe,
    TuiLink,
    TuiButtonSelect,
    FormsModule,
    TuiDataList,
    TuiDropdown,
    TuiChevron,
    TuiBadge,
    TuiButton,
    RouterLink,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class EthernetTable {
  protected readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly service = injectFormService<EthernetPort[]>()
  protected readonly permissions = signal(['Admin', 'Guest'])

  readonly ethernetTable = input<EthernetPort[]>([])

  onPermissions() {
    this.service.save([...this.ethernetTable()])
  }

  makeWan(index: number) {
    this.dialogs
      .open(TUI_CONFIRM, {
        label: 'Are you sure?',
        size: 's',
        data: { content: 'This action is not reversible.' },
      })
      .pipe(filter(Boolean))
      .subscribe(() => {
        const items = this.ethernetTable().slice()

        items.forEach(i => (i.wan = false))
        items[index].wan = true

        this.service.save(items)
      })
  }
}
