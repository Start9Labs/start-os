import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { RouterLink } from '@angular/router'
import { TuiTable, TuiTableDirective } from '@taiga-ui/addon-table'
import { TuiDataList, TuiDropdown, TuiLink } from '@taiga-ui/core'
import { TuiBadge, TuiButtonSelect, TuiChevron } from '@taiga-ui/kit'
import { TuiSorterPipe } from 'src/app/pipes/sorter.pipe'
import { injectFormService } from 'src/app/services/form.service'

import { EthernetPort, EthernetService } from './service'

@Component({
  selector: '[ethernetTable]',
  template: `
    <thead>
      <tr>
        <th tuiTh [sorter]="'name' | tuiSorter">Port</th>
        <th tuiTh [sorter]="'profile' | tuiSorter">Security Profile</th>
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
                tuiDropdownAlign="start"
                [textContent]="item.profile"
                [(ngModel)]="item.profile"
                (ngModelChange)="onProfile()"
              >
                <tui-data-list *tuiDropdown>
                  <tui-opt-group label="Profiles">
                    @for (profile of profiles(); track $index) {
                      <button tuiOption [value]="profile">
                        {{ profile }}
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
        </tr>
      }
    </tbody>
  `,
  styles: `
    :host {
      [tuiLink]::after {
        vertical-align: text-bottom;
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
    RouterLink,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class EthernetTable {
  protected readonly service = injectFormService<
    EthernetPort[]
  >() as EthernetService

  readonly ethernetTable = input<EthernetPort[]>([])

  readonly profiles = computed(() => this.service.getProfiles())

  onProfile() {
    this.service.save([...this.ethernetTable()])
  }
}
