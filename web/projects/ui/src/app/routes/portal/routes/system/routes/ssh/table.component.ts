import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
  OnChanges,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { i18nPipe } from '@start9labs/shared'
import { TuiCheckbox, TuiFade, TuiSkeleton } from '@taiga-ui/kit'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { SSHKey } from 'src/app/services/api/api.types'

@Component({
  selector: '[keys]',
  template: `
    <table [appTable]="['Created At', 'Algorithm', 'Fingerprint']">
      <th [style.text-indent.rem]="1.75">
        <input
          tuiCheckbox
          size="s"
          type="checkbox"
          [disabled]="!keys()"
          [ngModel]="all()"
          (ngModelChange)="selected.set(($event && keys()) || [])"
        />
        {{ 'Hostname' | i18n }}
      </th>
      @for (key of keys(); track $index) {
        <tr (longtap)="!selected().length && onToggle(key)">
          <td [style.padding-left.rem]="2.5">
            <input
              tuiCheckbox
              size="s"
              type="checkbox"
              [ngModel]="selected().includes(key)"
              (ngModelChange)="onToggle(key)"
            />
            <div tuiFade class="hostname">{{ key.hostname }}</div>
          </td>
          <td class="date">{{ key.createdAt | date: 'medium' }}</td>
          <td class="algorithm">{{ key.alg }}</td>
          <td class="fingerprint" tuiFade>{{ key.fingerprint }}</td>
        </tr>
      } @empty {
        @if (keys()) {
          <tr>
            <td colspan="5">{{ 'No keys' | i18n }}</td>
          </tr>
        } @else {
          @for (i of ['', '']; track $index) {
            <tr>
              <td colspan="5">
                <div [tuiSkeleton]="true">{{ 'Loading' | i18n }}</div>
              </td>
            </tr>
          }
        }
      }
    </table>
  `,
  styles: `
    @use '@taiga-ui/core/styles/taiga-ui-local' as taiga;

    td {
      position: relative;

      &[colspan] {
        grid-column: span 2;
      }
    }

    input {
      position: absolute;
      top: 50%;
      left: 0.75rem;
      transform: translateY(-50%);
    }

    :host-context(tui-root._mobile) {
      table {
        &:has(:checked) tr {
          padding-inline-start: 2rem;
        }

        &:not(:has(:checked)) input {
          visibility: hidden;
        }
      }

      tr {
        grid-template-columns: 1fr 5rem;
        user-select: none;
      }

      input {
        left: 0.25rem;
      }

      td {
        width: 100%;

        &:first-child {
          padding: 0 !important;
        }
      }

      .hostname {
        order: 1;
        grid-column: span 2;
        font-weight: bold;
        text-transform: uppercase;
      }

      .fingerprint {
        order: 2;
        grid-column: span 2;
      }

      .date {
        order: 3;
        color: var(--tui-text-secondary);
      }

      .algorithm {
        order: 4;
        text-align: end;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    FormsModule,
    TuiCheckbox,
    TuiFade,
    TuiSkeleton,
    TableComponent,
    i18nPipe,
  ],
})
export class SSHTableComponent<T extends SSHKey> implements OnChanges {
  readonly keys = input<readonly T[] | null>(null)

  readonly selected = signal<readonly T[]>([])
  readonly all = computed(
    () =>
      !!this.selected()?.length &&
      (this.selected().length === this.keys()?.length || null),
  )

  ngOnChanges() {
    this.selected.set([])
  }

  onToggle(key: T) {
    if (this.selected().includes(key)) {
      this.selected.update(selected => selected.filter(s => s !== key))
    } else {
      this.selected.update(selected => [...selected, key])
    }
  }
}
